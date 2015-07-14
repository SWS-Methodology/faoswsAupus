##' Roll up food
##' 
##' This function takes SUA level data and standardizes up the required 
##' production values for processed commodities into a food value for their 
##' parents.  The idea is that we have official and estimated data at the SUA 
##' level, and the imbalances at this level translate into an imbalance in their
##' parent commodities.  To capture/account for this imbalance, we roll it up 
##' into food at the top line.
##' 
##' @param data The data.table containing the full dataset for standardization.
##' @param tree The commodity tree which provides the edge structure.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##'   
##' @return The same data.table as was passed ("data") but with updated food
##'   distributions to account for standardized production of children
##'   commodities.
##'   

rollUpFood = function(data, tree, standParams){
    
    tree = tree[, c(standParams$childVar, standParams$parentVar,
                    standParams$extractVar, standParams$groupID),
                with = FALSE]
    
    ## In this function, we'll want to do merging/aggregating but not group by
    ## the item variable.
    localMergeKey = c(standParams$mergeKey[standParams$mergeKey != standParams$itemVar])
    
    processingLevel = getCommodityLevel(commodityTree = tree,
                                        parentColname = standParams$parentVar,
                                        childColname = standParams$childVar)
    range = processingLevel[, max(level):min(level)]
    for(i in range){
        mergeFilter = data.table(childID = processingLevel[level == i, node])
        subTree = merge(tree, mergeFilter, by = standParams$childVar)
        setnames(subTree, standParams$childVar, standParams$itemVar)
        data = merge(data, subTree, by = standParams$itemVar, all.x = TRUE)
        
        ## If parentID is NA, that means the node shouldn't be aggregated to 
        ## anything else.  To allow this, have it roll up to itself with an 
        ## extraction rate of 1.  Also, define the groupID to be the CPC code 
        ## (which will certainly be unique across all "parents" of that CPC 
        ## code, as all other groupID's are parentCPC-childCPC).
        ## 
        ## If there is no parent, the food value should simply be kept (in 
        ## addition to any aggregations coming from children nodes).  This is 
        ## the "transferFlag".
        data[, transferFlag := is.na(parentID)]
        data[(transferFlag),
             c(standParams$parentVar, standParams$extractVar, standParams$groupID) :=
                 list(get(standParams$itemVar), 1, get(standParams$itemVar))]
        data[, parentValue := Value / get(standParams$extractVar)]
        ## Standard deviation scales in the same way as the mean
        data[, parentSd := standardDeviation / get(standParams$extractVar)]
        ## In the case of multiple commodities per unique group ID (i.e. 
        ## flour/bran/germ as children of wheat) determine which commodity gives
        ## the tightest requirement on wheat.  This is determined by just
        ## looking at the largest value (although including s.d. may also be a
        ## good idea at some point).
        data[(transferFlag & element == standParams$foodCode) |
             (!transferFlag & element == standParams$productionCode),
             aggToParentFlag := ifelse(parentValue ==
                        max(parentValue, na.rm = TRUE), TRUE, FALSE),
             by = c(localMergeKey, standParams$groupID)]
        newFood = data[(aggToParentFlag), list(Value = sum(parentValue),
                                               standardDeviation = sqrt(sum(standardDeviation^2))),
                       by = c(localMergeKey, standParams$parentVar)]

        data[, c("parentValue", "parentSd", "aggToParentFlag", "transferFlag",
                 standParams$parentVar, standParams$extractVar, standParams$groupID) := NULL]
        
        ## Put the aggregated food distributions back into the main dataset
        setnames(newFood, standParams$parentVar, standParams$itemVar)
        data = merge(data, newFood, by = standParams$mergeKey,
                     suffixes = c("", ".new"), all.x = TRUE)
        data[!is.na(Value.new) & element == standParams$foodCode,
             c("Value", "standardDeviation") :=
                 list(Value.new, standardDeviation.new)]
        deleteColumns = colnames(data)[grepl(".new", colnames(data))]
        data[, deleteColumns := NULL, with = FALSE]
    }
    
    return(data)
}