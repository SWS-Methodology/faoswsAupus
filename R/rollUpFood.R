##' Roll up food
##' 
##' This function takes SUA level data and standardizes up the required 
##' production values for processed commodities into a food value for their 
##' parents.  The idea is that we have official and estimated data at the SUA 
##' level, and the imbalances at this level translate into an imbalance in their
##' parent commodities.  To capture/account for this imbalance, we roll it up 
##' into food at the top line.  Note that the original estimates for food will
##' be stored in this table under a new element code: the current food element
##' code pasted with "_orig".  These original values need to be saved somewhere,
##' and this seemed like a reasonable place.
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

## Updates:
## 
## - At some point, we may want to adjust the "most stringent requirement" in
## the case of by-products.  Currently, it just looks at the largest mean value
## rather than incorporating standard deviation.

rollUpFood = function(data, tree, standParams){
    
    tree = tree[, c(standParams$childVar, standParams$parentVar,
                    standParams$extractVar, standParams$groupID),
                with = FALSE]
    
    ## In this function, we'll want to do merging/aggregating but not group by
    ## the item variable.
    localMergeKey = c(standParams$mergeKey[standParams$mergeKey != standParams$itemVar])
    
#     ## Save the original estimates for food
#     toBind = data[element == standParams$foodCode, ]
#     toBind[, element := paste0(element, "_orig")]
#     data = rbind(data, toBind)
    
    processingLevel = getCommodityLevel(commodityTree = tree,
                                        parentColname = standParams$parentVar,
                                        childColname = standParams$childVar)
    range = processingLevel[, max(level):min(level)]
    for(i in range){
        mergeFilter = data.table(childID = processingLevel[level == i, node])
        subTree = merge(tree, mergeFilter, by = standParams$childVar)
        setnames(subTree, standParams$childVar, standParams$itemVar)
        data = merge(data, subTree, by = standParams$itemVar, all.x = TRUE)
        
        data[, parentValue := Value / get(standParams$extractVar)]
        ## Standard deviation scales in the same way as the mean
        data[, parentSd := standardDeviation / get(standParams$extractVar)]
        ## In the case of multiple commodities per unique group ID (i.e. 
        ## flour/bran/germ as children of wheat) determine which commodity gives
        ## the tightest requirement on wheat.  This is determined by just
        ## looking at the largest value (although including s.d. may also be a
        ## good idea at some point).
        data[element == standParams$productionCode,
             aggToParentFlag := ifelse(parentValue ==
                        max(parentValue, na.rm = TRUE), TRUE, FALSE),
             by = c(localMergeKey, standParams$groupID)]
        foodProc = data[(aggToParentFlag),
                        list(Value = sum(parentValue),
                             standardDeviation = sqrt(sum(standardDeviation^2))),
                        by = c(localMergeKey, standParams$parentVar)]
        foodProc[, element := standParams$foodProcCode]

        data[, c("parentValue", "parentSd", "aggToParentFlag", "transferF",
                 standParams$parentVar, standParams$extractVar, standParams$groupID) := NULL]
        
        ## Put the aggregated food distributions back into the main dataset
        setnames(foodProc, standParams$parentVar, standParams$itemVar)
        data = merge(data, foodProc, by = c(standParams$mergeKey, "element"),
                     all.x = TRUE, suffixes = c("", ".new"))
        data[!is.na(Value.new), c("Value", "standardDeviation") :=
                 list(Value.new, standardDeviation.new)]
        data[, c("Value.new", "standardDeviation.new") := NULL]
    }
    
    return(data)
}