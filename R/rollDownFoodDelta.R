##' Roll Down Food Delta
##' 
##' Balancing of the SUAs is down at the primary product level.  This balancing 
##' will likely adjust the food for that primary commodity, and an adjustment of
##' the food value thus implies that the production of the children commodities 
##' should be likewise adjusted.
##' 
##' A negative change implies that production of children commodities should be 
##' reduced according to their variance.  A positive change implies that no 
##' adjustment is necessarily required, but it is still important to increase 
##' flour production, for example, so that the correct bran and germ amounts are
##' created.  Thus, production for first processing level commodities only 
##' should be increased if the delta value is positive, and these increases 
##' should be such that the production of the first processing level products 
##' follows the share ratios as closely as possible.
##' 
##' This code is thus rather complicated: we must propogate the change in the 
##' primary level food into adjustments in each of the processing amounts for 
##' all the children.  The production updates for these elements will then 
##' impose an adjustment to the balances of those SUAs (but only to the food 
##' element, as we do not want to change the balanced trade data).  This, then,
##' will impose adjustments to production of all of the processed elements from
##' this commodity, and so on.  Thus, we must update the entire SUA.
##' 
##' @param data The data.table containing the full dataset for standardization.
##' @param tree The commodity tree which provides the edge structure.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##' @param shares A data.table containing, for each parent commodity, the 
##'   allocation of it's processed values into it's children.  Thus, the shares 
##'   should sum to one when the sum is done within each parent.
##' @param delta The change in the food quantity due to balancing.  A positive 
##'   delta value implies that balancing the food increased it (over the 
##'   original food value).
##'   
##' @return A data.table
##'   

rollDownFoodDelta = function(data, tree, standParams, shares){
    ## Input checks
    if(!standParams$adjustVar %in% colnames(data))
        stop("standParams$adjustVar must be a column of data, otherwise there ",
             "is no adjustment to propogate down!")
    
    ## Remove unneeded columns
    tree = tree[, c(standParams$childVar, standParams$parentVar,
                standParams$extractVar, standParams$groupID),
            with = FALSE]
    
    ## In this function, we'll want to do merging/aggregating but not group by
    ## the item variable.
    localMergeKey = c(standParams$mergeKey[standParams$mergeKey != standParams$itemVar])
    
    ## Don't need to mess with non food/production data
    staticData = data[!element %in% c(standParams$foodCode,
                                      standParams$productionCode,
                                      paste0(standParams$foodCode, "_orig")), ]
    data = data[element %in% c(standParams$foodCode,
                               standParams$productionCode,
                               paste0(standParams$foodCode, "_orig")), ]
    
    processingLevel = getCommodityLevel(commodityTree = tree,
                                        parentColname = standParams$parentVar,
                                        childColname = standParams$childVar)
    range = processingLevel[, min(level):max(level)]
    for(i in range){
        ## Only consider parents at this current processing level
        mergeFilter = data.table(parentID = processingLevel[level == i, node])
        subTree = merge(tree, mergeFilter, by = standParams$parentVar)
        ## Food changes must be allocated back to processed products as well as
        ## back to the food at the current level.  So, we need to include an
        ## edge for each node back to itself:
        subTree = rbind(subTree, data.table(parentID = unique(subTree$parentID),
                                            childID = unique(subTree$parentID),
                                            extractionRate = 1,
                                            groupID = unique(subTree$parentID)))
        setnames(subTree, standParams$parentVar, standParams$itemVar)
        dataToUpdate = merge(data[element %in% standParams$foodCode, ], subTree,
                             by = standParams$itemVar, allow.cartesian = TRUE)
        ## Change colnames of dataToUpdate so that we can merge childID back to
        ## data again.
        setnames(dataToUpdate, c(standParams$itemVar, standParams$childVar),
                 c(standParams$parentVar, standParams$itemVar))
        dataToUpdate = merge(dataToUpdate,
                             ## Don't merge all the columns:
                             data[, !c("metFlag", "obsFlag", "adjustment"),
                                  with = FALSE],
                             by = c(standParams$itemVar, localMergeKey),
                             suffixes = c("", ".child"))
        ## If the child is the item itself (i.e. wheat to wheat), then we want
        ## the food element.  If it's a processed product, then we want the
        ## production element.
        dataToUpdate = dataToUpdate[(element.child == paste0(standParams$foodCode, "orig") &
                                         parentID != measuredItemCPC) |
                                    (element.child == standParams$productionCode &
                                         parentID == measuredItemCPC), ]
        
        stop("Stop code here")

        ## If parentID is NA, that means the node shouldn't be adjusted in this
        ## stage.  To allow this, have it roll up to itself with an extraction
        ## rate of 1.  Also, define the groupID to be the CPC code (which will
        ## certainly be unique across all "parents" of that CPC code, as all
        ## other groupID's are parentCPC-childCPC).
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