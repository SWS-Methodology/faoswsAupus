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
    
    ## Helper function to convert NA's to 0 in the summation below
    na2zero = function(x){
        if(length(x) == 0) # No data for variable, return 0
            return(0)
        if(is.na(x))
            return(0) # Missing, return 0
        if(length(x) > 1)
            stop("Length of x should be 1 or 0")
        return(x)
    }
    
    processingLevel = getCommodityLevel(commodityTree = tree,
                                        parentColname = standParams$parentVar,
                                        childColname = standParams$childVar)
    range = processingLevel[, max(level):(min(level)+1)]
    for(i in range){
        ## Compute required production for processed products at the level which
        ## will be updated.  This "required production" is the production that
        ## is required to cover deficits due to exports, food, etc.
        toUpdate = computeProcessedProduction(
            data = data[get(standParams$itemVar) %in%
                            processingLevel[level == i, node], ],
            tree = tree, standParams = standParams)
        ## Only production will get udpated:
        toUpdate = toUpdate[element == standParams$productionCode, ]
        data = merge(data, toUpdate, suffixes = c("", ".new"),
                     by = c(standParams$mergeKey, "element"),
                     all = TRUE)
        data[!is.na(Value.new), c("Value", "standardDeviation", "metFlag", "obsFlag") :=
                 list(Value.new, standardDeviation.new, metFlag.new, obsFlag.new)]
        data[, c("Value.new", "standardDeviation.new", "metFlag.new", "obsFlag.new") :=
                 NULL]

        ## Determine availability for each commodity so that children with
        ## multiple parents can be allocated up appropriately.
        availability = data[, list(availability = 
            na2zero(.SD[element == standParams$productionCode, Value]) -
            na2zero(.SD[element == standParams$exportCode, Value]) +
            na2zero(.SD[element == standParams$importCode, Value]) -
            na2zero(.SD[element == standParams$stockCode, Value]) -
            na2zero(.SD[element == standParams$foodCode, Value]) -
            na2zero(.SD[element == standParams$foodProcCode, Value]) -
            na2zero(.SD[element == standParams$feedCode, Value]) -
            na2zero(.SD[element == standParams$wasteCode, Value]) -
            na2zero(.SD[element == standParams$seedCode, Value]) -
            na2zero(.SD[element == standParams$industrialCode, Value]) -
            na2zero(.SD[element == standParams$touristCode, Value]) -
            na2zero(.SD[element == standParams$residualCode, Value])),
        by = c(standParams$mergeKey)]
        availability[availability < 0, availability := 0]
        
        mergeFilter = data.table(childID = processingLevel[level == i, node])
        subTree = merge(tree, mergeFilter, by = standParams$childVar)
        setnames(subTree, standParams$childVar, standParams$itemVar)
        ## Create foodProc to hold the food required for processing data.  This
        ## will eventually get merged back into data.
        foodProc = merge(data[element == standParams$productionCode, ],
                         subTree, by = standParams$itemVar, all.x = TRUE,
                         ## Allow cartesian because one commodity may have
                         ## several parents
                         allow.cartesian = TRUE)
        ## Merge on availability for the parents to determine allocations
        setnames(availability, standParams$itemVar, standParams$parentVar)
        foodProc = merge(foodProc, availability,
                         by = c(localMergeKey, standParams$parentVar),
                         all.x = TRUE)
        foodProc[is.na(availability), availability := 0]
        foodProc[, totalParentAvailability := sum(availability),
                 by = c(standParams$mergeKey)]
        foodProc[, totalParents := .N, by = c(standParams$mergeKey)]
        foodProc[, Value := ifelse(totalParentAvailability == 0,
                                   ## If no availability, split equally among parents
                                   Value / totalParents,
                                   ## Otherwise, split by proportion of availability
                                   Value * availability / totalParentAvailability)]
        
        foodProc[, parentValue := Value / get(standParams$extractVar)]
        ## Standard deviation scales in the same way as the mean
        foodProc[, parentSd := standardDeviation / get(standParams$extractVar)]
        ## In the case of multiple commodities per unique group ID (i.e. 
        ## flour/bran/germ as children of wheat) determine which commodity gives
        ## the tightest requirement on wheat.  This is determined by just
        ## looking at the largest value (although including s.d. may also be a
        ## good idea at some point).
        foodProc[element == standParams$productionCode,
                 aggToParentFlag := ifelse(parentValue ==
                    max(parentValue, na.rm = TRUE), TRUE, FALSE),
                 by = c(localMergeKey, standParams$groupID)]
        foodProc = foodProc[(aggToParentFlag),
                            list(Value = sum(parentValue),
                                 standardDeviation = sqrt(sum(standardDeviation^2))),
                            by = c(localMergeKey, standParams$parentVar)]
        foodProc[, element := standParams$foodProcCode]

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