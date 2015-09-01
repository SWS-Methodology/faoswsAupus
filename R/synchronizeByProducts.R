##' Synchronize By-Products
##' 
##' @param data The data.table containing the full dataset for standardization.
##' @param tree The commodity tree which provides the edge structure.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##' @param specificTree Logical.  If TRUE, it is assumed that there is a unique 
##'   record in tree for each unique element of standParams$mergeKey.  If FALSE,
##'   extraction rates for each parent/child commodity are assumed to apply 
##'   globally over all time.
##'   
##' @return A data.table of the same form as data, but after the processing has
##'   been performed.
##' 

synchronizeByProducts = function(data, tree, standParams, specificTree = FALSE){

    ## In this function, we'll want to do merging/aggregating but often not
    ## group by the item variable.
    localMergeKey = standParams$mergeKey[standParams$mergeKey != standParams$itemVar]
    if(specificTree){
        aggKey = c(standParams$mergeKey[standParams$mergeKey != standParams$itemVar],
                   standParams$groupID)
    } else {
        aggKey = c(standParams$groupID)
    }
    
    standTree = copy(tree)
    setnames(standTree, standParams$childVar, standParams$itemVar)
    dataToUpdate = merge(data[element == standParams$productionCode, ],
                              standTree, by = standParams$itemVar)
    dataToUpdate[, count := .N, by = aggKey]
    dataToUpdate = dataToUpdate[count > 1, c(colnames(data), standParams$groupID,
                                             standParams$extractVar), with = FALSE]

    if(specificTree){
        missingProducts = tree[groupID %in% dataToUpdate$groupID &
                                   !childID %in% dataToUpdate$measuredItemCPC,
                               c(standParams$childVar, standParams$groupID,
                                 standParams$extractVar, localMergeKey),
                               with = FALSE]
    } else {
        missingProducts = tree[groupID %in% dataToUpdate$groupID &
                                   !childID %in% dataToUpdate$measuredItemCPC,
                               c(standParams$childVar, standParams$groupID,
                                 standParams$extractVar),
                               with = FALSE]
        missingProducts[, mergeVar := 1]
        toMerge = unique(dataToUpdate[, localMergeKey, with = FALSE])
        toMerge[, mergeVar := 1]
        missingProducts = merge(missingProducts, toMerge, by = "mergeVar",
                                allow.cartesian = TRUE)
        missingProducts[, mergeVar := NULL]
    }
        
    ## Add current production values to missingProducts
    dataVals = data[element == params$productionCode,
                    c(params$mergeKey, "Value"), with = FALSE]
    setnames(dataVals, params$itemVar, params$childVar)
    missingProducts = merge(missingProducts, dataVals,
                            by = c(localMergeKey, params$childVar),
                            all.x = TRUE)
    missingProducts[is.na(Value), Value := 0]
    setnames(missingProducts, standParams$childVar, standParams$itemVar)
    missingProducts[, element := standParams$productionCode]
    dataToUpdate = rbindlist(list(dataToUpdate, missingProducts), fill = TRUE)
    
    ## Compute the required allocation from the parent for all commodities in a
    ## group, and take the max.
    dataToUpdate[, requiredParent := max(Value / get(standParams$extractVar)),
                 by = aggKey]
    dataToUpdate[, Value := requiredParent * get(standParams$extractVar)]
    
    ## Adjust the original data
    dataToUpdate = unique(dataToUpdate[, c(standParams$itemVar,
                    "Value", "element", localMergeKey), with = FALSE])
    dataToUpdate[, element := standParams$productionCode]
    data = merge(data, dataToUpdate, by = c(standParams$itemVar,
                                            localMergeKey, "element"),
                 all = TRUE, suffixes = c("", ".new"))
    data[!is.na(Value.new), Value := Value.new]
    data[, Value.new := NULL]
}