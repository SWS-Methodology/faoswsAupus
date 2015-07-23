##' Final Standardization to Primary Equivalent
##' 
##' After the full SUA has been balanced, all the commodities need to be rolled 
##' up to their primary equivalents.  This function does this, aggregating up
##' trade and food.
##' 
##' @param data The data.table containing the full dataset for standardization.
##' @param tree The commodity tree which provides the edge structure.  Note that
##'   this tree may be different than the processing tree, in particular if some
##'   commodities will be standardized into a different aggregate.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##'   
##' @return A data.table with the aggregated primary commodities.
##' 

finalStandardizationToPrimary = function(data, tree, standParams){
    keyCols = standParams$mergeKey[standParams$mergeKey != standParams$itemVar]
    standTree = collapseEdges(edges = tree, keyCols = keyCols)
    out = data[, standardizeTree(data = .SD, tree = standTree,
                                 standParams = standParams, elements = "Value"),
               by = element]
    
    ## Production and food for processing should never be standardized. 
    ## Instead, take the primary value directly.
    out = merge(out, data[, c(standParams$mergeKey, "element", "Value"),
                          with = FALSE],
                by = c(standParams$mergeKey, "element"),
                suffixes = c("", ".old"))
    out[element %in% c(standParams$productionCode, standParams$foodProcCode),
        Value := Value.old]
    out[, Value.old := NULL]
    
    return(out)
}