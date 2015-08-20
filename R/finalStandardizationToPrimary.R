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
##' @param sugarHack Logical.  See standardizeTree for details.
##'   
##' @return A data.table with the aggregated primary commodities.
##' 

finalStandardizationToPrimary = function(data, tree, standParams, sugarHack = TRUE){
    
    warning("We need a way of setting shares on tree based on processed ",
            "availability.  For example, beet sugar and cane sugar may be ",
            "processed into refined sugar.  When standardizing, we need to ",
            "standardize refined sugar back to these parents and split by some ",
            "shares.  The shares should **probably** be the split rates from ",
            "processing, if available, but such an approach is not currently ",
            "implemented.")
    
    keyCols = standParams$mergeKey[standParams$mergeKey != standParams$itemVar]
    standTree = collapseEdges(edges = tree, keyCols = keyCols)
    out = data[, standardizeTree(data = .SD, tree = standTree,
                                 standParams = standParams, elements = "Value",
                                 sugarHack = sugarHack),
               by = element]
    
    ## Add on the primary value for use in some special cases of
    ## standardization.
    out = merge(out, data[, c(standParams$mergeKey, "element", "Value"),
                          with = FALSE],
                by = c(standParams$mergeKey, "element"),
                suffixes = c("", ".old"), all.x = TRUE)
    
#     ## Standardizing food values is complicated.  The value reported under
#     ## "food" for the primary commodity includes the quantity eaten as such plus
#     ## the quantity of the primary commodity allocated to processing.  Thus, we
#     ## must determine (a) how much of the primary commodity is eaten as such and
#     ## (b) the standardized food values of just the processed products.
#     ## 
#     ## To determine (a), we compute the standardized production value and 
#     ## subtract from it the food value of the primary commodity.
#     ## 
#     ## To determine (b), we compute the standardized food values of the 
#     ## processed products.
#     ## 
#     ## Thus, standardized food is computed as:
#     ## Food(primary) - (Production(standardized) - Production(primary)) +
#     ## Food(standardized) - Food(primary) =
#     ## Production(primary) - Production(standardized) + Food(standardized)
#     correctFood = out[, .SD[element == standParams$productionCode, Value.old] -
#                         .SD[element == standParams$productionCode, Value] +
#                         .SD[element == standParams$foodCode, Value],
#                       by = c(standParams$mergeKey)]
#     setnames(correctFood, "V1", "Value")
#     correctFood[, element := standParams$foodCode]
#     out = merge(out, correctFood, suffixes = c("", ".new"),
#                 by = c(standParams$mergeKey, "element"), all.x = TRUE)
#     out[!is.na(Value.new), Value := Value.new]
#     out[, Value.new := NULL]
    
    ## Production should never be standardized. 
    ## Instead, take the primary value directly.
    out = merge(out, data[, c(standParams$mergeKey, "element", "Value"),
                          with = FALSE],
                by = c(standParams$mergeKey, "element"),
                suffixes = c("", ".old"), all.x = TRUE)
    out[element %in% c(standParams$productionCode),
        Value := Value.old]
    out[, Value.old := NULL]
    
    return(out)
}