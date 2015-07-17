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
    importVals = standardizeTree(data = toPrint[element == params$importCode, ],
                                 tree = standTree, standParams = params,
                                 elements = "Value")
    exportVals = standardizeTree(data = toPrint[element == params$exportCode, ],
                                 tree = standTree, standParams = params,
                                 elements = "Value")
    foodVals = standardizeTree(data = toPrint[element == params$foodCode, ],
                               tree = standTree, standParams = params,
                               elements = "Value")
#     castFormula = paste(paste(standParams$mergeKey, collapse = "+"), "~ element")
#     data[, element := ifelse(element == standParams$productionCode, "Production",
#                       ifelse(element == standParams$importCode, "Imports",
#                       ifelse(element == standParams$exportCode, "Exports",
#                       ifelse(element == standParams$stockCode, "StockChange",
#                       ifelse(element == standParams$foodCode, "Food",
#                       ifelse(element == standParams$feedCode, "Feed",
#                       ifelse(element == standParams$seedCode, "Seed",
#                       ifelse(element == standParams$wasteCode, "Waste",
#                       ifelse(element == standParams$industrialCode, "Industrial",
#                       ifelse(element == standParams$touristCode, "Tourist",
#                       ifelse(element == standParams$residualCode, "Residual",
#                              "NotApplicable")))))))))))]
#     output = dcast.data.table(data = data, formula = castFormula,
#                               value.var = "Value", fun.aggregate = mean)
    ## Filter data based on merge keys (don't want all SUA commodities)
    output = merge(data, importVals[, standParams$mergeKey, with = FALSE],
                   by = standParams$mergeKey)
    
    ## Imports
    importVals[, element := standParams$importCode]
    output = merge(output, importVals, by = c(standParams$mergeKey, "element"),
                   all.x = TRUE, suffixes = c("", ".new"))
    output[, Value := ifelse(is.na(Value.new), Value, Value.new)]
    output[, Value.new := NULL]

    ## Exports
    exportVals[, element := standParams$exportCode]
    output = merge(output, exportVals, by = c(standParams$mergeKey, "element"),
                   all.x = TRUE, suffixes = c("", ".new"))
    output[, Value := ifelse(is.na(Value.new), Value, Value.new)]
    output[, Value.new := NULL]

    ## Food
    foodVals[, element := standParams$foodCode]
    output = merge(output, foodVals, by = c(standParams$mergeKey, "element"),
                   all.x = TRUE, suffixes = c("", ".new"))
    ## Don't count "food" of primary as that's just processed.
    output[, Value := ifelse(is.na(Value.new), Value, Value.new - Value)]
    output[, Value.new := NULL]

    return(output)
}