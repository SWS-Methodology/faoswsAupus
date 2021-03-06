##' Compute Processed Production
##' 
##' This function estimates the required "production" values for processed 
##' products.  Of course, processed products don't really have true "production"
##' values, but this rather represents how much of the current commodity is 
##' available because of processing of a parent commodity.  The estimation of 
##' the required amount of production is done by examining the imbalance between
##' imports, exports, and food (i.e. production = exports + food - imports, or 0
##' if imports > exports + food).  This value is then standardized to provide an
##' estimated distribution for the parent product.
##' 
##' @param data The data.table containing the full dataset for standardization.
##' @param tree The commodity tree which provides the edge structure.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##'   
##' @return Nothing is returned, but the passed data.table "data" is updated
##'   with estimated distributions for the production for all elements which did
##'   not previously have distributions.
##'   

computeProcessedProduction = function(data, tree, standParams){
    ## Identify parent nodes as nodes that are never children.  We do this
    ## because we should only compute production for processed products, never
    ## for primary products.
    processingLevel = getCommodityLevel(tree, parentColname = "parentID",
                                        childColname = "childID")
    parentIDs = processingLevel[level == 0, node]
    
    ## Compute the production at the processing level as the balance of the
    ## other elements.
    data[, newProduction :=
             na2zero(.SD[element == standParams$exportCode, Value]) -
             na2zero(.SD[element == standParams$importCode, Value]) +
             na2zero(.SD[element == standParams$stockCode, Value]) +
             na2zero(.SD[element == standParams$foodCode, Value]) +
             na2zero(.SD[element == standParams$foodProcCode, Value]) +
             na2zero(.SD[element == standParams$feedCode, Value]) +
             na2zero(.SD[element == standParams$wasteCode, Value]) +
             na2zero(.SD[element == standParams$seedCode, Value]) +
             na2zero(.SD[element == standParams$industrialCode, Value]) +
             na2zero(.SD[element == standParams$touristCode, Value]) +
             na2zero(.SD[element == standParams$residualCode, Value]),
         by = c(standParams$mergeKey)]
    data[, newProduction := ifelse(newProduction < 0, 0, newProduction)]
    data[, newProductionSD := sqrt(
             na2zero(.SD[element == standParams$exportCode, standardDeviation])^2 +
             na2zero(.SD[element == standParams$importCode, standardDeviation])^2 +
             na2zero(.SD[element == standParams$stockCode, standardDeviation])^2 +
             na2zero(.SD[element == standParams$foodCode, standardDeviation])^2 +
             na2zero(.SD[element == standParams$foodProcCode, standardDeviation])^2 +
             na2zero(.SD[element == standParams$feedCode, standardDeviation])^2 +
             na2zero(.SD[element == standParams$wasteCode, standardDeviation])^2 +
             na2zero(.SD[element == standParams$seedCode, standardDeviation])^2 +
             na2zero(.SD[element == standParams$industrialCode, standardDeviation])^2 +
             na2zero(.SD[element == standParams$touristCode, standardDeviation])^2 +
             na2zero(.SD[element == standParams$residualCode, standardDeviation])^2),
         by = c(standParams$mergeKey)]
    ## Only keep the production value IF:
    ## 
    ## - The current value is missing (we don't want to overwrite official data)
    ## 
    ## - We're looking at a processed product
    ## 
    ## - we're looking at the production element (i.e. don't put this value in
    ## seed/imports/...!)
    data[element == standParams$productionCode &
             (!measuredItemCPC %in% parentIDs) &
             (is.na(Value) | Value == 0),
         c("Value", "standardDeviation") := list(newProduction, newProductionSD)]
    data[, c("newProduction", "newProductionSD") := NULL]
}