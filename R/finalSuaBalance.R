##' Final SUA Balance
##' 
##' After the rolling up of production requirements and the rolling back down of
##' the adjusted food for processing, we may still have commodities that have 
##' not been adjusted (for example, if production of a processed commodity is 
##' official, or if it's assumed zero because the imports can cover exports + 
##' consumption).  In these cases, we still need to ensure the SUAs are 
##' balanced, and so this function balances each of the SUAs that are out of 
##' balance.  It does this very simply: differences are allocated to food or 
##' feed (food unless the element is in feedElements).
##' 
##' @param data The data.table containing the full dataset for standardization.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##' @param feedElements Sometimes excess production will need to be allocated to
##'   some processed product.  If all utilizations are N(0,0), we must allocate 
##'   it somewhere.  The default is to place it into food, but this list 
##'   specifies which elements should allocate such a difference to feed.
##' @param imbalanceThreshold The size that the imbalance must be in order for 
##'   an adjustment to be made.
##'   
##' @return Nothing is returned, but the Value column of the passed data.table
##'   is updated.
##'   

finalSuaBalance = function(data, standParams, feedElements = c(),
                           imbalanceThreshold = 10){
    p = standParams
    data[, imbalance := sum(ifelse(is.na(Value), 0, Value) *
            ifelse(element == p$productionCode, 1,
            ifelse(element == p$importCode, 1,
            ifelse(element == p$exportCode, -1,
            ifelse(element == p$stockCode, -1,
            ifelse(element == p$foodCode, -1,
            ifelse(element == p$foodProcCode, -1,
            ifelse(element == p$feedCode, -1,
            ifelse(element == p$wasteCode, -1,
            ifelse(element == p$seedCode, -1,
            ifelse(element == p$industrialCode, -1,
            ifelse(element == p$touristCode, -1,
            ifelse(element == p$residualCode, -1, 0))))))))))))),
         by = c(standParams$mergeKey)]
    data[abs(imbalance) > 10, Value := ifelse(
        (element == p$foodCode & !get(p$itemVar) %in% feedElements) |
        (element == p$feedCode & get(p$itemVar) %in% feedElements),
            Value + imbalance, Value)]
    if(any(data$Value < 0, na.rm = TRUE)){
        warning("Some negative balanced values are occurring!  Investigation ",
                "needed!!!")
    }
    data[, imbalance := NULL]
}