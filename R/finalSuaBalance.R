##' Residual Balance
##' 
##' This function forces a balance in the passed elements by allocating the 
##' imbalance to one element.
##' 
##' @param data The data.table containing the full dataset for standardization.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##' @param feedCommodities Sometimes excess supply will need to be allocated to 
##'   some processed product.  The default is to place it into food, but this 
##'   list specifies which elements should allocate such a difference to feed.
##' @param indCommodities Same as feedCommodities, but for commodities where we 
##'   allocate the difference to industrial utilization.
##' @param foodProcessCommodities Same as feedCommodities, but for commodities
##'   where we allocate the difference to food processing.
##' @param imbalanceThreshold The size that the imbalance must be in order for 
##'   an adjustment to be made.
##'   
##' @return Nothing is returned, but the Value column of the passed data.table 
##'   is updated.
##'   

residualBalance = function(data, standParams, feedCommodities = c(),
                           indCommodities = c(), primaryCommodities = c(),
                           foodProcessCommodities = c(), imbalanceThreshold = 10){
    p = standParams
    data[, imbalance := sum(ifelse(is.na(Value), 0, Value) *
            ifelse(element == p$productionCode, 1,
            ifelse(element == p$importCode, 1,
            ifelse(element == p$exportCode, -1,
            ifelse(element == p$stockCode, -1,
            ifelse(element == p$foodCode, -1,
            ifelse(element == p$foodProcCode, 0,
            ifelse(element == p$feedCode, -1,
            ifelse(element == p$wasteCode, -1,
            ifelse(element == p$seedCode, -1,
            ifelse(element == p$industrialCode, -1,
            ifelse(element == p$touristCode, -1,
            ifelse(element == p$residualCode, -1, 0))))))))))))),
         by = c(standParams$mergeKey)]
    data[, newValue := ifelse(is.na(Value), 0, Value) + imbalance]
    data[abs(imbalance) > 10 & (!get(standParams$itemVar) %in% primaryCommodities),
         Value := ifelse(
            (element == p$feedCode & get(p$itemVar) %in% feedCommodities) |
            (element == p$foodProcessingCode & get(p$itemVar) %in% foodProcessCommodities) |
            (element == p$industrialCode & get(p$itemVar) %in% indCommodities) |
            (element == p$foodCode & !(get(p$itemVar) %in%
                            c(indCommodities, feedCommodities, foodProcessCommodities))),
            newValue, Value)]
    data[, c("imbalance", "newValue") := NULL]
}