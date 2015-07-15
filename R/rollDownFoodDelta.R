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
    
    return(data)
}