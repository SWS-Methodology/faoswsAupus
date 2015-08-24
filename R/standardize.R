##' Standardize
##' 
##' This function implements the new standardization process.  The algorithm is 
##' as follows:
##' 
##' 1. Balance the processed products in the SUA by creating production of 
##' processed products.  If a node is set to be processed forward, then it is 
##' also balanced via a residual (i.e. food processing) and processed forward.
##' 
##' 2. Availability at the "balancing level" (usually primary level, but 
##' possibly lower if a node is processed forward or standardized in a different
##' tree) is determined.
##' 
##' 3. Deficits are standardized according to availability.  This must be done 
##' by looping, and availability is recalculated at each step (otherwise if one 
##' node is the parent of many different children, we could have utilization 
##' vastly exceeding supply).  This defines the food processing element at
##' balancing level.
##' 
##' 4. Balance at the balancing level.

standardizeWrapper = function(data, tree){
    
}