##' Allocate Delta via Maximum Likelihood
##' 
##' @param param1 A vector of the first parameter for each of the elements.  For
##'   a normal distribution, this is the mean.
##' @param param2 A vector of the second parameter for each of the elements. For
##'   a normal distribution, this is the standard deviation.
##' @param dist A vector of the name of the distribution for each distribution. 
##'   Currently, only "Normal" is allowed.
##' @param sign A vector of the sign of each element.  These values should all
##'   be +1 or -1.
##'   

allocateDelta = function(param1, param2, dist = rep("Normal", length(param1)), sign){
    ## Input Checks
    N = length(param1)
    stopifnot(length(param2) == N)
    stopifnot(length(dist) == N)
    stopifnot(length(sign) == N)
    stopifnot(dist %in% "Normal")
    stopifnot(sign %in% c(-1, 1))
    if(any(dist == "Normal" & param2 < 1)){
        param2[dist == "Normal" & param2 < 1] = 1
        warning("Some standard deviations (for a normal distribution) were ",
                "small (<1) and could cause a problem for the optimization.  ",
                "These were adjusted up to 1.")
    }
    
    ## value should be of length = length(param1)-1 so that the final element
    ## can be computed.
    functionToOptimize = function(value){
        ## We must have sum(value * sign) = 0 if all elements are included. 
        ## Thus, the difference in the first N-1 is sum(value[-last] * sign[-last])
        residual = sum(value * sign[-N])
        value = c(value, -residual*sign[N])
        densities = ifelse(dist == "Normal",
                           dnorm(value * sign, mean = param1,
                                 sd = param2, log = TRUE),
                           NA)
        # Negative log-likelihood
        return(-sum(densities))
    }
    
    ## Initialize the mean to param1
    meanVec = param1[-N]
    optimizedResult = optim(par = meanVec, fn = functionToOptimize,
                            method = "L-BFGS-B", lower = 0)
    finalValues = optimizedResult$par
    residual = -sum(finalValues * sign[-N])
    finalValues = c(finalValues, residual * sign[N])
    return(finalValues)
}

## Testing:
# param1 = c(rep(1, 5), rep(-1,5))
# param2 = rep(1, 10)
# balance(param1, param2, sign = rep(1, 10))
# balance(param1, param2, sign = rep(-1, 10))
# param1 = c(10, 20, -35)
# param2 = c(1, 1, 1)
# balance(param1, param2, sign = c(1, 1, -1))
# param2 = c(1, 1, 3)
# balance(param1, param2, sign = c(1, 1, -1))
