##' Full Standardization Process
##' 
##' This function implements the new standardization process.  The algorithm is 
##' as follows:
##' 
##' 1. Any edges with a target of "F" should be immediately "processed forward".
##' This amounts to computing the residual and allocating it to food processing,
##' and, hence, to production of the children of this node.
##' 
##' 2. Balance the processed products in the SUA by creating production of 
##' processed products.  If a node is set to be processed forward, then it is 
##' also balanced via a residual (i.e. food processing).
##' 
##' 3. Availability at the "balancing level" (usually primary level, but 
##' possibly lower if a node is processed forward or standardized in a different
##' tree) is determined.  Note that at this point, all edges designated with an 
##' "F" (i.e. forward) will have been removed, and so "balancing level" is the 
##' same as the top node of the tree.  This availability defines shares for 
##' standardization.  If no availability of parents is available, the initial
##' shares are used (in tree[, get(standParams$shareVar)]).
##' 
##' 4. Standardize commodities according to the commodity tree.  This defines 
##' the food processing element at balancing level.
##' 
##' 5. Balance at the balancing level.
##' 
##' 6. Update calories of processed products proportionally based on updated 
##' food element values.
##' 
##' @param data The data.table containing the full dataset for standardization.
##' @param tree
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##'   
##' @return
##' 

standardizationWrapper = function(data, tree, standParams, printCodes = c()){
    # load("~/Documents/Github/privateFAO/OrangeBook/preStandardizedWheat.RData")
    # data = wheatData
    # tree = fread("~/Documents/Github/privateFAO/OrangeBook/standardizationTree.csv")
    # printCodes = c("0111", "23110", "23140.02")
    # standParams = defaultStandardizationParameters()
    # files = dir("~/Documents/Github/faoswsAupus/R", full.names = TRUE)
    # temp = sapply(files, source)
    # files = dir("~/Documents/Github/Balancing_ML/R", full.names = TRUE, pattern = "*.R$")
    # temp = sapply(files[1:4], source)
    # standardizationWrapper(data, tree, standParams, printCodes)
    
    ## Reassign standParams to p for brevity
    p = standParams
    
    ## STEP 0: Data Quality Checks
    stopifnot(c(p$geoVar, p$yearVar, p$itemVar,
                "element", "Value") %in% colnames(data))
    stopifnot(c(p$childVar, p$parentVar, p$extractVar,
                p$targetVar, p$shareVar) %in% colnames(tree))
    if(nrow(data[, .N, by = c(p$geoVar, p$yearVar)]) > 1)
        stop("standardizationWrapper works with one country/year at a time only!")
    if(any(is.na(tree[, get(p$childVar)]))){
        warning("Tree has some NA children.  Those edges have been deleted.")
        tree = tree[!is.na(get(p$childVar)), ]
    }
    if(any(is.na(tree[, get(p$parentVar)]))){
        warning("Tree has some NA parents.  Those edges have been deleted.")
        tree = tree[!is.na(get(p$parentVar)), ]
    }

    if(length(printCodes) > 0){
        cat("Initial SUA table:")
        print(printSUATable(data = data, standParams = p, printCodes = printCodes))
    }
        
    ## STEP 1: Process forward.
    data = processForward(data = data, tree = tree,
                          standParams = p)$data
    tree = tree[get(p$targetVar) != "F", ]
    if(length(printCodes) > 0){
        cat("\nSUA table after processing forward:")
        print(printSUATable(data = data, standParams = p, printCodes = printCodes))
    }
    
    ## STEP 2: Balance some products by specifying which element should get the 
    ## residual.  Don't balance the "balancing level" (usually primary level) by
    ## passing NA.
    level = findProcessingLevel(tree, from = p$parentVar,
                                to = p$childVar, aupusParam = p)
    primaryEl = level[processingLevel == 0, get(p$itemVar)]
    prodEl = level[processingLevel > 0, get(p$itemVar)]
    foodProcEl = unique(tree[get(p$targetVar) == "F",
                             get(p$parentVar)])
    officialProd = data[element == p$productionCode & Value > 0,
                        get(p$itemVar)]
    ## Elements with official production shouldn't have their production 
    ## updated.  Instead, the food value should be updated, and this is what 
    ## will happen if that element is not specified to any of the groupings in
    ## balanceResidual()
    prodEl = setdiff(prodEl, officialProd)
    balanceResidual(data, p,
                    primaryCommodities = primaryEl,
                    prodCommodities = prodEl,
                    foodProcessCommodities = foodProcEl
                    )
    if(length(printCodes) > 0){
        cat("\nSUA table after balancing processed elements:")
        print(printSUATable(data = data, standParams = p, printCodes = printCodes))
    }
    
    ## STEP 3: Compute availability and hence shares
    data[, availability := sum(ifelse(is.na(Value), 0, Value) *
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
         by = c(p$mergeKey)]
    # There's only one availability value per group, but we need an aggregation
    # function so we use mean.
    mergeToTree = data[, list(availability = mean(availability)),
                        by = c(p$itemVar)]
    setnames(mergeToTree, p$itemVar, p$parentVar)
    tree = merge(tree, mergeToTree, by = p$parentVar, all.x = TRUE)
    availability = calculateAvailability(tree, p)
    tree = collapseEdges(edges = tree, parentName = p$parentVar,
                         childName = p$childVar,
                         extractionName = p$extractVar,
                         keyCols = NULL)
    tree[, availability := NULL]
    tree = merge(tree, availability, by = c(p$childVar,
                                            p$parentVar))
    tree[, newShare := availability / sum(availability),
         by = c(p$childVar)]
    tree[, c(p$shareVar) := ifelse(is.na(newShare),
                                             get(p$shareVar),
                                             newShare)]
    if(length(printCodes) > 0){
        cat("\nAvailability of parents/children:\n\n")
        print(tree[parentID %in% printCodes | childID %in% printCodes, ])
    }

    ## STEP 4: Standardize commodities to balancing level
    data = finalStandardizationToPrimary(data = data, tree = tree,
                                         standParams = p, sugarHack = FALSE,
                                         specificTree = FALSE)
    if(length(printCodes) > 0){
        cat("\nSUA table after standardization:")
        printSUATable(data = data, standParams = p, printCodes = printCodes)
    }
    
    ## STEP 5: Balance at the balancing level.
    data = data[element %in% c(p$productionCode, p$importCode, p$exportCode,
                               p$stockCode, p$foodCode, p$feedCode, p$seedCode,
                               p$touristCode, p$industrialCode, p$wasteCode), ]
    warning("Not sure how to compute standard deviations!  Currently just 10% ",
            "of value!")
    data[, standardDeviation := Value * .1]
    data[, balancedValue := balancing(param1 = sapply(Value, na2zero),
              param2 = sapply(standardDeviation, na2zero),
              sign = ifelse(element %in% c(p$productionCode, p$importCode), 1, -1),
              lbounds = ifelse(element %in% c(p$stockCode, p$touristCode), -Inf, 0),
              optimize = "constrOptim", constrTol = 1e-6)]
    data[, Value := balancedValue]
    if(length(printCodes) > 0){
        cat("\nSUA table after balancing:")
        print(printSUATable(data = data, standParams = p, printCodes = printCodes,
                            printProcessing = FALSE))
    }

    ## STEP 6: Update calories of processed products proportionally based on
    ## updated food element values.
    
    
}