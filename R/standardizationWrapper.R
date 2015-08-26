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
##' 2.1 Since food quantities are now available, we can compute initial calorie 
##' estimates.
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
##' @param tree The commodity tree which details how elements can be processed 
##'   into other elements.  It does not, however, specify which elements get 
##'   aggregated into others.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##' @param standTree The commodity tree which details how elements are
##'   standardized/aggregated into other elements.  Defaults to tree, but is
##'   likely different.
##' @param nutrientData A data.table containing one column with the item codes 
##'   (and this column's name must match standParams$itemVar) and additional 
##'   columns representing nutrient information.  For example, you could have 4 
##'   columns: measuredItemCPC, calories, proteins, fats.  In the calories, 
##'   proteins, and fats columns there should be numeric values representing 
##'   multipliers to convert kilograms of the item into calories/proteins/fats. 
##'   If NULL, nothing is done with nutrients.
##' @param printCodes A list of the item codes which should be printed at each 
##'   step to show the progress of the algorithm.
##'   
##' @return A data.table containing the final balanced and standardized SUA 
##'   data.  Additionally, this table will have new elements in it if 
##'   nutrientData was provided.
##'   

standardizationWrapper = function(data, tree, standParams, standTree = tree,
                                  nutrientData = NULL, printCodes = c()){
    
    ## Reassign standParams to p for brevity
    p = standParams
    
    ## STEP 0: Data Quality Checks
    stopifnot(c(p$geoVar, p$yearVar, p$itemVar,
                "element", "Value") %in% colnames(data))
    stopifnot(c(p$childVar, p$parentVar, p$extractVar,
                p$targetVar, p$shareVar) %in% colnames(tree))
    stopifnot(c(p$childVar, p$parentVar, p$extractVar,
                p$targetVar, p$shareVar) %in% colnames(standTree))
    if(nrow(data[, .N, by = c(p$geoVar, p$yearVar)]) > 1)
        stop("standardizationWrapper works with one country/year at a time only!")
    if(any(is.na(tree[, get(p$childVar)]))){
        warning("Tree has some NA children.  Those edges have been deleted.")
        tree = tree[!is.na(get(p$childVar)), ]
    }
    if(any(is.na(standTree[, get(p$childVar)]))){
        warning("standTree has some NA children.  Those edges have been deleted.")
        standTree = standTree[!is.na(get(p$childVar)), ]
    }
    if(any(is.na(tree[, get(p$parentVar)]))){
        warning("Tree has some NA parents.  Those edges have been deleted.")
        tree = tree[!is.na(get(p$parentVar)), ]
    }
    if(any(is.na(standTree[, get(p$parentVar)]))){
        warning("standTree has some NA parents.  Those edges have been deleted.")
        standTree = standTree[!is.na(get(p$parentVar)), ]
    }
    if(!is.null(nutrientData)){
        stopifnot(p$itemVar %in% colnames(nutrientData))
        stopifnot(ncol(nutrientData) > 1)
        nutrientElements = colnames(nutrientData)[2:ncol(nutrientData)]
    } else {
        nutrientElements = c()
    }

    ## STEP 0.1: Print out table
    if(length(printCodes) > 0){
        cat("Initial SUA table:")
        old = copy(data)
        print(printSUATable(data = data, standParams = p, printCodes = printCodes))
    }
    
    ## STEP 0.2: Add missing element codes for commodities that are in the data
    data = addMissingElements(data, standParams)
        
    ## STEP 1: Process forward.
    data = processForward(data = data, tree = tree,
                          standParams = p)$data
    tree = tree[get(p$targetVar) != "F", ]
    if(length(printCodes) > 0){
        cat("\nSUA table after processing forward:")
        data = markUpdated(new = data, old = old, standParams = p)
        old = copy(data)
        print(printSUATable(data = data, standParams = p, printCodes = printCodes))
    }
    
    ## STEP 2: Balance some products by specifying which element should get the 
    ## residual.  Don't balance the "balancing level" (usually primary level) by
    ## passing NA.
    level = findProcessingLevel(tree, from = p$parentVar,
                                to = p$childVar, aupusParam = p)
    primaryEl = level[processingLevel == 0, get(p$itemVar)]
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
        data = markUpdated(new = data, old = old, standParams = p)
        old = copy(data)
        print(printSUATable(data = data, standParams = p,
                            printCodes = printCodes))
    }
    
    ## STEP 2.1 Compute calories
    if(!is.null(nutrientData)){
        data = merge(data, nutrientData, by = p$itemVar, all.x = TRUE)
        ## Convert nutrient values into total nutrient info using food
        ## allocation.
        sapply(nutrientElements, function(nutrient){
            data[, c(nutrient) := get(nutrient) * Value[element == p$foodCode],
                 by = c(p$itemVar)]
        })
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
    plotTree = copy(standTree)
    standTree = merge(standTree, mergeToTree, by = p$parentVar, all.x = TRUE)
    availability = calculateAvailability(standTree, p)
    standTree = collapseEdges(edges = standTree, parentName = p$parentVar,
                              childName = p$childVar,
                              extractionName = p$extractVar,
                              keyCols = NULL)
    standTree[, availability := NULL]
    standTree = merge(standTree, availability,
                      by = c(p$childVar, p$parentVar))
    standTree[, newShare := availability / sum(availability),
              by = c(p$childVar)]
    standTree[, c(p$shareVar) :=
                  ifelse(is.na(newShare), get(p$shareVar), newShare)]
    if(length(printCodes) > 0){
        cat("\nAvailability of parents/children:\n\n")
        print(standTree[get(p$childVar) %in% printCodes,
                   c(p$childVar, p$parentVar, p$extractVar, "availability"),
                   with = FALSE])
        plotTree = plotTree[!is.na(get(p$childVar)) & !is.na(get(p$parentVar)) &
                                get(p$childVar) %in% printCodes, ]
        plotSingleTree(edges = plotTree, parentColname = p$parentVar,
                       childColname = p$childVar,
                       extractionColname = p$extractVar, box.size = .06,
                       box.type = "circle", cex.txt = 1, box.prop = .5,
                       box.cex = 1)
    }

    ## STEP 4: Standardize commodities to balancing level
    data = finalStandardizationToPrimary(data = data, tree = standTree,
                                         standParams = p, sugarHack = FALSE,
                                         specificTree = FALSE,
                                         additiveElements = nutrientElements)
    if(length(printCodes) > 0){
        cat("\nSUA table after standardization:")
        data = markUpdated(new = data, old = old, standParams = p)
        old = copy(data)
        print(printSUATable(data = data, standParams = p,
                            printCodes = printCodes,
                            nutrientElements = nutrientElements,
                            printProcessing = FALSE))
    }
    
    ## STEP 5: Balance at the balancing level.
    data = data[element %in% c(p$productionCode, p$importCode, p$exportCode,
                               p$stockCode, p$foodCode, p$feedCode, p$seedCode,
                               p$touristCode, p$industrialCode, p$wasteCode,
                               nutrientElements), ]
    data[, nutrientElement := element %in% nutrientElements]
    warning("Not sure how to compute standard deviations!  Currently just 10% ",
            "of value!")
    data[, standardDeviation := Value * .1]
    data[!element %in% nutrientElements,
         balancedValue := balancing(param1 = sapply(Value, na2zero),
              param2 = sapply(standardDeviation, na2zero),
              sign = ifelse(element %in% c(p$productionCode, p$importCode), 1, -1),
              lbounds = ifelse(element %in% c(p$stockCode, p$touristCode), -Inf, 0),
              optimize = "constrOptim", constrTol = 1e-6)]
    ## To adjust calories later, compute the ratio for how much food has been 
    ## adjusted by.  This looks like a "mean", but really we're just using the
    ## mean to select the one non-NA element.
    data[, foodAdjRatio := mean(ifelse(element == p$foodCode,
                                       balancedValue / Value, NA),
                                na.rm = TRUE),
         by = c(p$itemVar)]
    ## The balancedValue will be given for all non-nutrient elements.  Update
    ## all these elements with their balanced values.
    data[!(nutrientElement), Value := balancedValue]
    if(length(printCodes) > 0){
        cat("\nSUA table after balancing:")
        data = markUpdated(new = data, old = old, standParams = p)
        old = copy(data)
        print(printSUATable(data = data, standParams = p, printCodes = printCodes,
                            printProcessing = FALSE,
                            nutrientElements = nutrientElements))
        data[, updateFlag := NULL]
    }

    ## STEP 6: Update calories of processed products proportionally based on
    ## updated food element values.
    data[(nutrientElement), Value := Value * foodAdjRatio]
    
    ## Last step: clean up column names of data
    data[, c("balancedValue", "nutrientElement", "foodAdjRatio") := NULL]
    return(data)
}