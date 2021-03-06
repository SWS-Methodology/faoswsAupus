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
##' 7. (only if fbsTree is provided) Sum all commodities up to their FBS level
##' categories.  This is the final step to prepare data for FAOSTAT.
##' 
##' @param data The data.table containing the full dataset for standardization.
##' @param tree The commodity tree which details how elements can be processed 
##'   into other elements.  It does not, however, specify which elements get 
##'   aggregated into others.
##' @param fbsTree This "tree" should just have three columns: 
##'   standParams$parentID, standParams$childID, and standParams$extractVar 
##'   (which if missing will just be assigned all values of 1).  This tree 
##'   specifies how SUA commodities get combined to form the FBS aggregates.  If
##'   NULL, the last step (aggregation to FBS codes) is skipped and data is 
##'   simply returned at SUA level.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
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

standardizationWrapper = function(data, tree, fbsTree = NULL, standParams,
                                  nutrientData = NULL, printCodes = c()){
    
    ## Reassign standParams to p for brevity
    p = standParams
    
    ## STEP 0: Data Quality Checks
    # Checks for data
    stopifnot(c(p$geoVar, p$yearVar, p$itemVar,
                "element", "Value") %in% colnames(data))
    if(!"standardDeviation" %in% colnames(data))
        data[, standardDeviation := NA]
    data[, c(p$geoVar) := as.character(get(p$geoVar))]
    data[, c(p$yearVar) := as.character(get(p$yearVar))]
    data[, c(p$itemVar) := as.character(get(p$itemVar))]
    # Checks for tree
    stopifnot(c(p$childVar, p$parentVar, p$extractVar,
                p$targetVar, p$shareVar) %in% colnames(tree))
    if(nrow(data[, .N, by = c(p$geoVar, p$yearVar)]) > 1)
        stop("standardizationWrapper works with one country/year at a time only!")
    if(any(is.na(tree[, get(p$childVar)]))){
        warning("tree has some NA children.  Those edges have been deleted.")
        tree = tree[!is.na(get(p$childVar)), ]
    }
    if(!p$standParentVar %in% colnames(tree)){
        warning("p$standParentVar is not in the colnames of tree!  No ",
                "commodities will be grafted onto a different tree!")
        tree[, c(p$standParentVar) := NA]
    }
    if(!p$standExtractVar %in% colnames(tree)){
        warning("p$standExtractVar is not in the colnames of tree!  No ",
                "new extraction rates will be used!")
        tree[!is.na(get(p$standParentVar)),
             c(p$standExtractVar) := get(p$extractVar)]
    }
    stopifnot(!is.na(tree[, get(p$extractVar)]))
    ## Check that all standParentVar are NA or a value, never ""
    stopifnot(tree[!is.na(get(p$standParentVar)), get(p$standParentVar)] != "")
    # Checks for fbsTree
    if(!is.null(fbsTree)){
        stopifnot(c(p$itemVar, p$extractVar, "fbsID1",
                    "fbsID2", "fbsID3", "fbsID4") %in% colnames(fbsTree))
    }
    if(any(is.na(tree[, get(p$parentVar)]))){
        warning("tree has some NA parents.  Those edges have been deleted.")
        tree = tree[!is.na(get(p$parentVar)), ]
    }
    # Checks for nutrientData
    if(!is.null(nutrientData)){
        stopifnot(p$itemVar %in% colnames(nutrientData))
        stopifnot(ncol(nutrientData) > 1)
        nutrientElements = colnames(nutrientData)[2:ncol(nutrientData)]
    } else {
        nutrientElements = c()
    }

    ## STEP 0.1: Add missing element codes for commodities that are in the data
    ## (so that we can print it).  Then, print the table!
    data = addMissingElements(data, p)
    if(length(printCodes) > 0){
        cat("Initial SUA table:")
        old = copy(data)
        print(printSUATable(data = data, standParams = p, printCodes = printCodes))
    }

    ## STEP 1: Process forward.
    data = processForward(data = data, tree = tree,
                          standParams = p)$data
    ## Delete nodes processed forward
    forwardParents = tree[get(p$targetVar) == "F", unique(get(p$parentVar))]
    tree = tree[!parentID %in% forwardParents, ]
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
    ## Add in elements not in the tree, as they are essentially parents
    nonTreeEl = data[[p$itemVar]]
    nonTreeEl = nonTreeEl[!nonTreeEl %in% level[[p$itemVar]]]
    primaryEl = c(primaryEl, nonTreeEl)
    foodProcEl = unique(tree[get(p$targetVar) == "F",
                             get(p$parentVar)])
    officialProd = data[element == p$productionCode & Value > 0,
                        get(p$itemVar)]
    ## Elements with official production shouldn't have their production 
    ## updated.  Instead, the food value should be updated, and this is what 
    ## will happen if that element is not specified to any of the groupings in
    ## balanceResidual()
    balanceResidual(data, p,
                    primaryCommodities = primaryEl,
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
    plotTree = copy(tree)
    tree = merge(tree, mergeToTree, by = p$parentVar, all.x = TRUE)
    availability = calculateAvailability(tree, p)
    tree = collapseEdges(edges = tree, parentName = p$parentVar,
                         childName = p$childVar,
                         extractionName = p$extractVar,
                         keyCols = NULL)
    tree[, availability := NULL]
    tree = merge(tree, availability,
                      by = c(p$childVar, p$parentVar))
    tree[, newShare := availability / sum(availability, na.rm = TRUE),
              by = c(p$childVar)]
    tree[, c(p$shareVar) :=
                  ifelse(is.na(newShare), get(p$shareVar), newShare)]
    tree[, newShare := NULL]
    if(length(printCodes) > 0){
        cat("\nAvailability of parents/children:\n\n")
        print(knitr::kable(tree[get(p$childVar) %in% printCodes,
                   c(p$childVar, p$parentVar, p$extractVar, "availability"),
                   with = FALSE]))
        plotTree = plotTree[!is.na(get(p$childVar)) & !is.na(get(p$parentVar)) &
                                get(p$childVar) %in% printCodes, ]
        if(nrow(plotTree) > 0){
            plotSingleTree(edges = plotTree, parentColname = p$parentVar,
                           childColname = p$childVar,
                           extractionColname = p$extractVar, box.size = .06,
                           box.type = "circle", cex.txt = 1, box.prop = .5,
                           box.cex = 1)
        }
    }

    ## STEP 4: Standardize commodities to balancing level
    data = finalStandardizationToPrimary(data = data, tree = tree,
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
                            printProcessing = TRUE))
    }
    
    ## STEP 5: Balance at the balancing level.
    data = data[element %in% c(p$productionCode, p$importCode, p$exportCode,
                               p$stockCode, p$foodCode, p$feedCode, p$seedCode,
                               p$touristCode, p$industrialCode, p$wasteCode,
                               nutrientElements, p$foodProcCode), ]
    data[, nutrientElement := element %in% nutrientElements]
    warning("Not sure how to compute standard deviations!  Currently just 10% ",
            "of value!")
    data[, standardDeviation := Value * .1]
    data[!element %in% nutrientElements,
         balancedValue := balancing(param1 = sapply(Value, na2zero),
              param2 = sapply(standardDeviation, na2zero),
              sign = ifelse(element %in% c(p$productionCode, p$importCode), 1, -1),
              lbounds = ifelse(element %in% c(p$stockCode, p$touristCode), -Inf, 0),
              optimize = "constrOptim", constrTol = 1e-6),
         by = c(p$itemVar)]
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
                            printProcessing = TRUE,
                            nutrientElements = nutrientElements))
        data[, updateFlag := NULL]
    }

    ## STEP 6: Update calories of processed products proportionally based on
    ## updated food element values.
    data[(nutrientElement), Value := Value * foodAdjRatio]
    if(length(printCodes) > 0){
        cat("\nSUA table with updated nutrient values:")
        data = markUpdated(new = data, old = old, standParams = p)
        old = copy(data)
        print(printSUATable(data = data, standParams = p, printCodes = printCodes,
                            printProcessing = TRUE,
                            nutrientElements = nutrientElements))
        data[, updateFlag := NULL]
    }
    data[, c("balancedValue", "nutrientElement", "foodAdjRatio") := NULL]
    
    ## STEP 7: Aggregate to FBS Level
    if(is.null(fbsTree)){
        # If no FBS tree, just return SUA-level results
        return(data)
    } else {
        out = computeFbsAggregate(data = data, fbsTree = fbsTree,
                                  standParams = p)
        printCodeTable = fbsTree[get(p$itemVar) %in% printCodes, ]
        p$mergeKey[p$mergeKey == p$itemVar] = "fbsID4"
        p$itemVar = "fbsID4"
        cat("\nFBS Table at first level of aggregation:\n")
        print(printSUATable(data = out[[1]], standParams = p,
                            printCodes = printCodeTable[, fbsID4],
                            printProcessing = TRUE))
        p$mergeKey[p$mergeKey == p$itemVar] = "fbsID3"
        p$itemVar = "fbsID3"
        cat("\nFBS Table at second level of aggregation:\n")
        print(printSUATable(data = out[[2]], standParams = p,
                            printCodes = printCodeTable[, fbsID3],
                            printProcessing = TRUE))
        p$mergeKey[p$mergeKey == p$itemVar] = "fbsID2"
        p$itemVar = "fbsID2"
        cat("\nFBS Table at third level of aggregation:\n")
        print(printSUATable(data = out[[3]], standParams = p,
                            printCodes = printCodeTable[, fbsID2],
                            printProcessing = TRUE))
        p$mergeKey[p$mergeKey == p$itemVar] = "fbsID1"
        p$itemVar = "fbsID1"
        cat("\nFBS Table at final level of aggregation:\n")
        print(printSUATable(data = out[[4]], standParams = p,
                            printCodes = printCodeTable[, fbsID1],
                            printProcessing = TRUE))
        return(out)
    }
}