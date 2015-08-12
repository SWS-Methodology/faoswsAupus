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
##' @param specificTree Logical.  If TRUE, it is assumed that there is a unique 
##'   record in tree for each unique element of standParams$mergeKey.  If FALSE,
##'   extraction rates for each parent/child commodity are assumed to apply 
##'   globally over all time.
##' @param feedElements Sometimes excess production will need to be allocated to
##'   some processed product.  If all utilizations are N(0,0), we must allocate 
##'   it somewhere.  The default is to place it into food, but this list 
##'   specifies which elements should allocate such a difference to feed.
##'   
##' @return A data.table of the same form as data, but after the processing has
##'   been performed.
##'   

rollDownFoodDelta = function(data, tree, standParams, specificTree = FALSE,
                             feedElements = c()){
    ## Input checks
    if(!standParams$adjustVar %in% colnames(data))
        stop("standParams$adjustVar must be a column of data, otherwise there ",
             "is no adjustment to propogate down!")
    if(!standParams$shareVar %in% colnames(tree)){
        stop("standParams$shareVar must be a column of tree, otherwise there ",
             "will be no way to process down positive differences.")
    }
    
    ## Remove unneeded columns
    tree = tree[, c(standParams$childVar, standParams$parentVar,
                standParams$extractVar, standParams$groupID,
                standParams$shareVar),
            with = FALSE]
    
    ## In this function, we'll want to do merging/aggregating but often not
    ## group by the item variable.
    localMergeKey = c(standParams$mergeKey[standParams$mergeKey != standParams$itemVar])
    
    processingLevel = getCommodityLevel(commodityTree = tree,
                                        parentColname = standParams$parentVar,
                                        childColname = standParams$childVar)
    range = processingLevel[, min(level):(max(level)-1)]
    for(i in range){
        ## Only consider parents at this current processing level
        mergeFilter = data.table(parentID = processingLevel[level == i, node])
        subTree = merge(tree, mergeFilter, by = standParams$parentVar)
        
        #######################################################################
        # Propogate food for processing changes to production changes         #
        #######################################################################
        
        ## Step 1: Connect the food for processing to the tree
        setnames(subTree, standParams$parentVar, standParams$itemVar)
        dataToUpdate = merge(data[element %in% standParams$foodProcCode, ],
                             subTree, by = standParams$itemVar,
                             allow.cartesian = TRUE, all.y = TRUE)
        setnames(subTree, standParams$itemVar, standParams$parentVar)
        
        ## Step 2: Connect the other node of the tree to the production element
        ## of the children.
        setnames(dataToUpdate, c(standParams$itemVar, standParams$childVar),
                 c(standParams$parentVar, standParams$itemVar))
        dataToUpdate = merge(dataToUpdate,
                             ## Don't merge all the columns:
                             data[element == standParams$productionCode,
                                  !c("metFlag", "obsFlag", "adjustment"),
                                  with = FALSE],
                             by = c(standParams$itemVar, localMergeKey),
                             suffixes = c("", ".child"), allow.cartesian = TRUE)
        dataToUpdate[is.na(Value.child), Value.child := 0]
        dataToUpdate[, strictestProduct := Value.child / extractionRate ==
                         max(Value.child / extractionRate),
                     by = c(localMergeKey, "groupID")]
        dataToUpdate = dataToUpdate[(strictestProduct), ]
        dataToUpdate[, strictestProduct := NULL]
        
        ## Step 3: Allocate the difference to the production of the children.
        dataToUpdate[, adjustment.child := NA_real_]
        dataToUpdate[adjustment < 0, adjustment.child :=
                         ## Must convert processed back into parent for the balancing
                         balancing(param1 = c(rep(0, .N), adjustment[1])/c(extractionRate, 1),
                                   param2 = c(standardDeviation.child, 0)/c(extractionRate, 1),
                                   sign = rep(1, .N+1),
                                   lbounds = c(-Value.child, adjustment[1]),
                                   optimize = "constrOptim",
                                   constrTol = 1e-4)[1:.N]*
                         extractionRate,
                     by = c(localMergeKey, standParams$parentVar)]
        ## This isn't the best approach.  It just reassigns adjustments based on
        ## the shares.  The problem is that there is likely an imbalance 
        ## already, and so adjustments should be assigned so as to move to the 
        ## appropriate share rather than by actual shares.  For example, shares 
        ## could be .9/.05/.05 and the current ratios might be 0.88/0.04/0.08. 
        ## In this case, no allocation should be given to the third element but
        ## instead only to the first and second (to move closer to the shares).
        warning("More improvements could be made to improve the adjustment ",
                "allocated to approach shares")
        dataToUpdate[adjustment >= 0, adjustment.child :=
                         adjustment * share * extractionRate]
        ## Two adjustments may be applied to the same child, as would be the 
        ## case when we have changes in multiple parents flowing to one child. 
        ## Thus, we must aggregate at this point to get the total changes for
        ## each child.
        dataToUpdate =
            dataToUpdate[, adjustment.child := sum(adjustment.child),
                     by = c(standParams$mergeKey, "element",
                            standParams$extractVar,
                            "element.child", "Value.child",
                            "standardDeviation.child")]
        
        ## Step 4: Make by-products consistent by adjusting the production of 
        ## by-products to the largest element.  For example, suppose 85 kg of 
        ## wheat go to flour with an extraction rate of 0.85.  Then, if the 
        ## extraction rate for bran is 0.12 we should require 12 kilograms of 
        ## bran as well, even if we only need 4 (for example).  So, all 
        ## by-products should be adjusted to the largest value because we have 
        ## accounted for the largst value when computing the food requirement 
        ## for the parent commodity.
        dataToUpdate[, Value.child := Value.child +
                         ifelse(is.na(adjustment.child), 0, adjustment.child)]
        ## Add in the edges we're currently missing, but only for processes we
        ## want to include.
        if(specificTree){
            missingProducts = tree[groupID %in% dataToUpdate$groupID &
                                       !childID %in% dataToUpdate$measuredItemCPC,
                                   c(standParams$childVar, standParams$groupID,
                                     standParams$extractVar, localMergeKey),
                                   with = FALSE]
        } else {
            missingProducts = tree[groupID %in% dataToUpdate$groupID &
                                       !childID %in% dataToUpdate$measuredItemCPC,
                                   c(standParams$childVar, standParams$groupID,
                                     standParams$extractVar),
                                   with = FALSE]
            missingProducts[, mergeVar := 1]
            toMerge = unique(dataToUpdate[, localMergeKey, with = FALSE])
            toMerge[, mergeVar := 1]
            missingProducts = merge(missingProducts, toMerge, by = "mergeVar",
                                    allow.cartesian = TRUE)
            missingProducts[, mergeVar := NULL]
        }
        
        ## Add current production values to missingProducts
        dataVals = data[element == params$productionCode,
                        c(params$mergeKey, "Value"), with = FALSE]
        setnames(dataVals, params$itemVar, params$childVar)
        missingProducts = merge(missingProducts, dataVals,
                                by = c(localMergeKey, params$childVar),
                                all.x = TRUE)
        setnames(missingProducts, "Value", "Value.child")
        
        missingProducts[, element.child := standParams$productionCode]
        missingProducts[, c("adjustment.child") := 0]
        setnames(missingProducts, standParams$childVar, standParams$itemVar)
        dataToUpdate = rbindlist(list(dataToUpdate, missingProducts), fill = TRUE)
        ## Now, scale up production for by-products
        dataToUpdate[, maxParentAllocation := max(Value.child/extractionRate, na.rm = TRUE),
                     by = c(localMergeKey, standParams$groupID)]
        dataToUpdate[, adjustment.child := adjustment.child +
                         maxParentAllocation * extractionRate - Value.child]
        dataToUpdate[, maxParentAllocation := NULL]
        
        ## Step 5: Merge on the new data to the production
        dataToUpdate = unique(dataToUpdate[, c(standParams$itemVar,
                        "adjustment.child", localMergeKey), with = FALSE])
        dataToUpdate[, element := standParams$productionCode]
        data = merge(data, dataToUpdate, by = c(standParams$itemVar,
                                                localMergeKey, "element"),
                     all = TRUE)
        data[is.na(adjustment), adjustment := adjustment.child]
        data[!is.na(adjustment.child), Value := ifelse(is.na(Value), 0, Value) +
                 adjustment.child]
        data[, adjustment.child := NULL]

        #######################################################################
        # Propogate production changes to food and food for proc changes      #
        #######################################################################
        
        ## Step 1: Filter to only the production at this processing level
        filter = data.table(unique(subTree[[standParams$childVar]]))
        setnames(filter, standParams$itemVar)
        dataToUpdate = merge(data[element == standParams$productionCode, ],
                             filter, by = standParams$itemVar,
                             allow.cartesian = TRUE)
        
        ## Step 2: Connect the updated data to the non-production elements (but 
        ## the same item).  However, we must make sure we create all elements 
        ## for all items, so do a cartesian join first and then overwrite all 
        ## the values/standard deviations when data is available.  This is
        ## important as we wish to ensure all elements for by-products which may
        ## not be in the original data at all.
        dataToUpdate[, mergeDummy := 1]
        mergeElements = unique(data[element != standParams$productionCode,
                                    element])
        dataToUpdate = merge(data.table(mergeDummy = 1,
                                        element.child = mergeElements),
                             dataToUpdate, by = "mergeDummy",
                             allow.cartesian = TRUE)
        dataToUpdate[, mergeDummy := NULL]
        setnames(dataToUpdate, c("element", "element.child"),
                 c("element.parent", "element"))
        dataToUpdate = merge(dataToUpdate,
                             data[, c(standParams$mergeKey, "element", "Value",
                                      "standardDeviation"),
                                  with = FALSE],
                             by = c(standParams$mergeKey, "element"),
                             suffixes = c("", ".child"),
                             all.x = TRUE)
        dataToUpdate = dataToUpdate[adjustment != 0, ]
        ## If missing a distribution for a element, assume N(0,0)
        dataToUpdate[is.na(Value.child), c("Value.child",
                                           "standardDeviation.child") := 0]
        ## Some products may not have any adjustable products.  In this case,
        ## set the variance of food or feed to the adjustment value (technically
        ## any positive number should work, but making it big should make
        ## optimization easier).
        forced = dataToUpdate[, list(forced = all(standardDeviation.child <= 0)),
                              by = c(standParams$mergeKey)]
        forced = forced[(forced), ]
        if(nrow(forced) > 0){
            forced[, element := ifelse(get(standParams$itemVar) %in% feedElements,
                                       standParams$feedCode, standParams$foodCode)]
            dataToUpdate = merge(dataToUpdate, forced,
                                 by = c(standParams$mergeKey, "element"),
                                 all.x = TRUE)
            dataToUpdate[(forced), standardDeviation.child := adjustment]
            dataToUpdate[, forced := NULL]
        }
        
        ## Some of the code below will have issues if dataToUpdate is an empty
        ## data.table.  At this point, we can skip to the next step in the loop
        ## if dataToUpdate is empty anyways.
        if(nrow(dataToUpdate) == 0)
            next
        
        ## Step 3: Allocate the difference in the production to the other 
        ## elements.  Note that we therefore need no extraction rate or shares
        ## (positive adjustments will also be proportioned using the balancing
        ## method instead of the shares method above).
        dataToUpdate[, adjustment.child :=
                         balancing(param1 = c(rep(0, .N), adjustment[1]),
                                   param2 = c(standardDeviation.child, 0),
                                   sign = rep(1, .N+1),
                                   lbounds = c(-Value, adjustment[1]),
                                   optimize = "constrOptim")[1:.N],
                     by = c(standParams$mergeKey)]

        ## No by-products (all within commodity allocation) so there's no step
        ## 4 for this case.
        
        ## Step 5: Merge on the new data to the relevant elements
        dataToUpdate = dataToUpdate[, c(standParams$mergeKey, "adjustment.child",
                                      "element"), with = FALSE]
        data = merge(data, dataToUpdate,
                     by = c(standParams$mergeKey, "element"), all = TRUE)
        data[is.na(adjustment), adjustment := adjustment.child]
        data[!is.na(adjustment.child), Value := ifelse(is.na(Value), 0, Value) +
                 adjustment.child]
        data[, adjustment.child := NULL]
    }
    
    data[is.na(adjustment), adjustment := 0]
    
    return(data)
}