##' Standardize Tree
##' 
##' This function takes an input dataset and commodity tree and standardizes the
##' data according to the tree.
##' 
##' @param data A data.table object containing the data of interest.
##' @param tree The commodity tree, specified as a data.table object.  The 
##'   columns should be childVar (the commodity code of the child), parentVar 
##'   (the commodity code of the parent), extractionVar (numeric value 
##'   specifying the extraction rate), and shareVar (numeric value specifying 
##'   how the commodity should be split up).  There are also two optional 
##'   columns: targetVar (either "T", "B" or "F" indicating if that commodity is
##'   a target commodity, should be backward standardized, or should be forward 
##'   standardized) and standDev (containing the standard deviation estimates 
##'   which should be aggregated as well).  If the target is missing, everything
##'   is assumed to be backward standardized.  If no standDev is provided, a 
##'   deviation of 0 is assumed.  The actual names of the columns are specified 
##'   in standParams.
##' @param elements The element codes for nodes that should be standardized. 
##'   These correspond to the different "elements" of the FBS, such as 
##'   production, imports, exports, etc.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##' @param sugarHack Logical.  Indicates if the commodity tree should be edited 
##'   by this program to give the correction standardization of sugar?  This is 
##'   a hack and should be fixed, but for now it is generally necessary.
##'   
##' @return A data.table with the commodities standardized to the highest level 
##'   in the tree.
##'   

standardizeTree = function(data, tree, elements, standParams,
                           additiveElements = c(), sugarHack = TRUE){

    ## Assign parameters
    geoVar = standParams$geoVar
    yearVar = standParams$yearVar
    itemVar = standParams$itemVar
    elementPrefix = standParams$elementPrefix
    childVar = standParams$childVar
    parentVar = standParams$parentVar
    extractVar = standParams$extractVar
    shareVar = standParams$shareVar
    
    ## Data Quality Checks
    stopifnot(is(data, "data.table"))
    stopifnot(is(tree, "data.table"))
    stopifnot(c(geoVar, yearVar, itemVar, additiveElements,
              paste0(elementPrefix, elements)) %in%
                  colnames(data))
    stopifnot(c(geoVar, yearVar, childVar, parentVar, extractVar, shareVar)
              %in% colnames(tree))
    if(!all(sapply(data[, paste0(elementPrefix, c(elements)), with = FALSE],
                  is.numeric))){
        stop("Some of the elements passed are not numeric!")
    }
    if(!"target" %in% colnames(tree)){
        tree[, target := "B"]
    }
    stopifnot(all(tree[, target] %in% c("B", "T", "F")))
    if(!"standDev" %in% colnames(tree)){
        returnStandDev = FALSE
        tree[, standDev := 0]
    } else {
        returnStandDev = TRUE
    }
    stopifnot(all(tree[, standDev] >= 0))
    
    elements = paste0(elementPrefix, elements)
    
    ## Restructure the data for easier standardization
    standardizationData = data.table:::melt.data.table(
        data = data, measure.vars = elements,
        id.vars = c(geoVar, yearVar, itemVar),
        variable.name = "measuredElement", value.name = "Value")
    standardizationData[, measuredElement :=
                            gsub(elementPrefix, "", measuredElement)]
    
    ## To ensure commodities are standardized up multiple levels, we have to
    ## collapse the tree (otherwise if A -> B -> C in the tree, C may be
    ## standardized only to B and not to A, as desired).
    standKey = standParams$mergeKey[standParams$mergeKey != standParams$itemVar]
    tree = collapseEdges(edges = tree, parentName = standParams$parentVar,
                         childName = standParams$childVar,
                         extractionName = standParams$extractVar,
                         keyCols = standKey)
    
    ## Merge the tree with the node data
    tree[, c(parentVar, childVar, yearVar, geoVar) :=
             list(as.character(get(parentVar)), as.character(get(childVar)),
                  as.character(get(yearVar)), as.character(get(geoVar)))]
    setnames(standardizationData, itemVar, childVar)
    standardizationData[, c(childVar, yearVar, geoVar) :=
                            list(as.character(get(childVar)),
                                 as.character(get(yearVar)),
                                 as.character(get(geoVar)))]
    standardizationData = merge(standardizationData, tree,
                                by = c(yearVar, geoVar, childVar),
                                all.x = TRUE, allow.cartesian = TRUE)
    
    ##' If an element is not a child in the tree, then "standardize" it to
    ##' itself with a rate of 1 and a share of 1.
    standardizationData[is.na(get(parentVar)),
                        c(parentVar, extractVar, shareVar) :=
                            list(get(childVar), 1, 1)]
    ## Standardizing backwards is easy: we just take the value, divide by the 
    ## extraction rate, and multiply by the shares.  However, we don't 
    ## standardize the production element (because production of flour is 
    ## derived from the production of wheat already).  We standardize everything
    ## backwards, and then edges marked as forwards (i.e. target == "F") get
    ## standardized down.
    output = standardizationData[, list(
        Value = sum(Value/get(extractVar)*get(shareVar), na.rm = TRUE)),
        by = c(yearVar, geoVar,
               "measuredElement", parentVar)]
    
    forwardEdges = tree[target == "F", ]
    if(sugarHack){
        ## Hacking sugar tree!
        warning("HACK!  Manually editing the sugar tree (the only forward process) ",
                "because it's difficult to understand how to properly code it!")
        ## We don't want the 156 to 158 edge as this is an intermediate step.  And
        ## shares should all be 1, as we're moving forward now.
        forwardEdges = forwardEdges[childID == 162, ]
        forwardEdges[, share := 1]
        outputForward = merge(output, forwardEdges,
                              by = c(parentVar, yearVar, geoVar))
        update = outputForward[, list(Value = sum(Value*get(extractVar)*get(shareVar), na.rm = TRUE)),
                               by = c(yearVar, geoVar,
                                      "measuredElement", childVar)]
        outputForwardProd = outputForward
        outputForwardProd[, childID := parentID]
        outputForwardProd = outputForwardProd[, list(get(yearVar),
                                                     get(geoVar),
                                                     measuredElement,
                                                     get(childVar), Value)]
        outputForwardProd = unique(outputForwardProd)
        update = rbindlist(list(update, outputForwardProd))
        setnames(update, childVar, parentVar)
        ## Remove the old rows that got corrected in the update
        output = output[!output$parentID %in% forwardEdges$parentID, ]
        ## Bind back in the corrected rows
        output = rbind(output, update)
    }

    ## Reshape to put back into the same shape as the passed data
    setnames(output, parentVar, itemVar)
    output[, measuredElement := paste0(elementPrefix,
                                       measuredElement)]
    form = as.formula(paste(yearVar, "+", geoVar, "+", itemVar, "~ measuredElement"))
    output = dcast.data.table(data = output, formula = form, value.var = "Value",
                              fun.aggregate = mean, na.rm = TRUE)
    return(output)
}
