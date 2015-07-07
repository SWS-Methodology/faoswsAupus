##' Standardize Tree
##' 
##' This function takes an input dataset and commodity tree and standardizes the
##' data according to the tree.
##' 
##' @param data A data.table object containing the data of interest.
##' @param elements The element codes for nodes that should be standardized. 
##'   These correspond to the different "elements" of the FBS, such as 
##'   production, imports, exports, etc.
##' @param tree The commodity tree, specified as a data.table object.  The 
##'   columns should be childID (the commodity code of the child), parentID (the
##'   commodity code of the parent), target (either "T", "B" or "F" indicating 
##'   if that commodity is a target commodity, should be backward standardized, 
##'   or should be forward standardized), extractionRate (numeric value 
##'   specifying the extraction rate), and share (numeric value specifying how 
##'   the commodity should be split up).
##'   
##' @return A data.table with the commodities standardized to the highest level
##'   in the tree.
##'   

standardizeTree = function(data, tree, elements, geoVar = "geographicAreaFS",
                           yearVar = "timePointYearsSP",
                           itemVar = "measuredItemFS",
                           elementPrefix = "Value_measuredElementFS_",
                           childVar = "childID", parentVar = "parentID",
                           extractVar = "extractionRate", shareVar = "share"){

    ## Data Quality Checks
    stopifnot(is(data, "data.table"))
    stopifnot(is(tree, "data.table"))
    stopifnot(is(elements, "numeric"))
    stopifnot(c(geoVar, yearVar, itemVar,
              paste0(elementPrefix, elements)) %in%
                  colnames(data))
    stopifnot(c(geoVar, timeVar, childVar, parentVar, extractVar, shareVar)
              %in% colnames(tree))
    if(!"target" %in% colnames(tree)){
        tree[, target := "B"]
    }
    
    elements = paste0(elementPrefix, elements)
    
    ## Restructure the data for easier standardization
    standardizationData = data.table:::melt.data.table(
        data = data, measure.vars = elements,
        id.vars = c(areaVar, yearVar, itemVar),
        variable.name = "measuredElement", value.name = "Value")
    standardizationData[, measuredElement :=
                            gsub(elementPrefix, "", measuredElement)]
    
    ## Merge the tree with the node data
    tree[, c(parentVar, childVar, yearVar, areaVar) :=
             list(as.character(get(parentVar)), as.character(get(childVar)),
                  as.character(get(yearVar)), as.character(get(areaVar)))]
    setnames(standardizationData, itemVar, childVar)
    standardizationData[, c(childVar, yearVar, areaVar) :=
                            list(as.character(get(childVar)),
                                 as.character(get(yearVar)),
                                 as.character(get(areaVar)))]
    standardizationData = merge(standardizationData, tree,
                                by = c(yearVar, areaVar, childVar),
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
        by = c(timeVar, areaVar,
               "measuredElement", parentVar)]
    
    forwardEdges = tree[target == "F", ]
    ## Hacking sugar tree!
    warning("HACK!  Manually editing the sugar tree (the only forward process) ",
            "because it's difficult to understand how to properly code it!")
    ## We don't want the 156 to 158 edge as this is an intermediate step.  And
    ## shares should all be 1, as we're moving forward now.
    forwardEdges = forwardEdges[childID == 162, ]
    forwardEdges[, share := 1]
    outputForward = merge(output, forwardEdges,
                          by = c(parentVar, timeVar, areaVar))
    update = outputForward[, list(Value = sum(Value*get(extractVar)*get(shareVar), na.rm = TRUE)),
                           by = c(timeVar, areaVar,
                                  "measuredElement", childVar)]
    outputForwardProd = outputForward
    outputForwardProd[, childID := parentID]
    outputForwardProd = outputForwardProd[, list(get(yearVar),
                                                 get(areaVar),
                                                 measuredElement,
                                                 get(childVar), Value)]
    outputForwardProd = unique(outputForwardProd)
    update = rbindlist(list(update, outputForwardProd))
    setnames(update, childVar, parentVar)
    ## Remove the old rows that got corrected in the update
    output = output[!output$parentID %in% forwardEdges$parentID, ]
    ## Bind back in the corrected rows
    output = rbind(output, update)
    
    ## Reshape to put back into the same shape as the passed data
    setnames(output, parentVar, itemVar)
    output[, measuredElement := paste0(elementPrefix,
                                       measuredElement)]
    form = as.formula(paste(yearVar, "+", areaVar, "+", itemVar, "~ measuredElement"))
    output = dcast.data.table(data = output, formula = form, value.var = "Value",
                              fun.aggregate = mean, na.rm = TRUE)
    return(output)
}
