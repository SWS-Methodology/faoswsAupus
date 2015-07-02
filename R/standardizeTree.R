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

standardizeTree = function(data, tree, elements){

    ## Data Quality Checks
    stopifnot(is(data, "data.table"))
    stopifnot(is(tree, "data.table"))
    stopifnot(is(elements, "numeric"))
    stopifnot(c("geographicAreaFS", "timePointYearsSP", "measuredItemFS",
              paste0("Value_measuredElementFS_", elements)) %in%
                  colnames(data))
    stopifnot(c("timePointYearsSP", "geographicAreaFS", "childID",
                "parentID", "extractionRate", "share") %in% colnames(tree))
    if(!"target" %in% colnames(tree)){
        tree[, target := "B"]
    }
    
    elements = paste0("Value_measuredElementFS_", elements)
    
    ## Restructure the data for easier standardization
    standardizationData = data.table:::melt.data.table(
        data = data, measure.vars = elements,
        id.vars = c("geographicAreaFS", "timePointYearsSP", "measuredItemFS"),
        variable.name = "measuredElement", value.name = "Value")
    standardizationData[, measuredElement := gsub("Value_measuredElementFS_", "",
                                                  measuredElement)]
    
    ## Merge the tree with the node data
    tree[, c("parentID", "childID", "timePointYearsSP", "geographicAreaFS") :=
             list(as.character(parentID), as.character(childID),
                  as.character(timePointYearsSP), as.character(geographicAreaFS))]
    setnames(standardizationData, "measuredItemFS", "childID")
    standardizationData[, c("childID", "timePointYearsSP", "geographicAreaFS") :=
                            list(as.character(childID), as.character(timePointYearsSP),
                                 as.character(geographicAreaFS))]
    standardizationData = merge(standardizationData, tree,
                                by = c("timePointYearsSP", "geographicAreaFS",
                                       "childID"), all.x = TRUE,
                                allow.cartesian = TRUE)
    
    ##' If an element is not a child in the tree, then "standardize" it to
    ##' itself with a rate of 1 and a share of 1.
    standardizationData[is.na(parentID),
                        c("parentID", "extractionRate", "share") :=
                            list(childID, 1, 1)]
    ## Standardizing backwards is easy: we just take the value, divide by the 
    ## extraction rate, and multiply by the shares.  However, we don't 
    ## standardize the production element (because production of flour is 
    ## derived from the production of wheat already).  We standardize everything
    ## backwards, and then edges marked as forwards (i.e. target == "F") get
    ## standardized down.
    output = standardizationData[measuredElement != productionElement, list(
        Value = sum(Value/extractionRate*share, na.rm = TRUE)),
        by = c("timePointYearsSP", "geographicAreaFS",
               "measuredElement", "parentID")]
    
    forwardEdges = tree[target == "F", ]
    ## Hacking sugar tree!
    warning("HACK!  Manually editing the sugar tree (the only forward process) ",
            "because it's difficult to understand how to properly code it!")
    ## We don't want the 156 to 158 edge as this is an intermediate step.  And
    ## shares should all be 1, as we're moving forward now.
    forwardEdges = forwardEdges[childID == 162, ]
    forwardEdges[, share := 1]
    outputForward = merge(output, forwardEdges,
                          by = c("parentID", "timePointYearsSP", "geographicAreaFS"))
    update = outputForward[measuredElement != productionElement,
                           list(Value = sum(Value*extractionRate*share, na.rm = TRUE)),
                           by = c("timePointYearsSP", "geographicAreaFS",
                                  "measuredElement", "childID")]
    outputForwardProd = outputForward[measuredElement == productionElement, ]
    outputForwardProd[, childID := parentID]
    outputForwardProd = outputForwardProd[, list(timePointYearsSP,
                                                 geographicAreaFS,
                                                 measuredElement,
                                                 childID, Value)]
    outputForwardProd = unique(outputForwardProd)
    update = rbindlist(list(update, outputForwardProd))
    setnames(update, "childID", "parentID")
    ## Remove the old rows that got corrected in the update
    output = output[!output$parentID %in% forwardEdges$parentID, ]
    ## Bind back in the corrected rows
    output = rbind(output, update)
    
    ## Reshape to put back into the same shape as the passed data
    setnames(output, "parentID", "measuredItemFS")
    output[, measuredElement := paste0("Value_measuredElementFS_",
                                       measuredElement)]
    output = dcast.data.table(data = output,
        formula = timePointYearsSP + geographicAreaFS + measuredItemFS ~ measuredElement,
        value.var = "Value", fun.aggregate = mean, na.rm = TRUE)
    return(output)
}
