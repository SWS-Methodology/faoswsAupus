##' Standardize
##' 
##' This function standardizes the SUA level commodities using the logic 
##' implemented by Marteen Van't Reet in "Flexible Aggregation of FAO's Supply 
##' Utilization Accounts"
##' 
##' @param aupusData A list of two data.table objects, the names of which should
##'   be "nodes" and "edges".  This object thus contains the AUPUS data.  This 
##'   object is usually the output from the Aupus function.
##' @param fbsElements The element codes for nodes that should be standardized. 
##'   These correspond to the different "elements" of the FBS, such as 
##'   production, imports, exports, etc.  Usually the default value will not 
##'   need to be modified (it has production (51), imports (61), exports (91), 
##'   feed (101), seed (111), waste (121), food (141), industrial uses (151), 
##'   and to stocks (71)).
##'   
##' @return A list of two elements: suaOutput and fbsOutput.  The first, 
##'   suaOutput, is a data.table with the SUA data standardized back to the 
##'   primary commodity.  The second, fbsOutput, is a list of four data.tables, 
##'   each of the same format as suaOutput.  The first element of this list is 
##'   suaOutput standardized to the first FBS level, the second element is the 
##'   second FBS level, etc.  Higher FBS levels simply group more commodities
##'   together.
##'   

standardize = function(aupusData, fbsElements = c(51, 61, 91, 101, 111, 121,
                                                  141, 151, 71), fbsTree, suaTree){
    productionElement = 51
    ## Load fbsTree and suaTree
    
#     warning("Hacking the fbsTree to switch some Vegetables, Other with ",
#             "Fruits, Other")
#     fbsTree
    
    warning("Hacking the commodity tree!  These fixes are not stable!")
#     suaTree = suaTree[!parentID %in% c(267, # Sunflower Seed
#                                        866, # Cattle (live-weight)
#                                        976, # Sheep (live-weight)
#                                        1034, # Pig (live-weight)
#                                        1057), ] # Chickens (live-weight)
    suaTree = suaTree[childID == 162 | !childID %in% fbsTree$commodityID, ]

    ## Input checks

    ## Make fbsElements the column names rather than numeric values
    fbsElements = paste0("Value_measuredElementFS_", fbsElements)
    
    ## Restructure the data for easier standardization
    standardizationData = data.table:::melt.data.table(
        data = aupusData$nodes, measure.vars = fbsElements,
        id.vars = c("geographicAreaFS", "timePointYearsSP", "measuredItemFS"),
        variable.name = "measuredElement", value.name = "Value")
    standardizationData[, measuredElement := gsub("Value_measuredElementFS_", "",
                                                  measuredElement)]
    
    ## Get the standardization tree using the country specific shares/extraction
    ## rates when available, and the default values otherwise.
    specificTree = getSpecificTree(aupusData = aupusData, suaTree = suaTree)

    ## Merge the tree with the node data
    setnames(standardizationData, "measuredItemFS", "childID")
    standardizationData = merge(standardizationData, specificTree,
                                by = c("timePointYearsSP", "geographicAreaFS",
                                       "childID"), all.x = TRUE,
                                allow.cartesian = TRUE)
    
    ##' If an element is not a child in the tree, then "standardize" it to
    ##' itself with a rate of 1 and a share of 1.
    standardizationData[is.na(parentID),
                        c("parentID", "extractionRate", "calorieExtractionRate",
                          "share") := list(childID, 1, 1, 1)]
    ## Standardizing backwards is easy: we just take the value, divide by the 
    ## extraction rate, and multiply by the shares.  However, we don't 
    ## standardize the production element (because production of flour is 
    ## derived from the production of wheat already).  We standardize everything
    ## backwards, and then edges marked as forwards (i.e. target == "F") get
    ## standardized down.
    output = standardizationData[, list(
        Value = ifelse(measuredElement != productionElement,
                       sum(Value/extractionRate*share, na.rm = TRUE),
                       sum(Value[parentID == childID]))),
                                 by = c("timePointYearsSP", "geographicAreaFS",
                                        "measuredElement", "parentID")]
    forwardEdges = specificTree[target == "F", ]
    ## Hacking sugar tree!
    warning("HACK!  Manually editing the sugar tree (the only forward process) ",
            "because it's difficult to understand how to properly code it!")
    ## We don't want the 156 to 158 edge as this is an intermediate step.  And
    ## shares should all be 1, as we're moving forward now.
    forwardEdges = forwardEdges[childID == 162, ]
    forwardEdges[, share := 1]
    outputForward = merge(output, forwardEdges,
                          by = c("parentID", "timePointYearsSP", "geographicAreaFS"))
    update = outputForward[, list(Value = sum(Value*extractionRate*share,
                                              na.rm = TRUE)),
                  by = c("timePointYearsSP", "geographicAreaFS",
                         "measuredElement", "childID")]
    setnames(update, "childID", "parentID")
    ## Remove the old rows that got corrected in the update
    output = output[!output$parentID %in% forwardEdges$parentID, ]
    ## Bind back in the corrected rows
    output = rbind(output, update)
    
    setnames(output, "parentID", "measuredItemFS")
    output[, measuredElement := paste0("Value_measuredElementFS_",
                                       measuredElement)]
    suaOutput = dcast.data.table(data = output,
        formula = timePointYearsSP + geographicAreaFS +
            measuredItemFS ~ measuredElement, value.var = "Value",
        fun.aggregate = sum)
    
    ## Aggregate to FBS level
    fbsAggregateTable = merge.data.frame(output, fbsTree,
                                         by.x = "measuredItemFS",
                                         by.y = "commodityID")
    fbsAggregateTable = data.table(fbsAggregateTable)
    fbsOutput = list()
    fbsOutput[[1]] = fbsAggregateTable[, list(Value = sum(Value * conversionFactor,
                                                          na.rm = TRUE)),
                                       by = c("timePointYearsSP", "geographicAreaFS",
                                              "measuredElement", "fbsID4")]
    fbsOutput[[2]] = fbsAggregateTable[, list(Value = sum(Value * conversionFactor,
                                                          na.rm = TRUE)),
                                       by = c("timePointYearsSP", "geographicAreaFS",
                                              "measuredElement", "fbsID3")]
    fbsOutput[[3]] = fbsAggregateTable[, list(Value = sum(Value * conversionFactor,
                                                          na.rm = TRUE)),
                                       by = c("timePointYearsSP", "geographicAreaFS",
                                              "measuredElement", "fbsID2")]
    fbsOutput[[4]] = fbsAggregateTable[, list(Value = sum(Value * conversionFactor,
                                                          na.rm = TRUE)),
                                       by = c("timePointYearsSP", "geographicAreaFS",
                                              "measuredElement", "fbsID1")]
    
    ## Rename columns to be consistent across all fbs tables
    lapply(fbsOutput, setnames, c("timePointYearsSP", "geographicAreaFS",
                                  "measuredElement", "fbsID", "Value"))
    ## Cast data to have a different format
    fbsOutput = lapply(fbsOutput, dcast.data.table,
        formula = timePointYearsSP + geographicAreaFS + fbsID ~ measuredElement,
        value.var = "Value")
    
    return(list(suaOutput = suaOutput, fbsOutput = fbsOutput))
}