##' Standardize
##' 
##' This function standardizes the SUA level commodities using the logic 
##' implemented by Marteen Van't Reet in "Flexible Aggregation of FAO's Supply 
##' Utilization Accounts."
##' 
##' Below are some comments of interest about standardize.  These decisions may 
##' seem arbitrary, but they were made to cause agreement between the numbers 
##' produced from the R standardization and the existing FAOSTAT numbers.
##' 
##' \itemize{
##' 
##' \item Forward Standardization: For a few of commodities (in particular 
##' sugar), standardization proceeds forward: even though raw sugar beet sugar 
##' (159) is processed into sugar (162), we standardize to sugar.  This creates 
##' some complexity in the code.  Moreover, it seems strange that sugar beets 
##' (157) which are a parent of raw beet sugar (159) do not get standardized to 
##' sugar (162) but instead remain in their own group in standardization.
##' 
##' \item Production: Production does not get standardized, as production of 
##' (for example) flour (16) is computed based on the amount of wheat (15) 
##' allocated to processing.  This leads to some complexity in the code as well,
##' in particular when also accounting for forward standardization.  The code 
##' simply leaves the production element fixed for each commodity, and then 
##' reports production for all elements.  However, only elements of interest 
##' will be included in the FAO roll-ups, and so this approach should be valid.
##' 
##' \item Autocuts: Some children do not get standardized to their parents (such
##' as beer products).  These commodities should be specified in an autocuts 
##' file, but examination of this file and the data showed that it was missing 
##' many autocuts.  Thus, instead we force an autocut whenever a commodity is 
##' listed as an FBS aggregate.  For example, Oil of sunflower seed (268) is a 
##' child of sunflower seed (267) and is not specified in the autocuts file. 
##' Thus, we would assume that oil of sunflower seed should be standardized to 
##' sunflower seed.  However, this leads to wrong numbers, particularly as oil 
##' of sunflower seed should be included in the oil aggregation (contained under
##' FBS commodity code 2573).  Thus, any commodity code appearing as a component
##' of the FBS commodity code was forced to be autocut.
##' 
##' \item Some edges of annex 6 (the default extraction rates used to construct 
##' the trees, provided by Marteen van't Reet) go from a commodity to it's 
##' grandchild. The problem with such a conversion is that if country specific 
##' extraction rates become available, we miss the new information. For example,
##' suppose we have a 75% extraction rate from wheat to flour and 75% from flour
##' to bread. We could then write a default conversion rate of bread to wheat of
##' 1/(.75*.75) = 1.7778.  But, we may obtain country specific information that 
##' states the extraction from wheat to flour is 50%, and thus the conversion 
##' from bread to wheat should be much different.  But, the default conversion 
##' won't be updated to reflect this unless we change the two-level conversion 
##' into the individual one level conversions.  This was done manually, but in 
##' such a way that the product of the parent-child and child-grandchild 
##' extraction rates will give the same parent-grandchild extraction rate as 
##' originally provided.
##' 
##' \item 900 (Dry Whey) is a child of 903 (Whey, Fresh) and vice-versa.  One 
##' must be removed, so we'll remove the edge from 900 to 903 (to agree with 
##' documented commodity trees).
##' 
##' \item Weights of zero in the commodity tree were implemented via infinite 
##' extraction rates.
##' 
##' \item Country specific extraction rates can overwrite the default extraction
##' rates, and the default standardization shares are used only when no
##' production exists for any of the parents.  If production does exist, then
##' the standardization shares are proportioned according to the proportions of
##' production of the parent.
##' 
##' }
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
    # suaTree = suaTree[childID == 162 | !childID %in% fbsTree$commodityID, ]
    suaTree = suaTree[!childID %in% fbsTree$commodityID, ]
    
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
    warning("HACK!  Assigning any nodes with target == 'T' to not standardize")
    specificTree[target == "T", extractionRate := Inf]
    warning("Hacking the pigmeat tree!  Pig skins roll up into pig ",
            "meat, which doesn't make any sense.")
    specificTree = specificTree[!(parentID == 1035 & childID %in% (1044:1047)), ]
    warning("Hacking the Palm Oil tree!  Res Fatty Subst (1277) rolls up into ",
            "Oil of Palm (257) on the trees, but shouldn't according to the ",
            "documented commodity trees.")
    specificTree = specificTree[!(parentID == 257 & childID == 1277), ]
    warning("HACK! Not sure why, but it seems Citrus Juice (513 and 514) ",
            "don't roll up into Citrus Fruit nes (512).  However, it seems ",
            "513 and 514 do roll up for imports.  The logic for this is ",
            "unknown.")
    specificTree = specificTree[!(parentID == 512 & childID %in% 513:514), ]
    warning("HACK! It seems we can improve standardization by not rolling up ",
            "anything into the oil trees.")
    specificTree = specificTree[!(parentID %in% c(237, 244, 252, 258, 261,
                                                  266, 268, 271, 290, 293,
                                                  313, 331, 334)), ]

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
    output = standardizationData[measuredElement != productionElement, list(
        Value = sum(Value/extractionRate*share, na.rm = TRUE)),
        by = c("timePointYearsSP", "geographicAreaFS", "measuredElement", "parentID")]
    ## Don't modify production elements!
    outputProd = standardizationData[measuredElement == productionElement, ]
    outputProd[, parentID := childID]
    outputProd[, c("childID", "target", "extractionRate",
                   "calorieExtractionRate", "share") := NULL]
    outputProd = unique(outputProd)
    ## Combine the two data.frames
    output = rbind(output, outputProd)

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
    update = outputForward[measuredElement != productionElement,
                           list(Value = sum(Value*extractionRate*share, na.rm = TRUE)),
                           by = c("timePointYearsSP", "geographicAreaFS",
                                  "measuredElement", "childID")]
    outputForwardProd = outputForward[measuredElement == productionElement, ]
    outputForwardProd[, childID := parentID]
    outputForwardProd[, c("parentID", "target", "extractionRate",
                          "calorieExtractionRate", "share") := NULL]
    outputForwardProd = unique(outputForwardProd)
    update = rbind(update, outputForwardProd)
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
    warning("HACK!  Updating the fbs tree because it seems that 254 does not "
            ,"get rolled up into 2562!")
    fbsTree[fbsID4 == 2562 & commodityID == 254, conversionFactor := 0]
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