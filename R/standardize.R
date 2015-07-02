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
##' @param calorieElements The numeric codes for the elements which should be 
##'   standardized via simple addition (i.e. calories, proteins, fats)
##' @param productionElement The numeric code for production.  This code should
##'   be specified separately as it is not standardized.
##' @param fbsTree The commodity tree which specifies how a commodityID (the 
##'   commodity code) aggregates to the FBS aggregates (fbsID4 < fbsID3 < fbsID2
##'   < fbsID1).  Additionally, conversionFactor is a column which specifies how
##'   items are rolled up, and is almost always 1.  Generally loaded from a 
##'   file.
##' @param suaTree The tree specifying how commodities should be standardized in
##'   the SUA.  Required columns are childID, parentID (both commodity codes), 
##'   extractionRate, target (a flag indicating forward or backward 
##'   standardization, or none) and calorieExtractionRate.  Generally loaded 
##'   from a file.
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
                                                  141, 151, 71),
                       calorieElements = c(261, 271, 281),
                       productionElement = 51, fbsTree, suaTree){
    
    groupKey = c("timePointYearsSP", "geographicAreaFS", "measuredItemFS")

    warning("Hacking the commodity tree!  These fixes are not stable!")
#     suaTree = suaTree[!parentID %in% c(267, # Sunflower Seed
#                                        866, # Cattle (live-weight)
#                                        976, # Sheep (live-weight)
#                                        1034, # Pig (live-weight)
#                                        1057), ] # Chickens (live-weight)
    # suaTree = suaTree[childID == 162 | !childID %in% fbsTree$commodityID, ]
    suaTree = suaTree[!childID %in% fbsTree$commodityID, ]
    
    ## Input checks
    
    ## Get the standardization tree using the country specific shares/extraction
    ## rates when available, and the default values otherwise.
    specificTree = getSpecificTree(aupusData = aupusData, suaTree = suaTree)
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
    calorieTree = copy(specificTree)
    warning("HACK!  Assigning any nodes with target == 'T' to not standardize")
    specificTree[target == "T", extractionRate := Inf]

    ## Standardize elements except production
    suaOutput = standardizeTree(data = aupusData$nodes, tree = specificTree,
                                elements = fbsElements[fbsElements != productionElement])
    ## Add in production element
    suaOutput = merge(aupusData$nodes[, c(groupKey, productionElement),
                                        with = FALSE], suaOutput,
                        by = groupKey)
    ## Standardize other elements (calories, etc.)
    calorieTree[, extractionRate := calorieExtractionRate]
    ## Standardization is skipped by cases where target == "T", but by-products
    ## (for example) can be assigned targets.  To ensure their calories are
    ## standardized, we set all target flags to "B".
    calorieTree[target == "T", target := "B"]
    calorieData = standardizeTree(data = aupusData$nodes, tree = calorieTree,
                                  elements = calorieElements)
    suaOutput = merge(suaOutput, calorieData, by = groupKey)
    
    ## Aggregate to FBS level
    warning("HACK!  Updating the fbs tree because it seems that 254 does not "
            ,"get rolled up into 2562!")
    fbsTree[fbsID4 == 2562 & commodityID == 254, conversionFactor := 0]
    ## Modify fbsTree to apply to all years/countries (to merge, create a dummy column)
    fbsTree[, dummyColumn := 1]
    specificTree[, dummyColumn := 1]
    fbsTree = merge(fbsTree,
        unique(specificTree[, list(timePointYearsSP, geographicAreaFS, dummyColumn)]),
        by = "dummyColumn", allow.cartesian = TRUE)
    fbsTree[, dummyColumn := NULL]
    setnames(fbsTree, c("commodityID", "conversionFactor"),
             c("childID", "share"))
    fbsTree[, extractionRate := 1]
    fbsOutput = lapply(c("fbsID4", "fbsID3", "fbsID2", "fbsID1"), function(name){
        setnames(fbsTree, old = name, new = "parentID")
        out = standardizeTree(data = suaOutput, tree = fbsTree,
                              elements = c(fbsElements, calorieElements))
        setnames(fbsTree, old = "parentID", new = name)
        return(out)
    })

#     ## Rename columns to be consistent across all fbs tables
#     lapply(fbsOutput, setnames, c("timePointYearsSP", "geographicAreaFS",
#                                   "measuredElement", "fbsID", "Value"))
#     ## Cast data to have a different format
#     fbsOutput = lapply(fbsOutput, dcast.data.table,
#         formula = timePointYearsSP + geographicAreaFS + fbsID ~ measuredElement,
#         value.var = "Value")
    
    return(list(suaOutput = suaOutput, fbsOutput = fbsOutput))
}