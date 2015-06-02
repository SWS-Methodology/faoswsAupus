##' Get Standardization Tree
##' 
##' This function pulls the tree for standardization.
##' 
##' @param aupusData The list object containing the many different tables used 
##'   in the AUPUS process.  This argument is needed because it provides a way
##'   to overwrite the default extraction rates with country specific ones, if
##'   available.
##' @param defaultOnly Logical.  Should only default extraction rates be used? 
##'   Or should country-specific rates be used when available and default rates 
##'   used in all other cases?  The old methodology isn't clear about what is 
##'   used.
##' 
##' @result A data.table object containing 4 columns: child, parent,
##'   extractionRate, and caloriesOnly.  The child and parent columns contain
##'   character values of the items, extractionRate contains the extraction
##'   rates, and caloriesOnly is a logical indicating if the edge of interest
##'   corresponds to an aggregation of just calories or both calories and
##'   quantities.
##' 

getStandardizationTree = function(aupusData, defaultOnly = FALSE){
    
    ## Data Quality Checks
    if(length(unique(aupusData$extractionRateData$geographicAreaFS)) > 1){
        stop("Multiple countries have been supplied in the passed AUPUS data.",
             " No specific extraction rates can then be used because it's ",
             "unclear which country should be used.")
    }
    if(length(unique(aupusData$extractionRateData$timePointYearsSP)) > 1){
        stop("Multiple years have been supplied in the passed AUPUS data. ",
             "No specific extraction rates can then be used because it's ",
             "unclear which year should be used.")
    }
    
    ## NOTE (Josh): Not sure which tree to use...
#     fbsTree = GetCodeTree(domain = "suafbs", dataset = "sua",
#                           dimension = "measuredItemSuaFbs")
#     fbsTree = faoswsUtil::adjacent2edge(fbsTree)
#     ## Everything standardizes up to S0000 and so remove it to get aggregates at
#     ## finer levels.
#     fbsTree = fbsTree[parent != "S0000", ]
    fbsTree = GetCodeTree(domain = "suafbs", dataset = "fbs_balanced",
                          dimension = "measuredItemSuaFbs")
    fbsTree = faoswsUtil::adjacent2edge(fbsTree)
    fbsTree = unique(fbsTree)
#     ## Everything standardizes up to S2901, S2903, and S2941.  Remove these to
#     ## get aggregates at finer levels.
#     fbsTree = fbsTree[!parent %in% c("S2901", "S2903", "S2941"), ]
    ## Filter to just the relevant fbs codes
    fbsTree = fbsTree[grepl("S[0-9]{4}", parent), ]
    ## HACK!  There's an issue with the mapping from CPC to FCL: the CPC code
    ## 0112 maps to multiple FCL codes (Maize, White Maize, and Popcorn).  Maize
    ## should be classified under S2514 (Maize and Products) while White Maize
    ## belongs under S2520 (Other Cereals).  Removing it from the tree here
    ## causes all Maize to standardize to 2514, which should be very accurate in
    ## most cases.
    fbsTree = fbsTree[children != "0112" | parent != "S2520", ]
    fbsTree[!grepl("S", children), children :=
                faoswsUtil::cpc2fcl(cpcCodes = children, returnFirst = TRUE)]
    ## Mapping to CPC can create NA's, remove those:
    fbsTree = fbsTree[!is.na(children), ]
    setnames(fbsTree, c("parent", "children"), c("parent", "child"))

    load("~/Documents/Github/faoswsAupus/data/itemTree.RData")
    newTree = copy(itemTree)
    ## Remove edges without parents
    newTree = newTree[!is.na(targetCode), ]
    ## Convert codes to characters with four digits
    newTree[, itemCode := formatC(itemCode, format = "g",
                                  width = 4, flag = "0")]
    newTree[, targetCode := formatC(targetCode, format = "g",
                                    width = 4, flag = "0")]
    newTree[, c("itemName", "targetName", "fbsName", "incTot",
                "aggCom", "weight", "target") := NULL]
    ## For top nodes, remove them (as they're already included in fbsTree).
    newTree = newTree[itemCode != targetCode, ]
    newTree[, targetCode := as.character(targetCode)]
    newTree[, itemCode := as.character(itemCode)]

    ## Clean up some things
    newTree[, fbsCode := NULL]
    newTree[, caloriesOnly := (convType == "(cal.)")]
    newTree[, convType := NULL]
    setnames(newTree, c("itemCode", "targetCode", "baseExtraction"),
             c("child", "parent", "extractionRate"))
    
    ## Combine the two trees together, but first remove any duplicated children
    ## (giving preference to newTree)
    fbsTree = fbsTree[!child %in% newTree$child, ]
    newTree = rbindlist(list(newTree, fbsTree), use.names = TRUE, fill = TRUE)
    ## Missing rates/calories from the fbsTree should all be 1/FALSE
    newTree[is.na(extractionRate), extractionRate := 1]
    newTree[is.na(caloriesOnly), caloriesOnly := FALSE]
    ## Extraction rate is wrongly defined as it's reciprical:
    newTree[, extractionRate := 1/extractionRate]

    ## Overwrite extraction rates with country specific rates, if available and
    ## if desired (i.e. defaultOnly = FALSE).
    newTree = merge.data.frame(newTree, aupusData$extractionRateData,
                               by.x = "child", by.y = "measuredItemChildFS",
                               all.x = TRUE)
    newTree = data.table(newTree)
    if(!defaultOnly){
        newTree[!is.na(Value_extraction), extractionRate := Value_extraction]
    }
    newTree[, c("Value_extraction", "flagFaostat_extraction") := NULL]
    
    ## Country specific data may overwrite extraction rates with productivity
    ## factors, as different things are stored in element 41 in the old system
    ## for processed products then for primary commodities.  Thus, we need to
    ## adjust these back.  Find these cases as children which are only numeric
    ## codes but parents with S + numeric codes.
    newTree[grepl("^[0-9]", child) & grepl("S", parent), extractionRate := 1]
    
    ## Reformat so it's similar to aupus edges
    setnames(newTree, c("child", "parent", "extractionRate"),
             c("measuredItemChildFS", "measuredItemParentFS",
               "Value_extraction"))
    newTree[, timePointYearsSP := mean(timePointYearsSP, na.rm = TRUE)]
    newTree[, geographicAreaFS := mean(geographicAreaFS, na.rm = TRUE)]
    newTree[, Value_extraction := 10000 * Value_extraction]
    
    return(newTree)
}