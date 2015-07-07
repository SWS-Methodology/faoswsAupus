##' Get Standardization Tree
##' 
##' This function pulls the tree for standardization.
##' 
##' @param aupusER The extraction rate table from the AUPUS data object.  This
##'   argument is needed because it provides a way to overwrite the default
##'   extraction rates with country specific ones, if available.
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

getStandardizationTree = function(aupusER, defaultOnly = FALSE){
    
    ## Data Quality Checks
    if(length(unique(aupusER$geographicAreaFS)) > 1){
        stop("Multiple countries have been supplied in the passed AUPUS data.",
             " No specific extraction rates can then be used because it's ",
             "unclear which country should be used.")
    }
    if(length(unique(aupusER$timePointYearsSP)) > 1){
        stop("Multiple years have been supplied in the passed AUPUS data. ",
             "No specific extraction rates can then be used because it's ",
             "unclear which year should be used.")
    }
    
    ## Make a copy of aupusER locally.  This prevents changes within this
    ## function from modifying the actual aupusER object.
    aupusER = copy(aupusER)
    
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
    ## HACK!  The FBS tree needs some modifications.  These should be done on
    ## the SWS rather than here.
    fbsTree = rbindlist(list(fbsTree, data.table(parent = "S2542",
                                                 child = "0162")))
    fbsTree = rbindlist(list(fbsTree, data.table(parent = "S2520",
                                                 child = "0068")))
    fbsTree = rbindlist(list(fbsTree, data.table(parent = "S2919",
                                                 child = "S2615")))
    fbsTree = rbindlist(list(fbsTree, data.table(parent = "S2512",
                                                 child = "0027")))
    fbsTree = rbindlist(list(fbsTree, data.table(parent = "S2541",
                                                 child = "0163")))

    if(Sys.info()[4] == "JOSH_LAPTOP"){
        load("~/GitHub/faoswsAupus/data/itemTree.RData")
    } else {
        load("~/Documents/Github/faoswsAupus/data/itemTree.RData")
    }
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
    aupusER[, measuredItemChildFS := formatC(measuredItemChildFS, width = 4,
                                             flag = "0")]
    newTree = merge.data.frame(newTree, aupusER, by.x = "child",
                               by.y = "measuredItemChildFS", all.x = TRUE)
    newTree = data.table(newTree)

    ## HACK!  Some of the extraction rates being used are wrong.
    warning("HACK!  Manually adjusting extraction rates!!!")
    ## See page 88 of annexes-part-II.pdf
    newTree[parent == "0512" & child == "0514", extractionRate := 0.1]
    ## See page 98 of annexes-part-II.pdf
    newTree[parent == "0882" & child == "0898", extractionRate := 1/10.527]
    newTree[parent == "0882" & child == "0899", extractionRate := 1/11.2785]
    newTree[parent == "0882" & child == "0917", extractionRate := 1/35.091]
    ## See page 111 of annexes-part-II.pdf
    newTree[parent == "0312" & child == "0311", extractionRate := 0.5]
    ## Undocumented (but cyclical):
    newTree = newTree[!(child == "0328"), ]

    if(!defaultOnly){
        newTree[!is.na(Value_extraction), extractionRate := Value_extraction / 10000]
    }
    newTree[, c("Value_extraction", "flagFaostat_extraction") := NULL]
    
    ## Country specific data may overwrite extraction rates with productivity
    ## factors, as different things are stored in element 41 in the old system
    ## for processed products then for primary commodities.  Thus, we need to
    ## adjust these back.  Find these cases as children which are only numeric
    ## codes but parents with S + numeric codes.
    newTree[grepl("^[0-9]", child) & grepl("S", parent), extractionRate := 1]

    ## HACK!  The tree still needs some modifications.
    ## See page 104 of annexes-part-II.pdf
    newTree[parent == "S2805" & child == "0027", extractionRate := 1/0.667]
    ## See page 104 of annexes-part-II.pdf
    newTree[parent == "S2512" & child == "0027", extractionRate := 1/0.78]
    ## See page 105 of annexes-part-II.pdf
    newTree[parent == "S2556" & child == "0242", extractionRate := 1/0.7]
    ## See page 111 of annexes-part-II.pdf
    newTree[parent == "S2818" & child == "0162", extractionRate := 1/0.92]

    ## Reformat so it's similar to aupus edges
    setnames(newTree, c("child", "parent", "extractionRate"),
             c("measuredItemChildFS", "measuredItemParentFS",
               "Value_extraction"))
    newTree[, timePointYearsSP := mean(timePointYearsSP, na.rm = TRUE)]
    newTree[, geographicAreaFS := mean(geographicAreaFS, na.rm = TRUE)]
    newTree[, Value_extraction := 10000 * Value_extraction]
    
    return(newTree)
}