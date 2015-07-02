##' Get Specific Tree
##' 
##' This function gets the standardization tree specific to the passed AUPUS 
##' data: it uses the country specific extraction weights when available, and it
##' uses the standardization shares based on production.  If these specific 
##' values are not available, it uses the default values.
##' 
##' @param aupusData A list of two data.table objects, the names of which should
##'   be "nodes" and "edges".  This object thus contains the AUPUS data.  This 
##'   object is usually the output from the Aupus function.
##' @param suaTree A data.table containing columns childID, parentID,
##'   extractionRate, and calorieExtractionRate.  This table specifies how SUA
##'   level commodities will get standardized to their primary equivalents.
##'   
##' @return A data.table with the extraction rates and shares for each 
##'   year/country/parent/child.
##'   

getSpecificTree = function(aupusData, suaTree){
    
    ## Update tree with specific shares
    allPairs = aupusData$nodes[, .N, by = c("geographicAreaFS", "timePointYearsSP")]
    allPairs[, N := NULL]
    countrySuaTree = merge.data.frame(allPairs, suaTree)
    ## This seems a bit strange.  Marteen's document states (in flex_ag.pdf,
    ## page 27) that "The default shares are specified in the Output-Input list,
    ## annex 2."  However, in annex 2, we see (for example) that commodity 903
    ## can be produced from 9 different inputs, each which have a share of 1. 
    ## That doesn't make sense from a standardization perspective, but let's see
    ## if it causes errors (because I don't know what else we could do!)
    countrySuaTree$defaultShare = 1

    ## Merge the default tree with the country specific one.  Keep the structure
    ## of the specific tree (i.e. have all.x = TRUE) but don't add in extra
    ## edges from the country specific tree.  We don't want bran standardizing
    ## to flour if there's a country specific extraction.
    specificTree = aupusData$edges[, c("measuredItemParentFS", "measuredItemChildFS",
                                       "timePointYearsSP", "geographicAreaFS",
                                       "Value_extraction"), with = FALSE]
    specificTree[, Value_extraction := Value_extraction / 10000]
    if(any(specificTree$Value_extraction == 0)){
        warning("Removing extraction rates of 0: there is likely is some ",
                "error in the AUPUS edge dataset provided.")
        specificTree = specificTree[Value_extraction > 0, ]
    }
    specificTree = merge(
        countrySuaTree, specificTree,
        by.x = c("parentID", "childID", "timePointYearsSP", "geographicAreaFS"),
        by.y = c("measuredItemParentFS", "measuredItemChildFS",
                 "timePointYearsSP", "geographicAreaFS"), all.x = TRUE)
    specificTree = data.table(specificTree)
    ## We may have added new rows.  These edges should have a default share of
    ## 0, and currently the value is NA.
    specificTree[is.na(defaultShare), defaultShare := 0]
    
    ## If the commodity doesn't get standardized in the default tree (i.e. 
    ## extraction rate is Inf) then don't standardize in the specific tree. 
    ## Otherwise, use specific extraction rate when it exists, or the default if
    ## no specific rate exists.
    specificTree[, extractionRate := ifelse(extractionRate == Inf, Inf,
                                     ifelse(is.na(Value_extraction),
                                         extractionRate, Value_extraction))]
    specificTree[, Value_extraction := NULL]
    
    ## Compute the shares based on production, or use 1 if there's only one
    ## parent in the tree.
    productionTotal = aupusData$nodes[, c("geographicAreaFS", "timePointYearsSP",
                                          "measuredItemFS",
                                          "Value_measuredElementFS_51"), with = F]
    setnames(productionTotal, "Value_measuredElementFS_51", "production")
    productionTotal[is.na(production), production := 0]
    specificTree = merge.data.frame(specificTree, productionTotal,
        by.x = c("timePointYearsSP", "geographicAreaFS", "parentID"),
        by.y = c("timePointYearsSP", "geographicAreaFS", "measuredItemFS"))
    specificTree = data.table(specificTree)
    specificTree[, share := production / sum(production),
                 by = c("timePointYearsSP", "geographicAreaFS", "childID")]
    specificTree[is.na(share), share := defaultShare]
    specificTree[, c("defaultShare", "production") := NULL]
    
    warning("HACK!  We must remove any cycles with cotton (328->329 and 329->328)!")
    specificTree = specificTree[!parentID == 328, ]
    
    ## Collapse the tree so that each child is standardized directly to it's ancestor.
    specificTree = collapseEdges(edges = specificTree)
    
    ## Set key columns to character
    specificTree[, childID := as.character(childID)]
    specificTree[, parentID := as.character(parentID)]
    specificTree
}