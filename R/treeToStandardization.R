##' Tree to Standardization
##' 
##' During the AUPUS procedure, two classes of trees are used: the commodity 
##' trees which specify how elements are to be processed down, and the 
##' standardization trees which specify how commodities roll up.  This function 
##' updates an aupus network (list of edges and ndoes) with a standardization 
##' tree in place of the AUPUS tree.
##' 
##' @param aupusNetwork The network object, consisting of a list of two objects:
##'   nodes and edges.  Each individual object is a data.table object.  See 
##'   ?suaToNetworkRepresentation.
##' @param aupusData The list object containing the many different tables used 
##'   in the AUPUS process.  This argument is needed because it provides a way 
##'   to overwrite the default extraction rates with country specific ones, if 
##'   available.
##' @param defaultOnly Logical.  Should only default extraction rates be used? 
##'   Or should country-specific rates be used when available and default rates 
##'   used in all other cases?  The old methodology isn't clear about what is 
##'   used.
##'   
##' @result The aupusNetwork, updated with the new edges and nodes.
##'   

treeToStandardization = function(aupusNetwork, aupusData, defaultOnly = FALSE){
    
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
    
    newEdges = getStandardizationTree(aupusData, defaultOnly = defaultOnly)
    newNodes = aupusNetwork$nodes[1, colnames(aupusNetwork$nodes)[1:2],
                                  with = FALSE]
    valueCols = colnames(aupusNetwork$nodes)[grepl("Value",
                                                   colnames(aupusNetwork$nodes))]
    newNodes = cbind(newNodes, aupusNetwork$nodes[1, valueCols, with = FALSE])
    newNodes[, c(valueCols) := 0]
    
    ## Rewrite the measuredItemFS code on nodes to have four digits
    aupusNetwork$nodes[, measuredItemFS := formatC(as.numeric(measuredItemFS),
                                                   format = "g", width = 4,
                                                   flag = "0")]
    missingNodes = newEdges[, union(measuredItemChildFS, measuredItemParentFS)]
    missingNodes = missingNodes[!missingNodes %in%
                                    aupusNetwork$nodes$measuredItemFS]
    
    if(length(missingNodes) > 0){
        ## To merge with data.table, you need a column in each data.table with 
        ## the same name.  It seems easiest to just add a dummy column here that
        ## will always merge, as we're assuming we only have one year.
        year = unique(aupusData$extractionRateData$timePointYearsSP)
        newNodes = merge(data.table(measuredItemFS = missingNodes,
                                    timePointYearsSP = year), newNodes,
                         by = "timePointYearsSP")
    }
    
    ## Update the network
    aupusNetwork$edges = newEdges
    aupusNetwork$nodes = rbindlist(list(aupusNetwork$nodes, newNodes),
                                   use.names = TRUE, fill = TRUE)
    return(aupusNetwork)
}