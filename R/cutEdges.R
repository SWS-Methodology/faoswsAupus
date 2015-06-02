##' Cut Edges
##' 
##' In the old AUPUS processing, some edges were removed.  The rationale seems 
##' to be prevention of standardization of alcoholic beverages back to their 
##' parents and the removal of cycles.
##' 
##' @param aupusNetwork A list of two data.tables ("nodes" and "edges").  This 
##'   is typically produced from ?suaToNetworkRepresentation.
##' @param edgeChildren To specify which edges should be removed, pass a 
##'   character vector containing the ID's of the children.  The default value
##'   is what is specified in the AUPUS program provided by Maarten van 't Riet
##'   (by way of Nick Connell).
##'   
##' @return An object that is the same as aupusNetwork but with some edges 
##'   removed.
##'   

cutEdges = function(aupusNetwork, edgeChildren = c("0026", "0039", "0051",
                                                   "0066", "0082", "0086",
                                                   "0166", "0172", "0517",
                                                   "0564", "0632", "0634")){
    aupusNetwork$edges = aupusNetwork$edges[!measuredItemChildFS %in% edgeChildren, ]
    return(aupusNetwork)
}