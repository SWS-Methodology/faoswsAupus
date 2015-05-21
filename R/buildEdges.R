##' Build Edges
##' 
##' Function to merge shares and extraction rate and build the
##' relation graph.
##'
##' @param dataList A list of AUPUS datasets to be analyzed, typically as
##' produced by the function getAupusDataset.  For more details on this
##' argument, please see ?getAupusDataset.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return A data.table object with the same keys as dataList$inputData
##' (typically geographicAreaFS, measuredItemParentFS, measuredItmChildFS, and
##' timePointYearsSP).  Additional columns are Value_share (shares as specified
##' in dataList$shareData), Value_extraction (extraction rates as specified in
##' dataList$extractionRateData), and Value_input, flagFaostat_input (as
##' specified in dataList$inputData).
##' 
##' @export
##' 


## NOTE (Michael): The extraction rate should only apply to process
##                 commodity.

buildEdges = function(dataList, aupusParam){
    
    ## TODO (Michael): Need to check this, if there are no extraction rates then
    ## probably the defaults are filled in. Also need to check how the input 
    ## from processing data base is built.
    ## 
    ## Update (Josh): Shares may not exist for a particular commodity, but we 
    ## may still have extraction rates.  In this case, the hierarchy (i.e. 
    ## parent/child relationships) will be defined in the input from processing 
    ## table.  Thus, it is important that we keep all rows in the merges (but we
    ## may eventually throw out rows with NA parent/child commodity codes).
    edgeData = merge(dataList[["inputData"]], dataList[["extractionRateData"]],
                     by = intersect(colnames(dataList[["inputData"]]),
                                    colnames(dataList[["extractionRateData"]])),
                     all = FALSE)
    edgeData = merge(edgeData, dataList[["shareData"]],
                     by = intersect(colnames(edgeData),
                                    colnames(dataList[["shareData"]])),
                     all.x = TRUE, all.y = FALSE)
    setkeyv(edgeData, key(dataList$shareData))
    ## If no record exists in the shares dataset, assume the value is 0.
    edgeData[is.na(get(aupusParam$keyNames$shareName)),
             c(aupusParam$keyNames$shareName) := 0]
    edgeData
}
