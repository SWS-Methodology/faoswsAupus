##' Build Nodes
##' 
##' This function creates the node structure by merging aupus, ratio, balance
##' element, item info and population datasets.
##'
##' @param dataList A list of AUPUS datasets to be analyzed, typically as
##' produced by the function getAupusDataset.  For more details on this
##' argument, please see ?getAupusDataset.
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' 
##' @return A data.table with the node data.  Essentially, this is a data.table
##' containing all the variables meant to be stored in the nodes: AUPUS, input,
##' ratio, balance element, item info, and population.
##'
##' @export
##' 

buildNodes = function(dataList, aupusParam){
    
    ## Data Quality Checks
    stopifnot(sapply(dataList, is.data.table))
    
    ## Merge together datasets to create nodes
    aupusKey = key(dataList$aupusData)
    dataList$aupusData = merge(dataList$aupusData, dataList$itemInfoData,
                               all.x = TRUE)
    setkeyv(dataList$aupusData, aupusKey)
    dataList$aupusData = merge(dataList$aupusData, dataList$ratioData,
                               all.x = TRUE)
    setkeyv(dataList$aupusData, aupusKey)
    dataList$aupusData = merge(dataList$aupusData, dataList$balanceElementData,
                               all.x = TRUE)
    setkeyv(dataList$aupusData, aupusKey)
    populationColumns = c(key(dataList$populationData),
                          "Value_population_11", "Value_population_21")
    dataList$aupusData = merge(dataList$aupusData,
                               dataList$populationData[, populationColumns,
                                                       with = FALSE],
                               all.x = TRUE)
    dataList$aupusData[, c(aupusParam$keyNames$itemName) :=
                           as.character(get(aupusParam$keyNames$itemName))]
    setkeyv(dataList$aupusData, aupusKey)
    dataList$aupusData
}
