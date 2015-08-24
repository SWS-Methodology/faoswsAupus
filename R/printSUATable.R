##' Print SUA Table
##' 
##' @param data The data.table containing the full dataset for standardization.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##' @param printCodes A character vector of the elements of interest.  This
##'   is required to keep the table from getting to big/unreadable.
##' @param printProcessing Logical.  Should food processing also be printed?
##'   
##' @return Nothing is returned, but a table is returned in a nice format to 
##'   show the current SUA data.
##'   


## Function for printing the main table
printSUATable = function(data, standParams, printCodes, printProcessing = TRUE){
    printDT = copy(data)
    printDT = printDT[, c(standParams$mergeKey, "element", "Value"),
                      with = FALSE]
    printDT[, element := paste0("Value_measuredElement_", element)]
    printDT = printDT[get(standParams$itemVar) %in% printCodes, ]
    
    fbsElements = c(standParams$productionCode, standParams$feedCode,
                    standParams$seedCode, standParams$wasteCode,
                    standParams$foodCode, standParams$stockCode,
                    standParams$importCode, standParams$exportCode,
                    standParams$foodProcCode, standParams$industrialCode,
                    standParams$touristCode)

    printDT[is.na(Value), Value := "-"]
    printDT = tidyr::spread(data = printDT, key = "element", value = "Value",
                            fill = NA)
    setnames(printDT, standParams$itemVar, "Item")

    if(printProcessing){
        setnames(printDT, paste0("Value_measuredElement_", fbsElements),
            c("Production", "Feed", "Seed", "Loss",
              "Food", "StockChange", "Imports", "Exports",
              "Food Processing", "Industrial", "Tourist"))
        items = c("Name", "Production", "Imports", "Exports", "StockChange",
                  "Food", "Food Processing", "Feed", "Seed", "Tourist",
                  "Industrial", "Loss")
    } else {
        fbsElements = fbsElements[fbsElements != standParams$foodProcCode]
        setnames(printDT, paste0("Value_measuredElement_", fbsElements),
            c("Production", "Feed", "Seed", "Loss",
              "Food", "StockChange", "Imports", "Exports",
              "Industrial", "Tourist"))
        items = c("Name", "Production", "Imports", "Exports", "StockChange",
                  "Food", "Feed", "Seed", "Tourist", "Industrial", "Loss")
    }
    sapply(items, function(colName){
        if(!colName %in% colnames(printDT)){
            printDT[, c(colName) := 0]
        } else {
            printDT[is.na(get(colName)), c(colName) := "-"]
        }
    })
    out = knitr::kable(printDT[, items, with = FALSE], align = 'r')
    return(out)
}
