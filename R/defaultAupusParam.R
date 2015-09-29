##' Default AUPUS parameter
##' 
##' This function returns a list of parameters for many of the faoswsAupus
##' functions.  It can be called and then directly modified to handle specific
##' scenarios.
##' 
##' @return A list of parameters.
##' 
##' @export
##' 

defaultAupusParam = function(){
    list(
        areaCode = c(),
        itemCode = c(),
        elementCode = c(),
        year = c(),
        keyNames = list(areaName = "geographicAreaFS",
                        itemName = "measuredItemFS",
                        itemParentName = "measuredItemParentFS",
                        itemChildName = "measuredItemChildFS",
                        itemTypeName = "measuredItemTypeFS",
                        elementName = "measuredElementFS",
                        extractionRateName = "Value_extractionRate",
                        balanceElementName = "Value_balanceElement",
                        inputName = "Value_input",
                        shareName = "Value_share",
                        yearName = "timePointYearsSP",
                        valuePrefix = "Value_",
                        flagPrefix = "flagFaostat_",
                        ratioPrefix = "Ratio_")
    )
}