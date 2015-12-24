library(data.table)
library(faosws)
library(faoswsUtil)

if(Sys.info()[7] == "rockc_000"){ # Josh's personal
    setwd("~/GitHub/faoswsAupus/")
}

extractRate = read.fwf("documentation/FBS_LST (from Nick)/defextr.lst",
                       skip = 2, widths = c(4, 6))
colnames(extractRate) = c("measuredItemCPC", "Value")
extractRate$geographicAreaM49 = "0"
extractRate$measuredElement = "5423"
extractRate$timePointYears = "0"
extractRate$Value = extractRate$Value / 100
extractRate$measuredItemCPC := fcl2cpc(extractRate$measuredItemCPC)

io = read.fwf("documentation/FBS_LST (from Nick)/ioext.lst",
              widths = 1000, stringsAsFactors = FALSE)
io = io[grepl("^b", io$V1), ]
shareCode = substr(io, start = 5, stop = 13)
com1 = substr(io, start = 21, stop = 24)
com2 = substr(io, start = 41, stop = 44)
com3 = substr(io, start = 61, stop = 64)
io = data.table(shareCode, com1, com2, com3)
io = data.table:::melt.data.table(io, id.vars = "shareCode")
io[, variable := NULL]
io[, value := as.numeric(value)]
io = io[!is.na(value), ]
setnames(io, "value", "measuredItemChildCPC")
io[, shareCode := gsub(" ", "", shareCode)]

shares = read.fwf("documentation/FBS_LST (from Nick)/shrsval.lst",
                  widths = c(2,9,2,14), skip = 3)
shares$V1 = NULL
shares$V3 = NULL
colnames(shares) = c("shareCode", "Value")

## Merge shares and io together
shares = merge(shares, io, by = "shareCode")
shares = data.table(shares)
shares[, measuredItemParentCPC := as.numeric(substr(shareCode, 1,
                                                    nchar(shareCode)-5))]
shares[, geographicAreaM49 := "0"]
shares[, measuredShare := "1"]
shares[, timePointYearsSP := "0"]
shares[, shareCode := NULL]
shares[, measuredItemChildCPC := formatC(measuredItemChildCPC, width = 4,
                                         flag = "0")]
shares[, measuredItemParentCPC := formatC(measuredItemParentCPC, width = 4,
                                          flag = "0")]
shares[, measuredItemChildCPC := fcl2cpc(as.character(measuredItemChildCPC))]
shares[, measuredItemParentCPC := fcl2cpc(as.character(measuredItemParentCPC))]
