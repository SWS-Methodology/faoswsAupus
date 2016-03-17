library(data.table)
library(faosws)
library(faoswsUtil)

if(Sys.info()[7] == "rockc_000"){ # Josh's personal
    setwd("~/GitHub/faoswsAupus/")
} else if(Sys.info()[7] == "josh"){
    setwd("~/Documents/Github/faoswsAupus/")
} else {
    stop("Undefined user!")
}

SetClientFiles(dir = "~/R certificate files/QA")
GetTestEnvironment(
    ## baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
    ## token = "7b588793-8c9a-4732-b967-b941b396ce4d"
    baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
    token = "0297f8b1-62ed-4d6a-a07e-bd1bacc6e615"
)

extractRate = read.fwf("documentation/FBS_LST (from Nick)/defextr.lst",
                       skip = 2, widths = c(4, 6))
colnames(extractRate) = c("measuredItemCPC", "Value")
extractRate$geographicAreaM49 = "0"
extractRate$measuredElement = "5423"
extractRate$timePointYears = "0"
extractRate$Value = extractRate$Value / 100
extractRate$measuredItemCPC = formatC(extractRate$measuredItemCPC, width = 4, flag = "0")
extractRate$measuredItemCPC = fcl2cpc(extractRate$measuredItemCPC)
# Many FCL codes are missing in the map, but I only found one in the SUA working
# table: 1159 Offals of Other Camelids.
extractRate = extractRate[!is.na(extractRate$measuredItemCPC), ]

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

write.csv(shares, file = "data/defaultShares.csv", row.names = FALSE)
write.csv(extractRate, file = "data/defaultExtractionRates.csv", row.names = FALSE)
