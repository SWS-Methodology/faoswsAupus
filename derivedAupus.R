## This module is designed to estimate the production of a derived commodity 
## using the supply quantity of the corresponding primary product.  It uses the 
## original AUPUS logic, or at least a small part of the logic, to convert 
## production primary -> processing primary -> input to processing -> production
## of derived (where the arrows are defined by multiplying by the corresponding
## ratio, share, and extraction rate, respectively).

## load the library
library(faosws)
library(faoswsUtil)
library(faoswsFlag)
library(data.table)

## Setting up variables
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")

if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
    cat("Not on server, so setting up environment...\n")
    
    ## Define directories
    if(Sys.info()[7] == "josh")
        apiDirectory = "~/Documents/Github/faoswsAupus/R"
    if(Sys.info()[7] == "rockc_000")
        apiDirectory = "~/Github/faoswsAupus/R"

    ## Get SWS Parameters
    GetTestEnvironment(
        ## baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        ## token = "7b588793-8c9a-4732-b967-b941b396ce4d"
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "3242f11d-28b2-4429-86b0-6fab97cb50bb"
    )

    ## Source local scripts for this local test
    for(file in dir(apiDirectory, full.names = T))
        source(file)
}

## 1. Get ratios, shares, and extraction rates (specific to country and year).
## 
param = defaultAupusParam()
param$areaCode = m492fs(swsContext.datasets[[1]]@dimensions[[areaVar]]@keys)
param$itemCode = cpc2fcl(swsContext.datasets[[1]]@dimensions[[itemVar]]@keys,
                         returnFirst = TRUE)
param$itemCode = param$itemCode[!is.na(param$itemCode)]
param$itemCode = as.character(as.numeric(param$itemCode))
param$elementCode = "131"
param$year = swsContext.datasets[[1]]@dimensions[[yearVar]]@keys
getRatioData(aupusParam = param, database = "new")

## 2. Get list of commodities I need (primary that are parents of processed, and
## derived).
## 
## 3. Get production and stock changes for primary products.
## 
## 4. Get trade for primary products.
## 
## 5. Compute production of derived commodities.
## 
## 6. Get production of derived products currently in the system, and determine
## which should be overwritten.

