## This script is designed to construct a default commodity tree by reading in 
## input files from Marteen.  The logic is documented in the document titled 
## flex-ag.pdf (which stands for flexible aggregation).  Supporting documents 
## and data for this pdf are available in a pdf format in annexes-part-II.pdf, 
## and it is this pdf file where we will be extracting most of our data.
## 
## First, though, I used Adobe Acrobat to extract the tables/annexes from the
## pdf file and place them into individual annexes.  We'll be reading these
## files.
## 
## Note that this is a default commodity tree.  Country specific extraction 
## rates can overwrite the extraction rates given here, and the default 
## standardization shares are used only when no production exists for any of the
## parents.  If production does exist, then the standardization shares are 
## proportioned according to the proportions of the parent.
## 

library(data.table)
dataDir = "~/Documents/Github/faoswsAupus/documentation/"

###############################################################################
# Output-Input list defines the tree structure.
###############################################################################

oiList = read.csv(paste0(dataDir, "annex2.csv"), header = FALSE)
colnames(oiList) = c("edgeID", "inputOutput", "commodityCode",
                     "commodityName", "share")

## Set up a data frame to hold the data, with column names c("parentID",
## "childID", "parentName", "childName", "edgeID", "share").
oiData = NULL

## Have to use a for loop because we have multiple, varying numbers of rows
## which correspond to a single data point.
for(i in 1:nrow(oiList)){
    if(oiList$inputOutput[i] == "O"){
        childID = oiList$commodityCode[i]
        childName = as.character(oiList$commodityName[i])
    } else if(oiList$inputOutput[i] == "I"){
        oiData = rbind(oiData, data.frame(
            parentID = oiList$commodityCode[i],
            childID = childID,
            parentName = as.character(oiList$commodityName[i]),
            childName = childName,
            edgeID = oiList$edgeID[i],
            share = oiList$share[i]
        ))
    } else {
        stop("Unexpected Input/Output code")
    }
}

oiData = data.table(oiData)
oiData[share > 0, length(unique(parentID)), by = childID]

###############################################################################
# Autocuts remove edges from the tree
###############################################################################

autocuts = fread(paste0(dataDir, "annex4.csv"))
oiData = oiData[!childID %in% autocuts$V1, ]

###############################################################################
# Commodities with weight 0
###############################################################################

weights = fread(paste0(dataDir, "annex0.csv"))
setnames(weights, c("plus", "commodityCode", "commodityName", "inGrandTotal",
                    "input", "output", "target", "weight", "Target",
                    "fbsTargetCode", "fbsTargetName", "caloriesOnly"))

###############################################################################
# Default Conversion Factors
###############################################################################

conversionFactor = fread(paste0(dataDir, "annex6.csv"))
setnames(conversionFactor, c("childID", "childName", "parentID", "parentName",
                             "conversionFactor"))
## Some edges of annex 6 go from a parent commodity to a grandchild commodity. 
## The problem with such a conversion is that if country specific extraction 
## rates become available, we miss the new information.  For example, suppose we
## have a 75% extraction rate from wheat to flour and 75% from flour to bread. 
## We could then write a default conversion rate of bread to wheat of 
## 1/(.75*.75) = 1.7778.  But, we may obtain country specific information that 
## states the extraction from wheat to flour is 50%, and thus the conversion 
## from bread to wheat should be much different.  But, the default conversion 
## won't be updated to reflect this unless we change the two-level conversion 
## into the individual one level conversions.  The following code does this,
## with the use of two additional files (manually created by Josh Browning, June
## 18 2015) based on annex6.
deleteEdges = fread(paste0(dataDir, "annex6_obsToDelete.csv"))
addEdges = fread(paste0(dataDir, "annex6_obsToAdd.csv"))
## Delete columns by merging and then removing all rows that matched
conversionFactor = merge(conversionFactor, deleteEdges,
                         by = c("parentID", "childID"), all = TRUE)
conversionFactor = conversionFactor[is.na(comment), ]
conversionFactor[, comment := NULL]
conversionFactor = rbindlist(list(conversionFactor, addEdges), use.names = TRUE)

conversionFactor[, extractionRate := 1/conversionFactor]
conversionFactor[childID %in% c(158, 159), ]
# 158 and 159 are parents of 162, not the other way around
conversionFactor[childID %in% c(158, 159), c("childID", "parentID") :=
                     list(parentID, childID)]
conversionFactor[childID == "328", ] # Not sure what to do with this case

###############################################################################
# Construct standardization tree
###############################################################################

## Reverse standardization direction for nodes which are set to be processed
## forward.  This won't work, though, because production won't be computed
## correctly.
# suaTree[parentID == 158, c("childID", "parentID", "extractionRate")
#         := list(parentID, childID, 1/extractionRate)]

suaTree = unique(oiData[share > 0, c("parentID", "childID"), with = FALSE])
suaTree = merge.data.frame(suaTree, weights[, c("commodityCode", "weight",
                                                "caloriesOnly", "target"), with = FALSE],
                           by.x = "childID", by.y = "commodityCode")
## HACK!  Process sugar nodes forward to 162.
suaTree$target[suaTree$childID == 162] = "F"
## suaTree and conversionFactor both have child and parent ID's, so it would 
## seem logical to join on both.  However, they are different at times: bread 
## gets standardized back to flour then to wheat, and so the O-I table has two 
## records whereas the conversion rate table takes bread straight to wheat.  So,
## join on only child, and update the parent if the extraction rate is
## available.  Otherwise, the parent from the O-I table is important (for
## calorie standardization).
suaTree = merge(suaTree, conversionFactor[, c("childID", "parentID",
                                              "extractionRate"), with = FALSE],
                by = "childID", all = TRUE)
## The above merge adds some records in conversionFactors that aren't in
## suaTree.  These records are valid conversions, so their weight and
## caloriesOnly values should be "".
suaTree = data.table(suaTree)
suaTree[is.na(weight), c("weight", "caloriesOnly") := list("", "")]

suaTree[, parentID := ifelse(is.na(extractionRate), parentID.x, parentID.y)]
suaTree[, c("parentID.x", "parentID.y") := NULL]
suaTree = unique(suaTree)

## 900 (Dry Whey) is a child of 903 (Whey, Fresh) and vice-versa.  One must be
## removed, so we'll remove the connect 900 to 903
suaTree = suaTree[!(childID == 903 & parentID == 900), ]

## If the weight is 0, then the extraction rate should be Inf (to get a 0
## multiplier).
suaTree[weight == "w=0", extractionRate := Inf]
## According to Marteen's documentation, missing extraction rates are assumed to
## be 100%
suaTree[is.na(extractionRate), extractionRate := 1]
suaTree[, calorieExtractionRate := ifelse(weight == "w=0" & caloriesOnly == "", 0, 1)]
suaTree[, c("weight", "caloriesOnly") := NULL]

fbsTree = fread(paste0(dataDir, "annex8.csv"))
## Use fbsID4 for easier merging later on
setnames(fbsTree, c("fbsID4", "fbsName", "commodityID",
                    "commodityName", "conversionFactor"))
fbsTree[, c("fbsName", "commodityName") := NULL]
fbsHierarchy = fread(paste0(dataDir, "annex7.csv"))
setnames(fbsHierarchy, c("fbsID1", "fbsID2", "fbsID3", "fbsID4"))
fbsTree = merge(fbsTree, fbsHierarchy, by = "fbsID4")
setcolorder(fbsTree, c("commodityID", "conversionFactor", "fbsID4",
                       "fbsID3", "fbsID2", "fbsID1"))

save(fbsTree, suaTree, file = "~/Documents/Github/faoswsAupus/data/commodityTrees.RData")
