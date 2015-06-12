## This script is designed to construct a default commodity tree by reading in 
## input files from Marteen.  The logic is documented in the document titled 
## flex-ag.pdf (which stands for flexible aggregation).  Supporting documents 
## and data for this pdf are available in a pdf format in annexes-part-II.pdf, 
## and it is this pdf file where we will be extracting most of our data.
## 
## Note that this is a default commodity tree.  Country specific extraction
## rates can overwrite the extraction rates given here, and the default
## standardization shares are used only when no production exists for any of the
## parents.  If production does exist, then the standardization shares are
## proportioned according to the proportions of the parent.
## 

library(tm)

## Grab the data from the annexes using tm
uri = "/home/josh/Documents/Github/faoswsAupus/documentation/annexes-part-II.pdf"
readPDFFunc = readPDF(engine = "xpdf")
wordList = readPDFFunc(elem = list(uri = uri), language = "en")

## Annex 0
lengthAnnex0 = 386
annex0 = NULL
for(i in 0:5){
    currWords = wordList$content[73 + i*lengthAnnex0 + 0:(lengthAnnex0-1)]
