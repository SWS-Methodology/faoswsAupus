% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/getExtractionRateData.R
\name{getExtractionRateData}
\alias{getExtractionRateData}
\title{Get Extraction Rate Data}
\usage{
getExtractionRateData(aupusData, aupusParam)
}
\arguments{
\item{aupusData}{The data returned from the function getAupusData}

\item{aupusParam}{A list of running parameters to be used in pulling the data.
Typically, this is generated from getAupusParameter (see that function for
a description of the required elements).}
}
\value{
A data.table with the same key columns as aupusData and
additional columns "Value_extraction" and "flagFaostat_extraction".
This are the columns of aupusData corresponding to element 41.
}
\description{
This function extracts the extraction rate data from the aupus data by
grabbing the key columns of aupusData as well as the column corresponding
to extraction rate (as specified by element41Num).
}
\details{
Note: the old function queried the data.base for this data, but this new,
simpler function just pulls directly from a table that has already been
loaded into R.
}

