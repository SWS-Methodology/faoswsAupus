% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/collapseSpecificData.R
\name{collapseSpecificData}
\alias{collapseSpecificData}
\title{Collapse Specific Data}
\usage{
collapseSpecificData(aupusParam, listData, verbose = FALSE)
}
\arguments{
\item{aupusParam}{A list of running parameters to be used in pulling the data.
Typically, this is generated from getAupusParameter (see that function for
a description of the required elements).}

\item{listData}{A list of three data.tables, typically as obtained from the
functions getShare, getBalanceElementData, or getRatioData.  This
list is of length 3 with data.tables as elements (specific, yearWildCard,
and areaYearWildCard).  Each data.table contains information at a
different level of specificity.}

\item{verbose}{Whether the output should be printed.}
}
\value{
Returns a single data.table derived by condensing the listData
list into one dataset.
}
\description{
Certain datasets are extracted from the database in three chunks:
country/year specific data, country specific data, and generic data
(applying to all countries/years).  The most specific data available
should be used when available, otherwise a more general value should
be used.  This function collapses the three different datasets
(supplied in listData) into one final dataset.  Note that all three
datasets should have the same columns except for the key columns which
provide the level of detail (i.e. item, item/country, and
item/country/year).
}

