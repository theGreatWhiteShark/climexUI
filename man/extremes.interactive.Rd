% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/climex_preprocessing.R
\name{extremes.interactive}
\alias{extremes.interactive}
\title{Extracting extreme events in the \code{climex} app}
\usage{
extremes.interactive(x.xts, buttonMinMax, radioEvdStatistics,
  sliderBlockLength, sliderThreshold, checkboxDecluster)
}
\arguments{
\item{x.xts}{Time series of class \pkg{xts} which has to be
cleaned.}

\item{buttonMinMax}{Character (radio) input determining whether
the GEV/GP distribution shall be fitted to the smallest or
biggest values. Choices: c( "Max", "Min ), default = "Max".}

\item{radioEvdStatistics}{Character (radio) input determining
whether the GEV or GP distribution shall be fitted to the
data. Choices: c( "GEV", "GP" ), default = "GEV".}

\item{sliderBlockLength}{Numerical (slider) input determining the
block length used in the GEV flavor of extreme value theory. On 
default it is set to one year.}

\item{sliderThreshold}{Numerical (slider) input determining the
threshold used within the GP fit and the extraction of the
extreme events. Boundaries: minimal and maximal value of the
deseasonalized time series (rounded). Default: 0.8* the upper
end point.}

\item{checkboxDecluster}{Logical (checkbox) input determining
whether to remove all clusters in a time series and to replace
them by their maximal value. This box will be only available if
\code{radioEvdStatistics} equals \code{"GP"} and else will be
\code{NULL}.}
}
\value{
Time series of class \pkg{xts}.
}
\description{
Function to  extract the extreme  event from  a time
  series.
}
\details{
If \code{radioEvdStatistics}  is set to \code{"GEV"}, the
  time series  will be  block. If  it's on the  other hand  set to
  \code{"GP"}, all values above a threshold \code{sliderThreshold}
  will be extracted.
}
\seealso{
Other preprocessing: \code{\link{cleaning.interactive}},
  \code{\link{data.extremes}},
  \code{\link{deseasonalize.interactive}},
  \code{\link{deseasonalizeSelectionInput}},
  \code{\link{deseasonalizeSelection}},
  \code{\link{generalButtonMinMaxInput}},
  \code{\link{generalButtonMinMax}},
  \code{\link{generalExtremeExtractionInput}},
  \code{\link{generalExtremeExtraction}}
}
\author{
Philipp Mueller
}
\concept{preprocessing}
