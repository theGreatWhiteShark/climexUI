% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/climex_fitting.R
\name{fit.interactive}
\alias{fit.interactive}
\title{Interactive fits in \code{climex} app}
\usage{
fit.interactive(x.kept, radioEvdStatistics, buttonMinMax, sliderThreshold,
  selectDataBase)
}
\arguments{
\item{x.kept}{Time series of class \pkg{xts}. Removing clicked or
brushed values has to be done beforehand.}

\item{radioEvdStatistics}{Character (radio) input determining
whether the GEV or GP distribution shall be fitted to the
data. Choices: c( "GEV", "GP" ), default = "GEV".}

\item{buttonMinMax}{Character (radio) input determining whether
the GEV/GP distribution shall be fitted to the smallest or
biggest values. Choices: c( "Max", "Min ), default = "Max".}

\item{sliderThreshold}{Numerical (slider) input determining the
threshold used within the GP fit and the extraction of the
extreme events. Boundaries: minimal and maximal value of the
deseasonalized time series (rounded). Default: 0.8* the upper
end point.}

\item{selectDataBase}{Character (select) input to determine the
data source. It is either of one of the names of the provided
list in the \code{list.data.sources} argument of the
\code{\link{climex}} function or \emph{Artificial data}. In case
of the latter choice, the function \code{\link{data.selection}}
will provide a \emph{reactive} object containing random numbers
drawn from the distribution specified using
\code{radioEvdStatistics}. Default = a random element of
the provided input.}
}
\value{
Object of class \code{climex.fit.gev} or
  \code{climex.fit.gpd}, depending on the choice in
  \code{radioEvdStatistics}
}
\description{
Function  to perform  the  GEV/GP  fit within  the
  \code{climex} app.
}
\details{
This function does not wait for the initialization of its
  slider, checkbox etc. inputs. This  way the fit can be performed
  with  its  default  settings   in  the  \emph{Map}  tab  without
  switching to the \emph{General} tab first.
}
\seealso{
Other climex-fitting: \code{\link{color.table}},
  \code{\link{data.fitting}},
  \code{\link{generalFitStatisticsTable}},
  \code{\link{generalFitStatistics}}
}
\author{
Philipp Mueller
}
\concept{climex-fitting}
