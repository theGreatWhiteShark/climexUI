##' Daily  maximum  temperatures of  the  Potsdam  station in  Germany
##' provided by the DWD. If you intend to use this data please look at
##' the  \href{ftp://ftp-cdc.dwd.de/pub/CDC/Terms_of_use.pdf}{terms of
##' use}. The data is stored in the \pkg{xts} format.
##'
##' The series is the same one as provided by the \pkg{climex} package
##' but formatted in such a way the \pkg{climexUI} functions can
##' handle it properly.
##'
##' The object is a \emph{list of length 1}, which is named "daily
##' max. temp.". This \emph{element is again a list of length 1},
##' named "Potsdam", which now contains the actual time series of
##' class \pkg{xts}.
##'
##' @format Class \code{list(list(xts))}
##' @source \url{http://www.dwd.de/}
##' @family fallbackData
##' @name list.data.sources.fallback
NULL

##' Longitude, latitude, and altitude information for the station data
##' provided in the \code{list.data.sources.fallback} data within this
##' package. It is a data.frame of one row and four columns.
##' @format Class \code{data.frame}
##' @family fallbackData
##' @name data.frame.positions.fallback
NULL
