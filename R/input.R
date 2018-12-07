### input.R - Bunch of functions used to handle the input to the
### climex() function
##' @title Check whether the input fulfills our requirements.
##' @description The time series have to be provided as a named list
##'   of class \pkg{xts} objects and the position data has to be a
##'   data.frame containing at least the named columns \emph{name},
##'   \emph{longitude}, and \emph{latitude}.
##' @details All named elements of `list.time.series`, which do not
##'   appear in `data.frame.positions$name` will be discarded and vice
##'   versa.
##'
##' @param list.time.series A named list of \pkg{xts}-class time
##'   series.
##' 
##' @param data.frame.positions A data.frame containing at least the
##'   named columns \emph{name}, \emph{longitude}, and
##'   \emph{latitude}.
##'
##' @importFrom zoo is.zoo
##' 
##' @return A list containing the result of the check (TRUE if the
##'   input has the right format) and a version of the input stripped
##'   of any list elements and data.frame columns, which just appear
##'   in one of the input arguments.
##'   \itemize{
##'     \item{ check.result: Boolean }
##'     \item{ list.time.series: Same format as input }
##'     \item{ data.frame.positions: Same format as input }
##'  }
##' @author Philipp Mueller
check.input <- function( list.time.series, data.frame.positions ){
  ## Checking format
  check.result <- TRUE
  if ( !is.list( list.time.series ) ){
    warning( "Please provide list.time.series as a list of 'xts'-class objects!" )
    check.result <- FALSE
  }
  if ( !all( Reduce( c, lapply( list.time.series, is.zoo ) ) ) ){
    warning( "All elements of list.time.series have to be 'xts'-class objects!" )
    check.result <- FALSE
  }
  if ( is.null( list.time.series ) ){
    warning( "list.time.series has to be named!" )
    check.result <- FALSE
  }
  if ( !is.data.frame( data.frame.positions ) ){
    warning( "data.frame.positions has to be a data.frame!" )
    check.result <- FALSE
  }
  if ( !( all( c( "name", "longitude", "latitude" ) %in%
           colnames( data.frame.positions ) ) ) ){
    warning(
        "data.frame.positions has to have at least the columns 'name', 'longitude', and 'latitude" )
    check.result <- FALSE
  }

  ## Discard all series, which are only present in one of the data
  ## sets.
  list.time.series <- list.time.series[
      names( list.time.series ) %in% data.frame.positions$name ]
  data.frame.positions <- data.frame.positions[
    data.frame.positions$name %in% names( list.time.series ), ]

  return( list( check.result = check.result,
               list.time.series = list.time.series,
               data.frame.positions = data.frame.positions ) )
}

