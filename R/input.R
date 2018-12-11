### input.R - Bunch of functions used to handle the input to the
### climex() function
##' @title Check whether the input fulfills our requirements.
##' @description The time series have to be provided as a named list
##'   of class \pkg{xts} objects and the position data has to be a
##'   data.frame containing at least the named columns \emph{name},
##'   \emph{longitude}, and \emph{latitude}.
##' @details All named elements of `list.data.sources`, which do not
##'   appear in `data.frame.positions$name` will be discarded and vice
##'   versa.
##'
##' @param list.data.sources A named list of \pkg{xts}-class time
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
##'     \item{ list.data.sources: Same format as input }
##'     \item{ data.frame.positions: Same format as input }
##'  }
##' @author Philipp Mueller
check.input <- function( list.data.sources, data.frame.positions ){
  ## Checking format
  check.result <- TRUE
  if ( !is.list( list.data.sources ) ){
    warning( "Please provide list.data.sources as a list of 'xts'-class objects!" )
    check.result <- FALSE
  }
  if ( !all( Reduce( c, lapply( list.data.sources, is.list ) ) ) ){
    warning( "All elements of list.data.sources have to be lists!" )
    check.result <- FALSE
  }
  if ( !all( Reduce( c, lapply( list.data.sources, function( ss )
    Reduce( c, lapply( ss, is.zoo ) ) ) ) ) ){
    warning( "All elements of the lists within list.data.sources have to be 'xts'-class objects!" )
    check.result <- FALSE
  }
  if ( is.null( names( list.data.sources ) ) ||
       any( Reduce( c, lapply( list.data.sources, function( ll )
                              is.null( names( ll ) ) ) ) ) ){
    warning( "All elements in list.data.sources have to be named!" )
    check.result <- FALSE
  }
  if ( !is.data.frame( data.frame.positions ) &&
      !any( class( data.frame.positions ) == "SpatialPointsDataFrame" )
      ){
    warning( "data.frame.positions has to be a data.frame or a SpatialPointsDataFrame!" )
    check.result <- FALSE
  } else if ( is.data.frame( data.frame.positions ) &&
              !( all( c( "name", "longitude", "latitude" ) %in%
                      colnames( data.frame.positions ) ) ) ){
    warning(
        "data.frame.positions has to have at least the columns 'name', 'longitude', and 'latitude" )
    check.result <- FALSE
  } else if ( any( class( data.frame.positions ) ==
                   "SpatialPointsDataFrame" ) &&
             !( "name" %in% colnames( data.frame.positions@data ) ) ){
    warning( "a column called 'name' containing the corresponding station names must be included in data.frame.positions!" )
    check.results <- FALSE
  }

  if ( any( class( data.frame.positions ) == "SpatialPointsDataFrame" )){
    ## This type of input is only provided for convenience. It has to
    ## be convert into data.frame to be dealt with inside dwd2r.
    data.frame.positions <-
      cbind( data.frame.positions@coords, data.frame.positions@data )
  }
    
  ## Discard all series, which are only present in one of the data
  ## sets.
  list.data.sources.clean <- lapply( list.data.sources, function( ss )
    ss[ names( ss ) %in% data.frame.positions$name ] )
  names( list.data.sources.clean ) <- names( list.data.sources )
  ## Obtain a character vector containing all station names in all
  ## climatological variables
  list.data.sources.names <-
    Reduce( c, lapply( list.data.sources.clean, names ) )
  data.frame.positions <- data.frame.positions[
    data.frame.positions$name %in% list.data.sources.names, ]

  return( list( check.result = check.result,
               list.data.sources = list.data.sources.clean,
               data.frame.positions = data.frame.positions ) )
}

