### input.R - Bunch of functions used to handle the input to the
### climex() function
##' @title Check whether the input fulfills our requirements
##' 
##' @description The  time  series  has to  be  provided  as a  named
##'   \code{list}  of   named  \code{list}s  of  class   \pkg{xts}  or
##'   \code{data.frame}  objects.   The  position  data has  to  be  a
##'   \code{data.frame}   containing  at   least  the   named  columns
##'   \emph{name},   \emph{longitude},   and  \emph{latitude}   or   a
##'   \code{\link[sp]{SpatialPointsDataFrame}}       containing      a
##'   \emph{name} column in their data slot.
##'
##' @details  All named elements of  code{list.data.sources}, which do
##'   not     appear     in      the     \code{name}     column     of
##'   \code{data.frame.positions} will be discarded and vice versa.
##'
##'   Internally the \code{data.frame}-type of
##'   \code{list.data.sources} will be converted into the \pkg{xts}
##'   type and the \code{\link[sp]{SpatialPointsDataFrame}} type of
##'   \code{data.frame.positions} will be converted into its
##'   \code{data.frame} equivalent.
##'
##' @param list.data.sources A named \code{list} of named \code{list}s
##'   of \pkg{xts}-class time series or \code{data.frame}s. The first
##'   level of hierarchy in the list corresponds to the different
##'   climatological variables, which will be accessible via the first
##'   drop down menu in the sidebar. The second level corresponds to
##'   the names of the individual stations measurements of the top
##'   level variable are available for. The series themselves can be
##'   provided either as a \pkg{xts} object to \code{data.frame}
##'   containing a numerical \code{value} and a time-date \code{date}
##'   column.
##' 
##' @param data.frame.positions Either a \code{data.frame} containing
##'   at least the named columns \emph{name}, \emph{longitude}, and
##'   \emph{latitude} or a \code{\link[sp]{SpatialPointsDataFrame}}
##'   containing a \code{name} column in its \code{@data} slot.
##' @param silent Whether to display warnings if the check
##'   failed. Default = FALSE.
##' 
##' @importFrom zoo is.zoo
##' 
##' @return A list containing the result of the check (TRUE if the
##'   input has the right format) and a version of the input stripped
##'   of any \code{list} elements and \code{data.frame} columns, which
##'   just appear in one of the input arguments.
##'   \itemize{
##'     \item{ check.result: Boolean }
##'     \item{ list.data.sources: Same format as input }
##'     \item{ data.frame.positions: Same format as input }
##'  }
##' @author Philipp Mueller
check.input <- function( list.data.sources, data.frame.positions,
                        silent = FALSE ){
  ## Checking format
  check.result <- TRUE
  if ( !is.list( list.data.sources ) ){
    if ( !silent ){
      warning( "Please provide list.data.sources as a list of 'xts'-class objects!" )
    }
    check.result <- FALSE
  }
  if ( !all( Reduce( c, lapply( list.data.sources, is.list ) ) ) ){
    if ( !silent ){
      warning( "All elements of list.data.sources have to be lists!" )
    }
    check.result <- FALSE
  }
  if ( !all( Reduce( c, lapply( list.data.sources, function( ss )
    Reduce( c, lapply( ss, is.zoo ) ) ) ) ) ){
      if ( all( Reduce( c, lapply( list.data.sources, function( ss )
        Reduce( c, lapply( ss, is.data.frame ) ) ) ) ) ){
        ## This second data format allows the individual time series
        ## to be of type 'data.frame'. Have to be converted into class
        ## 'xts'.
        if ( !silent ){
          print( "Converting the input data into class 'xts' for internal handling..." )
        }
        ## An auxiliary object will be generated holding both the
        ## successfully converted series and those replaced by NULL
        ## since their conversion failed.
        list.data.sources.aux <-
          lapply( list.data.sources, function( ss )
            lapply( ss, climexUI:::convert.data.frame.to.xts ) )
        ## Discard all series for which the conversion did fail.
        list.data.sources <-
          lapply( list.data.sources.aux, function( ss )
            ss[ !Reduce( c, lapply( ss, is.null ) ) ] )
      } else {
        if ( !silent ){
          warning( "All elements of the lists within list.data.sources have to be 'xts'-class objects!" )
        }
        check.result <- FALSE
      }
  }
  if ( is.null( names( list.data.sources ) ) ||
       any( Reduce( c, lapply( list.data.sources, function( ll )
         is.null( names( ll ) ) ) ) ) ){
    if ( !silent ){
      warning( "All elements in list.data.sources have to be named!" )
    }
    check.result <- FALSE
  }
  if ( !is.data.frame( data.frame.positions ) &&
      !any( class( data.frame.positions ) == "SpatialPointsDataFrame" )
      ){
    if ( !silent ){
      warning( "data.frame.positions has to be a data.frame or a SpatialPointsDataFrame!" )
    }
    check.result <- FALSE
  } else if ( is.data.frame( data.frame.positions ) &&
              !( all( c( "name", "longitude", "latitude" ) %in%
                      colnames( data.frame.positions ) ) ) ){
    if ( !silent ){
      warning( "data.frame.positions has to have at least the columns 'name', 'longitude', and 'latitude" )
    }
    check.result <- FALSE
  } else if ( any( class( data.frame.positions ) ==
                   "SpatialPointsDataFrame" ) &&
              !( "name" %in% colnames( data.frame.positions@data ) ) ){
    if ( !silent ){
      warning( "a column called 'name' containing the corresponding station names must be included in data.frame.positions!" )
    }
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

##' @title Convert time series
##' @description Converts a time series provided as \code{data.frame}
##'   into an object of class \pkg{xts}.
##'
##' @param  input.df  \code{data.frame},  which has  to  contain  two
##'   columns called  \code{value} and \code{date}. The  former has to
##'   be of class \code{numeric} and the latter of class \code{Date}.
##'
##' @importFrom xts xts
##' @return Either \code{NULL} if the conversion failed or an object
##'   of class \pkg{xts}.
convert.data.frame.to.xts <- function( input.df ){
  ## Check the structure of the input
  if ( !is.data.frame( input.df ) ||
       !all( colnames( input.df ) %in% c( "value", "date" ) ) ||
       !is.numeric( input.df$value ) ||
       !( class( input.df$date ) == "Date" ) ){
    return( NULL )
  }
  
  output.xts <- try( xts( input.df$value, order.by = input.df$date ) )
  ## Check whether the conversion did work.
  if ( any( class( output.xts ) == "try-error" ) ){
    return( NULL )
  } else {
    return( output.xts )
  }
}
  
