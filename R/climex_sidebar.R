### Contains all modules associated with the sidebar of the climex
### app.

##' @title Selecting the database in the \code{climex} app
##' @description Selecting the database.
##' @details Provides the \code{\link[shinydashboard]{menuItemOutput}}
##'   for \code{\link{sidebarDataBase}}. See the later one for
##'   details. 
##'
##' @importFrom shinydashboard menuItemOutput
##'
##' @family sidebar
##' 
##' @return \code{\link[shinydashboard]{menuItemOutput}}
##' @author Philipp Mueller 
sidebarDataBaseInput <- function(){
  menuItemOutput( "sidebarDataBase" )
}

##' @title Selecting the database in the \code{climex} app
##' @description Selecting the database.
##' @details For now there are three different data sources you can
##'   choose:the input provided when calling \code{\link{climex}} on
##'   localhost or a file loaded via the sidebar, the station data of
##'   the German weather service (DWD) and artificial data sampled
##'   from a GEV distribution. When the climex app is used with the
##'   shiny-server, the input option will be disabled. Beware: it's
##'   not a true module! Using the namespaces does not really make
##'   sense because it has to be accessed from outside of this
##'   context. 
##'
##' @param session Namespace session. For details check out
##'   \url{http://shiny.rstudio.com/articles/modules.html}
##' @param climex.environment Environment containing the global
##'   variables used within the \code{climex} app. Namely the last
##'   values displayed in the table and the lists containing the
##'   station data. 
##' 
##' @import shiny
##'
##' @family sidebar
##'
##' @return \code{\link[shinydashboard]{renderMenu}}
##' @author Philipp Mueller 
sidebarDataBase <- function( session, climex.environment ){
  ## Disable the input option for the shiny server
  renderMenu( {
    selectInput( "selectDataBase", "Data set",
                choices = c( names(
                  climex.environment$list.data.sources ),
                  "Artificial data" ),
                selected = names(
                  climex.environment$list.data.sources )[ 1 ] )
  })
}

##' @title Sidebar menu selection in the \code{climex} app
##' @description Sidebar menu selection.
##' @details Provides the \code{\link[shinydashboard]{menuItemOutput}}
##'   for \code{\link{sidebarDataSource}}. See the later one for
##'   details.
##'
##' @importFrom shinydashboard menuItemOutput
##'
##' @family sidebar
##'
##' @return \code{\link[shinydashboard]{menuItemOutput}}
##' @author Philipp Mueller 
sidebarDataSourceInput <- function(){
  menuItemOutput( "sidebarDataSource" )
}

##' @title Sidebar menu selection in the \code{climex} app
##' @description Sidebar menu selection.
##' @details Provides the second selection menu in the sidebar.
##'   If \code{selectDataBase}, provided by
##'   \code{\link{sidebarDataBase}}, was set to \code{"Artificial data"}, the
##'   menu will be a numerical input slider to provide the location
##'   parameter for the GEV or scale parameter for the GP
##'   distribution. Else it will be a drop down menu listing the names
##'   of all available stations (see \code{\link{data.chosen}}).
##'
##' @param  selectDataBase Character  (select) input to  determine the
##'   data source.  It  is either of one of the  names of the provided
##'   list   in   the   \code{list.data.sources}   argument   of   the
##'   \code{\link{climex}} function or \emph{Artificial data}. In case
##'   of the latter  choice, the function \code{\link{data.selection}}
##'   will provide a \emph{reactive}  object containing random numbers
##'   drawn     from     the      distribution     specified     using
##'   \code{radioEvdStatistics}. Default =  a random element of
##'   the provided input.
##' @param  radioEvdStatistics Character  (radio) input  determining
##'   whether  the GEV  or  GP  distribution shall  be  fitted to  the
##'   data. Choices: c( "GEV", "GP" ), default = "GEV".
##' @param  reactive.chosen Reactive  value containing  a list  of the
##'   \code{list}  of all  provided stations  and a  \code{data.frame}
##'   containing the meta data.
##' @param   selected.station  Reactive   value   provided  by   the
##'   \code{\link{leafletClimex}}  function. It  contains the  name of
##'   the  station   selected  by   the  user   by  clicking   on  the
##'   \pkg{leaflet} map. It's provided  as a character and \code{NULL}
##'   if no click occurred so far.
##'
##' @import shiny
##'
##' @family sidebar
##'
##' @return \code{\link[shinydashboard]{renderMenu}}
##' @author Philipp Mueller 
sidebarDataSource <- function( selectDataBase, radioEvdStatistics,
                              reactive.chosen, selected.station ){
  renderMenu( {
    ## If artificial data was choosen as the input source, display
    ## a slider for the location parameter of the parent GEV
    ## distribution
    if ( is.null( selectDataBase() ) ||
         ( ( selectDataBase() != "Artificial data" ) &&
           is.null( reactive.chosen() ) ) ){
      return( NULL )
    } else if ( selectDataBase() == "Artificial data" ){
      if ( is.null( radioEvdStatistics() ) ||
           radioEvdStatistics() == "GEV" ){
        ## For GEV data
        sliderInput( "sliderArtificialDataLocation",
                    "Location", -30, 30, 1, round = 0 )
      } else {
        ## For GP data just skip the slider for the location parameter
        sliderInput( "sliderArtificialDataScale",
                    "Scale", 0, 4, .8, round = 0, step = .1 )
      }
    } else {
      ## Since this is not a crucial element and the other elements
      ## have a fallback to the Potsdam time series we can just wait
      ## until input$selectDataBase and input$sliderYears( used in
      ## data.chosen ) are initialized. 
      ## Use the Map tab to choose a individual station
      if ( !is.null( selected.station() ) ){
        station.name <- selected.station()
      } else {
        ## If no station was selected yet, pick a random one.
          station.name <- as.character(
            reactive.chosen()$positions$name[
              sample( seq( 1 : nrow( reactive.chosen()$position ) ),
                     size = 1 ) ] )
      }
      ## export drop-down menu
      selectInput( "selectDataSource", "Station",
                  choices = as.character( reactive.chosen()$positions$name ),
                  selected = as.character( station.name ) )
    }
  } )
}

##' @title Sidebar menu selection in the \code{climex} app
##' @description Sidebar menu selection.
##' @details Provides the \code{\link[shinydashboard]{menuItemOutput}}
##'   for \code{\link{sidebarDataType}}. See the later one for
##'   details. 
##'
##' @importFrom shinydashboard menuItemOutput
##'
##' @family sidebar
##'
##' @return \code{\link[shinydashboard]{menuItemOutput}}
##' @author Philipp Mueller 
sidebarDataTypeInput <- function(){
  menuItemOutput( "sidebarDataType" )
}

##' @title Sidebar menu selection in the \code{climex} app
##' @description Sidebar menu selection.
##' @details  Provides the  third selection menu  in the  sidebar.  If
##'   \code{selectDataBase},                provided                by
##'   \code{\link{sidebarDataBase}},         was        set         to
##'   \code{"Artificial  data"}, the  menu will  be a  numerical input
##'   slider  to provide  the scale  parameter  for the  GEV or  shape
##'   parameter for the  GP distribution. Else it will be  a drop down
##'   menu listing the different types of data available at the chosen
##'   station.
##'
##' @param selectDataBase Character (select) input to determine the
##'   data source. It is either of one of the names of the provided
##'   list in the \code{list.data.sources} argument of the
##'   \code{\link{climex}} function or \emph{Artificial data}. In case
##'   of the latter choice, the function \code{\link{data.selection}}
##'   will provide a \emph{reactive} object containing random numbers
##'   drawn from the distribution specified using
##'   \code{radioEvdStatistics}. Default = a random element of
##'   the provided input.
##' @param radioEvdStatistics Character (radio) input determining
##'   whether the GEV or GP distribution shall be fitted to the
##'   data. Choices: c( "GEV", "GP" ), default = "GEV".
##' 
##' @import shiny
##'
##' @family sidebar
##'
##' @return \code{\link[shinydashboard]{renderMenu}}
##' @author Philipp Mueller
sidebarDataType <- function( selectDataBase, radioEvdStatistics ){
  renderMenu( {
    if ( !is.null( selectDataBase() ) &&
         selectDataBase() == "Artificial data" ){
        if ( is.null( radioEvdStatistics() ) ||
             radioEvdStatistics() == "GEV" ){
          sliderInput( "sliderArtificialDataScale", "Scale", 0, 4,
                      0.8, round = 0, step = .1 )
        } else {
          ## For GP distributed data
          sliderInput( "sliderArtificialDataShape", "Shape", -.7, .7,
                      -.25, round = -2, step = .1 )
        }
    } else {
      return( NULL )
    }
  } )
}

##' @title Sidebar menu selection in the \code{climex} app
##' @description Sidebar menu selection.
##' @details Provides the \code{\link[shinydashboard]{menuItemOutput}}
##'   for \code{\link{sidebarLoading}}. See the later one for
##'   details. 
##'
##' @importFrom shinydashboard menuItemOutput
##'
##' @family sidebar
##'
##' @return \code{\link[shinydashboard]{menuItemOutput}}
##' @author Philipp Mueller 
sidebarLoadingInput <- function(){
  menuItemOutput( "sidebarLoading" )
}

##' @title Sidebar menu selection in the \code{climex} app
##' @description Sidebar menu selection.
##' @details  Provides the fourth  selection menu in the  sidebar.  If
##'   \code{selectDataBase},                provided                by
##'   \code{\link{sidebarDataBase}},         was        set         to
##'   \code{"Artificial  data"}, the  menu will  be a  numerical input
##'   slider to provide the shape parameter for the GEV or \code{NULL}
##'   for the  GP distribution. Else it  will be a file  input for the
##'   user to load additional station data.
##' @param session  Namespace session.  For details  check out  \url{
##'   http://shiny.rstudio.com/articles/modules.html}
##' @param  selectDataBase Character  (select) input to  determine the
##'   data source.  It is either of  one of the names  of the provided
##'   list   in   the   \code{list.data.sources}   argument   of   the
##'   \code{\link{climex}} function or \emph{Artificial data}. In case
##'   of the latter  choice, the function \code{\link{data.selection}}
##'   will provide a \emph{reactive}  object containing random numbers
##'   drawn     from     the      distribution     specified     using
##'   \code{radioEvdStatistics}. Default =  a random element of
##'   the provided input.
##' @param  radioEvdStatistics  Character (radio)  input  determining
##'   whether  the GEV  or  GP  distribution shall  be  fitted to  the
##'   data. Choices: c( "GEV", "GP" ), default = "GEV".
 
##' @import shiny
##'
##' @family sidebar
##'
##' @return \code{\link[shinydashboard]{renderMenu}}
##' @author Philipp Mueller
sidebarLoading <- function( session, selectDataBase,
                           radioEvdStatistics ){
  renderMenu( {
    if ( is.null( selectDataBase() ) )
      return( NULL )
    if ( selectDataBase() == "Artificial data" &&
         ( is.null( radioEvdStatistics() ) ||
           radioEvdStatistics() == "GEV" ) ){
      sliderInput( "sliderArtificialDataShape", "Shape", -.7, .7,
                  -0.25, round = -2, step = .1 )
    } else if ( selectDataBase() == "Input" && (
      session$clientData$url_hostname == "localhost" ||
      session$clientData$url_hostname == "127.0.0.1"  ) ){
      ## due to security concerns only allow the file selection on
      ## localhost
      fileInput( "fileInputSelection", "Choose a .RData file" )
    } else
      return( div( id = "aux-placeholder", style = "height: 0px;" ) )
  } )
}

##' @title Data cleaning in the \code{climex} app
##' @description Toggling the cleaning of the time series.
##' @details Provides the \code{\link[shinydashboard]{menuItemOutput}}
##'   for \code{\link{sidebarCleaning}}. See the later one for
##'   details. 
##'
##' @importFrom shinydashboard menuItemOutput
##'
##' @family sidebar
##'
##' @return \code{\link[shinydashboard]{menuItemOutput}}
##' @author Philipp Mueller 
sidebarCleaningInput <- function(){
  uiOutput( "sidebarCleaning" )
}

##' @title Data cleaning in the \code{climex} app
##' @description Toggling the cleaning of the time series.
##' @details A checkbox input. If toggled and the GEV distribution was
##'   chosen via \code{radioEvdStatistics}, all incomplete years will
##'   be removed from the time series. If on the other hand the GP
##'   distribution was picked, all cluster will be removed and only
##'   their highest point will remain.
##' 
##' @param radioEvdStatistics Character (radio) input determining
##'   whether the GEV or GP distribution shall be fitted to the
##'   data. Choices: c( "GEV", "GP" ), default = "GEV".
##' 
##' @param selectDataBase Character (select) input to determine the
##'   data source. It is either of one of the names of the provided
##'   list in the \code{list.data.sources} argument of the
##'   \code{\link{climex}} function or \emph{Artificial data}. In case
##'   of the latter choice, the function \code{\link{data.selection}}
##'   will provide a \emph{reactive} object containing random numbers
##'   drawn from the distribution specified using
##'   \code{radioEvdStatistics}. Default = a random element of
##'   the provided input.
##' 
##' @import shiny
##'
##' @family sidebar
##'
##' @return \code{\link[shinydashboard]{renderMenu}}
##' @author Philipp Mueller
sidebarCleaning <- function( radioEvdStatistics, selectDataBase ){
  renderUI({
    ## When applying the blocking method incomplete years distort
    ## the time series and have to be excluded. When using the
    ## threshold method on the other hand clusters are most likely
    ## to occure due to short range correlations. This has to be
    ## avoided by using declustering algorithms (which mainly picks
    ## the maximum of a specific cluster)
    if ( is.null( selectDataBase() ) ||
         selectDataBase() != "Artificial data" ){
      if ( is.null( radioEvdStatistics() ) ||
           radioEvdStatistics() == "GEV" ){
        checkboxInput( "checkboxIncompleteYears",
                      "Remove incomplete years", TRUE )
      } else {
        checkboxInput( "checkboxDecluster",
                      "Declustering of the data", TRUE )
      }
    } else {
      ## When working with artificial data cleaning does not make
      ## sense. Instead we need to resample the time series.
      div( actionButton( "buttonDrawTS", "Draw" ),
          style = "padding: 12px 15px 0px 12px;" )
    }
  })
}

##' @title Series length in the \code{climex} app
##' @description Slider to determine the length of an artificial time
##'   series.
##' @details Provides the \code{\link[shinydashboard]{menuItemOutput}}
##'   for \code{\link{sidebarSeriesLength}}. See the later one for
##'   details.
##'
##' @importFrom shinydashboard menuItemOutput
##'
##' @family sidebar
##'
##' @return \code{\link[shinydashboard]{menuItemOutput}}
##' @author Philipp Mueller 
sidebarSeriesLengthInput <- function(){
  menuItemOutput( "sidebarSeriesLength" )
}

##' @title Series length in the \code{climex} app
##' @description Slider to determine the length of an artificial time
##'   series.
##' @details Numerical slider which will only be displayed when
##' \code{"Artificial data"} is selected in \code{selectDataBase}.
##' 
##' @param selectDataBase Character (select) input to determine the
##'   data source. It is either of one of the names of the provided
##'   list in the \code{list.data.sources} argument of the
##'   \code{\link{climex}} function or \emph{Artificial data}. In case
##'   of the latter choice, the function \code{\link{data.selection}}
##'   will provide a \emph{reactive} object containing random numbers
##'   drawn from the distribution specified using
##'   \code{radioEvdStatistics}. Default = a random element of
##'   the provided input.
##' 
##' @import shiny
##'
##' @family sidebar
##'
##' @return \code{\link[shinydashboard]{menuItemOutput}}
##' @author Philipp Mueller 
sidebarSeriesLength <- function( selectDataBase ){
  renderMenu({
    if ( !is.null( selectDataBase() ) &&
         selectDataBase() == "Artificial data" ){
      ## In order to display the return levels on a
      ## logarithmic scale, the exponent will be
      ## chosen via the slider and its transformation
      ## to 10^x is done in the script and inside a
      ## JavaScript function 
      sliderInput( "sliderSeriesLength", "Length", 1.477121, 3,
                  2, round = 1, step = .1 )
    } else {
      div( id = "aux-placeholder", style = "height: 0px" )
    }
  })
}

##' @title Data selection in the \code{climex} app
##' @description Reactive value selecting an individual time series.
##' @details According to the choice in \code{selectDataBase} an
##'   artificial time series will be sampled or one from a database
##'   will be selected. For the latter one the reactive value
##'   \code{\link{data.chosen}} will be constulted. The length of
##'   the generated time series is determined by the
##'   \code{sliderSeriesLength}.
##'
##' @param reactive.chosen Reactive value containing a list of the
##'   list of all provided stations and a \code{data.frame} containing
##'   the meta data.
##' @param selectDataSource Menu output in the sidebar. It is a
##'   \code{character string} describing the selected station's name.
##' @param selectDataBase Character (select) input to determine the
##'   data source. It is either of one of the names of the provided
##'   list in the \code{list.data.sources} argument of the
##'   \code{\link{climex}} function or \emph{Artificial data}. In case
##'   of the latter choice, the function \code{\link{data.selection}}
##'   will provide a \emph{reactive} object containing random numbers
##'   drawn from the distribution specified using
##'   \code{radioEvdStatistics}. Default = a random element of
##'   the provided input.
##' @param sliderThreshold Numerical (slider) input determining the
##'   threshold used within the GP fit and the extraction of the
##'   extreme events. Boundaries: minimal and maximal value of the
##'   deseasonalized time series (rounded). Default: 0.8* the upper
##'   end point. 
##' @param radioEvdStatistics Character (radio) input determining
##'   whether the GEV or GP distribution shall be fitted to the
##'   data. Choices: c( "GEV", "GP" ), default = "GEV".
##' @param sliderArtificialDataLocation Numerical (slider) input
##'   providing the location parameter to generate an artificial time
##'   series. If \code{radioEvdStatistics} equals \code{"GP"} or
##'   \code{selectDataBase} does not equal \code{"Artificial data"},
##'   this argument will be \code{NULL}.
##' @param sliderArtificialDataScale Numerical (slider) input
##'   providing the scale parameter to generate an artificial time
##'   series. If \code{selectDataBase} does not equal
##'   \code{"Artificial data"}, this argument will be \code{NULL}.
##' @param sliderArtificialDataShape Numerical (slider) input
##'   providing the shape parameter to generate an artificial time
##'   series. If \code{selectDataBase} does not equal\code{"Artificial
##'   data"}, this argument will be \code{NULL}.
##' @param reactive.redrawing Reactive event triggering whenever the
##'   user presses the \emph{Draw} button displayed when
##'   \code{"Artificial data"}' is chosen in \code{selectDataBase}. If
##'   not chosen, this button will not be defined.
##' @param sliderSeriesLength Numerical slider determining the length
##'   of an artificial time series. This one will only be present when
##'   \code{selectDataBase} is set to \code{"Artificial data"}.
##'
##' @family sidebar
##'
##' @importFrom xts xts
##' @import shiny
##'
##' @return Reactive value containing a \pkg{xts} class time series.
##' @author Philipp Mueller 
data.selection <- function( reactive.chosen, selectDataSource,
                           selectDataBase, sliderThreshold,
                           radioEvdStatistics,
                           sliderArtificialDataLocation,
                           sliderArtificialDataScale,
                           sliderArtificialDataShape,
                           reactive.redrawing, sliderSeriesLength ){
  reactive( {
    ## Selecting the data out of a pool of different possibilities
    ## or generate them artificially
    data.selected <- reactive.chosen()
    if ( ( is.null( selectDataSource() ) ||
           is.null( selectDataBase() ) ) ||
         ( is.null( data.selected ) &&
           selectDataBase() != "Artificial data" ) ){
      return( NULL )
    }
    if( selectDataBase() == "Artificial data" ){
      ## Wait for the sidebar to initials the sliders of the GEV
      ## parameters.
      if ( is.null( sliderArtificialDataLocation() ) ||
           is.null( sliderArtificialDataScale() ) ||
           is.null( sliderArtificialDataShape() ) ){
        return( NULL )
      }
      ## Redraw the time series using a button in the sidebar
      reactive.redrawing()
      ## To transform the artificial time series today's time
      ## stamp will be used and moved to 1900. All following
      ## points will be future years from this point on. This
      ## keeps the idea of annual block maxima.
      ## Since the slider just determines the exponent to the
      ## base of 10 in order to display it in a logarithmic way,
      ## I have to transform the input.
      series.length <- 10^sliderSeriesLength()
      dates <- rep( lubridate::now(), series.length )
      lubridate::year( dates ) <- 1900 +
        seq( 1, series.length )
      ## Whether to use GEV or GPD data.
      if ( is.null( radioEvdStatistics() ) ||
           radioEvdStatistics() == "GEV" ){
        model <- "gev"
      } else {
        model <- "gpd"
      }
      x.xts <- xts(
          climex::revd( n = series.length,
                        location = sliderArtificialDataLocation(),
                        scale = sliderArtificialDataScale(),
                        shape = sliderArtificialDataShape(),
                        model = model, silent = TRUE,
                        threshold = 0 ),
        order.by = dates )
    } else {
      ## There is a bug when switching from one data base into
      ## another: since the input$selectDataSource needs a
      ## little bit longer to update it tries to access a value
      ## in here which is might not present in the newly
      ## selected one. If this happens, just select the first
      ## element instead.
      if ( !any( names( data.selected[[ 1 ]] ) ==
                  selectDataSource() ) ){
        x.xts <- data.selected[[ 1 ]][[ 1 ]]
      } else
        x.xts <- data.selected[[ 1 ]][[
                   which( names( data.selected[[ 1 ]] ) ==
                          selectDataSource() ) ]] }
    return( x.xts )
  } )
}

##' @title Gif in the \code{climex} app
##' @description Showing a Gif in the sidebar while the app is
##'   processing/on hold 
##' @details Rendering of \code{\link{sidebarLoadingGif}}.
##'
##' @param id Namespace prefix
##'
##' @family sidebar
##'
##' @import shiny
##'
##' @return \code{\link[shiny]{div}}
##' @author Philipp Mueller 
sidebarLoadingGifOutput <- function( id ){
  # Create a namespace function using the provided id
  ns <- NS( id )
  div( uiOutput( ns( "loadingScript" ) ),
      uiOutput( ns( "loadingImage" ) ),
      id = "loadingWrapper" )
} 

##' @title Gif in the \code{climex} app
##' @description Showing a Gif in the sidebar while the app is
##'   processing/on hold 
##' @details This module will display a loading gif. Most of the code
##'   of this function is already written in a JavaScript script
##'   installed alongside the \pkg{climex} package. It just determines
##'   if the app is run on either a localhost (direct interaction) or
##'   on a server waiting for client interaction. Accordingly it looks
##'   up both the JavaScript script and the corresponding gif file.
##'
##' @param input Namespace input. For more details check out
##'   \url{http://shiny.rstudio.com/articles/modules.html}
##' @param output Namespace output.
##' @param session Namespace session.
##'
##' @family sidebar
##'
##' @import shiny
##' 
##' @return \code{\link[shiny]{div}}
##' @author Philipp Mueller 
sidebarLoadingGif <- function( input, output, session ){
  output$loadingImage <- renderUI({
    if ( session$clientData$url_hostname != "localhost" &&
         session$clientData$url_hostname != "127.0.0.1" ){
      folder <- "/assets/"
    } else
      folder <- ""
    return( img( src = paste0( folder, "loading.gif" ),
                id = session$ns( "loadingGif" ) ) ) } )
  output$loadingScript <- renderUI({
    if ( session$clientData$url_hostname != "localhost" &&
         session$clientData$url_hostname != "127.0.0.1" ){
      folder <- "/assets/"
    } else
      folder <- ""
    return( div( singleton(
        tags$script( src = paste0( folder, "loadingGif.js" )
                    ) ) ) )
  } )
}

##' @title Imprint of the \code{climex} app
##' @description Linking the imprint of the \code{climex} application.
##' @details Whenever the web app is run using shiny-server a link to
##'   an arbitrary HTML page (preferably an imprint) will be rendered.
##'   Else just an empty div will be added.
##'   Rendering of \code{\link{sidebarImprint}}.
##'
##' @family sidebar
##'
##' @importFrom shinydashboard menuItemOutput
##'
##' @return \code{\link[shinydashboard]{menuItemOutput}}
##' @author Philipp Mueller 
sidebarImprintInput <- function(){
  menuItemOutput( "sidebarImprint" )
}

##' @title Imprint of the \code{climex} app
##' @description Linking the imprint of the \code{climex} application.
##' @details Whenever the web app is run using shiny-server a link to
##'   an arbitrary HTML page (preferably an imprint) will be rendered.
##'   Else just an empty div will be added.
##'
##' @param session Namespace session. For details check out
##'   \url{http://shiny.rstudio.com/articles/modules.html}
##'
##' @family sidebar
##'
##' @import shiny
##'
##' @return \code{\link[shiny]{div}}
##' @author Philipp Mueller 
sidebarImprint <- function( session ){
  renderMenu({
    ## Only display the imprint in the shiny-server and not on
    ## localhost
    if ( session$clientData$url_hostname == "localhost" ||
         session$clientData$url_hostname == "127.0.0.1" ){
      div( id = "aux-placeholder", style = "height: 0px;" )
    } else {
      div( a( "Imprint", href = "/imprint.html",
             id = "climexImprint" ) )
    }
  })
} 
