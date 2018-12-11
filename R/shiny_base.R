##' @title The Climex app initialization
##' @description Shiny app combining most of the tools used in extreme
##'   value analysis using the generalized extreme value (GEV) or
##'   generalized Pareto (GP) fitting via maximum likelihood.
##'
##' @details This app needs its own css file. In order to assure its
##'   correct behavior, the folder \emph{resource.directory} will be
##'   generated to store all the necessary configuration scripts.
##'
##' @param resource.directory Folder all the resources of the app,
##'   like JavaScript scripts, images etc., will be copied in. If
##'   NULL, the content will be copied in the directory contained in
##'   the global \emph{climex.path} variable pointing at
##'   \emph{~/R/climex/}. Note that the folder won't be removed
##'   afterwards. Default: NULL.
##' 
##' @family top-level
##'
##' @return starts a shiny app
##' @export
##' @import climex
##' @import shiny
##' @import leaflet
##' @importFrom xts xts
##' @author Philipp Mueller 
climex <- function( list.data.sources = NULL,
                   data.frame.positions = NULL,
                   resource.directory = NULL ){
  ## Checking the input
  if ( !is.null( list.data.sources ) &&
       !is.null( data.frame.positions ) ){
    input.checked <- climexUI:::check.input( list.data.sources,
                                            data.frame.positions )
    if ( input.checked$check.result == FALSE ||
         length( input.checked$list.data.sources ) == 0 ||
         nrow( input.checked$data.frame.positions ) == 0 ){
      list.data.sources <- NULL
      data.frame.positions <- NULL
    } else {
      ## The `check.input` function will convert the input into a
      ## format the functions of the climexUI package can handle. In
      ## addition, it discards all stations who's names did not occur
      ## in the data.frame.positions table.
      list.data.sources <- input.checked$list.data.sources
      data.frame.positions <- input.checked$data.frame.positions
    }
  }

  ## Fallback of the position data.
  if ( is.null( list.data.sources ) ||
       is.null( data.frame.positions ) ){
    ## Both input arguments have to be supplied.
    if ( !( is.null( list.data.sources ) &&
            is.null( data.frame.positions ) ) ){
      warning(
              "Both 'list.data.sources' and 'data.frame.positions' have to be provided. The default series will be used instead!" )
    }
    data( "list.data.sources.fallback", package = "climexUI" )
    data( "data.frame.positions.fallback", package = "climexUI" )
    list.data.sources <- list.data.sources.fallback
    data.frame.positions <- data.frame.positions.fallback
  }
  
  if ( is.null( resource.directory ) ){
    resource.directory <- getOption( "climex.path" )
  }
  ## Creating a folder, which will contain all the resources necessary
  ## to power the web app.
  if ( !dir.exists( resource.directory ) ){
    dir.create( resource.directory, recursive = TRUE )
  }
  ## Write out the data so the climex server can open and use it.
  save( list.data.sources, data.frame.positions,
       file = file.path( resource.directory, "input.RData" ) )
  
  ## This is, unfortunately, necessary since some JavaScript scripts
  ## are written out and some images are plotted which have to be
  ## accessible within the shiny app.  If there are not the
  ## appropriate files present to run the shiny app, the apps as well
  ## as the folder structure has to be generated
  if ( !dir.exists( file.path( resource.directory, "app" ) ) )
    dir.create( file.path( resource.directory, "app" ) )
  if ( !dir.exists( paste0( resource.directory, "app/www" ) ) )
    dir.create( file.path( resource.directory, "app/www" ) )
  ## In order to display the animation the app needs the jquery
  ## scianimator. Since it is not able to access a system file on its
  ## own the wrapper climex() will copy it to the apps folder
  if ( !dir.exists( file.path( resource.directory,
                              "app/www/jquery.scianimator.min.js" ) ) ){
    file.copy( file.path( system.file( "climex_app",
                                      package = "climexUI" ),
                         "/js/jquery.scianimator.min.js" ),
              to = file.path( resource.directory,
                             "app/www/jquery.scianimator.min.js" ),
              overwrite = TRUE )
  }
  ## source of the gif: http://i.imgur.com/seuaOqf.gif since this is a
  ## non-commercial product there shouldn't be any problems
  ## http://imgur.com/tos
  ## Get the newest copy of the script containing the loading GIF.
  file.copy(
    file.path( system.file( "climex_app", package = "climexUI" ),
              "/js/loadingGif.js" ),
    to = file.path( resource.directory, "app/www/loadingGif.js" ),
    overwrite = TRUE ) 
  file.copy( file.path( system.file( "climex_app",
                                    package = "climexUI" ),
                       "/res/loading.gif" ),
            to = file.path( resource.directory, "app/www/loading.gif" ) )
  writeLines( "shinyUI( climex.ui() )",
             con = file.path( resource.directory, "app/ui.R" ) )
  writeLines( "shinyServer( climex.server )",
             con = file.path( resource.directory, "app/server.R" ) )
  runApp( paste0( resource.directory, "app" ) )
}


##' @title The Climex app server
##' @description Server-side part of the \code{\link{climex}} function.
##'
##' @details Since it grew organically most of the features are
##'   defined inside this function. Okay, why did I decided to define
##'   the climex.server and the climex.ui function? Since in this way
##'   I can provide a local and a server side variant of the code
##'   which is maintained in the same place. I do not really like the
##'   idea of installing the newest version of the code from Github
##'   and than copy/pasting the code in some files somewhere in
##'   /srv/shiny-server.
##'
##' @param input Namespace input. For more details check out
##'   \url{http://shiny.rstudio.com/articles/modules.html}
##' @param output Namespace output.
##' @param session Namespace session.
##'
##' @return Function acting as the shiny server.
##' @import shiny
##' @import climex
##' @importFrom xts xts
##' @import dygraphs
##' @import ggplot2
##'
##' @family top-level
##' 
##' @export
##' 
##' @author Philipp Mueller 
climex.server <- function( input, output, session ){
  
  ## Create a custom environment to host the variables `last.values`,
  ## `last.1`, `last.2`, and `last.3` in, as well as the station data
  ## in. By referring to this environment the corresponding variables
  ## do not have to be defined globally.
  climex.environment <- new.env( parent = emptyenv() )

  ## Do we already see the fallback input?
  if ( !file.exists( file.path( getOption( "climex.path" ),
                            "input.RData" ) ) ){
    stop( "No input found to display on the climex server" )
  }
  load( file.path( getOption( "climex.path" ), "input.RData" ),
       envir = climex.environment )
  
######################################################################
######### Customizing the sidebar and launching its reactives ########
######################################################################
  ## Everything in my code is always of the style this.is.a.name. So
  ## why do I use the camel case thisIsAName for the shiny objects?
  ## Well, since CSS file do not support the point separator. Type of
  ## database (input, DWD, artificial data)
  output$sidebarDataBase <-
    climexUI:::sidebarDataBase( session, climex.environment )
  ## Individual station or location (GEV)/scale(GP) for artificial
  ## data
  output$sidebarDataSource <-
    climexUI:::sidebarDataSource( reactive( input$selectDataBase ),
                                 reactive( input$radioEvdStatistics ),
                                 reactive.chosen,
                                 selected.station )
  ## Measurement type or scale (GEV)/shape(GP) for artificial data
  output$sidebarDataType <-
    climexUI:::sidebarDataType( reactive( input$selectDataBase ),
                             reactive( input$radioEvdStatistics ) )
  ## File input prompt for "input" or shape for GEV artificial data
  output$sidebarLoading <-
    climexUI:::sidebarLoading( session,
                            reactive( input$selectDataBase ),
                            reactive( input$radioEvdStatistics ) )
  ## Removing incomplete years (GEV) or clusters (GP)
  output$sidebarCleaning <-
    climexUI:::sidebarCleaning( reactive( input$radioEvdStatistics ),
                             reactive( input$selectDataBase ) )
  ## Slider for choosing the length of the artificial time series
  output$sidebarSeriesLength <-
    climexUI:::sidebarSeriesLength( reactive( input$selectDataBase ) )
  ## Introducing a dropdown menu for the deseasonalization in the
  ## Options box in the General tab.
  output$deseasonalizeSelection <-
    climexUI:::deseasonalizeSelection(
                 reactive( input$selectDataBase ) )
  ## A radioButton to determine whether to calculate the minimal or
  ## maximal extremes
  output$generalButtonMinMax <-
    climexUI:::generalButtonMinMax(
                 reactive( input$radioEvdStatistics ),
                 reactive( input$selectDataType ) )
  ## Displaying a loading gif whenever the app is busy
  callModule( climexUI:::sidebarLoadingGif, "busy" )
  ## Display a link to the app's imprint whenever it is run using
  ## shiny-server
  output$sidebarImprint <- climexUI:::sidebarImprint( session )
  ## Reactive value which holds the selected stations chosen either
  ## via the leaflet map or the sidebar
  reactive.chosen <- climexUI:::data.chosen(
                                  reactive( input$selectDataBase ),
                                  reactive( input$sliderYears ),
                                  reactive( input$selectDataType ),
                                  climex.environment )
  ## Reactive value selecting a specific time series according to the
  ## choices in the sidebar/leaflet map
  reactive.selection <-
    climexUI:::data.selection(
                 reactive.chosen, reactive( input$selectDataSource ),
                 reactive( input$selectDataBase ),
                 reactive( input$sliderThreshold ),
                 reactive( input$radioEvdStatistics ),
                 reactive( input$sliderArtificialDataLocation ),
                 reactive( input$sliderArtificialDataScale ),
                 reactive( input$sliderArtificialDataShape ),
                 reactive( input$buttonDrawTS ),
                 reactive( input$sliderSeriesLength ) )
######################################################################
  
######################################################################
#################### Data preprocessing and fitting ##################
######################################################################
  ## Slider input determining the block length (GEV) or threshold (GP)
  output$generalExtremeExtraction <-
    climexUI:::generalExtremeExtraction(
                 reactive( input$radioEvdStatistics ),
                 climexUI:::deseasonalize.interactive,
                 reactive( input$selectDeseasonalize ),
                 reactive( input$buttonMinMax ), reactive.selection,
                 reactive( input$selectDataBase ) )
  ## Reactive value containing a list of the extracted extreme events,
  ## the deseasonalized and pure time series. In addition, it's also
  ## performing both the extraction and the deseasonalization
  reactive.extreme <-
    climexUI:::data.extremes(
                 reactive.selection,
                 reactive( input$radioEvdStatistics ),
                 reactive( input$sliderBlockLength ),
                 reactive( input$sliderThreshold ),
                 reactive( input$checkboxDecluster ),
                 climexUI:::deseasonalize.interactive,
                 reactive( input$selectDeseasonalize ),
                 reactive( input$selectDataBase ),
                 reactive( input$buttonMinMax ),
                 climexUI:::extremes.interactive,
                 climexUI:::cleaning.interactive,
                 reactive( input$checkboxIncompleteYears ) )
  ## Reactive value performing the GEV/GP fit to the selected time
  ## series and providing the fitted object of class 'climex.fit.gev'
  ## or climex.fit.gpd'.
  reactive.fitting <-
    climexUI:::data.fitting(
                 reactive.extreme, reactive.rows,
                 climexUI:::fit.interactive,
                 reactive( input$radioEvdStatistics ),
                 reactive( input$buttonMinMax ),
                 reactive( input$sliderThreshold ),
                 reactive( input$selectDataBase ) )
######################################################################
  
######################################################################
############### Plotting of the time series and fit ##################
######################################################################
  ## Displaying of the original, intermediate and final time series.
  ## Reactive value containing a logical vector indicating which
  ## element of reactive.extreme()[[ 1 ]] should still be used after
  ## clicking and brushing in the extreme's ggplot2 plot. In addition
  ## the plotting and rendering of the extremes, deseasonalized and
  ## pure time series is done in here.
  reactive.rows <- callModule( climexUI:::generalTimeSeriesPlot, "ts",
                              reactive.extreme,
                              reactive( input$selectDataBase ),
                              reactive( input$selectDataType ),
                              reactive( input$radioEvdStatistics ),
                              reactive( input$sliderThreshold ),
                              reactive( input$buttonMinMax ) )
  ## Plotting of the fitted distribution and the corresponding PP, QQ
  ## and return level goodness-of-fit plots.
  callModule( climexUI:::generalFitPlot, "fit", reactive.extreme,
             reactive.rows, reactive.fitting,
             reactive( input$buttonMinMax ),
             reactive( input$radioEvdStatistics ),
             reactive( input$selectDataBase ), 
             reactive( input$selectDataType ),
             reactive( input$sliderThreshold ) )
  ## Table containing the fits results. Those four global variable
  ## will store the previous results of the GEV/GP fitting. This is
  ## unfortunately necessary in order to highlight the progress in the
  ## table red or green.

  ## Assign the place holder to the custom environment. This way they
  ## are accessible between function calls without handing them over,
  ## but are not defined globally either.
  climex.environment$last.values <-
    climex.environment$last.1 <-
      climex.environment$last.2 <-
        climex.environment$last.3 <- rep( 0,  )
  output$generalFitStatistics <-
    climexUI:::generalFitStatistics(
                 reactive.fitting, reactive.extreme,
                 reactive( input$sliderThreshold ),
                 reactive( input$buttonMinMax ),
                 reactive( input$radioEvdStatistics ),
                 climexUI:::color.table, climex.environment )
######################################################################
  ## Module providing the leaflet map tab and returning a reactive
  ## value containing all stations with a length longer than the
  ## minimal length slider in the Leaflet tab.
  selected.station <- callModule(
      climexUI:::leafletClimex, "leaflet", reactive.chosen,
      reactive( input$buttonMinMax ),
      reactive( input$radioEvdStatistics ),
      reactive( input$sliderYears ), reactive.extreme,
      reactive.fitting,
      reactive( input$sliderThreshold ), climexUI:::fit.interactive,
      climexUI:::cleaning.interactive,
      climexUI:::deseasonalize.interactive,
      climexUI:::extremes.interactive,
      reactive( input$selectDataSource ),
      reactive( input$checkboxIncompleteYears ),
      reactive( input$checkboxDecluster ),
      reactive( input$selectDeseasonalize ),
      reactive( input$sliderBlockLength ),
      reactive( input$selectDataBase ), climex.environment )
}

##' @title The Climex app UI
##' @description The user interface for the \code{\link{climex}}
##'   function. 
##'
##' @param selected Choose which tab is supposed to be selected when
##' starting the app
##'
##' @details Contains all the HTML codes.
##'
##' @return HTML code of the interface
##' @import shiny
##' @import climex
##' @importFrom shinydashboard box
##' @importFrom shinydashboard dashboardPage
##' @importFrom shinydashboard dashboardHeader
##' @importFrom shinydashboard dashboardSidebar
##' @importFrom shinydashboard dashboardBody
##' @importFrom shinydashboard menuItemOutput
##' @importFrom shinydashboard menuItem
##' @importFrom shinydashboard renderMenu
##' @importFrom shinydashboard sidebarMenu
##' @importFrom shinydashboard tabItem
##' @importFrom shinydashboard tabItems
##' @importFrom shinydashboard tabBox
##' @importFrom dygraphs dygraphOutput
##' @importFrom htmltools includeCSS
##' @importFrom htmltools h2
##' 
##' @export
##'
##' @family top-level
##' 
##' @author Philipp Mueller 
climex.ui <- function( selected = c( "Map", "General" ) ){
  if ( missing( selected ) )
    selected <- "Map"
  selected <- match.arg( selected )
  dashboardPage(
    shinytoastr::useToastr(),
    header = dashboardHeader(
        title = a(
            "Climex",
            href = "https://github.com/theGreatWhiteShark/climex",
            id = "climexLink" )
    ),
    sidebar = dashboardSidebar(
      sidebarMenu(
        menuItem( "Map", tabName = "tabMap",
                 icon = icon( "leaf", lib = "glyphicon" ),
                 selected = ifelse( selected == "Map",
                                   TRUE, FALSE ) ),
        menuItem( "General", tabName = "tabGeneral",
                 icon = icon( "stats", lib = "glyphicon" ),
                 selected = ifelse( selected == "General",
                                   TRUE, FALSE ) ),
        climexUI:::sidebarDataBaseInput(),
        climexUI:::sidebarDataSourceInput(),
        climexUI:::sidebarDataTypeInput(),
        climexUI:::sidebarLoadingInput(),
        climexUI:::sidebarSeriesLengthInput(),
        climexUI:::sidebarCleaningInput(),
        climexUI:::sidebarLoadingGifOutput( "busy" ),
        climexUI:::sidebarImprintInput() ) ),
    body = dashboardBody(
      ## shinyjs::useShinyjs(),
        includeCSS( paste0(
            system.file( "climex_app", package = "climexUI" ),
            "/css/climex.css" ) ),
        includeCSS( paste0(
            system.file( "climex_app", package = "climexUI" ),
            "/css/reset.css" ) ),
        includeCSS( paste0(
            system.file( "climex_app", package = "climexUI" ),
            "/css/styles.css" ) ),
        includeCSS( paste0(
            system.file( "climex_app", package = "climexUI" ),
            "/css/scianimator.css" ) ),
      tabItems(
        tabItem(
          tabName = "tabMap",
          tags$style( type = "text/css",
                     "#leaflet-map {height: calc(100vh - 80px) !important;}" ),
         climexUI:::leafletClimexUI( "leaflet" ) ),
        tabItem(
          tabName = "tabGeneral",
          ## In order guarantee the correct behavior of the rendering
          ## of the boxes and tables for smaller screen sizes too I
          ## assign customized classes to the boxes and reconfigure
          ## their ordering using CSS3 if the max-width is below a
          ## certain threshold
          fluidRow(
            climexUI:::generalFitPlotOutput( "fit" ),
            box( title = h2( "Options" ), width = 4,
              height = 505, background = "orange",
              id = "boxGevStatistics",
              radioButtons( "radioEvdStatistics", 
                           label = "Limiting distribution",
                           inline = TRUE,
                           choices = c( "GEV", "GP" ),
                           selected = "GEV" ),
              climexUI:::generalExtremeExtractionInput(),
              climexUI:::generalButtonMinMaxInput(),
              climexUI:::deseasonalizeSelectionInput() ) ),
          fluidRow(
              climexUI:::generalFitStatisticsTable(),
              climexUI:::generalTimeSeriesPlotOutput( "ts" ) ) ) ) ) )
}
