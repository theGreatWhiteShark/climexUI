### Contains all modules associated with the "Map" tab of the climex
### app.

##' @title Leaflet interface of the \code{climex} app
##' @description Leaflet  interface to  select stations  or visualize
##'   spacial information.
##' 
##' @details  This   function   provides  the   user  interface   to
##'   \code{\link{leafletClimex}}.         It       consists        of
##'   \code{\link[leaflet]{leafletOutput}}            and            two
##'   \code{\link[shiny]{absolutePanel}} containing  the return level
##'   and time series length sliders as well as  the table displaying
##'   all the stations information.
##'
##' @param id Namespace prefix
##'
##' @family leaflet
##'
##' @import shiny
##' @import leaflet
##'
##' @return \code{\link[shiny]{tagList}}
##' @author Philipp Mueller 
leafletClimexUI <- function( id ){
  # Create a namespace function using the provided id
  ns <- NS( id )
  tagList(
      leafletOutput( ns( "map" ), width = "100%", height = 1000 ),
      ## 50px is the thickness of the top navigation bar
      absolutePanel( top = 50, right = 0, id = ns( "box" ),
                    ## This slider will not be wrapped in the ns()
                    ## function, since I have to access it outside of
                    ## this module too. (Not sure how to handle this
                    ## with namespacing)
                    sliderInput( "sliderYears",
                                "Minimal length [years]",
                                0, 155, value = 80, step = 1 ),
                    tableOutput( ns( "table" ) ),
                    ## a plot of height 0? Well, its actually a very
                    ## nice trick since I need a width value for the
                    ## generated pngs in the animation in pixel. But I
                    ## really want to make app to be rendered nicely
                    ## on different screen sizes.  Via the
                    ## session$clientData I can access the width and
                    ## height of plots. Thus I can access the width of
                    ## this specific box via the plotPlaceholder
                    ## without seeing it at all.
                    plotOutput( ns( "placeholder" ),
                               height = '1px', width = '100%' ) ),
      ## lift it a little but upwards so one can still see the
      ## card licensing
      absolutePanel( bottom = 32, right = 0,
                    id = ns( "markerBox" ),
                    ## In order to display the return levels on a
                    ## logarithmic scale, the exponent will be
                    ## chosen via the slider and its transformation
                    ## to 10^x is done in the script and inside a
                    ## JavaScript function 
                    sliderInput( ns( "sliderReturnLevel" ),
                                "Return level [years]",
                                1, 3, step = .1, round = 0,
                                value = 2 ),
                    actionButton( ns( "buttonDrawMarkers" ),
                                 "Calculate return levels" ) ) )
}

##' @title Leaflet interface in the \code{climex} app
##' @description Leaflet interface to select stations or visualize
##'   spacial information.
##' @details This module provides an interactive map to display the
##'   locations of the individual stations. The user can choose
##'   individual stations by clicking at them. In addition a dialog
##'   will pop up telling the stations name, the length of the
##'   time series, and the 20, 50, and 100 year return level
##'   calculated with the setting in the basic map without station
##'   positions select only those stations in Germany with a certain
##'   minimum number of years.
##'
##' @param input Namespace input. For more details check out
##'   \url{http://shiny.rstudio.com/articles/modules.html}
##' @param output Namespace output.
##' @param session Namespace session.
##' @param reactive.chosen Reactive value containing a list of the
##'   list of all provided stations and a \code{data.frame} containing
##'   the meta data. 
##' @param buttonMinMax Character (radio) input determining whether
##'   the GEV/GP distribution shall be fitted to the smallest or
##'   biggest values. Choices: c( "Max", "Min ), default = "Max".
##' @param radioEvdStatistics Character (radio) input determining
##'   whether the GEV or GP distribution shall be fitted to the
##'   data. Choices: c( "GEV", "GP" ), default = "GEV".
##' @param sliderYears Numerical (slider) input to determine the
##'   minimal length (in years) of the time series to be
##'   displayed. Minimal value is 0 and maximal is 155, the default
##'   value is 65 and the step width is 1.
##' @param reactive.extreme Reactive value returning a list containing
##'   three elements: 1. the blocked time series, 2. the
##'   deseasonalized time series, and 3. the pure time series.
##' @param reactive.fitting Reactive value containing the results of
##'   the fit (\code{\link[climex]{fit.gev}} or
##'   \code{\link[climex]{fit.gpd}} depending on
##'   \code{radioEvdStatistic}) to the blocked time series in 
##'   in the first element of the list returned by
##'   \code{\link{data.extremes}}.
##' @param sliderThreshold Numerical (slider) input determining the
##'   threshold used within the GP fit and the extraction of the
##'   extreme events. Boundaries: minimal and maximal value of the
##'   deseasonalized time series (rounded). Default: 0.8* the upper
##'   end point. 
##' @param fit.interactive Function used to perform the actual GEV/GP
##'   fit. \code{\link{fit.interactive}}
##' @param cleaning.interactive Function used to remove incomplete
##'   years from blocked time series or to remove clusters from data
##'   above a certain threshold. \code{\link{cleaning.interactive}}
##' @param deseasonalize.interactive Function used to remove
##'   seasonality from a given time
##'   series. \code{\link{deseasonalize.interactive}}
##' @param extremes.interactive Function used to split a time series
##'   into blocks of equal lengths and to just extract their maximal
##'   values or to extract all data points above a certain
##'   threshold value. Which option is chosen depends of the
##'   \code{radioEvdStatistic}. See
##'   \code{\link{extremes.interactive}}.
##' @param selectDataSource Menu output in the sidebar. Since this
##'   function should only be triggered when
##'   \code{selectDataBase} equals \code{"Input"}, this input
##'   will be a character string describing the selected station's
##'   name.
##' @param checkboxIncompleteYears Logical (checkbox) input
##'   determining whether to remove all incomplete years of a time
##'   series. This box  will be only available if
##'   \code{radioEvdStatistics} equals \code{"GEV"} and else will be
##'   \code{NULL}.
##' @param checkboxDecluster Logical (checkbox) input determining
##'   whether to remove all clusters in a time series and replace them
##'   by their maximal value. This box will be only available if
##'   \code{radioEvdStatistics} equals \code{"GP"} and else will be
##'   \code{NULL}.
##' @param selectDeseasonalize Character (select) input determining
##'   which deseasonalization method should be used to remove the
##'   short-range correlations from the provided time series.
##'   \code{\link{deseasonalizeSelectionInput}}
##' @param sliderBlockLength Numerical (slider) input determining the
##'   block length used in the GEV flavor of extreme value theory. On
##'   default it is set to one year.
##' @param selectDataBase Character (select) input to determine the
##'   data source. It is either of one of the names of the provided
##'   list in the \code{list.data.sources} argument of the
##'   \code{\link{climex}} function or \emph{Artificial data}. In case
##'   of the latter choice, the function \code{\link{data.selection}}
##'   will provide a \emph{reactive} object containing random numbers
##'   drawn from the distribution specified using
##'   \code{radioEvdStatistics}. Default = a random element of
##'   the provided input.
##' @param climex.environment Environment containing the global
##'   variables used within the \code{climex} app. Namely the last
##'   values displayed in the table and the lists containing the
##'   station data. 
##'
##' @family leaflet
##'
##' @import climex
##' @import shiny
##' @importFrom leaflet addLegend
##' 
##' @return Reactive value holding the selected station.
##' @author Philipp Mueller 
leafletClimex <- function( input, output, session, reactive.chosen,
                          buttonMinMax, radioEvdStatistics,
                          sliderYears, reactive.extreme,
                          reactive.fitting,
                          sliderThreshold, fit.interactive,
                          cleaning.interactive,
                          deseasonalize.interactive,
                          extremes.interactive, selectDataSource,
                          checkboxIncompleteYears, checkboxDecluster,
                          selectDeseasonalize, sliderBlockLength,
                          selectDataBase, climex.environment ){
  ## This variable contains the name of the previously selected
  ## station. It's a little bit ugly since it's global, but right now
  ## I'm lacking an alternative.
  station.name.previous <- NULL
  ## create custom markers.
  ## This is essentially the same marker but with different colors.
  ## The selected one should be colored red and all the others blue. 
  blue.icon <- makeIcon(
      iconUrl = paste0(
          system.file( "climex_app", package = "climexUI" ),
          "/www/marker-icon.png" ),
      iconWidth = 25, iconHeight = 41, iconAnchorX = 12.5,
      iconAnchorY = 41,
      shadowUrl = paste0( system.file( "climex_app",
                                      package = "climexUI" ),
                         "/www/marker-shadow.png" ), shadowWidth = 41,
      shadowHeight = 41, shadowAnchorX = 12.5, shadowAnchorY = 41 )
  red.icon <- makeIcon(
      iconUrl = paste0(
          system.file( "climex_app", package = "climexUI" ),
          "/www/select-marker.png" ),
      iconWidth = 25, iconHeight = 41, iconAnchorX = 12.5,
      iconAnchorY = 41,
      shadowUrl = paste0( system.file( "climex_app",
                                      package = "climexUI" ),
                         "/www/marker-shadow.png" ), shadowWidth = 41,
      shadowHeight = 41, shadowAnchorX = 12.5, shadowAnchorY = 41 )
  
  ## Create the underlying map containing the Openstreetmap tile.
  ## This is the fundamental layer and all the markers will be
  ## added on top of it.
  output$map <- renderLeaflet( {
    leaflet() %>% fitBounds( 5, 46, 13, 55 ) %>%
      ## Provide both the OpenTopoMaps and the OpenStreetMaps. The
      ## first one is more convenient for climate data but it is also
      ## quite unstable. Therefore the user has the option to choose
      ## the regular OSM map instead.
      addTiles( "http://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
               attribution = "<code> Kartendaten: \uA9 <a href='https://openstreetmap.org/copyright'>OpenStreetMap</a>-Mitwirkende, SRTM | Kartendarstellung: \uA9 <a href='http://opentopomap.org'>OpenTopoMap</a> (<a href='https://creativecommons.org/licenses/by-sa/3.0/'>CC-BY-SA</a> </code>)",
               group = "OpenTopoMaps" ) %>%
      addTiles( group = "OpenStreetMaps" ) %>%
      addLayersControl(
          baseGroups = c( "OpenTopoMaps", "OpenStreetMaps" ),
          overlayGroups = "stations",
          options = layersControlOptions( collapsed = FALSE ) )
    } )
  
  ## Depending on the number of minimal years and the selected data
  ## source markers will be placed at the geo-coordinates of the
  ## individual stations. 
  observe( {
    data.selected <- reactive.chosen()
    if ( !is.null( data.selected ) ){
      if ( any( is.na( c( data.selected[[ 2 ]]$longitude,
                         data.selected[[ 2 ]]$latitude ) ) ) ){
        ## I am dealing with either a placeholder or a compromised
        ## data.frame. Anyway, the leaflet map can not handle it
        print( "the selected data seems to be compromised" )
        return( NULL )
      }
      leafletProxy( session$ns( "map" ) ) %>%
        clearGroup( "stations" ) %>%
        addMarkers( data = data.selected[[ 2 ]], group = "stations",
                   lng = ~longitude,
                   icon = blue.icon, lat = ~latitude,
                   options = popupOptions( closeButton = FALSE ) )
        } } )
  
  ## The purpose of this function is to supply a data.frame
  ## containing the 50/100/500 year return level of all selected
  ## stations. Lets see how fast it will be. Maybe I will just
  ## calculate one return level per station.
  ## This will be calculated on demand (as soon as the user clicks
  ## the corresponding form)
  calculate.chosen.return.levels <- reactive( {
    ## Do not calculate the return level for more than
    ## max.number.of.stations stations. Else the calculation would
    ## just take way to long. Zooming is not helping
    max.number.of.stations <- 40
    data.selected <- reactive.chosen()
    if ( session$clientData$url_hostname != "localhost" &&
         session$clientData$url_hostname != "127.0.0.1" && 
         length( data.selected[[ 1 ]] ) > max.number.of.stations ){
      shinytoastr::toastr_error( "<center>Please select less stations using the 'Minimal length' slider! <br/>The calculation of the return level takes a lot of time.</center>",
                                title = "<center>Too many stations selected!</center>",
                                position = "top-center",
                                timeOut = 8000 )
      return( NULL )
    }
    ## selected return level and transform it to years
    return.level.year <- 10^input$sliderReturnLevel
    ## wait for initialization
    if ( is.null( input$sliderReturnLevel ) ||
         is.null( data.selected ) ){
      return( NULL )
    }
    ## if no geo-coordinates are provided for the time series,
    ## don't calculate the return levels
    if ( any( is.na( c( data.selected[[ 2 ]]$longitude,
                       data.selected[[ 2 ]]$latitude ) ) ) ){
      return( NULL )
    }
    ## clean the stations
    data.cleaned <- lapply(
        data.selected[[ 1 ]], cleaning.interactive,
        checkboxIncompleteYears, checkboxDecluster, sliderThreshold )
    ## deseasonalize them
    data.deseasonalized <-
      lapply( data.cleaned, deseasonalize.interactive,
             selectDeseasonalize, selectDataBase )
    ## block them
    data.blocked <- lapply( data.deseasonalized, extremes.interactive,
                           buttonMinMax, radioEvdStatistics,
                           sliderBlockLength, sliderThreshold,
                           checkboxDecluster )
    ## choose whether to calculate the GEV or GP parameters
    if ( is.null( radioEvdStatistics() ) ||
         radioEvdStatistics() == "GEV" ){
      model <- "gev"
      threshold <- NULL
    } else {
      model <- "gpd"
      threshold <- sliderThreshold()
    }
    ## Calculate the return level and append it to the
    ## data.selected[[ 2 ]] data.frame.
    return.level.vector <- rep( NaN, length( data.blocked ) )
    if ( is.null( buttonMinMax() ) || buttonMinMax() == "Max" ){ 
      for ( rr in 1 : length( data.blocked ) ){
        return.level.vector[ rr ] <-
          climex::return.level(
                      fit.interactive( data.blocked[[ rr ]],
                                      radioEvdStatistics,
                                      buttonMinMax,
                                      sliderThreshold ),
                      return.period = return.level.year,
                      model = model, error.estimation = "none",
                      total.length = length(
                          data.selected[[ 1 ]][[ rr ]] ),
                      threshold = threshold
                  )$return.level
      }
    } else {
      ## Calculating the return levels for the minimal extremes.
      for ( rr in 1 : length( data.blocked ) ){
        auxiliary.fit <- fit.interactive( data.blocked[[ rr ]],
                                         radioEvdStatistics,
                                         buttonMinMax,
                                         sliderThreshold )
        return.level.vector[ rr ] <-
          -1* climex::return.level(
                          c( -1* auxiliary.fit$par[ 1 ],
                            auxiliary.fit$par[ 2 ],
                            auxiliary.fit$par[ 3 ] ),
                          return.period = return.level.year,
                          model = model, error.estimation = "none",
                          total.length = length(
                              data.selected[[ 1 ]][[ rr ]] ),
                          threshold = threshold
                      )$return.level
      }
    }
    data.selected[[ 2 ]]$return.level <- return.level.vector
    return( data.selected[[ 2 ]] )
  } ) 
  
  observe( {
    ## the calculation of all the return levels of the stations
    ## just takes too long. I put it in a different observe object
    ## and the only way to start the calculation will be using a
    ## button
    if ( is.null( input$buttonDrawMarkers ) ||
         input$buttonDrawMarkers < 1 ){
      return( NULL )
    }
    isolate( data.return.levels <- calculate.chosen.return.levels() )
    if ( !is.null( data.return.levels ) ){
      if ( any( is.na( c( data.return.levels$longitude,
                         data.return.levels$latitude ) ) ) ){
        ## I am dealing with either a placeholder or a compromised
        ## data.frame. Anyway, the leaflet map can not handle it
        return( NULL )
      }
      ## Same trick as in the animation tab: I use a plot of height
      ## 0 to obtain the current width of the element I want to
      ## place the legend next to. Unfortunately I do not know of
      ## any other trick right now to adjust an objects width
      ## according to the current screen width (CSS3 magic)
      isolate({
          map.width <-
            session$clientData[[ 'output_leaflet-placeholder_width' ]]
      })
      if ( is.null( map.width ) ){
        warning(
            "The placeholder magic in the leaflet tab went wrong!" )
      }
      ## range of the return levels
      color.max <- max( data.return.levels$return.level )
      color.min <- min( data.return.levels$return.level )
      ## create a palette for the return levels of the individual
      ## circles
      palette <- colorNumeric( c( "navy", "skyblue", "limegreen",
                                 "yellow", "darkorange",
                                 "firebrick4" ),
                              c( color.min, color.max ) )
      map.leaflet <- leafletProxy( session$ns( "map" ) )
      map.leaflet <- clearGroup( map.leaflet, "returns" )
      map.leaflet <- addCircleMarkers(
          map.leaflet, data = data.return.levels,
          group = "returns", lng = ~longitude,
          color = ~palette( return.level ), lat = ~latitude,
          options = popupOptions( closeButton = FALSE ),
          fillOpacity = .8 )
      ## layer control to turn the return level layer on and off
      map.leaflet <- addLayersControl(
          map.leaflet, position = "bottomright",
          baseGroups = c( "OpenTopoMaps", "OpenStreetMaps" ),
          overlayGroups = c( "stations", "returns" ),
          options = layersControlOptions( collapsed = FALSE ) )
      map.leaflet <- leaflet::addLegend(
          map.leaflet, pal = palette, layerId = "leafletLegend",
          values = c( color.min, color.max ),
          orientation = "horizontal", width = map.width )
      return( map.leaflet )
    } } )
  ## Placeholder to determine the window's width
  ## output$placeholder <- renderPlot( {
  ##    } )
    ## ttplot( climex.environment$stations.temp.max[[ 2 ]] ) } )
  ## This chunk both updates/renders the table containing the summary
  ## statistics of an individual station and adds a red icon for the
  ## selected station.
  output$table <- renderTable( {
    data.selected <- reactive.chosen()
    ## station.name is picked according to the click of the user on
    ## the leaflet map
    station.name <- selected.station()
    if ( is.null( data.selected ) ||
         is.null( station.name ) || (
           ## dirty flag on changing
           is.null( input$map_marker_click ) &&
           is.null( selectDataSource() ) ) )
      return( NULL )
    ## If the artificial data was chosen as source, do not display
    ## anything.
    if ( selectDataBase() == "Artificial data" ){
      return( NULL )
    }
    selected.station <- data.selected[[ 2 ]][
        which( data.selected[[ 2 ]]$name == station.name ), ]
    leafletProxy( session$ns( "map" ) ) %>%
    clearGroup( group = "selected" )
    leafletProxy( session$ns( "map" ) ) %>%
    addMarkers( data = selected.station, group = "selected",
               icon = red.icon, lng = ~longitude,
               lat = ~latitude )
    ## calculate the GEV/GP fit and various return levels
    x.fit.evd <- reactive.fitting()
    x.data <- reactive.extreme()
    if ( is.null( x.fit.evd ) )
      return( NULL )
    if ( radioEvdStatistics() == "GEV" ){
      model <- "gev"
    } else {
      model <- "gpd"
    }
    if ( is.null( buttonMinMax() ) || buttonMinMax() == "Max" ){
      x.return.level <- climex::return.level(
                                    x.fit.evd,
                                    return.period = c( 100, 50, 20 ),
                                    model = model,
                                    error.estimation = "none",
                                    threshold = x.fit.evd$threshold,
                                    total.length = x.data[[ 1 ]]
                                )$return.level
    } else {
      x.return.level <- ( -1 )*
        climex::return.level(
                    c( -1* x.fit.evd$par[ 1 ],
                      x.fit.evd$par[ 2 ],
                      x.fit.evd$par[ 3 ] ),
                    return.period = c( 100, 50, 20 ),
                    model = model,
                    error.estimation = "none" )$return.level
    }
    x.df <- data.frame( names = c( "100y return level",
                                  "50y return level",
                                  "20y return level" ),
                       x.return.level, row.names = NULL )
    colnames( x.df ) <- c( station.name, "" )
    x.df
  }, rownames = FALSE, digits = 3 )

  ## Uses the coordinates of the click event in the leaflet map to
  ## determine the name of the station the user choose.
  selected.station <- reactive({
    data.selected <- reactive.chosen()
    if ( is.null( data.selected ) )
      return( NULL )
    if ( !is.null( input$map_marker_click ) ){
      map.click <- input$map_marker_click
      station.name.click <- as.character(
          data.selected[[ 2 ]]$name[ which(
                                   data.selected[[ 2 ]]$latitude %in%
                                   map.click$lat &
                                   data.selected[[ 2 ]]$longitude %in%
                                   map.click$lng ) ] )
    } else {
      station.name.click <- NULL
    }
    station.name.sidebar <- selectDataSource()
    if ( is.null( station.name.click ) ){
      station.name <- station.name.sidebar
      station.name.previous <<- station.name.sidebar
    } else {
      ## Now there is both a station name provided via click and the
      ## sidebar. Using station.name.previous to decide which was
      ## chosen more recently.
      if ( station.name.sidebar == station.name.previous &&
           station.name.click == station.name.previous ){
        ## This one will be visited on every click, since the sidebar
        ## will be updated according to the clicked marker
        station.name <- station.name.sidebar
      } else if ( station.name.sidebar == station.name.previous ){
        station.name <- station.name.click
        station.name.previous <<- station.name.click
      } else if ( station.name.click == station.name.previous ){
        station.name <- station.name.sidebar
        station.name.previous <<- station.name.sidebar
      } else {
        ## If none of the station names are matching, the user most
        ## probably switched the station more than one time using the
        ## sidebar.
        station.name <- station.name.sidebar
        station.name.previous <<- station.name.sidebar
      }
    }
    return( station.name )
  })
  return( selected.station )
}

##' @title Choosing a data set in the \code{climex} app
##' @description This functions extracts all stations containing more
##'   than a specified number of years of data.
##' @details It uses the current database and returns all stations,
##'   which are at least as long as \code{sliderYears}.
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
##' @param sliderYears Numerical (slider) input to determine the
##'   minimal length (in years) of the time series to be
##'   displayed. Minimal value is 0 and maximal is 155, the default
##'   value is 65 and the step width is 1.
##' @param climex.environment Environment containing the global
##'   variables used within the \code{climex} app. Namely the last
##'   values displayed in the table and the lists containing the
##'   station data. 
##'
##' @family leaflet
##'
##' @import shiny
##' @import climex
##' 
##' @return  Reactive \code{list}  containing  a  \code{list} of  all
##'   selected stations and their positions.
##' @author Philipp Mueller
data.chosen <- function( selectDataBase, sliderYears,
                        climex.environment ){
  data <- reactive( {
    if ( is.null( selectDataBase() ) ||
         is.null( sliderYears() ) ){
      return( NULL )
    }
    ## The generation of the artificial data is handled in the
    ## `data.selection` reactive function.
    if ( selectDataBase() == "Artificial data" ){
      return( NULL )
    } else {
      selection.list <-
        climex.environment$list.data.sources[[
          which( names( climex.environment$list.data.sources )
                %in% selectDataBase() ) ]]
      ## to also cope the possibility of importing such position data
      positions.all <- climex.environment$data.frame.positions
 
      if ( sliderYears() < 20 ){
        ## Display a warning and return for a slider value lesser than
        ## 20.
        shinytoastr::toastr_info( "We are done extreme value analysis. Please select longer time series!",
                                 preventDuplicates = TRUE )
        return( NULL )
      }
      ## Select time series with sufficient length.
      selection <- Reduce( c, lapply( selection.list, function( x )
        length( unique( lubridate::year( x ) ) ) ) ) >= sliderYears()
      stations.selected <- selection.list[ selection ]
      stations.selected.names <- names( selection.list )[ selection ]
      positions.selected <- positions.all[
        which( positions.all$name %in% stations.selected.names ),  ]
      ## The first element contains a list of all selected stations.
      ## The second contains a data.frame with at least the longitude,
      ## latitude, and name of each selected station.
      return( list( data = stations.selected,
                   positions = positions.selected ) )
    }
  } )
  return( data )
}
  
