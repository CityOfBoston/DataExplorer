library(RColorBrewer)
library(colourpicker)
library(spatialEco)

# # Leaflet bindings are a bit slow; for now we'll just sample to compensate
# set.seed(100)
# zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# # By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# # will be drawn last and thus be easier to see
# zipdata <- zipdata[order(zipdata$centile),]
port = 2590
shinyServer(function(input, output, session) { 

  ## Interactive Map ###########################################
  m <- leaflet()
  
  # the id of the panels
  nextId <- 2
  # counter for tracking which module has had its server function called
  minModuleCalled <- 0
  # stored values of front end values
  # df stores the ids, names colors, and other values related to each dataset
  # modalId is the id of the dataset that we want to display on the advanced features modal
  
  features <- reactiveValues(df=data.frame(id=c(1),name=c(""),
                             color=c("blue"), cluster=c(FALSE), parameter=c(""), stringsAsFactors=FALSE),
                             modalId=0, urltext= "" )
  
  # Create the map
  output$map <- renderLeaflet({
    features$urltext <- paste0('http://', session$clientData$url_hostname,':', session$clientData$url_port)
    m <<- leaflet() %>%
      # the styling of the map itself
      addProviderTiles(
        providers$Esri.WorldGrayCanvas,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      # centering the view on a specific location (Boston)
      setView(lng = -71.0589, lat = 42.31, zoom = 12) %>%
      addCityBound()
      
      
  })

  checkQuery <- function(){
    # check query before rendering dropdowns, add data
    query <- getQueryString()
    print(query)
    if(length(query) > 0){
      qdf <- data.frame(id=strsplit(query$id, ";")[[1]],
                        name=strsplit(query$name, ";")[[1]],
                        color=paste0('#',strsplit(query$color, ";")[[1]]),
                        cluster=(strsplit(query$cluster, ";")[[1]]=="TRUE"),
                        parameter=strsplit(query$parameter, ";")[[1]],
                        stringsAsFactors=FALSE)
      qdf$parameter[qdf$parameter=='none'] <- ''
      print(qdf)
      features$df <- qdf
      # adding data
      print(paste( 'pre update', features$urltext))
      proxy <- leafletProxy("map")
      by(features$df, 1:nrow(features$df), function(row){
        spData <- data.frame()
        name <- as.character(row$name)
        # if data is already downloaded, no need to download again
        if(name %in% names(downloadedData)){
          spData <- downloadedData[[name]]
        }else{
          link <- as.character(data[name])
          spData <- geojson_read(link, what="sp")
          downloadedData[[name]] <<- spData
        }
        # add/remove layers in a specific order to have markers/lines on top of polygons
        proxy %>%
          hideGroup("markers") %>%
          hideGroup("lines") %>%
          addData(data=spData, color=as.character(row$color), cluster=row$cluster, parameter=row$parameter) %>%
          showGroup("lines") %>%
          showGroup("markers")
      })
    }
  }
  
  
  
  # utility function to save values into the reactive features
  saveFeatures <- function(){
    features$df$name <- lapply(features$df$id, function(i){
      input[[NS(i)('data')]]
    })
    features$df$color <- lapply(features$df$id, function(i){
      input[[NS(i)('color')]]
    })
  }
  
  # Increment reactive values used to store how may rows we have rendered
  observeEvent(input$add,{
    if (length(features$df$id) > 8) return(NULL)
    saveFeatures()
    newRow <- c(id=nextId, name="", color="blue", cluster=FALSE, parameter="")
    features$df <- rbind(features$df, newRow)
    nextId <<- nextId + 1
  })
  
  # input data for choices about datasets
  df <- eventReactive(input$update, {
    saveFeatures()
    updateQueryString(createQueryString())
    features$urltext <- paste0('http://', session$clientData$url_hostname,':', session$clientData$url_port, '/', createQueryString())
    print(features$urltext)
    print(URLencode(features$urltext, reserved = TRUE, repeated = FALSE))
    features$df
  })
  observe({
    print(features$urltext)
    print(session$clientData$url_port)
    output$sharables <- renderUI({
      tags$div(
        tags$div(HTML(paste( '<div id="fb-root"></div><script>(function(d, s, id) {
                       var js, fjs = d.getElementsByTagName(s)[0];
                       if (d.getElementById(id)) return;
                       js = d.createElement(s); js.id = id;
                       js.src = "//connect.facebook.net/en_US/sdk.js#xfbml=1&version=v2.10";
                       fjs.parentNode.insertBefore(js, fjs);}(document, "script", "facebook-jssdk"));</script>
                       <div class="fb-share-button" data-href=',features$urltext,' 
                       data-layout="button" data-size="small" data-mobile-iframe="true"><a class="fb-xfbml-parse-ignore" target="_blank"
        
        href="https://www.facebook.com/sharer/sharer.php?u=',URLencode(features$urltext, reserved = TRUE, repeated = FALSE),'&amp;src=sdkpreparse">Share</a></div>'))),
        tags$div(HTML(paste0('<a href="https://twitter.com/share" data-text="Check out this map I made with Analyze Boston!" data-url=', gsub(" ","+",features$urltext),' class="twitter-share-button" data-show-count="false">
          Tweet</a><script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>'))))
        #tags$div(HTML('<a data-pin-do="buttonBookmark" data-pin-save="true" href="https://www.pinterest.com/pin/create/button/" data-pin-url="https://www.google.com/maps">
        #              </a> <script async defer src="//assets.pinterest.com/js/pinit.js"></script>')))
    }
    )
    runjs("twttr.widgets.load()")
  })
  
  
  createQueryString <- function(){
    formattedParams <- features$df$parameter
    formattedParams[formattedParams == ''] <- "none"
    paste0("?",
           paste(
             paste0("id=", paste(features$df$id, collapse=';')),
             paste0("name=", paste(features$df$name, collapse=';')),
             paste0("color=", paste(substring(features$df$color,2), collapse=';')),
             paste0("cluster=", paste(features$df$cluster, collapse=';')),
             paste0("parameter=", paste(formattedParams, collapse=';')),
          sep="&"))
  }
  
  # add data whenever the df is updated
  observe({
    df <- df()
    closeAlert(session, alertId="emptyDataSetError")
    if("" %in% df$name){
      createAlert(session, anchorId="errorAlert", alertId="emptyDataSetError",
                  style="warning", content="Please choose a data set!")
      return()
    }
    proxy <<- leafletProxy("map") %>% #####
    ###m %>% #####
      clearMarkers() %>%
      clearMarkerClusters() %>%
      clearShapes() %>%
      addCityBound()
    # look at datasets that user wants to visualize
    by(df, 1:nrow(df), function(row){
      spData <- data.frame()
      name <- as.character(row$name)
      # if data is already downloaded, no need to download again
      if(name %in% names(downloadedData)){
        spData <- downloadedData[[name]]
      }else{
        link <- as.character(data[name])
        spData <- geojson_read(link, what="sp")
        downloadedData[[name]] <<- spData
      }
      # add/remove layers in a specific order to have markers/lines on top of polygons
      proxy %>%
        hideGroup("markers") %>%
        hideGroup("lines") %>%
        addData(data=spData, color=as.character(row$color), cluster=row$cluster, parameter=row$parameter) %>%
        showGroup("lines") %>%
        showGroup("markers") 
    })
    
  })
  
  # variable used for on load one time actions
  onLoad <- TRUE

  # render the UI, adding the correct number of dataDropdowns
  observe({
    if(onLoad){
      checkQuery()
      onLoad <<- FALSE
    }
    output$dataDropdowns <- renderUI({
      rownum <- 0
      lastId <- 0
      # creates a list of collapsible panels, one per dataset
      panels <- lapply(features$df$id,function(i){
        rownum <<- rownum + 1
        lastId <<- i
        dataSelectPanelUI(id=i, features, rownum)
      })
      lapply(features$df$id, function(i){
        if(i > minModuleCalled){
          print(paste("new module",i))
          callModule(dataSelectPanel, i, features, i, session)
          minModuleCalled <<- i
        }
      })
      # returns the actual rows of collapse panels in the bsCollapse ui object, the last panel is marked as open
      do.call(bsCollapse, c(panels, open=NS(lastId)("collapse"), id="collapseGroup"))
    })

  })
  
  # boolean value representing whether a there are more than one panel or not
  # used to show or hide the "remove" button on each panel
  output$moreThanOnePanel <- reactive({
    length(features$df$id) > 1
  })
  
  # renders the modal based on which data set is selected
  observe({
    if(features$modalId > 0){
      output$optionsModalContent <- renderUI({
        advancedOptionsContentInput(features$modalId)
      })
      callModule(advancedOptionsContent, features$modalId, features$modalId)
    }
  })
  
  # Shiny module for the advanced options modal
  advancedOptionsContentInput <- function(id){
    ns <- NS(id)
    dataName <- as.character(features$df[features$df$id==id,'name'])
    cluster <- as.logical(features$df[features$df$id==id,'cluster'])
    parameter <- as.character(features$df[features$df$id==id,'parameter'])
    description <- as.character(allData[allData$name==dataName,'description'])
    tags$div(
      h3(dataName),
      p(description),
      hr(),
      h4("Advanced Features*"),
      checkboxInput(ns("cluster"), "Cluster Data", value=cluster),
      p("For data sets with a large number of point data, you can cluster the data so that a group of points is represented as one.", em("Note that this only functions on point data and will not affect line or polygon data at all.")),
      hr(),
      selectizeInput(ns("dataParameter"), "Data Parameter",
                  c("",as.character(names(downloadedData[[dataName]]))),
                  selected=parameter,
                  options = list(placeholder = 'Please select an option below')
      ),
      p("Public data sets often have lots of interesting parameters that aren't directly related to geography or location. With this feature, you can visualize data based on one of these parameters. Choose from one of the parameters above to visualize that parameter across the geographical data already presented. For point data, the radius of each point will reflect the parameter chosen. For polygon data, the color of each section will reflect the parameter chosen. ", em("Please only select numerical parameters!"), " Choosing non-numerical parameters may present you with useless data or may even cause your website to crash. As always, just refresh if anything goes wrong. If you are curious what kinds of parameters the data set has, check out the data viewer tab at the top of the main website!"),
      hr(),
      p("*Note that some of these functions are experimental, and may not work as expected unless utilized correctly. Feel free to try these tools out, and ", strong("if anything breaks, just refresh."), " Enjoy!")
    )
  }
  
  # server functions for the above Shiny module
  advancedOptionsContent <- function(input, output, session, modalId){
    observeEvent(input$cluster, {
      if(!is.null(input$cluster)){
        features$df[features$df$id==modalId,'cluster'] <- input$cluster
      }
    })
    
    observeEvent(input$dataParameter, {
      if(!is.null(input$dataParameter)){
        features$df[features$df$id==modalId,'parameter'] <- input$dataParameter
      }
    })
  }
  
  ## DATA TAB FUNCTIONS
  
  # renders the DT data table
  output$datatable <- DT::renderDataTable({
    if(length(df()) > 0){
      DT::datatable(downloadedData[[input$dataset]]@data)
    }
  })
  
  # updates the choices for the data sets based on what has been chosen
  observe({
    updateSelectInput(session, "dataset",
                      choices = df()$name
    )
  })
  
  ## SHOWCASE FUNCTIONS
  
  output$lightMap <- renderLeaflet({
    lightLink <- "https://data.boston.gov/dataset/52b0fdad-4037-460c-9c92-290f5774ab2b/resource/c2fcc1e3-c38f-44ad-a0cf-e5ea2a6585b5/download/streetlight-locations.csv"
    neighborhoodLink <- "http://bostonopendata-boston.opendata.arcgis.com/datasets/3525b0ee6e6b427f9aab5d0a1d0a1a28_0.geojson"
    
    lightFile <- "./data/light.rds"
    lightData <- NULL
    
    
    if(file.exists(lightFile)){
      lightData <- readRDS(lightFile)
    }else{
      lightData <- read.csv(lightLink)
      saveRDS(lightData, lightFile)
    }
    
    neighborhoodJson <- geojson_read(neighborhoodLink, what="sp")
    
    # make sure there are no N/A entries in data
    complete <- lightData[complete.cases(lightData),]
    
    sp::coordinates(complete) <- ~Long+Lat
    sp::proj4string(complete) <- sp::proj4string(neighborhoodJson)
    pts.poly <- point.in.poly(complete, neighborhoodJson)
    numLightsInNeighborhood <- tapply(pts.poly@data$OBJECTID, pts.poly@data$Name, FUN=length)
    
    neighborhoodJson@data$totalLights <- unname(numLightsInNeighborhood[neighborhoodJson@data$Name])
    
    neighborhoodJson@data$lightDensity <- neighborhoodJson@data$totalLights/neighborhoodJson@data$SqMiles
    
    pal <- colorNumeric(c("#0c0b2d", "white"), domain = neighborhoodJson@data$lightDensity)
    shinyjs::hide(id="loading4")
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      # centering the view on a specific location (Boston)
      setView(lng = -71.0589, lat = 42.3, zoom = 11) %>%
      # the legend for the shading of the zones
      addLegend("bottomright", pal = pal, values = neighborhoodJson@data$lightDensity,
                title = "Light Density In Neighborhoods",
                opacity = 1) %>%
      # adding the zones
      addPolygons(data = neighborhoodJson, weight = 0,
                  color = "#ffd6d6", popup = ~paste("<b>", Name, "</b><br/>Street Lights: ",
                                                          totalLights, "<br/>Area: ",
                                                          SqMiles, " square miles<br/>Light Density: ",
                                                          lightDensity), group = "zones",
                  fillColor = ~pal(lightDensity),
                  fillOpacity = 0.95,
                  label = ~Name
      )
  })
  
  #BERDO SERVER START
  output$SCHOOLmap <- renderLeaflet({
    blankicon <- makeIcon(iconUrl = ("https://raw.githubusercontent.com/agnev1021/GEHC-/master/Icons/blank.png?token=AcE3av8vBtM0JDio5ziah-yetTkn3yrWks5ZfSpmwA%3D%3D"), iconWidth = 1, iconHeight = 1)
    collegeicon <- makeIcon(iconUrl = ("https://cdn2.iconfinder.com/data/icons/location-map-simplicity/512/university_school-512.png"), iconWidth = 50, iconHeight = 55)
    publicschools <- geojsonio::geojson_read("http://bostonopendata-boston.opendata.arcgis.com/datasets/1d9509a8b2fd485d9ad471ba2fdb1f90_0.geojson", what = "sp")
    nonpublicschools <- geojsonio::geojson_read("http://bostonopendata-boston.opendata.arcgis.com/datasets/0046426a3e4340a6b025ad52b41be70a_1.geojson", what="sp")
    colleges <- geojsonio::geojson_read("http://bostonopendata-boston.opendata.arcgis.com/datasets/cbf14bb032ef4bd38e20429f71acb61a_2.geojson",what="sp")
    neighborhoodLink <- "http://bostonopendata-boston.opendata.arcgis.com/datasets/3525b0ee6e6b427f9aab5d0a1d0a1a28_0.geojson"
    neighborhoodJson <- geojsonio::geojson_read(neighborhoodLink,what = "sp")
    
    leaflet()%>%
      addProviderTiles(providers$Esri.WorldGrayCanvas
                       ) %>%
      setView(lng =-71.057083, lat = 42.3601, zoom = 11) %>%
      addPolygons(data = neighborhoodJson, weight = 2, color = "blue",
                  fill = TRUE, popup=~paste("<b>", Name),group = 'Neighborhoods')%>%
      addMarkers(data=publicschools,
                 clusterOptions=markerClusterOptions(),
                 icon=collegeicon,
                 layerId = "Names",
                 label=publicschools$SCH_NAME,
                 popup=paste(format(tags$b("Name:")), publicschools$SCH_NAME, "<br/>",format(tags$b("Grades:")), publicschools$SCH_TYPE), 
                 group="Public Schools")%>%
      addMarkers(data=nonpublicschools,
                 clusterOptions=markerClusterOptions(),
                 icon=collegeicon,
                 layerId = "Names",
                 label=nonpublicschools$NAME,
                 popup=paste(format(tags$b("Name:")), nonpublicschools$NAME, "<br/>",format(tags$b("Grades:")), nonpublicschools$GRADES,"<br/>",format(tags$b("Type:")), nonpublicschools$TYPE), 
                 group="Non-Public Schools")%>%
      addMarkers(data=colleges,
                 clusterOptions=markerClusterOptions(),
                 icon=collegeicon,
                 layerId = "Names",
                 label=colleges$Name,
                 popup=paste(format(tags$b("Name:")), colleges$Name, "<br/>",format(tags$b("Neighborhood:")), colleges$City,"<br/>","<a href='",colleges$URL,"' target='_blank'>",colleges$URL,"</a>"), 
                 group="Colleges/Universities")%>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Boston",
        onClick=JS("function(btn, map){ map.setZoom(11); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
      addMiniMap()%>%
      addLayersControl(
        baseGroups = c("Public Schools","Non-Public Schools","Colleges/Universities"),
        overlayGroups = c("Neighborhoods"),
        options=layersControlOptions(collapsed=FALSE)
      )
  })
  #BERDO SERVER END
  
  #Bike Map SERVER START
  output$bikemap <- renderLeaflet({
    bikelanes <- geojsonio::geojson_read("http://bostonopendata-boston.opendata.arcgis.com/datasets/d02c9d2003af455fbc37f550cc53d3a4_0.geojson", what = "sp")
    
    hubwaystations <- read.csv("https://s3.amazonaws.com/hubway-data/Hubway_Stations_2011_2016.csv")
    
    hubwaypoints <-cbind(as.numeric(hubwaystations$Longitude),as.numeric(hubwaystations$Latitude))
    
    hubway_popuptext <- paste(sep="<br/>",
                              hubwaystations$Station)
    
    leaflet(height = 100)%>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lng =-71.057083, lat = 42.3601, zoom = 11) %>%
      addPolylines(data=bikelanes,group="Bike Network",weight=4)%>%
      addMarkers(data=hubwaypoints,popup=hubway_popuptext,group="Hubway Stations",clusterId = 'bikes',clusterOptions = markerClusterOptions())%>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Boston",
        onClick=JS("function(btn, map){ map.setZoom(8); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
      addLayersControl(
        overlayGroups = c("Bike Network","Hubway Stations"),
        options=layersControlOptions(collapsed=FALSE)
      ) %>%
      addCityBound()
  })
  #Bike Map SERVER END
  
}
)