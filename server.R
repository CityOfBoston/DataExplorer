library(RColorBrewer)
library(colourpicker)
library(mapview)

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
      qdf <- data.frame(id=strsplit(query$id, ",")[[1]],
                        name=strsplit(query$name, ",")[[1]],
                        color=paste0('#',strsplit(query$color, ",")[[1]]),
                        cluster=(strsplit(query$cluster, ",")[[1]]=="TRUE"),
                        parameter=strsplit(query$parameter, ",")[[1]],
                        stringsAsFactors=FALSE)
      print(qdf)
      features$df <- qdf
      features$urltext <- paste0('http://', session$clientData$url_hostname,':', session$clientData$url_port)
      # adding data
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
    #features$urltext <- URLencode(features$urltext)
    #features$urltext<-gsub(" ","&",features$urltext)
    print(features$urltext)
    print(URLencode(features$urltext, reserved = TRUE, repeated = FALSE))
    features$df
  })
  #<script asyn src="//platform.twitter.com/widgets.js" charset="utf-8"></script>')),
  observe({
    print(features$urltext)
    print(session$clientData$url_port)
    #features$urltext<-"https://www.google.com/maps/"
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
        tags$div(HTML(paste0('<a href="https://twitter.com/share" data-text="Check out this map I made with Analyze Boston!" data-url=', gsub(" ","&",features$urltext),' class="twitter-share-button" data-show-count="false">
          Tweet</a><script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>'))))
        #tags$div(HTML('<a data-pin-do="buttonBookmark" data-pin-save="true" href="https://www.pinterest.com/pin/create/button/" data-pin-url="https://www.google.com/maps">
        #              </a> <script async defer src="//assets.pinterest.com/js/pinit.js"></script>')))
    }
    )
    runjs("twttr.widgets.load()")
    runjs("twttr.widgets.load()")
  })
  
  
  createQueryString <- function(){
    paste0("?",
           paste(
             paste0("id=", paste(features$df$id, collapse=',')),
             paste0("name=", paste(features$df$name, collapse=',')),
             paste0("color=", paste(substring(features$df$color,2), collapse=',')),
             paste0("cluster=", paste(features$df$cluster, collapse=',')),
             paste0("parameter=", paste(features$df$parameter, collapse=',')),
          sep="&"))
  }
  
  # add data whenever the df is updated
  observe({
    df <- df()
    print(df)
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
  
  ## take a snapshot
  observeEvent(input$snap,{
    ###proxy$mapId <- "proxMap"
    mapshot(m, file = paste0(getwd(), "/mapPics/prox.png"))
    print("hello")
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Snapshot saved!')
    }
  )


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
  
  observe({
    if(features$modalId > 0){
      output$optionsModalContent <- renderUI({
        advancedOptionsContentInput(features$modalId)
      })
      callModule(advancedOptionsContent, features$modalId, features$modalId)
    }
  })
  
  advancedOptionsContentInput <- function(id){
    ns <- NS(id)
    dataName <- as.character(features$df[features$df$id==id,'name'])
    cluster <- as.logical(features$df[features$df$id==id,'cluster'])
    parameter <- as.character(features$df[features$df$id==id,'parameter'])
    tags$div(
      h2(dataName),
      checkboxInput(ns("cluster"), "Cluster Graph (Only for Point Data)", value=cluster),
      selectizeInput(ns("dataParameter"), "Data Parameter",
                  c("",as.character(names(downloadedData[[dataName]]))),
                  selected=parameter,
                  options = list(placeholder = 'Please select an option below')
      )
    )
  }
  
  advancedOptionsContent <- function(input, output, session, modalId){
    observe({
      if(!is.null(input$cluster)){
        features$df[features$df$id==modalId,'cluster'] <- input$cluster
      }
    })
    
    observe({
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
}
)