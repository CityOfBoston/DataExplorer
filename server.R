library(RColorBrewer)
library(colourpicker)

# # Leaflet bindings are a bit slow; for now we'll just sample to compensate
# set.seed(100)
# zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# # By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# # will be drawn last and thus be easier to see
# zipdata <- zipdata[order(zipdata$centile),]

function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      # the styling of the map itself
      addProviderTiles( # providers$CartoDB.Positron,
        providers$Esri.WorldGrayCanvas,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      # centering the view on a specific location (Boston)
      setView(lng = -71.0589, lat = 42.31, zoom = 12) %>%
      addCityBound()
  })
  
  # stored values of front end values
  features <- reactiveValues(rendered=c(1),names=c("Public Schools"),
                             colors=c("blue"))
  
  # utility function to save values into the reactive features
  saveFeatures <- function(){
    features$names <- lapply(features$rendered, function(i){
      input[[paste0('data',i)]]
    })
    features$colors <- lapply(features$rendered, function(i){
      input[[paste0('color',i)]]
    })
  }
  
  # Increment reactive values used to store how may rows we have rendered
  observeEvent(input$add,{
    if (max(features$rendered) > 8) return(NULL)
    saveFeatures()
    features$names <- c(features$names, "Public Schools")
    features$colors <- c(features$colors, "blue")
    features$rendered <- c(features$rendered, max(features$rendered)+1)
  })
  
  # Remove reactive values
  observeEvent(input$remove,{
    if(max(features$rendered) < 2) return(NULL)
    features$names <- head(features$names, -1)
    features$colors <- head(features$colors, -1)
    features$rendered <- head(features$rendered, -1)
  })
  
  # input data for choices about datasets
  df <- eventReactive(input$update, {
    saveFeatures()
    out <- lapply(features$rendered,function(i){
      data.frame(name=as.character(features$names[i]), color=as.character(features$colors[i]))
    })
    do.call(rbind,out)
  })
  
  # add data whenever the df is updated
  observe({
    df <- df()
    proxy <- leafletProxy("map") %>%
      clearMarkers() %>%
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
      proxy %>%
        hideGroup("markers") %>%
        hideGroup("lines") %>%
        addData(data=spData, color=as.character(row$color)) %>%
        showGroup("lines") %>%
        showGroup("markers")
    })
  })
  
  # render the UI, adding the correct number of dataDropdowns
  observe({
    output$dataDropdowns <- renderUI({
      # create rows of collapsible panels, one per dataset
      panels <- lapply(features$rendered,function(i){
        bsCollapsePanel(value = paste0("collapse",i),
          selectizeInput(paste0("data",i), "Data Set", titles,
                         selected = as.character(features$names[i]),
                         options=list(placeholder='Search for a Data Set')),
          colourInput(paste0("color",i), "Color", showColour="background",
                      value=features$colors[i],
                      palette="limited"),
          # actionButton(paste0("openModal",i), "", icon=icon("cog")),
          # bsModal(id=paste0("optionsModal",i), title="More Options", trigger=paste0("openModal",i),
          #                      uiOutput("modalOutput")),
          title = HTML(titleWithColor(as.character(features$names[i]), features$colors[i]))
        )
      })
      # returns the actual rows of collapse panels in the bsCollapse ui object
      do.call(bsCollapse, c(panels, open=paste0("collapse",length(features$rendered)), id="collapseGroup"))
    })
    # output$modals <- renderUI({
    #   modals <- lapply(features$rendered,function(i){
    #     bsModal(id=paste0("optionsModal",i), title="More Options", trigger=paste0("openModal",i),
    #             uiOutput("modalOutput"))
    #   })
    #   do.call(shiny::tagList, modals)
    # })
  })
  
  titleWithColor <- function(title, color){
    return(paste(paste0("<div class='colorbox' style='background: ", color, ";'/>"), title))
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
  
  # zipsInBounds <- reactive({
  #   if (is.null(input$map_bounds))
  #     return(zipdata[FALSE,])
  #   bounds <- input$map_bounds
  #   latRng <- range(bounds$north, bounds$south)
  #   lngRng <- range(bounds$east, bounds$west)
  # 
  #   subset(zipdata,
  #     latitude >= latRng[1] & latitude <= latRng[2] &
  #       longitude >= lngRng[1] & longitude <= lngRng[2])
  # })

  # # Precalculate the breaks we'll need for the two histograms
  # centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks
  # 
  # output$histCentile <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  # 
  #   hist(zipsInBounds()$centile,
  #     breaks = centileBreaks,
  #     main = "SuperZIP score (visible zips)",
  #     xlab = "Percentile",
  #     xlim = range(allzips$centile),
  #     col = '#00DD00',
  #     border = 'white')
  # })
  # 
  # output$scatterCollegeIncome <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  # 
  #   print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
  # })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  # observe({
  #   colorBy <- input$color
  #   sizeBy <- input$size
  # 
  #   if (colorBy == "superzip") {
  #     # Color and palette are treated specially in the "superzip" case, because
  #     # the values are categorical instead of continuous.
  #     colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
  #     pal <- colorFactor("viridis", colorData)
  #   } else {
  #     colorData <- zipdata[[colorBy]]
  #     pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
  #   }
  # 
  #   if (sizeBy == "superzip") {
  #     # Radius is treated specially in the "superzip" case.
  #     radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
  #   } else {
  #     radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
  #   }
  # 
  #   leafletProxy("map", data = zipdata) %>%
  #     clearShapes() %>%
  #     addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
  #       stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
  #     addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
  #       layerId="colorLegend")
  # })
  # 
  # # Show a popup at the given location
  # showZipcodePopup <- function(zipcode, lat, lng) {
  #   selectedZip <- allzips[allzips$zipcode == zipcode,]
  #   content <- as.character(tagList(
  #     tags$h4("Score:", as.integer(selectedZip$centile)),
  #     tags$strong(HTML(sprintf("%s, %s %s",
  #       selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
  #     ))), tags$br(),
  #     sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
  #     sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
  #     sprintf("Adult population: %s", selectedZip$adultpop)
  #   ))
  #   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  # }
  # 
  # # When map is clicked, show a popup with city info
  # observe({
  #   leafletProxy("map") %>% clearPopups()
  #   event <- input$map_shape_click
  #   if (is.null(event))
  #     return()
  # 
  #   isolate({
  #     showZipcodePopup(event$id, event$lat, event$lng)
  #   })
  # })


  ## Data Explorer ###########################################
# 
#   observe({
#     cities <- if (is.null(input$states)) character(0) else {
#       filter(cleantable, State %in% input$states) %>%
#         `$`('City') %>%
#         unique() %>%
#         sort()
#     }
#     stillSelected <- isolate(input$cities[input$cities %in% cities])
#     updateSelectInput(session, "cities", choices = cities,
#       selected = stillSelected)
#   })
# 
#   observe({
#     zipcodes <- if (is.null(input$states)) character(0) else {
#       cleantable %>%
#         filter(State %in% input$states,
#           is.null(input$cities) | City %in% input$cities) %>%
#         `$`('Zipcode') %>%
#         unique() %>%
#         sort()
#     }
#     stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
#     updateSelectInput(session, "zipcodes", choices = zipcodes,
#       selected = stillSelected)
#   })
# 
#   observe({
#     if (is.null(input$goto))
#       return()
#     isolate({
#       map <- leafletProxy("map")
#       map %>% clearPopups()
#       dist <- 0.5
#       zip <- input$goto$zip
#       lat <- input$goto$lat
#       lng <- input$goto$lng
#       showZipcodePopup(zip, lat, lng)
#       map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
#     })
#   })
}
