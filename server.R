library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(rjson)
library(geojsonio)
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
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      # centering the view on a specific location (Boston)
      setView(lng = -71.0589, lat = 42.31, zoom = 12)
  })
  
  # stored values of front end values
  features <- reactiveValues(rendered=c(1),names=c("Charging Stations"),
                             colors=c("blue"))
  
  # Increment reactive values used to store how may rows we have rendered
  observeEvent(input$add,{
    if (max(features$rendered) > 2) return(NULL)
    features$names <- lapply(features$rendered, function(i){
      input[[paste0('data',i)]]
    })
    features$colors <- lapply(features$rendered, function(i){
      input[[paste0('color',i)]]
    })
    features$names <- c(features$names, "Charging Stations")
    features$colors <- c(features$colors, "blue")
    features$rendered <- c(features$rendered, max(features$rendered)+1)
  })
  
  # input data for choices about datasets
  df <- eventReactive(input$update, {
    out <- lapply(features$rendered,function(i){
      dataName <- paste0('data',i)
      dataColor <- paste0('color',i)
      data.frame(name=input[[dataName]], color=input[[dataColor]] )
    })
    do.call(rbind,out)
  })
  
  # add data whenever the df is updated
  observe({
    df <- df()
    proxy <- leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes()
    by(df, 1:nrow(df), function(row){
      link <- data[as.character(row$name)]
      spData <- geojson_read(as.character(link), what="sp")
      addData(proxy, data=spData, color=as.character(row$color))
    })
  })
  
  # render the UI, adding the correct number of dataDropdowns
  observe({
    output$dataDropdowns <- renderUI({
      rows <- lapply(features$rendered,function(i){
        tags$div(id = paste0("div",i),
          selectInput(paste0("data",i), "Data Set", titles,
                      selected = as.character(features$names[i])),
          colourInput(paste0("color",i), "Color", value=features$colors[i],
                      palette="limited")
        )
      })
      do.call(shiny::tagList,rows)
    })
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
# 
#   output$ziptable <- DT::renderDataTable({
#     df <- cleantable %>%
#       filter(
#         Score >= input$minScore,
#         Score <= input$maxScore,
#         is.null(input$states) | State %in% input$states,
#         is.null(input$cities) | City %in% input$cities,
#         is.null(input$zipcodes) | Zipcode %in% input$zipcodes
#       ) %>%
#       mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
#     action <- DT::dataTableAjax(session, df)
# 
#     DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
#   })
}
