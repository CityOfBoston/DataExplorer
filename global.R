library(leaflet)
library(rjson)
library(geojsonio)
library(shinyBS)
library(DT)

# the geospatial datasets from analyze boston (maxes at 100 data sets for now, can remove if desired)
datasets <- fromJSON(readLines("https://data.boston.gov/api/3/action/package_search?q=geojson&rows=100"))$result$results
# boston boundary
bostonLink<-"http://bostonopendata-boston.opendata.arcgis.com/datasets/142500a77e2a4dbeb94a86f7e0b568bc_0.geojson"

titles <- lapply(datasets, function(x){
  return (x$title)
})
urls <- lapply(datasets, function(x){
  return (x$resources[[1]]$url)
})
data <- setNames(urls,titles)
data <- data[order(names(data))]
boundJson <- geojson_read(bostonLink, what = "sp")

# reactive values list of cached data
downloadedData <- reactiveValues()

# smartly add the correct kind of data to the map
addData <- function(leaflet, data, color="blue", cluster=FALSE){
  slots <- slotNames(data)
  if("polygons" %in% slots){
    leaflet %>% addPolygons(data=data, color=color, weight=2, label=getLabel(data),group="polygons")
  }else if("lines" %in% slots){
    addPolylines(leaflet, data=data, color=color, weight=2, group="lines")
  }else if(cluster){
    addCircleMarkers(leaflet, data=data, fillOpacity=0.6, color=color,
                     label=getLabel(data), weight=2, clusterOptions=markerClusterOptions(),
                     group="markers")
  }else{
    addCircles(leaflet, data=data, fillOpacity=0.6, color=color,
                     label = getLabel(data), weight=2, radius=80, group="markers")
  }
}

# adds the boston city bound to the map
addCityBound <-function(leaflet, weight=2, color="black"){
  addPolygons(leaflet, data = boundJson, weight = weight, color = color,
              fill = FALSE)
}

# smartly get labels for data by looking for data columns that contain a matching pattern
getLabel <- function(data){
  for(name in names(data)){
    if(grepl("name", name, ignore.case=TRUE)){
      return(as.character(data[[name]]))
    }
  }
  return(NULL)
}

# data select module
dataSelectPanelUI <- function(id, features, rownum){
  ns <- NS(id)
  bsCollapsePanel(value = ns("collapse"),
                  selectizeInput(ns("data"), "Data Set", names(data),
                                 selected = as.character(features$df$names[rownum]),
                                 options=list(placeholder='Search for a Data Set')),
                  fluidRow(
                    column(10, colourInput(ns("color"), "Color", showColour="background",
                                                 value=features$df$color[rownum],
                                                 palette="limited")),
                    column(2, actionButton(ns("openModal"), "", icon=icon("cog")), style="margin-top: 25px;
                           margin-left:-20px;")
                  ),
                  title = tags$div(HTML(titleWithColor(features$df$names[rownum], features$df$color[rownum])),
                                   conditionalPanel('output.moreThanOnePanel',
                                     actionButton(ns("remove"), "", icon=icon('times'),
                                            style = "float: right; height: 20px; padding-top:0px;"),
                                     style = "float:right; height: 20px;"))
  )
}

# server function for data selects
dataSelectPanel <- function(input, output, session, features, panelId, realSession){
  # logic for the "remove" panel button
  observeEvent(input$remove, {
    if(length(features$df$id) < 2){
      return()
    }
    selectionVector = features$df$id != panelId
    features$df <- features$df[selectionVector,]
    print(features$df)
  })
  
  # live updating the data set name
  observe({
    if(!is.null(input$data) && input$data != ''){
      features$df[features$df$id==panelId,'names'] <- input$data
    }
  })
  
  # live updating the data color
  observe({
    if(!is.null(input$color) && input$color != ''){
      features$df[features$df$id==panelId, 'color'] <- input$color
    }
  })
  
  # open advanced options and populate modal
  observeEvent(input$openModal, {
    toggleModal(realSession, "optionsModal")
    features$modalId <- panelId
  })
}

# helper function to create titles with a colored label
titleWithColor <- function(title, color){
  return(paste(paste0("<div class='colorbox' style='background: ", color, ";'/>"), title))
}