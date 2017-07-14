library(leaflet)




datasets <- fromJSON(readLines("https://data.boston.gov/api/3/action/package_search?q=geojson&rows=60"))$result$results
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


downloadedData <- list()

addData <- function(leaflet, data, color="blue"){
  slots <- slotNames(data)
  if("polygons" %in% slots){
    addPolygons(leaflet, data=data, color=color, group="polygons")
  }else if("lines" %in% slots){
    addPolylines(leaflet, data=data, color=color, group="lines")
  }else{
    addCircleMarkers(leaflet, data=data, fillOpacity=0.6, color=color,
                     label = getLabel(data), group="markers")
  }
}

addCityBound <-function(leaflet, weight = 2, color = "black"){
  addPolygons(leaflet, data = boundJson, weight = weight, color = color,
              fill = FALSE)
}

# smartly get labels for data by looking for data columns that contain a matching pattern
getLabel <- function(data){
  for(name in names(data)){
    if(grepl("name", name, ignore.case=TRUE)){
      # print(name)
      return(as.character(data[[name]]))
    }
  }
  return(NULL)
}