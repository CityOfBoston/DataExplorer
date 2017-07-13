library(dplyr)
library(rjson)


#bostonLink<-"http://bostonopendata-boston.opendata.arcgis.com/datasets/142500a77e2a4dbeb94a86f7e0b568bc_0.geojson"


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


addData <- function(leaflet, data, color="blue"){
  slots <- slotNames(data)
  if("polygons" %in% slots){
    addPolygons(leaflet, data=data, color=color)
  }else if("lines" %in% slots){
    addPolylines(leaflet, data=data, color=color)
  }else{
    addCircleMarkers(leaflet, data=data, fillOpacity=0.6, color=color,
                     label = getLabel(data))
  }
}

addCitybound <-function(wei = 2){
  addPolygons(data = boundJson, weight = wei, color = "blue",
              fill = FALSE)
  addP
}

getLabel <- function(data){
  for(name in names(data)){
    if(grepl("name", name, ignore.case=TRUE)){
      print(name)
      return(as.character(data[[name]]))
    }
  }
  return(NULL)
  # if("Name" %in% names(data)){
  #   return(as.character(data$Name))
  # }else{
  #   return(NULL)
  # }
}