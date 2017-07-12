library(dplyr)
library(rjson)

datasets <- fromJSON(readLines("https://data.boston.gov/api/3/action/package_search?q=geojson&rows=60"))$result$results
titles <- lapply(datasets, function(x){
  return (x$title)
})
urls <- lapply(datasets, function(x){
  return (x$resources[[1]]$url)
})
data <- setNames(urls,titles)
data <- data[order(names(data))]

addData <- function(leaflet, data, color="blue"){
  slots <- slotNames(data)
  if("polygons" %in% slots){
    addPolygons(leaflet, data=data)
  }else if("lines" %in% slots){
    addPolylines(leaflet, data=data)
  }else{
    addMarkers(leaflet, data=data)
  }
}

# allzips <- readRDS("data/superzip.rds")
# allzips$latitude <- jitter(allzips$latitude)
# allzips$longitude <- jitter(allzips$longitude)
# allzips$college <- allzips$college * 100
# allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
# row.names(allzips) <- allzips$zipcode
# 
# cleantable <- allzips %>%
#   select(
#     City = city.x,
#     State = state.x,
#     Zipcode = zipcode,
#     Rank = rank,
#     Score = centile,
#     Superzip = superzip,
#     Population = adultpop,
#     College = college,
#     Income = income,
#     Lat = latitude,
#     Long = longitude
#   )
