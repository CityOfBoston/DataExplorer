library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(htmltools)
library(jsonlite)
library(geojsonio)
library(leaflet.extras)
library(shinyjs)
library(V8)
library(RCurl)
library(rgdal)


#schools Declarations START
blankicon <- makeIcon(iconUrl = ("https://raw.githubusercontent.com/agnev1021/GEHC-/master/Icons/blank.png?token=AcE3av8vBtM0JDio5ziah-yetTkn3yrWks5ZfSpmwA%3D%3D"), iconWidth = 1, iconHeight = 1)
collegeicon <- makeIcon(iconUrl = ("https://cdn2.iconfinder.com/data/icons/location-map-simplicity/512/university_school-512.png"), iconWidth = 50, iconHeight = 55)
publicschools <- geojsonio::geojson_read("http://bostonopendata-boston.opendata.arcgis.com/datasets/1d9509a8b2fd485d9ad471ba2fdb1f90_0.geojson", what = "sp")
nonpublicschools <- geojsonio::geojson_read("http://bostonopendata-boston.opendata.arcgis.com/datasets/0046426a3e4340a6b025ad52b41be70a_1.geojson", what="sp")
colleges <- geojsonio::geojson_read("http://bostonopendata-boston.opendata.arcgis.com/datasets/cbf14bb032ef4bd38e20429f71acb61a_2.geojson",what="sp")
neighborhoodLink <- "http://bostonopendata-boston.opendata.arcgis.com/datasets/3525b0ee6e6b427f9aab5d0a1d0a1a28_0.geojson"
neighborhoodJson <- geojsonio::geojson_read(neighborhoodLink,what = "sp")
#schools Declarations START
ui = fluidPage(
  leafletOutput("SCHOOLmap", height = "522px"),
  fluidRow( fluidRow(box(width = 1),box(width = 10,h5("This map helps people visualize education across Boston to show where different public and private schools, and colleges/universities are located.Learn about the City of Boston's initiative to improve public education and expand early education below.")), box(width = 1)),
    box(width = 4,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed.jpg", width = "100%"),href="https://imagine.boston.gov/wp-content/uploads/2017/07/Ib2030%20BOOK_Spreads--Energy%20and%20Environment.pdf",target="_blank"))),
    box(width = 4,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://pbs.twimg.com/media/CroEOLKWAAAnhhf.jpg:large", width = "100%"),href="https://www.bostonpublicschools.org/buildbps",target="_blank"))),
    box(width = 4,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://www.barrfoundation.org/assets/logo-821db212e468b22f9c4e0c3774d6214de98e41745c32d26bc3075a504f43f531.svg", width = "100%"),href="https://www.barrfoundation.org/partners/boston-k1ds",target="_blank")))
   )
)


server = function(input, output){
  #BERDO SERVER START
  output$SCHOOLmap <- renderLeaflet({
    leaflet()%>%
      addProviderTiles(providers$CartoDB.Positron) %>%
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
  
  
}
shinyApp(ui= ui, server = server)  