library(shiny)
library(leaflet)
library(geojsonio)

#Bike Declarations START

bikelanes <- geojsonio::geojson_read("http://bostonopendata-boston.opendata.arcgis.com/datasets/d02c9d2003af455fbc37f550cc53d3a4_0.geojson", what = "sp")

hubwaystations <- read.csv("https://s3.amazonaws.com/hubway-data/Hubway_Stations_2011_2016.csv")

hubwaypoints <-cbind(as.numeric(hubwaystations$Longitude),as.numeric(hubwaystations$Latitude))

hubway_popuptext <- paste(sep="<br/>",
                          hubwaystations$Station)

#Bike Declarations END

ui <- fluidPage(
  
  leafletOutput("bikemap"),
  h5("This map helps people find routes to get around", 
     a("Boston by Bike.", href="https://www.boston.gov/departments/boston-bikes",target="_blank"), 
     "We used two datasets from", 
     a("Analyze Boston:", href="https://data.boston.gov/", target="_blank"), 
     "Existing Bike Network shows which streets have bike lanes that riders can saftely use, and Hubway Stations.", 
     a("Hubway", href="https://www.thehubway.com/",target="_blank"), 
     "is a bike share program that allows riders to pick up a bike at one location, ride to their destination and drop it off at a different station."),
  h5(a("Go Boston 2030", href="https://www.boston.gov/transportation/go-boston-2030", target="_blank"), 
     "is a City of Boston initiative that envisions safer, more accessible, and more sustainable transportation in Boston's future."
  ),
  fluidRow(
    box(width = 4,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed.jpg", width = "100%"),href="https://imagine.boston.gov/wp-content/uploads/2017/07/Ib2030%20BOOK_Spreads--Energy%20and%20Environment.pdf",target="_blank"))),
    box(width = 4,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed%20(2).jpg", width = "100%"),href="http://climatechangedata.boston.gov/",target="_blank"))),
    box(width = 4,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed%20(1).jpg", width = "100%"),href="http://www.greenribboncommission.org/",target="_blank")))
  )
)

server <- function(input, output) {
  
  #Bike Map SERVER START
  output$bikemap <- renderLeaflet({
    leaflet(height = 100)%>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng =-71.057083, lat = 42.3601, zoom = 11) %>%
      addPolylines(data=bikelanes,group="Bike Network",weight=4)%>%
      addMarkers(data=hubwaypoints,popup=hubway_popuptext,group="Hubway Stations",clusterId = 'bikes',clusterOptions = markerClusterOptions())%>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Boston",
        onClick=JS("function(btn, map){ map.setZoom(8); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
      addMiniMap(width = 75, height = 75)%>%
      addLayersControl(
        overlayGroups = c("Bike Network","Hubway Stations"),
        options=layersControlOptions(collapsed=FALSE)
      )
  })
  #Bike Map SERVER END
  
}

shinyApp(ui, server)