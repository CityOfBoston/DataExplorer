library(shiny)
library(leaflet)
library(geojsonio)

ui <- fluidPage(
                leafletOutput("drivemap")
      )

server <- function(input, output) {
   output$drivemap <- renderLeaflet({
    leaflet()%>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng =-71.057083, lat = 42.3601, zoom = 11) %>%
      addMarkers(data=emergencyparking,popup=parking_popuptext,group="Emergency Parking",clusterId = 'parking',clusterOptions = markerClusterOptions())%>%
      addMarkers(data=chargingstations,popup=parking_popuptext,group="Charging Stations",clusterId = 'charging stations',clusterOptions = markerClusterOptions())%>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Boston",
        onClick=JS("function(btn, map){ map.setZoom(8); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
      addMiniMap(width = 75, height = 75)%>%
      addLayersControl(
        baseGroups=c("Emergency Parking","Charging Stations"),
        options=layersControlOptions(collapsed=FALSE)
      )
  })
}

shinyApp(ui, server)