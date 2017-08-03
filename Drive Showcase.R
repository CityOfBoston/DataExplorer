library(shiny)
library(leaflet)
library(geojsonio)

#Drive Declarations START

emergencyparking <- geojsonio::geojson_read("http://bostonopendata-boston.opendata.arcgis.com/datasets/53ebc23fcc654111b642f70e61c63852_0.geojson",what = "sp")

parking_popuptext <- paste(sep="<br/>",
                           emergencyparking$Name,
                           emergencyparking$Address,
                           emergencyparking$Fee)

chargingstations <-geojsonio::geojson_read("http://bostonopendata-boston.opendata.arcgis.com/datasets/465e00f9632145a1ad645a27d27069b4_2.geojson",what="sp")

charging_popuptext <-paste(sep="<br/>",
                           chargingstations$Station_Name,
                           chargingstations$Street_Address)
#Drive Declarations END

ui <- fluidPage(
                
  leafletOutput("drivemap"),
                h5("Test", a("Boston Bike", href="https://www.boston.gov/departments/boston-bikes",target="_blank"), "Text"),
                fluidRow(
                  box(width = 4,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed.jpg", width = "100%"),href="https://imagine.boston.gov/wp-content/uploads/2017/07/Ib2030%20BOOK_Spreads--Energy%20and%20Environment.pdf",target="_blank"))),
                  box(width = 4,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed%20(2).jpg", width = "100%"),href="http://climatechangedata.boston.gov/",target="_blank"))),
                  box(width = 4,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed%20(1).jpg", width = "100%"),href="http://www.greenribboncommission.org/",target="_blank")))
                  )
  )

server <- function(input, output) {
  
  #Drive Map SERVER START
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
  #Drive Map SERVER END
  
}

shinyApp(ui, server)