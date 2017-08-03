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
                h5("Utilizing the Emergency Parking and Charging Stations datasets from Boston we have produced an interactive visualization mapping out suitable parking tailored to an individuals needs."),
                fluidRow(
                  box(width = 4,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/rneogy/DataExplorer/master/Resources/IB2030Logo.jpg", width = "100%"),href="https://imagine.boston.gov/wp-content/uploads/2017/07/Ib2030%20BOOK_Spreads--Transportation.pdf",target="_blank"))),
                  box(width = 4,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/rneogy/DataExplorer/master/Resources/VisionZeroLogo.PNG", width = "100%"),href="http://www.visionzeroboston.org/",target="_blank"))),
                  box(width = 4,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/rneogy/DataExplorer/master/Resources/ZipCarLogo.png", width = "100%"),href="https://www.boston.gov/transportation/drive-boston",target="_blank")))
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
      addLayersControl(
        baseGroups=c("Emergency Parking","Charging Stations"),
        options=layersControlOptions(collapsed=FALSE)
      )
  })
  #Drive Map SERVER END
  
}

shinyApp(ui, server)