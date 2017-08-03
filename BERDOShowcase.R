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


#BERDO Declarations START
BERDO <- geojsonio::geojson_read("http://bostonopendata-boston.opendata.arcgis.com/datasets/82595a1b793a49c2bce7d61b751bdca5_2.geojson", what = "sp")

BERDO$EnergyStar_Score <- ifelse(is.na(BERDO$EnergyStar_Score),0, BERDO$EnergyStar_Score)
binScore <- c(seq(0,100,20)) # bind bind it with INF
palScore <- colorBin("YlGn", domain = BERDO$EnergyStar_Score, bins = binScore)

BERDO$Site_Energy_Use <- ifelse(is.na(BERDO$Site_Energy_Use),0, BERDO$Site_Energy_Use)
Energyuse_IQR <- IQR(BERDO$Site_Energy_Use)
Energyuse_Q3 <- summary(BERDO$Site_Energy_Use)[5] #returns 3rd quartile of data set
Energyuse_UpperBound <- Energyuse_Q3 + Energyuse_IQR*1.5
binUsage <- c(seq(0,Energyuse_UpperBound,Energyuse_UpperBound/5), Inf)
palUsage <- colorBin("OrRd", domain = BERDO$Site_Energy_Use, bins = binUsage)

BERDO$GHG_Emissions <- ifelse(is.na(BERDO$GHG_Emissions),0, BERDO$GHG_Emissions)
GHG_Emissions_IQR <- IQR(BERDO$GHG_Emissions)
GHG_Emissions_Q3 <- summary(BERDO$GHG_Emissions)[5] #returns 3rd quartile of data set
GHG_Emissions_UpperBound <- GHG_Emissions_Q3 + GHG_Emissions_IQR*1.5
binEmissions <- c(seq(0,GHG_Emissions_UpperBound,GHG_Emissions_UpperBound/5), Inf)
palEmissions <- colorBin("OrRd", domain = BERDO$GHG_Emissions,bins = binEmissions)
#BERDO DECLARATIONS END

ui = fluidPage(
                        leafletOutput("BERDOmap", height = "522px"),
                        box(
                          width=12,solidheader=TRUE,collapse=FALSE,status='primary',
                            h5("The",
                               a("Building Energy Reporting and Disclosure Ordinance (BERDO)",href="https://www.boston.gov/environment-and-energy/building-energy-reporting-and-disclosure-ordinance"), "reports annual energy usage of buildings in Boston. We have displayed this data in a gradient                           ")
                            ),
                        fluidRow(
                                  box(width = 3,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed.jpg", width = "100%"),href="https://imagine.boston.gov/wp-content/uploads/2017/07/Ib2030%20BOOK_Spreads--Energy%20and%20Environment.pdf",target="_blank"))),
                                  box(width = 3,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed%20(2).jpg", width = "100%"),href="http://climatechangedata.boston.gov/",target="_blank"))),
                                  box(width = 3,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed%20(1).jpg", width = "100%"),href="http://www.greenribboncommission.org/",target="_blank"))),
                                  box(width = 3,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed.png", width = "100%"),href="http://www.greenovateboston.org/",target="_blank")))
                                 )
                       )
              

server = function(input, output){
  #BERDO SERVER START
  output$BERDOmap <- renderLeaflet({
    leaflet()%>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lng =-71.057083, lat = 42.3601, zoom = 15) %>%
      addPolygons(
        data=BERDO,
        group="Energy Rating",
        weight=1,
        opacity=10,
        color="black",
        fillColor=~palScore(EnergyStar_Score),
        fillOpacity=5,
        popup=paste(BERDO$Property_Name,"<br/>","Type:",BERDO$Property_Uses,"<br/>","Energy Score:",ifelse(BERDO$EnergyStar_Score==0,"Not Enough Info",BERDO$EnergyStar_Score))
      )%>%
      addPolygons(
        data=BERDO,
        group="Energy Usage",
        weight=1,
        opacity=10,
        color="black",
        fillColor=~palUsage(Site_Energy_Use),
        fillOpacity=5,
        popup=paste(BERDO$Property_Name,"<br/>","Type:",BERDO$Property_Uses,"<br/>","Energy Usage:",ifelse(BERDO$Site_Energy_Use==0,"Not Enough Info",BERDO$Site_Energy_Use))
      )%>%
      addPolygons(
        data=BERDO,
        group="GHG Emissions",
        weight=1,
        opacity=10,
        color="black",
        fillColor=~palEmissions(GHG_Emissions),
        fillOpacity=5,
        popup=paste(BERDO$Property_Name,"<br/>","Type:",BERDO$Property_Uses,"<br/>","GHG Emissions:", ifelse(BERDO$GHG_Emissions==0,"Not Enough Info",BERDO$GHG_Emissions))
      )%>%
      addLayersControl(
        baseGroups = c("Energy Rating","Energy Usage","GHG Emissions"),
        options=layersControlOptions(collapsed=FALSE)
      )%>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Boston",
        onClick=JS("function(btn, map){ map.setZoom(8); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
      #addMiniMap(width = 75, height = 75)%>%
      addLegend(title="Energy Star Score",position=c("topright"),values=BERDO$EnergyStar_Score,pal=palScore)%>%
      addLegend(title="Energy Usage",position=c("topright"),values=BERDO$Site_Energy_Use,pal=palUsage)%>%
      addLegend(position=c("topright"),values=BERDO$GHG_Emissions,pal=palEmissions)
  })
    #BERDO SERVER END
  

}
shinyApp(ui= ui, server = server)  
 