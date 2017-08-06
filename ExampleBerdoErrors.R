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
library(RColorBrewer)

#BERDO Declarations START


BERDO <- geojsonio::geojson_read("http://bostonopendata-boston.opendata.arcgis.com/datasets/82595a1b793a49c2bce7d61b751bdca5_2.geojson", what = "sp")

#BERDO$EnergyStar_Score <- ifelse(is.na(BERDO$EnergyStar_Score),-14354, BERDO$EnergyStar_Score)  #-14354 i just a place holder. Map will not plot NA values
binScore <- c(seq(0,100,20)) # bind bind it with INF
palScore <- colorBin("YlGn", domain = BERDO$EnergyStar_Score, bins = binScore)
colorScore <- c("gray",brewer.pal(6,"YlGn"))
textScore <- c("NA","0 - 20","20 - 40","40 - 60", "60 - 80", "80 - 100",">100")

#BERDO$Site_Energy_Use <- ifelse(is.na(BERDO$Site_Energy_Use),-14354, BERDO$Site_Energy_Use)
Energyuse_UpperBound<-quantile(BERDO$Site_Energy_Use,probs=.97,na.rm=TRUE)
binUsage <- c(seq(0,signif(Energyuse_UpperBound,1),signif(Energyuse_UpperBound,1)/5), max(BERDO$Site_Energy_Use,na.rm=TRUE))
palUsage <- colorBin("OrRd", domain = BERDO$Site_Energy_Use, bins = binUsage)
colorUsage <-c("gray",brewer.pal(6,"OrRd"))
textUsage <- c("NA","0 - 20,000,000","20,000,000 - 40,000,000","40,000,000 - 60,000,000","60,000,000 - 80,000,000","80,000,000 - 100,000,000",">100,000,000")

#BERDO$GHG_Emissions <- ifelse(is.na(BERDO$GHG_Emissions),-14354, BERDO$GHG_Emissions)
GHG_Emissions_UpperBound <-quantile(BERDO$GHG_Emissions,probs=.975,na.rm=TRUE)
binEmissions <- c(seq(0,signif(GHG_Emissions_UpperBound,1),signif(GHG_Emissions_UpperBound,1)/5), max(BERDO$GHG_Emissions,na.rm=TRUE))
palEmissions <- colorBin("OrRd", domain = BERDO$GHG_Emissions,bins = binEmissions)
colorEmissions <-c("gray",brewer.pal(6,"OrRd"))
textEmissions<-c("NA","0 - 2,000","2,000 - 4,000","4,000 - 6,000","6,000 - 8,000", "8,000 - 10,000", ">10,000")

labels <- setNames(c("EnergyStar_Score", "Site_Energy_Use", "GHG_Emissions"), c("Energy Score","Energy Usage","GHG Emissions"))


#BERDO DECLARATIONS END

ui = fluidPage(
  leafletOutput("BERDOmap", height = "522px"),
  box(
    width=12,solidheader=TRUE,collapse=FALSE,status='primary',
    h5("The",
       a("Building Energy Reporting and Disclosure Ordinance (BERDO)",href="https://www.boston.gov/environment-and-energy/building-energy-reporting-and-disclosure-ordinance"), "reports annual energy usage of buildings in Boston. We have displayed this data in a gradient.",
       "Explore the energy star score, energy usage, and greenhouse gas emissions of buildings throughout the city by clicking on the map. See how ratings and emissions vary by building size, type, and location."
    )
  ),
  fluidRow(
    box(width = 3,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed.jpg", width = "100%"),href="https://imagine.boston.gov/wp-content/uploads/2017/07/Ib2030%20BOOK_Spreads--Energy%20and%20Environment.pdf",target="_blank"))),
    box(width = 3,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed%20(2).jpg", width = "100%"),href="http://climatechangedata.boston.gov/",target="_blank"))),
    box(width = 3,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed%20(1).jpg", width = "100%"),href="http://www.greenribboncommission.org/",target="_blank"))),
    box(width = 3,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed.png", width = "100%"),href="http://www.greenovateboston.org/",target="_blank")))
  ),
  absolutePanel(
    id = "controls", class = "panel panel-default", fixed = TRUE,
    draggable = FALSE, top = 40, left = "auto", right = 20, bottom = "auto",
    width = 250, height = "auto",
    selectInput("BERDODataLayer",label=h5("Choose a dataset:"),names(labels))
  )
)


server = function(input, output){
  #BERDO SERVER START
  output$BERDOmap <- renderLeaflet({
    print(input$BERDODataLayer)
    print(labels[input$BERDODataLayer])
    print(BERDO[[labels[input$BERDODataLayer]]])
    leaflet()%>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lng =-71.057083, lat = 42.3601, zoom = 15) %>%
      addPolygons(
        data=BERDO,
        group="Energy Rating",
        weight=1,
        opacity=10,
        color="black",
        fillColor=getPalette(input$BERDODataLayer),
        fillOpacity=5,
        popup=paste(BERDO$Property_Name,
                    "<br/>",
                    "Type:",
                    BERDO$Property_Uses,
                    "<br/>",
                    input$BERDODataLayer,
                    ":"
                    ,
                    ifelse(BERDO[[labels[input$BERDODataLayer]]]== -14354,"NA", prettyNum(BERDO[[labels[input$BERDODataLayer]]], big.mark=",")),  #Replaces -14354 with NA in popup
                    if(BERDO[[labels[input$BERDODataLayer]]]== -14354){getUnits(input$BERDODataLayer)}
        )
      )%>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Boston",
        onClick=JS("function(btn, map){ map.setZoom(8); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
      addLegend(title=getTitle(input$BERDODataLayer),
                position=c("bottomright"),
                colors=getColors(input$BERDODataLayer),
                labels=getText(input$BERDODataLayer)
      )
    
  })
  #BERDO SERVER END
  
  getPalette <- function(dataName){
    if(dataName=="Energy Score"){
      palScore(BERDO[[labels[input$BERDODataLayer]]])
    } else if(dataName=="Energy Usage"){
      palUsage(BERDO[[labels[input$BERDODataLayer]]])
    } else if(dataName=="GHG Emissions"){
      palEmissions(BERDO[[labels[input$BERDODataLayer]]])
    }
  }
  getPaletteFunction <-function(dataName){
    if (dataName=="Energy Score"){
      palScore
    } else if(dataName=="Energy Usage"){
      palUsage
    }
    else if(dataName=="GHG Emissions"){
      palEmissions
    }
  }
  getColors <-function(dataName){
    if (dataName=="Energy Score"){
      colorScore
    } else if(dataName=="Energy Usage"){
      colorUsage
    }
    else if(dataName=="GHG Emissions"){
      colorEmissions
    }
  }
  
  getText <-function(dataName){
    if (dataName=="Energy Score"){
      textScore
    } else if(dataName=="Energy Usage"){
      textUsage
    }
    else if(dataName=="GHG Emissions"){
      textEmissions
    }
  }
  getTitle <- function(dataName){
    if(dataName=="Energy Score"){
      "Energy Star Score"
    } else if(dataName=="Energy Usage"){
      "Energy Usage (kBTU/sq.ft.)"
    } else if(dataName=="GHG Emissions"){
      "Greenhouse Gas Emissions (Metrics Tons of CO2)"
    }
  }
  getUnits <- function(dataName){
    if(dataName=="Energy Usage"){
      "kBTU/sq.ft."
    } else if(dataName=="GHG Emissions"){
      "Metrics Tons of CO2"
    }  
  }
  
}
shinyApp(ui= ui, server = server)