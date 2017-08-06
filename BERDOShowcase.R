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

BERDO$EnergyStar_Score <- ifelse(is.na(BERDO$EnergyStar_Score),0, BERDO$EnergyStar_Score)
binScore <- c(seq(0,100,20)) # bind bind it with INF
palScore <- colorBin("YlGn", domain = BERDO$EnergyStar_Score, bins = binScore)
colorScore <- brewer.pal(5,"YlGn")
textScore <- c("0 - 20","20 - 40","40 - 60", "60 - 80", "80 - 100")

BERDO$Site_Energy_Use <- ifelse(is.na(BERDO$Site_Energy_Use),0, BERDO$Site_Energy_Use)
Energyuse_IQR <- IQR(BERDO$Site_Energy_Use)
Energyuse_Q3 <- summary(BERDO$Site_Energy_Use)[5] #returns 3rd quartile of data set
Energyuse_UpperBound <- Energyuse_Q3 + Energyuse_IQR*1.5
binUsage <- c(seq(0,signif(Energyuse_UpperBound,1),signif(Energyuse_UpperBound,1)/5), max(BERDO$Site_Energy_Use))
palUsage <- colorBin("OrRd", domain = BERDO$Site_Energy_Use, bins = binUsage)
colorUsage <-brewer.pal(6,"OrRd")
textUsage <- c("0 - 6,000,000","6,000,000 - 12,000,000","12,000,000 - 18,000,000","18,000,000 - 24,000,000","24,000,000 - 30,000,000",">30,000,000")

BERDO$GHG_Emissions <- ifelse(is.na(BERDO$GHG_Emissions),0, BERDO$GHG_Emissions)
GHG_Emissions_IQR <- IQR(BERDO$GHG_Emissions)
GHG_Emissions_Q3 <- summary(BERDO$GHG_Emissions)[5] #returns 3rd quartile of data set
GHG_Emissions_UpperBound <- GHG_Emissions_Q3 + GHG_Emissions_IQR*1.5
binEmissions <- c(seq(0,signif(GHG_Emissions_UpperBound,1),signif(GHG_Emissions_UpperBound,1)/5), max(BERDO$GHG_Emissions))
palEmissions <- colorBin("OrRd", domain = BERDO$GHG_Emissions,bins = binEmissions)
colorEmissions <-brewer.pal(6,"OrRd")
textEmissions<-c("0 - 400","400 - 800","800 - 1,200","1,200 - 1,600", "1,600 - 2,000", ">2,000")

labels <- setNames(c("EnergyStar_Score", "Site_Energy_Use", "GHG_Emissions"), c("Energy Score","Energy Usage","GHG Emissions"))


brap <-c(1,2,3,4,5,6)
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
        popup=paste(BERDO$Property_Name,"<br/>","Type:",BERDO$Property_Uses,"<br/>",input$BERDODataLayer, ":",ifelse(BERDO[[labels[input$BERDODataLayer]]]==0,"Not Enough Info",BERDO[[labels[input$BERDODataLayer]]]),getUnits(input$BERDODataLayer))
      )%>%
      # addPolygons(
      #   data=BERDO,
      #   group="Energy Usage",
      #   weight=1,
      #   opacity=10,
      #   color="black",
      #   fillColor=~palUsage(Site_Energy_Use),
      #   fillOpacity=5,
      #   popup=paste(BERDO$Property_Name,"<br/>","Type:",BERDO$Property_Uses,"<br/>","Energy Usage:",ifelse(BERDO$Site_Energy_Use==0,"Not Enough Info",BERDO$Site_Energy_Use),ifelse(BERDO$Site_Energy_Use==0,"","kBTU/sq.ft."))
      # )%>%
      # addPolygons(
      #   data=BERDO,
      #   group="GHG Emissions",
      #   weight=1,
      #   opacity=10,
      #   color="black",
      #   fillColor=~palEmissions(GHG_Emissions),
      #   fillOpacity=5,
      #   popup=paste(BERDO$Property_Name,"<br/>","Type:",BERDO$Property_Uses,"<br/>","GHG Emissions:", ifelse(BERDO$GHG_Emissions==0,"Not Enough Info",BERDO$GHG_Emissions),ifelse(BERDO$Site_Energy_Use==0,"","metric tons"))
      # )%>%
      # addLayersControl(
      #   baseGroups = c("Energy Rating","Energy Usage","GHG Emissions"),
      #   options=layersControlOptions(collapsed=FALSE)
      # )%>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Boston",
        onClick=JS("function(btn, map){ map.setZoom(8); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
      addLegend(title=getTitle(input$BERDODataLayer),position=c("bottomright"),
                #values=BERDO[[labels[input$BERDODataLayer]]],
                #pal=getPaletteFunction(input$BERDODataLayer))
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