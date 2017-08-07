navbarPage("Analyze Boston Data Explorer", id="nav", collapsible=TRUE,
  tabPanel(title="Interactive Map", value="tab1",
    bsModal("optionsModal", "Options", "", uiOutput("optionsModalContent"), size="large"),
    tags$html(HTML('<meta property="og:url"               content="https://rupayan.shinyapps.io/dataExplorer/" />
              <meta property="og:title"              content="Analyze Boston Data Explorer" />
              <meta property="og:description"        content="Visualize public city data with the click of a button!" />
              <meta property="og:image"              content="https://rupayan.shinyapps.io/dataExplorer/COB_B_Blue-01.png" />')),
    div(class="outer",
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js"),
        includeScript("widgets.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = FALSE, top = 80, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",
                    h2("Data Explorer"),
                    bsAlert("errorAlert"),
                    uiOutput("dataDropdowns"),
                    bsButton("add", "", icon=icon("plus")),
                    bsButton("update", "Update Datalayers", style="primary"),
                    # hidden text to keep the output$moreThanOnePanel value bound and updating
                    div(textOutput("moreThanOnePanel"), style="color:white; height:0px")
                
      ),
      absolutePanel(id = "share", class = "panel panel-default", fixed = TRUE,
                    draggable = FALSE, top = "auto", right = "auto", left = 5, bottom = 5,
                    width = "auto", height = "auto",
                    uiOutput("sharables")
                    
      )  
    )
  ),

  tabPanel("Data Viewer",
    fluidRow(
      column(3,
        selectInput("dataset", "Data Sets", choices=NULL)
      )
    ),
    hr(),
    DT::dataTableOutput("datatable")
  ),
  
  ########## SHOWCASES ############
  tabPanel("Data Showcases",
    useShinyjs(),
    h4("Here are some data visualizations created from Analyze Boston datasets. Use them for inspiration to create your own maps on the Data Explorer!",
       style="text-align: center;"),
    h5(id="mobileMessage", "Unfortunately, the Data Explorer is only available on non-mobile devices. If you wish to use the other functionality of this website, please use a larger device.",
       style="text-align: center;"),
    hr(),
    fluidRow(
      column(6,
             # see this for docs: https://ebailey78.github.io/shinyBS/docs/Collapses.html#bsCollapse
             bsCollapse(open="Biking in Boston",
                 bsCollapsePanel("Biking in Boston",
                                 absolutePanel(top="200px", left="45%", right="auto", bottom="auto",
                                               img(id="loading1", src="Resources/loading.gif")
                                 ),
                                   leafletOutput("bikemap"),
                                   br(),
                                   p("This map helps people find routes to get around", 
                                      a("Boston by Bike.", href="https://www.boston.gov/departments/boston-bikes",target="_blank"), 
                                      "We used two datasets from", 
                                      a("Analyze Boston:", href="https://data.boston.gov/", target="_blank"), 
                                      "Existing Bike Network shows which streets have bike lanes that riders can saftely use, and Hubway Stations.", 
                                      a("Hubway", href="https://www.thehubway.com/",target="_blank"), 
                                      "is a bike share program that allows riders to pick up a bike at one location, ride to their destination and drop it off at a different station."),
                                   p(a("Go Boston 2030", href="https://www.boston.gov/transportation/go-boston-2030", target="_blank"), 
                                      "is a City of Boston initiative that envisions safer, more accessible, and more sustainable transportation in Boston's future."
                                   ),
                                   fluidRow(
                                     column(width = 4, class="logoCol", tags$a(img(src="Resources/IB2030Logo.jpg", class="logo"),href="https://imagine.boston.gov/wp-content/uploads/2017/07/Ib2030%20BOOK_Spreads--Transportation.pdf",target="_blank")),
                                     column(width = 4, class="logoCol", tags$a(img(src="Resources/BostonBikesLogo.png", class="logo"),href="http://goboston2030.org/en/",target="_blank")),
                                     column(width = 4, class="logoCol", tags$a(img(src="Resources/HubwayLogo.png", class="logo"),href="https://www.thehubway.com/",target="_blank")),
                                     class="logoRow")
                 ),
                 bsCollapsePanel("Building Energy Reporting Showcase",
                             absolutePanel(top="200px", left="45%", right="auto", bottom="auto",
                                           img(id="loading3", src="Resources/loading.gif")
                                           ),
                             leafletOutput("BERDOmap"),
                             selectInput("BERDODataLayer",label=h5("Choose a dataset:"),names(BERDOlabels)),
                             br(),
                             p("The",
                                a("Building Energy Reporting and Disclosure Ordinance (BERDO)",href="https://www.boston.gov/environment-and-energy/building-energy-reporting-and-disclosure-ordinance"), "reports annual energy usage of buildings in Boston. We have displayed this data in a gradient.",
                                "Explore the energy star score, energy usage, and greenhouse gas emissions of buildings throughout the city by clicking on the map. See how ratings and emissions vary by building size, type, and location."
                             ),
                             fluidRow(
                                     column(width = 4, class="logoCol", tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed.jpg", class="logo"),href="https://imagine.boston.gov/wp-content/uploads/2017/07/Ib2030%20BOOK_Spreads--Energy%20and%20Environment.pdf",target="_blank")),
                                     column(width = 4, class="logoCol", tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed%20(1).jpg", class="logo"),href="http://www.greenribboncommission.org/",target="_blank")),
                                     column(width = 4, class="logoCol", tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed.png", class="logo"),href="http://www.greenovateboston.org/",target="_blank")),
                                     class="logoRow")
                 )
             )
            
      ),
      column(6,
             bsCollapse(open="Boston Education Opportunities",
               bsCollapsePanel("Boston Education Opportunities",
                               absolutePanel(top="200px", left="45%", right="auto", bottom="auto",
                                             img(id="loading2", src="Resources/loading.gif")
                               ),
                                 leafletOutput("SCHOOLmap"),
                                 br(),
                                 p("This map helps people visualize education across Boston to show where different public and private schools, and colleges/universities are located. Learn about the City of Boston's initiative to improve public education and expand early education below."),
                                 fluidRow(
                                    column(width = 4, class="logoCol", tags$a(img(src="Resources/IB2030Logo.jpg", class="logo"),href="https://imagine.boston.gov/wp-content/uploads/2017/07/Ib2030%20BOOK_Spreads--Energy%20and%20Environment.pdf",target="_blank")),
                                    column(width = 4, class="logoCol", tags$a(img(src="Resources/BuildBPSLogo.jpg", class="logo"),href="https://www.bostonpublicschools.org/buildbps",target="_blank")),
                                    column(width = 4, class="logoCol", tags$a(img(src="Resources/BarrFoundationLogo.svg", class="logo"),href="https://www.barrfoundation.org/partners/boston-k1ds",target="_blank")),
                                 class="logoRow")
               ),
               bsCollapsePanel("Street Lamps in Boston Neighborhoods",
                               absolutePanel(top="200px", left="45%", right="auto", bottom="auto",
                                             img(id="loading4", src="Resources/loading.gif")
                               ),
                               leafletOutput("lightMap"),
                               br(),
                               p("This map takes the ",
                                   a("Streetlight Locations data set",href="https://data.boston.gov/dataset/streetlight-locations"),
                                   " and intersects it with the ",
                                   a("Neighborhoods data set", href="https://data.boston.gov/dataset/boston-neighborhoods"),
                                   " in order to show how many streetlights there are per square mile in each neighborhood. With ",
                                   "this visualization, we can easily see both the level of light pollution near Downtown Boston and ",
                                   "the lack of well-lit areas in the surrounding areas."),
                               tags$b("Make an impact: "),
                               p("Did you know you can request better lighting in your neighborhood online?", 
                                " Check out ", a("Boston's Street Lighting Division website", href="https://www.cityofboston.gov/publicworks/lighting/"),
                                " for more info!")
               )
             )
      )
    )
  )
)