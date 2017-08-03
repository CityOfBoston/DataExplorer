navbarPage("Analyze Boston Data Explorer", id="nav",
  tabPanel("Interactive map",
    bsModal("optionsModal", "Options", "", uiOutput("optionsModalContent"), size="large"),
    div(class="outer",
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js"),
        includeScript("widgets.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class="modal" instead.
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
                    h5("Share"),
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
    fluidRow(
      column(6,
             # see this for docs: https://ebailey78.github.io/shinyBS/docs/Collapses.html#bsCollapse
             bsCollapse(open="Bike Map",
                 bsCollapsePanel("Bike Map",
                                 fluidPage(
                                   img(id="loading1", src="Resources/loading.gif"),
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
                                     column(width = 4, fluidPage(tags$a(img(src="https://raw.githubusercontent.com/rneogy/DataExplorer/master/Resources/IB2030Logo.jpg", width = "100%"),href="https://imagine.boston.gov/wp-content/uploads/2017/07/Ib2030%20BOOK_Spreads--Transportation.pdf",target="_blank"))),
                                     column(width = 4, fluidPage(tags$a(img(src="https://raw.githubusercontent.com/rneogy/DataExplorer/master/Resources/BostonBikesLogo.png", width = "100%"),href="http://goboston2030.org/en/",target="_blank"))),
                                     column(width = 4, fluidPage(tags$a(img(src="https://raw.githubusercontent.com/rneogy/DataExplorer/master/Resources/HubwayLogo.png", width = "100%"),href="https://www.thehubway.com/",target="_blank")))
                                   )
                                 )
                 ),
                 bsCollapsePanel("Driving in Boston",
                         fluidPage(
                             img(id="loading3", src="Resources/loading.gif"),
                             leafletOutput("drivemap"),
                             p("Utilizing the Emergency Parking and Charging Stations datasets from Boston we have produced an interactive visualization mapping out suitable parking tailored to an individuals needs."),
                             fluidRow(
                                     column(width = 4, fluidPage(tags$a(img(src="https://raw.githubusercontent.com/rneogy/DataExplorer/master/Resources/IB2030Logo.jpg", width = "100%"),href="https://imagine.boston.gov/wp-content/uploads/2017/07/Ib2030%20BOOK_Spreads--Transportation.pdf",target="_blank"))),
                                     column(width = 4, fluidPage(tags$a(img(src="https://raw.githubusercontent.com/rneogy/DataExplorer/master/Resources/VisionZeroLogo.PNG", width = "100%"),href="http://www.visionzeroboston.org/",target="_blank"))),
                                     column(width = 4, fluidPage(tags$a(img(src="https://raw.githubusercontent.com/rneogy/DataExplorer/master/Resources/ZipCarLogo.png", width = "100%"),href="https://www.boston.gov/transportation/drive-boston",target="_blank")))
                             )
                         )
                 )
             )
            
      ),
      column(6,
             bsCollapse(open="Boston Education",
               bsCollapsePanel("Boston Education",
                               fluidPage(
                                 img(id="loading2", src="Resources/loading.gif"),
                                 leafletOutput("SCHOOLmap"),
                                 br(),
                                 fluidRow(
                                   fluidRow(
                                      column(width = 1),column(width = 10,p("This map helps people visualize education across Boston to show where different public and private schools, and colleges/universities are located.Learn about the City of Boston's initiative to improve public education and expand early education below.")), column(width = 1)
                                    ),
                                    column(width = 4, fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed.jpg", width = "100%"),href="https://imagine.boston.gov/wp-content/uploads/2017/07/Ib2030%20BOOK_Spreads--Energy%20and%20Environment.pdf",target="_blank"))),
                                    column(width = 4, fluidPage(tags$a(img(src="https://pbs.twimg.com/media/CroEOLKWAAAnhhf.jpg:large", width = "100%"),href="https://www.bostonpublicschools.org/buildbps",target="_blank"))),
                                    column(width = 4, fluidPage(tags$a(img(src="https://www.barrfoundation.org/assets/logo-821db212e468b22f9c4e0c3774d6214de98e41745c32d26bc3075a504f43f531.svg", width = "100%"),href="https://www.barrfoundation.org/partners/boston-k1ds",target="_blank")))
                                 )
                               )
               ),
               bsCollapsePanel("Street Lamps in Boston",
                               img(id="loading4", src="Resources/loading.gif"),
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