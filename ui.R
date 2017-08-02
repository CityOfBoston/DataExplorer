navbarPage("Analyze Boston Data Explorer", id="nav",
  tabPanel("Interactive map",
    bsModal("optionsModal", "Options", "", uiOutput("optionsModalContent"), size="large"),
    div(class="outer",
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js"),
        includeScript("message-handler.js")
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
                    # bsButton("snap", "Save picture", style="primary"),
                    # hidden text to keep the output$moreThanOnePanel value bound and updating
                    div(textOutput("moreThanOnePanel"), style="color:white; height:0px")
                
      ),
      absolutePanel(id = "share", class = "panel panel-default", fixed = TRUE,
                    draggable = FALSE, top = "auto", right = "auto", left = 5, bottom = 5,
                    width = "auto", height = "auto",
                    fluidRow(
                      column(4, HTML(facebookHtml), style='padding:5px;'),
                      column(4, HTML(twitterHtml), style='padding:5px;'),
                      column(4, HTML(pinterestHtml), style='padding:5px;')
                    )
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
    fluidRow(
      column(6,
             # see this for docs: https://ebailey78.github.io/shinyBS/docs/Collapses.html#bsCollapse
             bsCollapse(
                 bsCollapsePanel("Map 1",
                               div("insert stuff here!")
                 ),
                 bsCollapsePanel("Map 3",
                               div("insert stuff here!")
                 )
             )
            
      ),
      column(6,
             bsCollapse(
               bsCollapsePanel("Map 2",
                               div("insert stuff here!")
               ),
               bsCollapsePanel("Map 4",
                               div("insert stuff here!")
               )
             )
      )
    )
  )
)