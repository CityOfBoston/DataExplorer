navbarPage("Analyze Boston Data Explorer", id="nav",
  tabPanel("Interactive map",
    div(class="outer",
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = FALSE, top = 80, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",
                    h2("Data Explorer"),
                    uiOutput("dataDropdowns"),
                    bsButton("add", "", icon=icon("plus")),
                    # bsButton("remove", "", icon=icon("minus")),
                    bsButton("update", "Update Datalayers", style="primary")
                    # bsButton("snapshot", "Export", style="secondary")
      ),
      uiOutput("modals")
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
  )
)
