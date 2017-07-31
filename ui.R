library(shinydashboard)

navbarPage("Analyze Boston Data Explorer", id="nav",
  tabPanel("Interactive map",
    bsModal("optionsModal", "Advanced Options", "", uiOutput("optionsModalContent")),
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
                    uiOutput("dataDropdowns"),
                    bsButton("add", "", icon=icon("plus")),
                    bsButton("remove", "", icon=icon("minus")),
                    bsButton("update", "Update Datalayers", style="primary"),
                    bsButton("snap", "Save picture", style="primary"),
                    # hidden text to keep the output$moreThanOnePanel value bound and updating
                    div(textOutput("moreThanOnePanel"), style="color:white; height:0px")
                
      ),
      absolutePanel(id = "share", class = "panel panel-default", fixed = TRUE,
                    draggable = FALSE, top = "auto", right = "auto", left = 10, bottom = 10,
                    width = "auto", height = "auto",
                    h5("Share"),
                    tags$div(HTML('<iframe src="https://www.facebook.com/plugins/share_button.php?href=http%3A%2F%2F127.0.0.1%3A6342%2F%3Fid%3D1%2C2%26name%3DPublic%20Schools%2CNon%20Public%20Schools%26color%3D0000FF%2C008B00%26cluster%3DFALSE%2CFALSE&layout=button_count&size=small&mobile_iframe=true&width=69&height=20&appId" width="69" height="20" style="border:none;overflow:hidden" scrolling="no" frameborder="0" allowTransparency="true"></iframe>')),
                    tags$div(HTML('<a class="twitter-share-button"
  href="https://twitter.com/intent/tweet"
                                  data-size="large">
                                  Tweet</a> <link rel="canonical"
  href="/web/tweet-button">'))#,
                    
                 
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
  )
)