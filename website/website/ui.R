library(leaflet)
library(shinydashboard)
library(shinyBS)
library(shinyjqui)
library(shinycssloaders)
library(shinyWidgets)
#library(ECharts2Shiny)

jsCode <- 
  "shinyjs.filename =
  	function (){
  		$(document).ready(function() {
        		console.log(document.getElementById('filename').value);
	    });
    }"

dbHeader <- dashboardHeader(title = "ICHNAEA",
                            tags$li(a(href = '',
                                      img(icon("question"), "Help", height = "30px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"),
                            tags$li(a(href = '',
                                      img(icon("info-circle"), "About", height = "30px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                      class = "dropdown")
                            )

options(shiny.sanitize.errors = FALSE)
dashboardPage(
  dbHeader,
  dashboardSidebar(
    sidebarMenuOutput("Semi_collapsible_sidebar")
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    tags$head(tags$script(src="js/table.js")),
    shinyjs::useShinyjs(),
    extendShinyjs(text = jsCode),

    
    tabItems(
      
        
        tabItem("tab1",
                # conditionalPanel(condition = ),
                box(
                  title = "Microbiological Data",
                  status = "primary",
                  width = 12,
                  withSpinner(DT::dataTableOutput("data"))
                  
                )

          
        ),
        
        tabItem("tab22",
                box(
                  title = "Choosing variables to create a model",
                  status = "primary",
                  width = 12,
                  materialSwitch(inputId = "pointSource", label = "Point source?", status = "primary"),
                  conditionalPanel(condition = "input.pointSource == 0",
                    sliderInput("aging", label = "Aged",  min = 0, max = 9000, value = c(0,9000))
                  ),
                  sliderInput("dissolution", label = "Dissolution",  min = 0, max = 9000, value = c(0,9000)),
                  
                  materialSwitch(inputId = "molecular", label = "Molecular variables?", status = "primary"),
                  materialSwitch(inputId = "human", label = "Human vs no Human?", status = "primary"),
                  
                  conditionalPanel(condition = "input.human == 0",
                     checkboxGroupButtons(
                       inputId = "somevalue", label = "Make a choice :", 
                       choices = c("Chicken", "Cow", " Poltry"), 
                       justified = TRUE, status = "primary",
                       checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                     )
                  )
                )
        ),
        
        tabItem("tab2",
                box(
                  title = "Creation of a predictive model",
                  status = "primary",
                  width = 12
                 
                )
                ),
                
        
        tabItem("tab3",
                  title = "Visualization",
                  status = "primary",
                  fluidRow(
                    column(
                      width = 6,
                      box(title = "a1", status = "primary"),
                      box(title = "a2",status = "primary")
                    ),
                    column(
                      width = 6,
                      box(
                        title = "a3",
                        status = "primary"
                       )
                      )
                    )
                )


        
    )
  )
)
        


                  

         
