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
                            tags$li(HTML( 
                                  "<a href='www/manual.txt' style='padding-top:10px; padding-bottom:8px;' download>
                                    <img height='20px'>
                                      <i class='fa fa-question' style='height:30px;'></i> Help 
                                  </a>"),  
                                  class = "dropdown"),
                            tags$li(HTML( 
                              "<a href='' style='padding-top:10px; padding-bottom:8px;'  data-toggle='modal' data-target='#myModal'>
                                    <img height='20px'>
                                      <i class='fa fa-info-circle' style='height:30px;'></i> About 

                               </a>"),  
                              class = "dropdown"))
                      


modalAbout <- HTML("
<!-- Modal -->
  <div id='myModal' class='modal fade' role='dialog'>
  <div class='modal-dialog'>
  
  <!-- Modal content-->
  <div class='modal-content'>
  <div class='modal-header'>
  <button type='button' class='close' data-dismiss='modal'>&times;</button>
  <h4 class='modal-title'>About this website</h4>
  </div>
  <div class='modal-body'>
  <p> 

    <center>
      <img src='img/logo_ub.png' style='width:250px;'/>
      <img src='img/logo_upc.png' style='width:250px;'/>
    </center>
    This website is possible thanks to the efforts made by Universitat Politècnica de Catalunya and Universitat of Barcelona.
  </p>
  </div>
  <div class='modal-footer'>
  <button type='button' class='btn btn-default' data-dismiss='modal'>Dismiss</button>
  </div>
  </div>
  
  </div>
  </div>
")

options(shiny.sanitize.errors = FALSE)
dashboardPage(
  dbHeader,
  dashboardSidebar(
    sidebarMenuOutput("Semi_collapsible_sidebar")
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css")),
    shinyjs::useShinyjs(),
    extendShinyjs(text = jsCode),
    useSweetAlert(),
    
    tabItems(
      
        
        tabItem("tab1",
                # conditionalPanel(condition = ),
                box(
                  title = "Microbiological Data",
                  status = "primary",
                  width = 12,
                  withSpinner(DT::dataTableOutput("data")),
                  # bsAlert("alert"),
                  modalAbout
                  
                )

          
        ),
        
        tabItem("tab22",
                box(
                  title = "Choosing variables to create a model",
                  status = "primary",
                  width = 12,
                  materialSwitch(inputId = "pointSource", label = "Point source?", status = "primary"),
                  materialSwitch(inputId = "molecular", label = "Molecular variables?", status = "primary"),
                  
                  conditionalPanel(condition = "input.pointSource == 0",
                    sliderInput("aging", label = "Aged",  min = 0, max = 9000, value = c(0,9000))
                  ),
                  sliderInput("dissolution", label = "Dissolution",  min = 0, max = 9000, value = c(0,9000)),
                  

                     checkboxGroupButtons(
                       inputId = "somevalue", label = "Make a choice :", 
                       choices = c("Chicken", "Cow", " Poltry"), 
                       justified = TRUE, status = "primary",
                       checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                     ),
                  div(class="llistes",
                    pickerInput(
                      inputId = "myPicker", 
                      label = "Select/deselect all + format selected", 
                      choices = LETTERS, 
                      options = list(
                        `actions-box` = TRUE, 
                        size = 10,
                        `selected-text-format` = "count > 3"
                      ), 
                      multiple = TRUE
                    ),
                    pickerInput(
                      inputId = "myPicker2", 
                      label = "Select/deselect all + format selected", 
                      choices = LETTERS, 
                      options = list(
                        `actions-box` = TRUE, 
                        size = 10,
                        `selected-text-format` = "count > 3"
                      ), 
                      multiple = TRUE
                    )
                  ),
                  div(class="right",
                      submitButton("Generate matrix", icon = icon("arrow-right"))
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
        


                  

         
