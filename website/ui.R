library(leaflet)
library(shinydashboard)
library(shinyBS)
library(shinyjqui)
library(shinycssloaders)
library(shinyWidgets)
library(gridExtra)
#library(ggraptR)
#library(ECharts2Shiny)


################################
# Sourcing global.R
################################
source("global.R", local = TRUE)

jsCode <- 
  "shinyjs.filename =
  function (){
    $(document).ready(function() {
      console.log(document.getElementById('filename').value);
    });
  }"

customSentence <- function(numItems, type) {
  paste("Selecciona el teu idioma")
}


# Function to call in place of dropdownMenu
dropdownMenuCustom <-     function (..., type = c("messages", "notifications", "tasks"), 
                                    badgeStatus = "primary", icon = NULL, .list = NULL, customSentence = customSentence) 
{
  type <- match.arg(type)
  if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li")
  dropdownClass <- paste0("dropdown ", type, "-menu")
  if (is.null(icon)) {
    icon <- switch(type, messages = shiny::icon("envelope"), 
                   notifications = shiny::icon("warning"), tasks = shiny::icon("tasks"))
  }
  numItems <- length(items)
  if (is.null(badgeStatus)) {
    badge <- NULL
  }
  else {
    badge <- span(class = paste0("label label-", badgeStatus), 
                  numItems)
  }
  tags$li(
    class = dropdownClass, 
    a(
      href = "#", 
      class = "dropdown-toggle", 
      `data-toggle` = "dropdown", 
      icon, 
      badge
    ), 
    tags$ul(
      class = "dropdown-menu", 
      tags$li(
        class = "header", 
        customSentence(numItems, type)
      ), 
      tags$li(
        tags$ul(class = "menu", items)
      )
    )
  )
}


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
                              class = "dropdown")
                            # ,
                            # tags$li(HTML( 
                            #   "<a href='' style='padding-top:10px; padding-bottom:8px;'>
                            #   <img height='20px'>
                            #   <i class='fa fa-language' style='height:30px;'></i> Languages</a>"),
                            #   class = "dropdown")
                            
                            )



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
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/prettyfy.css")),
    
    tags$head(tags$script(src="js/table.js")),
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
                bsAlert("alert"),
                modalAbout
                
              )
              
              
      ),
      
      tabItem("tab22",
              box(
                title = "Choosing variables to create a model",
                status = "primary",
                width = 12,
                div(class="llistes",
                    
                  div(class="column1",materialSwitch(inputId = "pointSource", label = "Point source?", status = "primary")), 
                  p(class="vs-titol2", " "),
                  div(class="column2",materialSwitch(inputId = "molecular", label = "Molecular variables?", status = "primary"))
                ),
                
                # conditionalPanel(condition = "input.pointSource == 0",
                #                  sliderInput("aging", label = "Aged",  min = 0, max = 9000, value = c(0,9000))
                # ),
                sliderInput("matrixSize", label = "Matrix Size",  min = 0, max = 100000, value = 1000, step = 1000),
                #uiOutput(matrixSizeSlider),
                
                sliderInput("percentatgeNAs", label = "Discard column from %NA",  min = 0, max = 100, value = 50, step = 1),
                p(class="titol" ,"Predict "), br(),
                div(class="llistes",
                    div(class="column1",
                      pickerInput(
                        inputId = "myPicker",
                        label = "Select predicts",
                        choices = "unique(predictValues)",
                        options = list(
                          `actions-box` = TRUE,
                          size = 10
                        ),
                        multiple = TRUE
                      )
                    
                    ),
                    
                    p(class="vs-titol", "VS"),
                    div(class="column2",
                      pickerInput(
                        inputId = "myPicker2",
                        label = "Select vs predicts",
                        choices = "unique(predictValues)",
                        options = list(
                          `actions-box` = TRUE,
                          size = 10
                        ),
                        multiple = TRUE
                      )
                    )
                ),
                div(class="right",
                  actionBttn(inputId = "generateMatrix", label = "Generate BIGMATRIX", 
                             icon = icon("cog"), color = "primary", style = "jelly")
                )
              )
      ),
      
      tabItem("tab2",
              box(
                title = "Creation of a predictive model",
                status = "primary",
                width = 12,
                pickerInput(inputId = "algorithm", 
                            label = "Algorithm to apply", 
                            choices = c("LDA", "QDA"), 
                            options = list(title = "Choose your algorithm ")
                ),
                HTML("<hr class='style11'>"),
                withSpinner(DT::dataTableOutput("data2"))

                
              )
              
      ),
      
      
      tabItem("tab3",
              title = "Visualization",
              box(title = "Visualization",width = 12, status = "primary",
                  div(class="right",
                    switchInput(inputId = "trainOrTest", 
                                onLabel = "Training", offLabel = "Test")
                    
                  ),
                  conditionalPanel(condition = "input.trainOrTest",
                                   HTML("

                                        <center><b class='titol'>Chosen variables</b></center>
                                        
                                        <table class='table table-striped'>
                                        <thead>
                                          <tr>
                                          <th><b>Variables</b></th>
                                          <th><b>Selected</b></th>
                                          </tr>
                                        </thead>
                                        <tbody>
                                        <tr>
                                        
                                        <td><b>Matrix Size:</b></td><td>
                                         </td>
                                        </tr>
                                        <tr>
                                        <td><b>Discard column from %NA:</b></td>
                                        <td>input.percentatgeNAs</td>
                                        </tr>
                                        <tr>
                                        <td><b>Algorithm chosen:</b></td>
                                        <td>QDA</td>
                                        
                                        </tr>
                                        <tr>
                                        <td><b>Target Class:</b> </td>
                                        <td>HUMAN VS NO-HUMAN</td>
                                        </tr>
                                        </tbody>
                                        </table>
                                        
                                        <center><b class='titol'>Truth Table</b></center>
                                        <br>
                                        <center><b>Prediction</b></center>
                                        
                                        <table class='table table-striped'>
                                        <thead>
                                        <tr>
                                        <th>Truth</th>
                                        <th>nonhuman</th>
                                        <th>human</th>
                                        </tr>
                                        </thead>
                                        <tbody>
                                        
                                        <tr>
                                        <tr>
                                        <td>nonhuman</td>
                                        <td>69</td>
                                        <td>0</td>
                                        </tr>
                                        <tr>
                                        <td>human</td>
                                        <td>2</td>
                                        <td>33</td>
                                        </tr>
                                        
                                        </tbody>
                                        </table>"
                                   )
                  ),
                  conditionalPanel(condition ="!input.trainOrTest",
                                   HTML("

                                        <center><b class='titol'>Chosen variables</b></center>
                                        
                                        <table class='table table-striped'>
                                        <thead>
                                        <tr>
                                        <th><b>Variables</b></th>
                                        <th><b>Selected</b></th>
                                        </tr>
                                        </thead>
                                        <tbody>
                                        <tr>
                                        <td><b>Matrix Size:</b></td>
                                        <td>10000</td>
                                        </tr>
                                        <tr>
                                        <td><b>Discard column from %NA:</b></td>
                                        <td>50%</td>
                                        </tr>
                                        <tr>
                                        <td><b>Algorithm chosen:</b></td>
                                        <td>QDA</td>
                                        
                                        </tr>
                                        <tr>
                                        <td><b>Target Class:</b> </td>
                                        <td>HUMAN VS NO-HUMAN</td>
                                        </tr>
                                        </tbody>
                                        </table>
                                        
                                        <center><b class='titol'>Truth Table</b></center>
                                        <br>
                                        <center><b>Prediction</b></center>
                                        
                                        <table class='table table-striped'>
                                        <thead>
                                        <tr>
                                        <th>Truth</th>
                                        <th>nonhuman</th>
                                        <th>human</th>
                                        </tr>
                                        </thead>
                                        <tbody>
                                        
                                        <tr>
                                        <tr>
                                        <td>nonhuman</td>
                                        <td>69</td>
                                        <td>0</td>
                                        </tr>
                                        <tr>
                                        <td>human</td>
                                        <td>2</td>
                                        <td>33</td>
                                        </tr>
                                        
                                        </tbody>
                                        </table>"
                                        )
              )
      
      
              ))
    )
  )
)





