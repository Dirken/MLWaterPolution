library(leaflet)
library(shinydashboard)
library(shinyBS)

jsCode <- 
  "shinyjs.filename =
  	function (){
  		$(document).ready(function() {
        		console.log(document.getElementById('filename').value);
	    });
    }"


dashboardPage(
  
  dashboardHeader(
    title = "ICHNAEA"
  ),
  dashboardSidebar(
    sidebarMenu(id = "tab",
                menuItem("Table", 
                         tabName = "tab1",
                         icon = icon("table")
                         
                                    
                ),
                menuItem("About", 
                         tabName = "tab2", 
                         icon = icon("info-circle")
                ),
                conditionalPanel("input.tab == 'tab1'",
                                 selectInput("readFunction", "Format to read", c(
                                   # Base R:
                                   "read.table",
                                   "read.csv",
                                   "read.csv2",
                                   "read.delim",
                                   "read.delim2",
                                   
                                   # foreign functions:
                                   "read.spss",
                                   "read.arff",
                                   "read.dbf",
                                   "read.dta",
                                   "read.epiiinfo",
                                   "read.mtp",
                                   "read.octave",
                                   "read.ssd",
                                   "read.systat",
                                   "read.xport",
                                   "read_excel",
                                   
                                   # Advanced functions:
                                   "scan",
                                   "readLines"
                                 )
                                 ),
                                 
                                 # Argument selecter:
                                 htmlOutput("ArgSelect"),
                                 
                                 # Argument field:
                                 htmlOutput("ArgText"),
                                 
                                 # Upload data:
                                 fileInput("file", "Browse file"),
                                 htmlOutput('response2')
                )
                
    )
    
  ),
  dashboardBody(
    tags$head(tags$script(src="js/table.js")),
    shinyjs::useShinyjs(),
    extendShinyjs(text = jsCode),
    tabItems(
      
        tabItem("tab1",
          DT::dataTableOutput("data")
        ),

        
        tabItem("tab2",
          p("hola2")
        )
    )
  )
)
        


                  

         
