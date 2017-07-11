library(shinydashboard)
library(leaflet)

dashboardPage(
  dashboardHeader(
    title = "UB",
    dropdownMenu(type = "messages",
                 messageItem(
                   from = "Sales Dept",
                   message = "Sales are steady this month."
                 ),
                 messageItem(
                   from = "New User",
                   message = "How do I register?",
                   icon = icon("question"),
                   time = "13:45"
                 ),
                 messageItem(
                   from = "Support",
                   message = "The new server is ready.",
                   icon = icon("life-ring"),
                   time = "2014-12-01"
                 )
    )
  
  ),
  
  dashboardSidebar(
    sidebarMenu(id = "tab",
      menuItem("Visualization", tabName = "tab1", icon = icon("dashboard")),
      menuItem("Table", icon = icon("table"), tabName = "tab2"),
      menuItem("About", tabName = "tab3", icon = icon("info-circle")),
      menuItem("Logout", tabName = "tab4", icon = icon("sign-out"))
    )
    
  ),
  
  dashboardBody(
    tabItems(
        tabItem("tab1",
          fluidRow(
            tabBox(
              title = "TabBox",
              # The id lets us use input$tabset1 on the server to find the current tab
              id = "tabset1", height = 500, width = 9,
              tabPanel("Tab1", "First tab content"),
              tabPanel("Tab2", "Tab content 2")
            ),
            column(width = 3,
                   box(width = NULL, 
                       uiOutput("routeSelect"),
                       checkboxGroupInput("directions", "Show",
                                          choices = c(
                                          ),
                       ),
                       p(
                         class = "text-muted",
                         paste("Note: Comment"
                         )
                       ),
                       actionButton("zoomButton", "Zoom to fit buses")
                   ),
                   box(width = NULL, 
                       selectInput("interval", "Refresh interval",
                                   choices = c(
                                   ),
                                   selected = "60"
                       ),
                       uiOutput("timeSinceLastUpdate"),
                       actionButton("refresh", "Refresh now"),
                       p(class = "text-muted",
                         br(),
                         "Source "
                       )
                   )
            )
          )
        ),
        ##############################################################################
        tabItem("tab2",
          # Input in sidepanel:
          sidebarPanel(width = 2,
            tags$style(type='text/css', ".well { max-width: 15em; }"),
            # Tags:
            tags$head(
              tags$style(type="text/css", "select[multiple] { width: 100%; height:10em}"),
              tags$style(type="text/css", "select { width: 100%}"),
              tags$style(type="text/css", "input { width: 19em; max-width:100%}"),
              tags$style(type="text/css", "shiny.tag.list { color: blue}"),
              #Sweet alert added:
              tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/limonte-sweetalert2/6.6.5/sweetalert2.min.js"),
              tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/limonte-sweetalert2/6.6.5/sweetalert2.min.css")
              
            ),
            
            # Select filetype:
            selectInput("readFunction", "Function to read data:", c(
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
            )),
            
            # Argument selecter:
            htmlOutput("ArgSelect"),
            
            # Argument field:
            htmlOutput("ArgText"),
            
            # Upload data:
            fileInput("file", "Upload data-file:"),
            
            
            br(),
            
            textInput("name","Dataset name:","Data"),
            
            downloadButton('downloadSave', 'Download binary')

          ),

          fluidRow(
            column(width = 9, d3tfOutput('table', height = "auto"))
          )
          
        )
                  
    )
  )
)


