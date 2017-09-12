library(leaflet)

navbarPage("Fecal Matter predictor",
           tabPanel("Table"                    
                    # # Input in sidepanel:
                    # sidebarPanel(width = 2,
                    #              tags$style(type='text/css', ".well { max-width: 15em; }"),
                    #              # Tags:
                    #              tags$head(
                    #                tags$style(type="text/css", "select[multiple] { width: 100%; height:10em}"),
                    #                tags$style(type="text/css", "select { width: 100%}"),
                    #                tags$style(type="text/css", "input { width: 19em; max-width:100%}"),
                    #                tags$style(type="text/css", "shiny.tag.list { color: blue}")
                    #              ),
                    #              
                    #              # Select filetype:
                    #              selectInput("readFunction", "Function to read data:", c(
                    #                # Base R:
                    #                "read.table",
                    #                "read.csv",
                    #                "read.csv2",
                    #                "read.delim",
                    #                "read.delim2",
                    #                
                    #                # foreign functions:
                    #                "read.spss",
                    #                "read.arff",
                    #                "read.dbf",
                    #                "read.dta",
                    #                "read.epiiinfo",
                    #                "read.mtp",
                    #                "read.octave",
                    #                "read.ssd",
                    #                "read.systat",
                    #                "read.xport",
                    #                "read_excel",
                    #                
                    #                # Advanced functions:
                    #                "scan",
                    #                "readLines"
                    #              )),
                    #              
                    #              # Argument selecter:
                    #              htmlOutput("ArgSelect"),
                    #              
                    #              # Argument field:
                    #              htmlOutput("ArgText"),
                    #              
                    #              # Upload data:
                    #              fileInput("file", "Upload data-file:"),
                    #              
                    #              
                    #              br(),
                    #              
                    #              textInput("name","Dataset name:","Data"),
                    #              
                    #              downloadButton('downloadSave', 'Download binary')
                    #              
                    # ),
                    # 
                    # mainPanel(
                    #   column(width = 9, d3tfOutput('table'))
                    # )         
                    # 
                    # 
                    # 
                    # 
           ),
           tabPanel("Scenario"),
           tabPanel("Modelling"),
           tabPanel("Results"),
           
            theme = "custom-bootstrap.css",
            tags$footer(class="web", tags$p(class="right", HTML("Website in collaboration with Universitat Politecnica de Catalunya 
                                                          <br> Developed by Ricard Meyerhofer Parra")))
          
)

