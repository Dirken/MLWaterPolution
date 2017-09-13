library(leaflet)

navbarPage(title="Fecal Matter predictor",
           tabsetPanel(id = "inTabset",
           tabPanel("Table",
                    p("This is tab 1"), 
                    actionButton("tab1", "Next")
                    #          # Select filetype:
                    #          selectInput("readFunction", "Function to read data:", c(
                    #            # Base R:
                    #            "read.table",
                    #            "read.csv",
                    #            "read.csv2",
                    #            "read.delim",
                    #            "read.delim2",
                    #            
                    #            # foreign functions:
                    #            "read.spss",
                    #            "read.arff",
                    #            "read.dbf",
                    #            "read.dta",
                    #            "read.epiiinfo",
                    #            "read.mtp",
                    #            "read.octave",
                    #            "read.ssd",
                    #            "read.systat",
                    #            "read.xport",
                    #            "read_excel",
                    #            
                    #            # Advanced functions:
                    #            "scan",
                    #            "readLines"
                    #          )),
                    #          
                    #          # Argument selecter:
                    #          htmlOutput("ArgSelect"),
                    #          
                    #          # Argument field:
                    #          htmlOutput("ArgText"),
                    #          
                    #          # Upload data:
                    #          fileInput("file", "Upload data-file:"),
                    #          
                    #          
                    #          br(),
                    #          
                    #          textInput("name","Dataset name:","Data"),
                    #          
                    #          downloadButton('downloadSave', 'Download binary'),
                    #          
                    #          
                    #          
                    #          mainPanel(
                    #            column(width = 12, d3tfOutput('table'))
                    #          )
           ),
           tabPanel("Scenario",
                    p("This is tab 2"),
                    actionButton("tab21", "Previous"),
                    actionButton("tab22", "Next")
           ),

           tabPanel("Modelling",
                    p("This is tab 3"),
                    actionButton("tab31", "Previous"),
                    actionButton("tab32", "Next")
           ),
           
           tabPanel("Results",p("This is tab 4"),
                    
                    actionButton("tab41", "Previous"))


           ),
           
            theme = "custom-bootstrap.css",
           
 
            
            tags$footer(class="web", tags$p(class="right", HTML("Developed by Ricard Meyerhofer Parra")))
          
)

