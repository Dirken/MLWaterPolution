library(leaflet)

navbarPage(title="Fecal Matter predictor",
           tabsetPanel(id = "inTabset",
           tabPanel("Table",
                    p("This is tab 1"), 
                    tags$div(class="right down",actionButton("tab1", "Next"))
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
                
                tags$h2("Prediction variables"),
                radioButtons("human", label = "Is it a human source?", choices = list("Yes" = 1, "No" = 2), 
                             selected = 1, inline = TRUE),
                ##choices animals.
                
                ##grafica: aged+diluted, point source, etc
                
                radioButtons("mollecular", label = "Do you want to use only mollecular variables?", choices = list("Yes" = 1, "No" = 2), 
                             selected = 1, inline = TRUE),
                tags$div(class="left down",actionButton("tab21", "Previous")),
                tags$div(class="right down",actionButton("tab22", "Next"))
              
           ),

           tabPanel("Modelling",
                    p("This is tab 3"),
                    tags$div(class="left down",actionButton("tab31", "Previous")),
                    tags$div(class="right down", actionButton("tab32", "Next"))
           ),
           
           tabPanel("Results",p("This is tab 4"),
                    
           tags$div(class="left down", actionButton("tab41", "Previous"))


           )
           ),
            theme = "custom-bootstrap.css",
           

            tags$footer(class="web", tags$p(class="right", HTML("Developed by Ricard Meyerhofer Parra")))
          
)

