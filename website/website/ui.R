library(leaflet)

navbarPage(title="Fecal Matter predictor",
           tabsetPanel(id = "inTabset",
                  tabPanel("Table",
                           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                         draggable = TRUE, left = "auto", right = 20, bottom = "auto",
                                         width = 250, height = "auto", 
                                         HTML('<button data-toggle="collapse" data-target="#demo"> <span class="glyphicon-class"></span></button>'),
                                         tags$div(id = 'demo',  class="collapse in", list(                                         
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
                                           
                                           htmlOutput("columnOut"),
                                           downloadButton('downloadData', 'Download')
                                      ))

     
                      
                      ),
                      br(),
                      
                      mainPanel(width = 12,

                               textInput("searchId", "Search", "Search"),
                               rHandsontableOutput("hot")
                      ),
                        
                    br()
                    # column(width = 12,
                    #        tags$div(
                    #          tags$div(class="center", tags$ul(class="asdf",list(
                    #            tags$li(img(class="small",src='table-active.png')),
                    #            tags$li(class="middle", img(class="big-dot", src='dot.png')),
                    #            tags$li(img(class="small",src='options.png')),
                    #            tags$li(class="middle", img(class="big-dot", src='dot.png')),
                    #            
                    #            tags$li(img(class="small",src='omg1.png')),
                    #            tags$li(class="middle", img(class="big-dot", src='dot.png')),
                    #            
                    #            tags$li(img(class="small",src='chart.png'))
                    #            
                    #          ))),
                    #          HTML("<br>"),
                    #          tags$div(class="right down",actionButton("tab1", "Next"))
                    #          
                    #        ))

           ),
           tabPanel("Scenario",
                
                tags$h2("Prediction variables"),
                radioButtons("human", label = "Is it a human source?", choices = list("Yes" = 1, "No" = 2), 
                             selected = 1, inline = TRUE),
                ##choices animals.
                
                ##grafica: aged+diluted, point source, etc
                
                radioButtons("mollecular", label = "Do you want to use only mollecular variables?", choices = list("Yes" = 1, "No" = 2), 
                             selected = 1, inline = TRUE)
                
                # 
                # tags$div(
                #   tags$div(class="left down",actionButton("tab21", "Previous")),
                #   tags$div(class="right down",actionButton("tab22", "Next")),
                #   tags$div(class="center", tags$ul(class="asdf",list(
                #     tags$li(img(class="small",src='table.png')),
                #     tags$li(class="middle", img(class="big-dot", src='dot.png')),
                #     tags$li(img(class="small",src='options-2.png')),
                #     tags$li(class="middle", img(class="big-dot", src='dot.png')),
                #     
                #     tags$li(img(class="small",src='omg1.png')),
                #     tags$li(class="middle", img(class="big-dot", src='dot.png')),
                #     
                #     tags$li(img(class="small",src='chart.png'))
                #     
                #   )))
                # )
              
           ),

           tabPanel("Modelling"
                    # tags$div(
                    #   tags$div(class="left down",actionButton("tab31", "Previous")),
                    #   tags$div(class="right down", actionButton("tab32", "Next")),
                    #   tags$div(class="center", tags$ul(class="asdf",list(
                    #     tags$li(img(class="small",src='table.png')),
                    #     tags$li(class="middle", img(class="big-dot", src='dot.png')),
                    #     tags$li(img(class="small",src='options.png')),
                    #     tags$li(class="middle", img(class="big-dot", src='dot.png')),
                    #     
                    #     tags$li(img(class="small",src='omg.png')),
                    #     tags$li(class="middle", img(class="big-dot", src='dot.png')),
                    #     
                    #     tags$li(img(class="small",src='chart.png'))
                    #     
                    #   )))
                    # )
                    
           ),
           
           tabPanel("Results",p("This is tab 4")
            #         
            # tags$div(
            #   tags$div(class="left down", actionButton("tab41", "Previous")),
            #   tags$div(class="center", tags$ul(class="asdf",list(
            #     tags$li(img(class="small",src='table.png')),
            #     tags$li(class="middle", img(class="big-dot", src='dot.png')),
            #     tags$li(img(class="small",src='options.png')),
            #     tags$li(class="middle", img(class="big-dot", src='dot.png')),
            #     
            #     tags$li(img(class="small",src='omg1.png')),
            #     tags$li(class="middle", img(class="big-dot", src='dot.png')),
            #     
            #     tags$li(img(class="small",src='chart-active.png'))
            #    
            #   )))
            # )


           )
           ),
            theme = "custom-bootstrap.css",
           
           

          #icon-table,  
          #icon-stats, icon-charts, icon-pie-chart
           
            tags$footer(class="web", tags$p(class="right", HTML("Developed by Ricard Meyerhofer Parra")))
          
)

