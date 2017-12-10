############################################
# Semi collapsible shiny
############################################

vals<-reactiveValues()
vals$collapsed=FALSE
observeEvent(input$SideBar_col_react,
             {
               vals$collapsed=!vals$collapsed
             }
)

output$Semi_collapsible_sidebar<-renderMenu({
  # if (vals$collapsed){
  #   sidebarMenu(id = "tab",
  #               menuItem(NULL, tabName = "tab1",icon = icon("table")),
  #               menuItem(NULL, tabName = "tab2", icon = icon("line-chart")),
  #               menuItem(NULL, tabName = "tab3",icon = icon("eye")),
  #               menuItem(NULL, tabName = "tab4", icon = icon("info-circle")),
  #               menuItem(NULL, tabName = "tab5",icon = icon("question"))
  #               
  #   )
  # }
  # else{
  sidebarMenu(id = "tab",
              menuItem("Table", tabName = "tab1",icon = icon("table")),
              menuItem("Scenario", tabName = "tab22", icon = icon("gears")),
              menuItem("Modeling", tabName = "tab2", icon = icon("line-chart")),
              menuItem("Visualization", tabName = "tab3",icon = icon("eye")),
              menuItem("About", tabName = "tab4", icon = icon("info-circle")),
              menuItem("Help", tabName = "tab5",icon = icon("question"))
              ,
              conditionalPanel("input.tab == 'tab1'",
                               materialSwitch(inputId = "id", label = "Do you have a model?", status = "primary"),
                               conditionalPanel(condition = "input.id == 0",
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
                               ),
                               conditionalPanel(condition = "input.id != 0",
                                                p("carregar model"),
                                                selectInput("readFunction2", "Format to read", c(
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
                                                htmlOutput("ArgSelect2"),
                                                
                                                # Argument field:
                                                htmlOutput("ArgText2"),
                                                
                                                # Upload data:
                                                fileInput("file2", "Browse file"),
                                                htmlOutput('response22')
                               )
                )
              
  )
  # }
  
  
})