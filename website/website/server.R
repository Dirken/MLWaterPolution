
auxRow <- NULL
auxCol <- NULL
shinyServer(function(input, output,session) {
  
  
  ################################
  # Back and forward buttons
  ################################
  
  observeEvent(input$tab1, {
    updateTabsetPanel(session, "inTabset", selected = "Scenario")
  })
  observeEvent(input$tab21, {
    updateTabsetPanel(session, "inTabset", selected = "Table")
  })
  observeEvent(input$tab22, {
    updateTabsetPanel(session, "inTabset", selected = "Modelling")
  })
  observeEvent(input$tab31, {
    updateTabsetPanel(session, "inTabset", selected = "Scenario")
  })
  observeEvent(input$tab32, {
    updateTabsetPanel(session, "inTabset", selected = "Results")
  })
  
  observeEvent(input$tab41, {
    updateTabsetPanel(session, "inTabset", selected = "Modelling")
  })
  
  ################################
  # File Loading
  ################################
  
  ### Argument names:
  ArgNames <- reactive({
    Names <- names(formals(input$readFunction)[-1])
    Names <- Names[Names!="..."]
    return(Names)
  })
  
  # Argument selector:
  output$ArgSelect <- renderUI({
    if (length(ArgNames())==0) return(NULL)
    selectInput("arg","Argument:",ArgNames())
  })
  
  
  ## Arg text field:
  output$ArgText <- renderUI({
    fun__arg <- paste0(input$readFunction,"__",input$arg)
    
    if (is.null(input$arg)) return(NULL)
    
    Defaults <- formals(input$readFunction)
    
    if (is.null(input[[fun__arg]]))
    {
      textInput(fun__arg, label = "Enter value:", value = deparse(Defaults[[input$arg]])) 
    } else {
      textInput(fun__arg, label = "Enter value:", value = input[[fun__arg]]) 
    }
  })
  
  output$columnOut <- renderUI(
    selectizeInput("columnOuput", "Display", colnames(Dataset()), selected = colnames(Dataset()), multiple = TRUE)
  )
  
  columnOut = reactive({
    Dataset = Dataset[colnames(Dataset())]
  })
  
  # This function will create the buttons for the datatable, they will be unique
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  Dataset <- reactiveValues()
  
  ### Data import:
  Dataset <- reactive({
    if (is.null(input$file)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    args <- grep(paste0("^",input$readFunction,"__"), names(input), value = TRUE)
    argList <- list()
    for (i in seq_along(args))
    {
      argList[[i]] <- eval(parse(text=input[[args[i]]]))
    }
    names(argList) <- gsub(paste0("^",input$readFunction,"__"),"",args)
    
    argList <- argList[names(argList) %in% ArgNames()]
    
    Dataset <- as.data.frame(do.call(input$readFunction,c(list(input$file$datapath),argList)))
    #id <<- showNotification(paste("Succesfully loaded"), duration = 5, type="message")
    newrow <- "Undefined season"
    Dataset <- insertRow2(Dataset, newrow ,1)
    newrow <- Dataset[1:1,] 
    newrow <- shinyInput(actionButton, length(newrow), 'button_', label = "Season", onclick = 'Shiny.onInputChange(\"show\",  this.id)' )
    as.data.frame(lapply(Dataset, as.numeric))
    
    Dataset <-insertRow2(Dataset, newrow ,1)
    # Dataset <- sapply(Dataset(), 
    #                    function(x){
    #                      if(is.numeric(x)) {scientific(as.numeric(x)) }
    #                      else if (is.integer(x)){ 
    #                      }
    #                    }
    #             )
    
    return(Dataset)
  })
  
  insertRow2 <- function(existingDF, newrow, r) {
    existingDF <- rbind(existingDF,newrow)
    existingDF <- existingDF[order(c(1:(nrow(existingDF)-1),r-0.5)),]
    row.names(existingDF) <- 1:nrow(existingDF)
    return(existingDF)
  }


  ################################
  # Dialog popup
  ################################
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "Select location:",
      sidebarPanel(uiOutput('select.folder'),
                   uiOutput('select.file')
                   ),
      mainPanel(
          plotOutput("plot"),
          auxRow <- input$data_cell_clicked[1]$row+1,
          auxCol <-input$data_cell_clicked[2]$col

      ),
      easyClose = TRUE,
      footer = tagList(actionButton("saveModal",label = "Save"),
                       modalButton("Dismiss"))
      
      
    ))
  })
  observeEvent(input$saveModal,{
    Dataset()[auxRow+1, auxCol] <- NULL
  })
  
  ################################
  # Reactive plot
  ################################
  
  observeEvent(input$filename, {
      output$plot <- reactive({
      document <- read.csv(file.path(root, input$folder.name,input$filename), sep="\t", dec = ",", header = FALSE, strip.white = TRUE)
  
      output$plot <- renderPlot({
        glmOutput <-glm(formula = document[,1] ~ document[,2],data=document,family=poisson())
        plot(glmOutput)
      }) 
    })
  })


  ####################################
  # Selecting folder and file reactive
  ####################################
  
  #root <- "/home/dirken/MLWaterPolution/website/website/persist" 
  root <- "C:/Users/Dirken/Downloads/UNI/MLWaterPolution/website/website/persist" 
  
  
  output$select.folder <-renderUI(selectInput(inputId = "folder.name",
                                              label = 'Location',
                                              choices = list.files(path = file.path(root))))
  

                         
  #filesInFolder <- grep(Dataset()[input$data_cell_clicked[2]$col], filesInFolder)
  output$select.file <-
    renderUI(selectInput("filename",
                                label = 'Gradient',
                                choices = list.files(path = file.path(root, input$folder.name))))

  
  
  ################################
  # Rendering table
  ################################
  
  

  
  output$data <- DT::renderDataTable(
    Dataset(), server = FALSE, escape = FALSE,
    callback=JS(' $(".dt-button").css("background","#3c8dbc");
                  $(".dt-button").css("color","white");
                  return table;'),
    extensions = "Buttons",selection = list(target = 'cell'),
    filter = list(position = 'top', clear = FALSE),
    options = list(
      scrollX='auto',
      scrollY='400px',
      paging = FALSE,
      regex = TRUE,
      searchHighlight = TRUE,
      search = list(regex = TRUE),
      columnDefs = list(list(className = 'dt-center', targets = 5)),
      
      #pageLength = 5,
      #lengthMenu = c(5, 10, 15, 20),
      dom = 'Bfrtip',
      buttons = c('csv', 'excel', I('colvis'))
    ))

  outputOptions(output, 'data', suspendWhenHidden = FALSE)
  
  
  
  
  
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
                    menuItem("Modeling", tabName = "tab2", icon = icon("line-chart")),
                    menuItem("Visualization", tabName = "tab3",icon = icon("eye")),
                    menuItem("About", tabName = "tab4", icon = icon("info-circle")),
                    menuItem("Help", tabName = "tab5",icon = icon("question"))
                    ,
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
      # }
    
    
  })
})


