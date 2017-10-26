
shinyServer(function(input, output,session) {
  
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
    

    return(insertRow2(Dataset, newrow ,1))
  })
  
  insertRow2 <- function(existingDF, newrow, r) {
    existingDF <- rbind(existingDF,newrow)
    existingDF <- existingDF[order(c(1:(nrow(existingDF)-1),r-0.5)),]
    row.names(existingDF) <- 1:nrow(existingDF)
    return(existingDF)
  }
  

  
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "Select location:",
      sidebarPanel(uiOutput('select.folder'),
                   uiOutput('select.file')
                   ),
      mainPanel(plotOutput("plot"),
                footer = actionButton("dismissModal",label = "Dismiss")
                )
      
      
    ))
  })
  
  observeEvent(input$filename, {
        output$plot <- reactive({
      document <- read.csv(file.path(root, input$folder.name,input$filename), sep="\t", dec = ",", header = FALSE, strip.white = TRUE)
  
      output$plot <- renderPlot({
        glmOutput <-glm(formula = document[,1] ~ document[,2],data=document,family=poisson())
        plot(glmOutput)
      }) 
    })
  })



  
  #root <- "/home/dirken/MLWaterPolution/website/website/persist" 
  root <- "C:/Users/Meyerhofer/Downloads/UNI/MLWaterPolution/website/website/persist" 
  
  
  output$select.folder <-renderUI(selectInput(inputId = "folder.name",
                                              label = 'Location',
                                              choices = list.files(path = file.path(root))))
  

                         
  #filesInFolder <- grep(Dataset()[input$data_cell_clicked[2]$col], filesInFolder)
  output$select.file <-
    renderUI(selectInput("filename",
                                label = 'Gradient',
                                choices = list.files(path = file.path(root, input$folder.name))))

  
  output$data <- DT::renderDataTable(
    Dataset(), server = FALSE, escape = FALSE,extensions = "Buttons",selection = list(target = 'cell'),
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

                     
  
})


