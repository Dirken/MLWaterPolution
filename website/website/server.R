
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
    id <<- showNotification(paste("Succesfully loaded"), duration = 5, type="message")
    
    
    newrow <- Dataset[1:1,] 
    newrow <-    HTML("<button type='button' class='btn btn-info btn-lg' data-toggle='modal' data-target='#dataModal'>Open Modal</button>")
    dataModal <- function(failed = FALSE) {
      modalDialog(
        textInput("dataset", "Choose data set",
                  placeholder = 'Try "mtcars" or "abc"'
        ),
        span('(Try the name of a valid data object like "mtcars", ',
             'then a name of a non-existent object like "abc")'),
        if (failed)
          div(tags$b("Invalid name of data object", style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok", "OK")
        )
      )
    }
    return(insertRow2(Dataset, newrow ,1))
  })
  
  insertRow2 <- function(existingDF, newrow, r) {
    existingDF <- rbind(existingDF,newrow)
    existingDF <- existingDF[order(c(1:(nrow(existingDF)-1),r-0.5)),]
    row.names(existingDF) <- 1:nrow(existingDF)
    return(existingDF)  
  }
  

  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data', '.csv', sep='')
    },
    content = function(con) {
      write.csv(Dataset(), con)
      id <<- showNotification(paste("Downloaded data"), duration = 5,  type="message")
    }  
    

  )
  
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "Important message",
      "This is an important message!",
      easyClose = TRUE
    ))
  })
  
  
  
  
  
  output$hot <-renderRHandsontable({rhandsontable(Dataset(),height = 600)%>%   
      hot_table( columnSorting = TRUE,highlightCol = TRUE, highlightRow = TRUE, search = TRUE)%>%
      hot_rows(fixedRowsTop = 1)%>%
      hot_cols(renderer = "html") %>%
      hot_cols(renderer = htmlwidgets::JS("safeHtmlRenderer"))
      #hot_col(1, colWidths = 0.1)
      
    

  })
  
  

  
})


