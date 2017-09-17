# output$contents <- renderTable({
#   inFile <- input$file1
#   
#   if(is.null(inFile))
#     return(NULL)
#   
#   ext <- tools::file_ext(inFile$name)
#   file.rename(inFile$datapath,
#               paste(inFile$datapath, ext, sep="."))
#   read_excel(paste(inFile$datapath, ext, sep="."), 1)
# })




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
    return(Dataset)
  })
  
  
  output$hot <-renderRHandsontable({rhandsontable(Dataset(),height = 600)%>%   
      hot_table( columnSorting = TRUE,highlightCol = TRUE, highlightRow = TRUE) %>% 
    hot_context_menu(
      customOpts = 
      list(
        csv = list(name = "Download to CSV",
                   callback = htmlwidgets::JS(
                     "function (key, options) {
                     var csv = csvString(this);
                     
                     var link = document.createElement('a');
                     link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                     encodeURIComponent(csv));
                     link.setAttribute('download', 'data.csv');
                     
                     document.body.appendChild(link);
                     link.click();
                     document.body.removeChild(link);
    }")),
        search = list(name = "Search",
                      callback = htmlwidgets::JS(
                        "function (key, options) {
                         var srch = prompt('Search criteria');

                         this.search.query(srch);
                         this.render();
                       }"))
        )
      )})

    
  
})
  
