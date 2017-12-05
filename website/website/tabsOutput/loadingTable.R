################################
# InsertRow
################################
source("tabsOutput/insertRow.R", local = TRUE)

################################
# Aux loading table
################################
source("tabsOutput/auxLoadingTable.R", local = TRUE)

################################
# File Loading
################################

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