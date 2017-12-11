################################
# InsertRow
################################
source("tabsOutput/insertRow.R", local = TRUE)

################################
# Aux loading table
################################
source("tabsOutput/auxLoadingTable.R", local = TRUE)

################################
# Menu creation 
################################
source("tabsOutput/menu.R", local = TRUE)

################################
# File Loading
################################

### Data import:
observeEvent(input$file,{
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
  
  Dataset$data <- as.data.frame(do.call(input$readFunction,c(list(input$file$datapath),argList)))
  #id <<- showNotification(paste("Succesfully loaded"), duration = 5, type="message")
  newrow <- "Undefined season"
  if(input$model != TRUE){
    print(input$model)
    newrow <- "Undefined season"
    Dataset$data <- insertRow2(Dataset$data, newrow ,1)
  }
  newrow <- Dataset$data[1:1,]
  newrow <- shinyInput(actionButton, length(newrow), 'button_', label = "Season", onclick = 'Shiny.onInputChange(\"show\",  this.id)' )
  as.data.frame(lapply(Dataset$data, as.numeric))
  
  Dataset$data <-insertRow2(Dataset$data, newrow ,1)
  # Dataset <- sapply(Dataset(), 
  #                    function(x){
  #                      if(is.numeric(x)) {scientific(as.numeric(x)) }
  #                      else if (is.integer(x)){ 
  #                      }
  #                    }
  #             )

})