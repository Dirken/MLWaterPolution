################################
# InsertRow
################################
source("files/webDesign/insertRow.R", local = TRUE)

################################
# Aux loading table
################################
source("files/webDesign/auxLoadingTable.R", local = TRUE)

################################
# Menu creation 
################################
source("files/webDesign/menu.R", local = TRUE)

################################
# File Loading
################################

### Data import:
observeEvent(input$file,{
  if (is.null(input$file)) {
    # User has not uploaded a file yet
    sendSweetAlert(
      session = session,
      title = "Error",
      text = "File is null",
      type = "error"
    )
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
  
  Dataset$data <- as.data.frame(do.call(input$readFunction,c(list(input$file$datapath),argList,  stringsAsFactors=FALSE)))
  
  Dataset$data <- sapply(Dataset$data, 
                         function(x){
                           if(is.numeric(x)) {scientific(as.numeric(x)) }
                           else if (is.integer(x)){ }
                           # else if (is.string(x)){ }
                           else{
                             x
                           }
                         }  )
  newrow <- "Undefined season"
  predictValues <<- substr(Dataset$data[,1],4,5)
  #print(Dataset$data[,1])
  #print(predictValues)
  if(input$model != TRUE){
    print(input$model)
    newrow <- "Undefined season"
    Dataset$data <- insertRow2(Dataset$data, newrow ,1)
    newrow <- Dataset$data[1:1,]
    newrow <- shinyInput(actionButton, length(newrow), 'button_', label = "Season", onclick = 'Shiny.onInputChange(\"show\",  this.id)' )
    # as.data.frame(lapply(Dataset$data, as.numeric))
    
    Dataset$data <-insertRow2(Dataset$data, newrow ,1)
  }
  else{
    Dataset$data <-  Dataset$data[-1, ]
    
    newrow <- Dataset$data[1:1,]
    newrow <- shinyInput(actionButton, length(newrow), 'button_', label = "Season", onclick = 'Shiny.onInputChange(\"show\",  this.id)' )
    
    Dataset$data <-insertRow2(Dataset$data, newrow ,1)
  }
  # validate({
  #   need(input$file,     
  #        sendSweetAlert(
  #     session = session,
  #     title = "Error!!",
  #     text = "Could not be loaded",
  #     type = "error"
  #   ))
  # })
  sendSweetAlert(
    session = session,
    title = "Success!!",
    text = "File was load successfully",
    type = "success"
  )




})