
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
  selectizeInput("columnOuput", "Display", colnames(Dataset$data), selected = colnames(Dataset$data), multiple = TRUE)
)

columnOut = reactive({
  Dataset = Dataset$data[colnames(Dataset$data)]
})

# This function will create the buttons for the datatable, they will be unique
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

