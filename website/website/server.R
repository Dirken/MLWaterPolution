
shinyServer(function(input, output,session) {

  Dataset <- reactiveValues()
  # Loading table
  source("files/loadingTable.R",local=TRUE)
  
  # Dialog popup
  source("files/dialog.R",local=TRUE)
  
  # Reactive plot
  source("files/reactivePlot.R", local = TRUE)

  # Selecting folder and file reactive
  source("files/folderfileReactive.R", local = TRUE)
  
  # Rendering table
  source("files/table.R", local = TRUE)
  
  # Menu shinydashboard
  source("files/menu.R", local = TRUE)

})


