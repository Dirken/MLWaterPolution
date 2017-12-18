
shinyServer(function(input, output,session) {

  
  
  Dataset <- reactiveValues()
  # Loading table
  source("tabsOutput/loadingTable.R",local=TRUE)
  
  # Dialog popup
  source("tabsOutput/dialog.R",local=TRUE)
  
  # Reactive plot
  source("tabsOutput/reactivePlot.R", local = TRUE)

  # Selecting folder and file reactive
  source("tabsOutput/folderfileReactive.R", local = TRUE)
  
  # Rendering table
  source("tabsOutput/table.R", local = TRUE)
  
  # Menu shinydashboard
  source("tabsOutput/menu.R", local = TRUE)


  
})


