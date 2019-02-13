
shinyServer(function(input, output,session) {

  Dataset <- reactiveValues()
  
  ###############################################
  # Website Design
  ###############################################
  # Loading table
  source("files/webDesign/loadingTable.R",local=TRUE)
  
  # Dialog popup
  source("files/webDesign/dialog.R",local=TRUE)
  
  # Reactive plot
  source("files/webDesign/reactivePlot.R", local = TRUE)

  # Selecting folder and file reactive
  source("files/webDesign/folderfileReactive.R", local = TRUE)
  
  # Rendering table
  source("files/webDesign/table.R", local = TRUE)
  
  # Menu shinydashboard
  source("files/webDesign/menu.R", local = TRUE)
  
  # Visualization part
  source("files/webDesign/visualization.R", local = TRUE)
  
  #Sliders, result tables.
  source("files/webDesign/components.R", local = TRUE)
  
  
  ###############################################
  # Machine Learning
  ###############################################
  #ML part
  # source("files/Analisi-lin.R", local = TRUE)
  
})


