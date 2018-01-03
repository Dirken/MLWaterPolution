
shinyServer(function(input, output,session) {

  Dataset <- reactiveValues()
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
  
  
  observeEvent(input$generateMatrix,{
    updateTabItems(session, "tab", selected = "tab2")
  })
  
  #ML part
  # source("files/Analisi-lin.R", local = TRUE)
  
})

