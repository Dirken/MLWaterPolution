################################
# Dialog popup
################################

observeEvent(input$show, {
  showModal(modalDialog(
    title = "Select location:",
    
    sidebarPanel(uiOutput('select.folder'),
                 uiOutput('select.file')
    ),
    mainPanel(
      withSpinner(plotOutput("plot"))

    ),
    easyClose = TRUE,
    footer = tagList(actionButton("saveModal",label = "Save"),
                     modalButton("Dismiss"))
    
    
  ))
})
observeEvent(input$saveModal,{
  Dataset$data[input$data_cell_clicked[1]$row+1, input$data_cell_clicked[2]$col] <- input$filename
  removeModal()
})