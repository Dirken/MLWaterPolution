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
      plotOutput("plot"),
      auxRow <- input$data_cell_clicked[1]$row+1,
      auxCol <-input$data_cell_clicked[2]$col
      
    ),
    easyClose = TRUE,
    footer = tagList(actionButton("saveModal",label = "Save"),
                     modalButton("Dismiss"))
    
    
  ))
})
observeEvent(input$saveModal,{
  Dataset()[auxRow+1, auxCol] <- NULL
})