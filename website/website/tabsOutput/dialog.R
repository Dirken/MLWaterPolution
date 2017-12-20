################################
# Dialog popup
################################

observeEvent(input$show, {
  showModal(modalDialog(
    title = "Select location:",
    
    sidebarPanel(
                 #uiOutput('select.folder'),
                 uiOutput('select.file'),
                 fileInput("file2", "Load location")
                 # shinyFilesButton('files', label='File select', title='Please select a file', multiple=FALSE)
                 # shinyDirButton("dir", "Use mine", "Upload")
    ),
    mainPanel(
      withSpinner(uiOutput("plot"))

    ),
    easyClose = TRUE,
    footer = tagList(actionButton("saveModal",label = "Save"),
                     modalButton("Dismiss")),
    size = "l"
    
    
  ))
})
observeEvent(input$saveModal,{
  Dataset$data[input$data_cell_clicked[1]$row+1, input$data_cell_clicked[2]$col+1] <- input$filename
  removeModal()
})