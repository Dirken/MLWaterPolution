################################
# Dialog popup
################################

observeEvent(input$show, {
  showModal(modalDialog(
    title = "Select location:",
    
    sidebarPanel(
                 prettyRadioButtons(inputId = "choosePlot",
                         label = "Which plot would you like?",
                         choices = c("lm","scatter"), icon = icon("check"), inline = TRUE,
                         status = "primary", fill = TRUE, animation = "jelly"),
                 prettyRadioButtons(inputId = "Id039",
                                    label = "Choose:", choices = c("Click me !",
                                                                   "Me !", "Or me !"), icon = icon("check"),
                                    bigger = TRUE, status = "info",
                                    animation = "jelly"),
                 #uiOutput('select.folder'),
                 uiOutput('select.file'),
                 prettyRadioButtons(inputId = "location",
                                    label = "Location data",
                                    choices = c("T99","T90", "Plain"), icon = icon("check"), inline = TRUE,
                                    status = "primary", fill = TRUE, animation = "jelly"),
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

