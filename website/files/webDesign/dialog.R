################################
# Dialog popup
################################

observeEvent(input$show, {
  showModal(modalDialog(
    title = "Select location:",
    
    sidebarPanel(
      
                 prettyRadioButtons(inputId = "plotChooser",
                         label = "Which plot would you like?",
                         choices = c("lm","scatter"), icon = icon("check"), inline = TRUE,
                         status = "primary", fill = TRUE,  bigger = TRUE, animation = "jelly"),
                 #uiOutput('select.folder'),
                 uiOutput('select.file'),
                 prettyRadioButtons(inputId = "locationChooser",
                                    label = "Location data",
                                    choices = c("T99","T90", "(2 - log10(k))/T", "Plain Rehearsal"), icon = icon("check"), inline = TRUE,
                                    status = "primary", fill = TRUE,  bigger = TRUE, animation = "jelly"),
                 HTML("<p style='font-size: 10px;'>To upload multiple files, press shift button"),
                 
                 fileInput("file2", "Load location", multiple = TRUE),
                 
                 HTML("<p style='font-weight: bold;'>List of files uploaded:"),
                 uiOutput("uploadedFilesName")
                 # shinyFilesButton('files', label='File select', title='Please select a file', multiple=FALSE)
                 # shinyDirButton("dir", "Use mine", "Upload")
    ),
    mainPanel(
      withSpinner(uiOutput("plot")),
      p("Uploaded data"),
      withSpinner(uiOutput("uploaded"))
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

