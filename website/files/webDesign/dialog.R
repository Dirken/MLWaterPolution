################################
# Dialog popup
################################

observeEvent(input$show, {
  showModal(modalDialog(
    title = "Select location:",
    
    sidebarPanel(
      prettyRadioButtons(inputId = "alreadyLoaded",
                         label = "Do you want to only consider the uploaded file?",
                         choices = c("Yes","No"), icon = icon("check"), inline = TRUE,
                         status = "primary", fill = TRUE,  bigger = TRUE, animation = "jelly", selected = "No"),
      conditionalPanel(condition = "input.alreadyLoaded == 'No'",
                 prettyRadioButtons(inputId = "plotChooser",
                         label = "Which plot would you like?",
                         choices = c("lm","scatter"), icon = icon("check"), inline = TRUE,
                         status = "primary", fill = TRUE,  bigger = TRUE, animation = "jelly"),
                 #uiOutput('select.folder'),
                 uiOutput('select.file')
      ),
       prettyRadioButtons(inputId = "locationChooser",
                          label = "Slope function",
                          choices = c("T99","T90"), icon = icon("check"), inline = TRUE,
                          status = "primary", fill = TRUE,  bigger = TRUE, animation = "jelly"),
       numericInput(inputId = "slope", label = "Insert your slope:", value = 0, step = 0.01),
       uiOutput("finalFunction"),
       div(class="right",
        actionBttn("submit","Add season",color = "primary", style = "jelly", icon = icon("plus"))
       ), br(),br()
    ),
    mainPanel(
      conditionalPanel(condition = "input.alreadyLoaded == 'No'",
        withSpinner(uiOutput("plot"))
      ),
      HTML("<p style='font-weight: bold;'>Uploaded data</p>"),
      withSpinner(plotOutput("uploaded"))
    ),
    easyClose = TRUE,
    footer = tagList(actionButton("saveModal",label = "Save"),
                     modalButton("Dismiss")),
    size = "l"
    
    
  ))
})
observeEvent(input$saveModal,{
  #hauria de guardar el slope. Per altra part, quan introdueixo les dades, hauria de guardar l'slope tal qual i no ho faig atm...
  conditionalPanel(condition = "input.alreadyLoaded == 'Yes'",
    Dataset$data[input$data_cell_clicked[1]$row+1, input$data_cell_clicked[2]$col+1] <- input$filename 
  )
  conditionalPanel(condition = "input.alreadyLoaded == 'No'",
                   Dataset$data[input$data_cell_clicked[1]$row+1, input$data_cell_clicked[2]$col+1] <- "Custom input" 
  )
  
  removeModal()
})

