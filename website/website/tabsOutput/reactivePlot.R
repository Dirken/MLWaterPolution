 ################################
# Reactive plot
################################
observeEvent(input$filename, {
  output$plot <- reactive({
    print("dins reactiveplot")
    for (i in 2:length(list.dirs(file.path(root, colnames(Dataset$data)[input$data_cell_clicked[2]$col+1])))){
        
      document <- read.csv(file.path(root, colnames(Dataset$data)[input$data_cell_clicked[2]$col+1])[i],
                           sep="\t", 
                           dec = ",",
                           header = FALSE,
                           strip.white = TRUE)
      output$plot <- renderPlot({
        glmOutput <- glm(formula = document[,1] ~ document[,2],data=document,family=poisson())
        plot(glmOutput)
      })
      
    }
    print("----")
     
  })
})
 
