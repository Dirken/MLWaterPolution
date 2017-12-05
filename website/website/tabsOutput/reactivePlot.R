################################
# Reactive plot
################################
observeEvent(input$filename, {
  output$plot <- reactive({
    document <- read.csv(file.path(root, input$folder.name,input$filename), sep="\t", dec = ",", header = FALSE, strip.white = TRUE)
    
    output$plot <- renderPlot({
      glmOutput <-glm(formula = document[,1] ~ document[,2],data=document,family=poisson())
      plot(glmOutput)
    }) 
  })
})