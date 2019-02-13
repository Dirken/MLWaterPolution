#### Components such as sliders, download buttons, and tables from the UI 

#output$matrixSizeSlider <- renderUI({sliderInput("matrixSize", 
#                              label = "Matrix Size",  
#                              min = 0, max = 100000, 
#                              value = 1000, step = 1000)})

#output$downloadRangeData <- downloadHandler(
#  filename = function() { paste(input$dataset, '_', values$first, '_', values$last, '_Range.csv', sep='') },
#  content = function(file) {
#    write.csv(datasetInput()[values$first:values$last,], file)
#  }
#)