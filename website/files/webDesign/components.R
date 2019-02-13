#### Components such as sliders from the UI 

output$matrixSizeSlider <- renderUI({sliderInput("matrixSize", 
                              label = "Matrix Size",  
                              min = 0, max = 100000, 
                              value = 1000, step = 1000)})

