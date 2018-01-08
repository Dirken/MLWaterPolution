################################
# Reactive plot
################################

source("files/webDesign/dialog.R", local = TRUE)

observeEvent(input$filename, {
  output$uploaded <- reactive({
    
    max_plots <- length(list.files(file.path(root, colnames(Dataset$data)[input$data_cell_clicked[2]$col+1],input$filename)))
    # Insert the right number of plot output objects into the web page
    output$plot <- renderUI({
      plot_output_list <- lapply(1:max_plots, function(i) {
        
        plotname <- paste("plot", i, sep="")
        plotOutput(plotname, height = 500, width = 500)
      })
      
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list)
    })
    
    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    for (i in 1:max_plots) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        plotname <- paste("plot", my_i, sep="")
        #print(input$filename)
        arrayFiles <- list.files(file.path(root, colnames(Dataset$data)[input$data_cell_clicked[2]$col+1],input$filename))
        #print(file.path(list.dirs(file.path(root, colnames(Dataset$data)[input$data_cell_clicked[2]$col+1]))[2]),input$filename)
        print(arrayFiles[1])
        print(file.path(root, colnames(Dataset$data)[input$data_cell_clicked[2]$col+1],input$filename,arrayFiles[my_i]))
        
        document <- read.csv(file.path(root, colnames(Dataset$data)[input$data_cell_clicked[2]$col+1],input$filename,arrayFiles[my_i]),
                             sep="\t", 
                             dec = ",",
                             header = FALSE,
                             strip.white = TRUE)
        output[[plotname]] <- renderPlot({
          if (input$plotChooser == "lm"){
            aux <- lm(formula= document[,1] ~ document[,2], data = document, )
            plot(aux, which=c(1,1))
            
          }
           else{
            scatter.smooth(document)
          }
          
        })
    })
    }
    
  })
})

observeEvent(input$file2, {
  output$uploaded <- reactive({
    max_plots <- length(input$file2[['name']])
    output$plot <- renderUI({
      plot_output_list <- lapply(1:max_plots, function(i) {
        
        plotname <- paste("plot", i, sep="")
        plotOutput(plotname, height = 500, width = 500)
      })
      
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list)
    })
    
    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    for (i in 1:max_plots) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        plotname <- paste("plot", my_i, sep="")
        document[my_i] <- read.csv(input$file2$datapath,     

                             sep="\t", 
                             dec = ",",
                             header = FALSE,
                             strip.white = TRUE)
        output[[plotname]] <- renderPlot({
          # K is exactly the slope. Now K = -1/T90 = -2/T99, whichever is given
          
          if (input$plotChooser == "T99"){
            plot(-2,document[my_i][,1])
            abline(-2,document[my_i][,1])
          }
          else if (input$plotChooser == "T90"){
            plot(-1,document[my_i][,1])
            abline(-1,document[my_i][,1])
          }
          else{
            #scatter.smooth(document)
          }
          
        })
      })
    }
    
  })
})
output$uploadedFilesName <- renderText( list <- input$file2[['name']])