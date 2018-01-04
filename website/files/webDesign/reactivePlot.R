 ################################
# Reactive plot
################################
 
 source("files/webDesign/dialog.R", local = TRUE)
 source("files/webDesign/gridPlot.R", local = TRUE)
 
 ggplotRegression <- function (fit) {
   
   require(ggplot2)
   
   ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
     geom_point() +
     stat_smooth(method = "lm", col = "red") +
     labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                        "Intercept =",signif(fit$coef[[1]],5 ),
                        " Slope =",signif(fit$coef[[2]], 5),
                        " P =",signif(summary(fit)$coef[2,4], 5)))
 }
 
 observeEvent(input$filename, {
   output$plot <- reactive({
     
     max_plots <- length(list.files(file.path(root, colnames(Dataset$data)[input$data_cell_clicked[2]$col+1],input$filename)))
     # Insert the right number of plot output objects into the web page
     output$plot <- renderUI({
       plot_output_list <- lapply(1:max_plots, function(i) {
         plotname <- paste("plot", i, sep="")
         plotOutput(plotname, height = 300, width = 300)
       })
       multiplot(plot_output_list)
     })
    
     
   })
 })