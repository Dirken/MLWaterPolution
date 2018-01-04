# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
# source("files/webDesign/reactivePlot.R", local = TRUE)


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




# 
# ####
# 
# # Call renderPlot for each one. Plots are only actually generated when they
# # are visible on the web page.
# for (i in 1:max_plots) {
#   # Need local so that each item gets its own number. Without it, the value
#   # of i in the renderPlot() will be the same across all instances, because
#   # of when the expression is evaluated.
#   local({
#     my_i <- i
#     plotname <- paste("plot", my_i, sep="")
#     #print(input$filename)
#     arrayFiles <- list.files(file.path(root, colnames(Dataset$data)[input$data_cell_clicked[2]$col+1],input$filename))
#     #print(file.path(list.dirs(file.path(root, colnames(Dataset$data)[input$data_cell_clicked[2]$col+1]))[2]),input$filename)
#     print(arrayFiles[1])
#     print(file.path(root, colnames(Dataset$data)[input$data_cell_clicked[2]$col+1],input$filename,arrayFiles[my_i]))
#     
#     document <- read.csv(file.path(root, colnames(Dataset$data)[input$data_cell_clicked[2]$col+1],input$filename,arrayFiles[my_i]),
#                          sep="\t", 
#                          dec = ",",
#                          header = FALSE,
#                          strip.white = TRUE)
#     output[[plotname]] <- 
#       # ggplotRegression(lm(formula = document[,1] ~ document[,2],data=document))
#       ggplotRegression(lm(Sepal.Length ~ Petal.Width, data = iris))
#     
#     
#   })
# }