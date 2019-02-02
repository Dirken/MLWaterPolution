####################################
# Selecting folder and file reactive
####################################

source("files/webDesign/dialog.R", local = TRUE)
#root <- "/home/dirken/MLWaterPolution/website/website/persist" 
#root <- "C:/Users/Dirken/Downloads/UNI/MLWaterPolution/website/new-persist"
root <- "C:/Users/Meyerhofer/Desktop/MLWaterPolution/website/new-persist"



#filesInFolder <- grep(Dataset()[input$data_cell_clicked[2]$col], filesInFolder)
output$select.file <-
  renderUI(selectInput("filename",
                       label = 'Location',
                       choices = list.files(path = file.path(root, colnames(Dataset$data)[input$data_cell_clicked[2]$col+1]))))

