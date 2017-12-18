####################################
# Selecting folder and file reactive
####################################

source("tabsOutput/dialog.R", local = TRUE)
#root <- "/home/dirken/MLWaterPolution/website/website/persist" 
root <- "C:/Users/Dirken/Downloads/UNI/MLWaterPolution/website/website/new-persist" 
#root <- "C:/Users/Meyerhofer/Downloads/UNI/MLWaterPolution/website/website/persist"

observe({
  root <- paste(root, Dataset$data[0, input$data_cell_clicked[2]$col+1], sep = "/")
  print(root)
})
output$select.folder <-renderUI(selectInput(inputId = "folder.name",
                                            label = 'Location',
                                            choices = list.files(path = file.path(root))))



#filesInFolder <- grep(Dataset()[input$data_cell_clicked[2]$col], filesInFolder)
output$select.file <-
  renderUI(selectInput("filename",
                       label = 'Gradient',
                       choices = list.files(path = file.path(root, input$folder.name))))




# seasonLoaded <- input$file2
# 
# seasonLoaded