####################################
# Selecting folder and file reactive
####################################

#root <- "/home/dirken/MLWaterPolution/website/website/persist" 
root <- "C:/Users/Dirken/Downloads/UNI/MLWaterPolution/website/website/persist" 
#root <- "C:/Users/Meyerhofer/Downloads/UNI/MLWaterPolution/website/website/persist"


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