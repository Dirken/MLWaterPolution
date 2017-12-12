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


# # dir
# shinyDirChoose(input, 'dir', roots = c(home = "C:/Users/Meyerhofer/Downloads/UNI/MLWaterPolution/website/website/persist"), filetypes = c('', 'csv'))
# dir <- reactive(input$dir)
# output$dir <- renderPrint(dir())
# 
# # path
# path <- reactive({
#   home <- normalizePath("~")
#   file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
# })
# 
# # files
# output$files <- renderPrint(list.files(path()))