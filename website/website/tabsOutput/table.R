output$data <- DT::renderDataTable(
  Dataset(), server = FALSE, escape = FALSE,
  callback=JS(' $(".dt-button").css("background","#3c8dbc");
              $(".dt-button").css("color","white");
              return table;'),
  extensions = "Buttons",selection = list(target = 'cell'),
  filter = list(position = 'top', clear = FALSE),
  options = list(
    scrollX='auto',
    scrollY='400px',
    paging = FALSE,
    regex = TRUE,
    searchHighlight = TRUE,
    search = list(regex = TRUE),
    columnDefs = list(list(className = 'dt-center', targets = 5)),
    
    #pageLength = 5,
    #lengthMenu = c(5, 10, 15, 20),
    dom = 'Bfrtip',
    buttons = c('csv', 'excel', I('colvis'))
  ))

#outputOptions(output, 'data', suspendWhenHidden = FALSE)