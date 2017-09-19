
options("googlesheets.webapp.client_id" = "209147630905-o6v21gjukcmcru6mt6lnuhffkj0i3lce.apps.googleusercontent.com")
options("googlesheets.webapp.client_secret" = "a9Bj82HkTprostjMfoN0-EUr")
options("googlesheets.webapp.redirect_uri" = "http://127.0.0.1:4642")



gap_ss <- gs_gap()

shinyServer(function(input, output,session) {

  output$gap_ss_info <- renderPrint(print(gap_ss))
  
  worksheet <- reactive({
    input$ws
  })
  
  observeEvent(input$tab1, {
    updateTabsetPanel(session, "inTabset", selected = "Scenario")
  })
  observeEvent(input$tab21, {
    updateTabsetPanel(session, "inTabset", selected = "Table")
  })
  observeEvent(input$tab22, {
    updateTabsetPanel(session, "inTabset", selected = "Modelling")
  })
  observeEvent(input$tab31, {
    updateTabsetPanel(session, "inTabset", selected = "Scenario")
  })
  observeEvent(input$tab32, {
    updateTabsetPanel(session, "inTabset", selected = "Results")
  })
  
  observeEvent(input$tab41, {
    updateTabsetPanel(session, "inTabset", selected = "Modelling")
  })
  
  ### Argument names:
  ArgNames <- reactive({
    Names <- names(formals(input$readFunction)[-1])
    Names <- Names[Names!="..."]
    return(Names)
  })
  
  # Argument selector:
  output$ArgSelect <- renderUI({
    if (length(ArgNames())==0) return(NULL)
    selectInput("arg","Argument:",ArgNames())
  })
  
  
  ## Arg text field:
  output$ArgText <- renderUI({
    fun__arg <- paste0(input$readFunction,"__",input$arg)
    
    if (is.null(input$arg)) return(NULL)
    
    Defaults <- formals(input$readFunction)
    
    if (is.null(input[[fun__arg]]))
    {
      textInput(fun__arg, label = "Enter value:", value = deparse(Defaults[[input$arg]])) 
    } else {
      textInput(fun__arg, label = "Enter value:", value = input[[fun__arg]]) 
    }
  })
  
  output$columnOut <- renderUI(
    selectizeInput("columnOuput", "Display", colnames(Dataset()), selected = colnames(Dataset()), multiple = TRUE)
    )
  
  ### Data import:
  Dataset <- reactive({
    if (is.null(input$file)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    args <- grep(paste0("^",input$readFunction,"__"), names(input), value = TRUE)
    argList <- list()
    for (i in seq_along(args))
    {
      argList[[i]] <- eval(parse(text=input[[args[i]]]))
    }
    names(argList) <- gsub(paste0("^",input$readFunction,"__"),"",args)
    
    argList <- argList[names(argList) %in% ArgNames()]
    
    Dataset <- as.data.frame(do.call(input$readFunction,c(list(input$file$datapath),argList)))
    return(Dataset)
  })
  
  
  output$downloadData <- downloadHandler(
      filename = function() {
        paste('data', '.csv', sep='')
      },
      content = function(con) {
        write.csv(Dataset(), con)
      }
  )
  
  




  output$hot <-renderRHandsontable({rhandsontable(Dataset(),height = 600)%>%   
      hot_table( columnSorting = TRUE,highlightCol = TRUE, highlightRow = TRUE, search = TRUE)

  })

  output$seasonPlot <- renderPlot({
    
    x    <- faithful$waiting
    diluted <- seq(min(x), max(x), length.out = input$diluted + 1)
    
    hist(x, breaks = diluted, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
  
  
  
  
  ## Make a button to link to Google auth screen
  ## If auth_code is returned then don't show login button
  output$loginButton <- renderUI({
    if (is.null(isolate(access_token()))) {
      tags$a("Authorize App",
             href = gs_webapp_auth_url(),
             class = "btn btn-default")
    } else {
      return()
    }
  })
  
  output$logoutButton <- renderUI({
    if (!is.null(access_token())) {
      # Revoke the token too? use access_token$revoke()
      tags$a("Logout",
             href = getOption("googlesheets.webapp.redirect_uri"),
             class = "btn btn-default")
    } else {
      return()
    }
  })
  
  ## Get auth code from return URL
  access_token  <- reactive({
    ## gets all the parameters in the URL. The auth code should be one of them.
    pars <- parseQueryString(session$clientData$url_search)
    
    if (length(pars$code) > 0) {
      ## extract the authorization code
      gs_webapp_get_token(auth_code = pars$code)
    } else {
      NULL
    }
  })
  
  gsLs <- reactive({
    gs_ls()
  })
  
  output$listSheets <- DT::renderDataTable({
    validate(
      need(!is.null(access_token()),
           message =
             paste("Click 'Authorize App' to redirect to a Google page where",
                   "you will authenticate yourself and authorize",
                   "this app to access your Google Sheets and Google Drive."))
    )
    
    dat <- gsLs() %>% select(1:6)
    DT::datatable(dat)
  })
  
  # Sheet selector
  output$selectSheet <- renderUI({
    validate(
      need(!is.null(access_token()), message = FALSE)
    )
    
    all_sheets <- gsLs()
    titles <- c(" ", all_sheets$sheet_title) %>%
      stringr::str_sort() %>%
      as.list()
    
    selectInput("selectSheet", label = "Pick a Spreadsheet",
                choices = titles, selected = NULL)
  })
  
  ## Worksheet selector
  output$selectWs <- renderUI({
    ss <- sheet()
    # default selected would be " "
    ws_titles <- c(" ", ss$ws$ws_title %>% as.list())
    
    selectInput("selectWs", label = "Select a worksheet",
                choices = ws_titles, selected = NULL)
  })
  
  sheet <- reactive({
    validate(
      need(!is.null(access_token()), message = FALSE),
      need(input$selectSheet != " ", message = FALSE)
    )
    
    ss <- gs_title(input$selectSheet)
    ss
  })
  
  output$sheetInfo <- DT::renderDataTable({
    ss <- sheet()
    
    ss_info <- data_frame(
      x = c("Spreadsheet title",
            "Spreadsheet author",
            "Date of googlesheets registration",
            "Date of last spreadsheet update",
            "Visibility", "Permissions", "Version"),
      y = c(ss$sheet_title, ss$author,
            ss$reg_date %>% format.POSIXct(usetz = TRUE) %>% as.character(),
            ss$updated %>% format.POSIXct(usetz = TRUE) %>% as.character(),
            ss$visibility, ss$perm, ss$version))
    
    DT::datatable(ss_info, rownames = FALSE,
                  colnames = c("", ""),
                  filter = "none",
                  options = list(paging = FALSE, searching = FALSE,
                                 ordering = FALSE, info = FALSE))
  })
  
  output$sheetWsInfo <- DT::renderDataTable({
    ss <- sheet()
    
    ss_ws_info <- data_frame("Worksheet" = ss$ws$ws_title,
                             "Row Extent" = ss$ws$row_extent,
                             "Column Extent" = ss$ws$col_extent)
    
    DT::datatable(ss_ws_info, options = list(paging = FALSE))
  })
  
  user_info <- reactive({
    validate(
      need(!is.null(access_token()), message = FALSE)
    )
    gs_user()
  })
  
  output$currentUser <- renderUI({
    validate(
      need(!is.null(access_token()),
           message = "No user is currently authorized.")
    )
    
    x <- user_info()
    line1 <- paste("displayName:", x$displayName)
    line2 <- paste("emailAddress:", x$emailAddress)
    line3 <- paste("date:", format(x$date, usetz = TRUE))
    
    HTML(paste(line1, line2, line3, sep = "</br><h>"))
  })
  
  
  
  output$greeting <- renderText({
    paste("Hello,  ", user_info()$displayName, ":)", sep = "\n")
  })
  
  output$plotSheet <- renderPlot({
    validate(
      need(input$selectWs != " ", message = FALSE)
    )
    
    ss <- sheet()
    ws <- gs_read_csv(ss, input$selectWs)
    gs_inspect(ws)
  })
  
  ## Update tab panel when sheet is selected
  observe({
    if (is.null(input$selectSheet)) {
      updateTabsetPanel(session, "panel", selected = "All Sheets")
    } else {
      if (input$selectSheet != " ") {
        updateTabsetPanel(session, "panel", selected = "Sheet Info")
      } else {
        updateTabsetPanel(session, "panel", selected = "All Sheets")
      }
    }
  })
  
})
  


