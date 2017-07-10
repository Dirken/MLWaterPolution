shinyUI(pageWithSidebar(
  
  # Header:
  headerPanel("TFG"),
  
  # Input in sidepanel:
  sidebarPanel(
    tags$style(type='text/css', ".well { max-width: 20em; }"),
    # Tags:
    tags$head(
      tags$style(type="text/css", "select[multiple] { width: 100%; height:10em}"),
      tags$style(type="text/css", "select { width: 100%}"),
      tags$style(type="text/css", "input { width: 19em; max-width:100%}"),
      tags$style(type="text/css", "shiny.tag.list { color: blue}"),
      #Sweet alert added:
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/limonte-sweetalert2/6.6.5/sweetalert2.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/limonte-sweetalert2/6.6.5/sweetalert2.min.css")
      
    ),
    
    # Select filetype:
    selectInput("readFunction", "Function to read data:", c(
      # Base R:
      "read.table",
      "read.csv",
      "read.csv2",
      "read.delim",
      "read.delim2",
      
      # foreign functions:
      "read.spss",
      "read.arff",
      "read.dbf",
      "read.dta",
      "read.epiiinfo",
      "read.mtp",
      "read.octave",
      "read.ssd",
      "read.systat",
      "read.xport",
      "read_excel",
      
      # Advanced functions:
      "scan",
      "readLines"
    )),
    
    # Argument selecter:
    htmlOutput("ArgSelect"),
    
    # Argument field:
    htmlOutput("ArgText"),
    
    # Upload data:
    fileInput("file", "Upload data-file:"),
    
    
    br(),
    
    textInput("name","Dataset name:","Data"),
    
    downloadLink('downloadDump', 'Download source'),
    downloadLink('downloadSave', 'Download binary')
    
  ),
  

))