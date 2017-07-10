library(shinydashboard)
library(leaflet)

header <- dashboardHeader(
  title = "UB",
  dropdownMenu(type = "messages",
               messageItem(
                 from = "Sales Dept",
                 message = "Sales are steady this month."
               ),
               messageItem(
                 from = "New User",
                 message = "How do I register?",
                 icon = icon("question"),
                 time = "13:45"
               ),
               messageItem(
                 from = "Support",
                 message = "The new server is ready.",
                 icon = icon("life-ring"),
                 time = "2014-12-01"
               )
  )

)

sidebar <- dashboardSidebar(
  sidebarMenu(id = "tab",
    menuItem("Visualization", tabName = "tab1", icon = icon("dashboard")),
    menuItem("Table", icon = icon("table"), tabName = "tab2"),
    menuItem("About", tabName = "tab3", icon = icon("info-circle")),
    menuItem("Logout", tabName = "tab4", icon = icon("sign-out"))
  )
  
)

dashboard <- dashboardBody(
  tabItems(
    tabItem(
      fluidRow(
        tabBox(
          title = "First tabBox",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1", height = 500, width = 9,
          tabPanel("Tab1", "First tab content"),
          tabPanel("Tab2", "Tab content 2")
        ),
        column(width = 3,
               box(width = NULL, status = "warning",
                   uiOutput("routeSelect"),
                   checkboxGroupInput("directions", "Show",
                                      choices = c(
                                      ),
                   ),
                   p(
                     class = "text-muted",
                     paste("Note: Comment"
                     )
                   ),
                   actionButton("zoomButton", "Zoom to fit buses")
               ),
               box(width = NULL, status = "warning",
                   selectInput("interval", "Refresh interval",
                               choices = c(
                               ),
                               selected = "60"
                   ),
                   uiOutput("timeSinceLastUpdate"),
                   actionButton("refresh", "Refresh now"),
                   p(class = "text-muted",
                     br(),
                     "Source "
                   )
               )
        )
      ),
      tabItem(
        table <- fluidRow(
          tabBox(
            title = "First tabBox",
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "tabset1", height = 500, width = 9,
            tabPanel("Tab1", "First tab content"),
            tabPanel("Tab2", "Tab content 2")
          )
        )
      )
    )
  )
)



ui <- dashboardPage(
  header,
  sidebar,
  dashboard
  
)