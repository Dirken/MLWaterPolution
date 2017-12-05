library(leaflet)
library(shinydashboard)
library(shinyBS)

jsCode <- 
  "shinyjs.filename =
  	function (){
  		$(document).ready(function() {
        		console.log(document.getElementById('filename').value);
	    });
    }"


dashboardPage(
  dashboardHeader(
    title = "ICHNAEA"
  ),
  dashboardSidebar(
    sidebarMenuOutput("Semi_collapsible_sidebar")
    # ,              
    # tags$script("$(document).on('click', '.sidebar-toggle', function () {
    #            Shiny.onInputChange('SideBar_col_react', Math.random())});"),
    # tags$script("$(document).on('click', '.treeview.active', function () {
    #            $(this).removeClass('active');
    #            $(this).find( 'ul' ).removeClass('menu-open'); 
    #            $(this).find( 'ul' ).css('display', 'none');});"
    # )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    tags$head(tags$script(src="js/table.js")),
    shinyjs::useShinyjs(),
    extendShinyjs(text = jsCode),
    tabItems(
        
        tabItem("tab1",
          conditionalPanel(id='loader',
                           condition="($('html').hasClass('shiny-busy'))",
                           HTML("<center> <img src='img/wait.gif'> </center>")
          ),
          DT::dataTableOutput("data")
        ),
        tabItem("tab2"
                
                
        ),
        
        tabItem("tab3"),

        
        tabItem("tab4",
          div( 
            HTML("<center>"),
              img(src="img/logo_ub.png", width ="500px"),
              img(src="img/logo_upc.png"),
            HTML("</center>")
          ),
          br(),
          
          p("This website is provided by Universitat Politècnica de Catalunya and Universitat de Barcelona. ")
               
          
        ),
        tabItem("tab5",
                p("help")
        )
    )
  )
)
        


                  

         
