library(leaflet)
library(shinydashboard)
library(shinyBS)
library(shinyjqui)
library(shinycssloaders)
library(shinyWidgets)
library(gridExtra)
#library(ECharts2Shiny)

jsCode <- 
  "shinyjs.filename =
function (){
$(document).ready(function() {
console.log(document.getElementById('filename').value);
});
}"
predictValues <- c("Aardvark",
                                 "Abyssinian",
                                 "Affenpinscher",
                                 "Akbash",
                                 "Akita",
                                 "Albatross",
                                 "Alligator",
                                 "Alpaca",
                                 "Angelfish",
                                 "Ant",
                                 "Anteater",
                                 "Antelope",
                                 "Ape",
                                 "Armadillo",
                                 "Ass",
                                 "Avocet",
                                 "Axolotl",
                                 "Baboon",
                                 "Badger",
                                 "Balinese",
                                 "Bandicoot",
                                 "Barb",
                                 "Barnacle",
                                 "Barracuda",
                                 "Bat",
                                 "Beagle",
                                 "Bear",
                                 "Beaver",
                                 "Bee",
                                 "Beetle",
                                 "Binturong",
                                 "Bird",
                                 "Birman",
                                 "Bison",
                                 "Bloodhound",
                                 "Boar",
                                 "Bobcat",
                                 "Bombay",
                                 "Bongo",
                                 "Bonobo",
                                 "Booby",
                                 "Budgerigar",
                                 "Buffalo",
                                 "Bulldog",
                                 "Bullfrog",
                                 "Burmese",
                                 "Butterfly",
                                 "Caiman",
                                 "Camel",
                                 "Capybara",
                                 "Caracal",
                                 "Caribou",
                                 "Cassowary",
                                 "Cat",
                                 "Caterpillar",
                                 "Catfish",
                                 "Cattle",
                                 "Centipede",
                                 "Chameleon",
                                 "Chamois",
                                 "Cheetah",
                                 "Chicken",
                                 "Chihuahua",
                                 "Chimpanzee",
                                 "Chinchilla",
                                 "Chinook",
                                 "Chipmunk",
                                 "Chough",
                                 "Cichlid",
                                 "Clam",
                                 "Coati",
                                 "Cobra",
                                 "Cockroach",
                                 "Cod",
                                 "Collie",
                                 "Coral",
                                 "Cormorant",
                                 "Cougar",
                                 "Cow",
                                 "Coyote",
                                 "Crab",
                                 "Crane",
                                 "Crocodile",
                                 "Crow",
                                 "Curlew",
                                 "Cuscus",
                                 "Cuttlefish",
                                 "Dachshund",
                                 "Dalmatian",
                                 "Deer",
                                 "Dhole",
                                 "Dingo",
                                 "Dinosaur",
                                 "Discus",
                                 "Dodo",
                                 "Dog",
                                 "Dogfish",
                                 "Dolphin",
                                 "Donkey",
                                 "Dormouse",
                                 "Dotterel",
                                 "Dove",
                                 "Dragonfly",
                                 "Drever",
                                 "Duck",
                                 "Dugong",
                                 "Dunker",
                                 "Dunlin",
                                 "Eagle",
                                 "Earwig",
                                 "Echidna",
                                 "Eel",
                                 "Eland",
                                 "Elephant",
                                 "Elephant seal",
                                 "Elk",
                                 "Emu",
                                 "Falcon",
                                 "Ferret",
                                 "Finch",
                                 "Fish",
                                 "Flamingo",
                                 "Flounder",
                                 "Fly",
                                 "Fossa",
                                 "Fox",
                                 "Frigatebird",
                                 "Frog",
                                 "Galago",
                                 "Gar",
                                 "Gaur",
                                 "Gazelle",
                                 "Gecko",
                                 "Gerbil",
                                 "Gharial",
                                 "Giant Panda",
                                 "Gibbon",
                                 "Giraffe",
                                 "Gnat",
                                 "Gnu",
                                 "Goat",
                                 "Goldfinch",
                                 "Goldfish",
                                 "Goose",
                                 "Gopher",
                                 "Gorilla",
                                 "Goshawk",
                                 "Grasshopper",
                                 "Greyhound",
                                 "Grouse",
                                 "Guanaco",
                                 "Guinea fowl",
                                 "Guinea pig",
                                 "Gull",
                                 "Guppy",
                                 "Hamster",
                                 "Hare",
                                 "Harrier",
                                 "Havanese",
                                 "Hawk",
                                 "Hedgehog",
                                 "Heron",
                                 "Herring",
                                 "Himalayan",
                                 "Hippopotamus",
                                 "Hornet",
                                 "Horse",
                                 "Human",
                                 "Hummingbird",
                                 "Hyena",
                                 "Ibis",
                                 "Iguana",
                                 "Impala",
                                 "Indri",
                                 "Insect",
                                 "Jackal",
                                 "Jaguar",
                                 "Javanese",
                                 "Jay",
                                 "Jay",
                                 " Blue",
                                 "Jellyfish",
                                 "Kakapo",
                                 "Kangaroo",
                                 "Kingfisher",
                                 "Kiwi",
                                 "Koala",
                                 "Komodo dragon",
                                 "Kouprey",
                                 "Kudu",
                                 "Labradoodle",
                                 "Ladybird",
                                 "Lapwing",
                                 "Lark",
                                 "Lemming",
                                 "Lemur",
                                 "Leopard",
                                 "Liger",
                                 "Lion",
                                 "Lionfish",
                                 "Lizard",
                                 "Llama",
                                 "Lobster",
                                 "Locust",
                                 "Loris",
                                 "Louse",
                                 "Lynx",
                                 "Lyrebird",
                                 "Macaw",
                                 "Magpie",
                                 "Mallard",
                                 "Maltese",
                                 "Manatee",
                                 "Mandrill",
                                 "Markhor",
                                 "Marten",
                                 "Mastiff",
                                 "Mayfly",
                                 "Meerkat",
                                 "Millipede",
                                 "Mink",
                                 "Mole",
                                 "Molly",
                                 "Mongoose",
                                 "Mongrel",
                                 "Monkey",
                                 "Moorhen",
                                 "Moose",
                                 "Mosquito",
                                 "Moth",
                                 "Mouse",
                                 "Mule",
                                 "Narwhal",
                                 "Neanderthal",
                                 "Newfoundland",
                                 "Newt",
                                 "Nightingale",
                                 "Numbat",
                                 "Ocelot",
                                 "Octopus",
                                 "Okapi",
                                 "Olm",
                                 "Opossum",
                                 "Orangutan",
                                 "Oryx",
                                 "Ostrich",
                                 "Otter",
                                 "Owl",
                                 "Ox",
                                 "Oyster",
                                 "Pademelon",
                                 "Panther",
                                 "Parrot",
                                 "Partridge",
                                 "Peacock",
                                 "Peafowl",
                                 "Pekingese",
                                 "Pelican",
                                 "Penguin",
                                 "Persian",
                                 "Pheasant",
                                 "Pig",
                                 "Pigeon",
                                 "Pika",
                                 "Pike",
                                 "Piranha",
                                 "Platypus",
                                 "Pointer",
                                 "Pony",
                                 "Poodle",
                                 "Porcupine",
                                 "Porpoise",
                                 "Possum",
                                 "Prairie Dog",
                                 "Prawn",
                                 "Puffin",
                                 "Pug",
                                 "Puma",
                                 "Quail",
                                 "Quelea",
                                 "Quetzal",
                                 "Quokka",
                                 "Quoll",
                                 "Rabbit",
                                 "Raccoon",
                                 "Ragdoll",
                                 "Rail",
                                 "Ram",
                                 "Rat",
                                 "Rattlesnake",
                                 "Raven",
                                 "Red deer",
                                 "Red panda",
                                 "Reindeer",
                                 "Rhinoceros",
                                 "Robin",
                                 "Rook",
                                 "Rottweiler",
                                 "Ruff",
                                 "Salamander",
                                 "Salmon",
                                 "Sand Dollar",
                                 "Sandpiper",
                                 "Saola",
                                 "Sardine",
                                 "Scorpion",
                                 "Sea Urchin",
                                 "Sea lion",
                                 "Seahorse",
                                 "Seal",
                                 "Serval",
                                 "Shark",
                                 "Sheep",
                                 "Shrew",
                                 "Shrimp",
                                 "Siamese",
                                 "Siberian",
                                 "Skunk",
                                 "Sloth",
                                 "Snail",
                                 "Snake",
                                 "Snowshoe",
                                 "Somali",
                                 "Sparrow",
                                 "Spider",
                                 "Sponge",
                                 "Squid",
                                 "Squirrel",
                                 "Starfish",
                                 "Starling",
                                 "Stingray",
                                 "Stinkbug",
                                 "Stoat",
                                 "Stork",
                                 "Swallow",
                                 "Swan",
                                 "Tang",
                                 "Tapir",
                                 "Tarsier",
                                 "Termite",
                                 "Tetra",
                                 "Tiffany",
                                 "Tiger",
                                 "Toad",
                                 "Tortoise",
                                 "Toucan",
                                 "Tropicbird",
                                 "Trout",
                                 "Tuatara",
                                 "Turkey",
                                 "Turtle",
                                 "Uakari",
                                 "Uguisu",
                                 "Umbrellabird",
                                 "Vicuña",
                                 "Viper",
                                 "Vulture",
                                 "Wallaby",
                                 "Walrus",
                                 "Warthog",
                                 "Wasp",
                                 "Water buffalo",
                                 "Weasel",
                                 "Whale",
                                 "Whippet",
                                 "Wildebeest",
                                 "Wolf",
                                 "Wolverine",
                                 "Wombat",
                                 "Woodcock",
                                 "Woodlouse",
                                 "Woodpecker",
                                 "Worm",
                                 "Wrasse",
                                 "Wren",
                                 "Yak",
                                 "Zebra",
                                 "Zebu",
                                 "Zonkey",
                                 "Zorse")

customSentence <- function(numItems, type) {
  paste("Selecciona el teu idioma")
}


# Function to call in place of dropdownMenu
dropdownMenuCustom <-     function (..., type = c("messages", "notifications", "tasks"), 
                                    badgeStatus = "primary", icon = NULL, .list = NULL, customSentence = customSentence) 
{
  type <- match.arg(type)
  if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li")
  dropdownClass <- paste0("dropdown ", type, "-menu")
  if (is.null(icon)) {
    icon <- switch(type, messages = shiny::icon("envelope"), 
                   notifications = shiny::icon("warning"), tasks = shiny::icon("tasks"))
  }
  numItems <- length(items)
  if (is.null(badgeStatus)) {
    badge <- NULL
  }
  else {
    badge <- span(class = paste0("label label-", badgeStatus), 
                  numItems)
  }
  tags$li(
    class = dropdownClass, 
    a(
      href = "#", 
      class = "dropdown-toggle", 
      `data-toggle` = "dropdown", 
      icon, 
      badge
    ), 
    tags$ul(
      class = "dropdown-menu", 
      tags$li(
        class = "header", 
        customSentence(numItems, type)
      ), 
      tags$li(
        tags$ul(class = "menu", items)
      )
    )
  )
}


dbHeader <- dashboardHeader(title = "ICHNAEA",
                            tags$li(HTML( 
                              "<a href='www/manual.txt' style='padding-top:10px; padding-bottom:8px;' download>
                              <img height='20px'>
                              <i class='fa fa-question' style='height:30px;'></i> Help 
                              </a>"),  
                              class = "dropdown"),
                            tags$li(HTML( 
                              "<a href='' style='padding-top:10px; padding-bottom:8px;'  data-toggle='modal' data-target='#myModal'>
                              <img height='20px'>
                              <i class='fa fa-info-circle' style='height:30px;'></i> About 
                              
                              </a>"),  
                              class = "dropdown"),
                            tags$li(HTML( 
                              "<a href='' style='padding-top:10px; padding-bottom:8px;'>
                              <img height='20px'>
                              <i class='fa fa-language' style='height:30px;'></i> Languages</a>"),
                              class = "dropdown")
                            
                            )



modalAbout <- HTML("
                   <!-- Modal -->
                   <div id='myModal' class='modal fade' role='dialog'>
                   <div class='modal-dialog'>
                   
                   <!-- Modal content-->
                   <div class='modal-content'>
                   <div class='modal-header'>
                   <button type='button' class='close' data-dismiss='modal'>&times;</button>
                   <h4 class='modal-title'>About this website</h4>
                   </div>
                   <div class='modal-body'>
                   <p> 
                   
                   <center>
                   <img src='img/logo_ub.png' style='width:250px;'/>
                   <img src='img/logo_upc.png' style='width:250px;'/>
                   </center>
                   This website is possible thanks to the efforts made by Universitat Politècnica de Catalunya and Universitat of Barcelona.
                   </p>
                   </div>
                   <div class='modal-footer'>
                   <button type='button' class='btn btn-default' data-dismiss='modal'>Dismiss</button>
                   </div>
                   </div>
                   
                   </div>
                   </div>
                   ")

options(shiny.sanitize.errors = FALSE)
dashboardPage(
  dbHeader,
  dashboardSidebar(
    sidebarMenuOutput("Semi_collapsible_sidebar")
  ),
                
 
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/prettyfy.css")),
    
    tags$head(tags$script(src="js/table.js")),
    shinyjs::useShinyjs(),
    extendShinyjs(text = jsCode),
    useSweetAlert(),
    
    tabItems(
      
      
      tabItem("tab1",
              # conditionalPanel(condition = ),
              box(
                title = "Microbiological Data",
                status = "primary",
                width = 12,
                withSpinner(DT::dataTableOutput("data")),
                bsAlert("alert"),
                modalAbout
                
              )
              
              
      ),
      
      tabItem("tab22",
              box(
                title = "Choosing variables to create a model",
                status = "primary",
                width = 12,
                div(class="llistes",
                    
                  div(class="column1",materialSwitch(inputId = "pointSource", label = "Point source?", status = "primary")),
                  div(class="column2",materialSwitch(inputId = "molecular", label = "Molecular variables?", status = "primary"))
                ),
                
                conditionalPanel(condition = "input.pointSource == 0",
                                 sliderInput("aging", label = "Aged",  min = 0, max = 9000, value = c(0,9000))
                ),
                sliderInput("matrixSize", label = "Matrix Size",  min = 0, max = 9000, value = c(0,9000)),
                
                sliderInput("dissolution", label = "Dissolution",  min = 0, max = 9000, value = c(0,9000)),
                p(class="titol" ,"Predict "), br(),
                div(class="llistes",
                    div(class="column1",
                      pickerInput(
                        inputId = "myPicker",
                        label = "Select predicts",
                        choices = predictValues,
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"
                        ),
                        multiple = TRUE
                      )
                    
                    ),
                    p(class="vs-titol", "VS"),
                    div(class="column2",
                      pickerInput(
                        inputId = "myPicker2",
                        label = "Select vs predicts",
                        choices = predictValues,
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"
                        ),
                        multiple = TRUE
                      )
                    )
                ),
                div(class="right",
                  actionBttn(inputId = "generateMatrix", label = "Generate BIGMATRIX", 
                             icon = icon("cog"), color = "primary", style = "jelly")
                )
              )
      ),
      
      tabItem("tab2",
              box(
                title = "Creation of a predictive model",
                status = "primary",
                width = 12,
                pickerInput(inputId = "algorithm", 
                            label = "Algorithm to apply", 
                            choices = c("LDA", "QDA"), 
                            options = list(title = "Choose your algorithm ")
                ),
                HTML("<hr class='style11'>")
                # withSpinner(DT::dataTableOutput("data")) mostrar la bigmatrix
                
                
              )
              
      ),
      
      
      tabItem("tab3",
              title = "Visualization",
              box(title = "Visualization",width = 12, status = "primary")
      )
      
      
      
    )
  )
)





