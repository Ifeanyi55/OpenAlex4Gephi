library(shiny)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)
library(spsComps)
library(bslib)
library(dplyr)
library(reactable)
library(reactablefmtr)

my_theme <- bs_theme(
  bg = "#fdfefe",
  fg = "black",
  primary = "red",
  base_font = font_google("Geologica"),
  "font-size-base" = "1.2rem",
  version = 5,
  "navbar-bg" = "#9f0be4"
)

options(
  shiny.browser = T,
  spinner.color = "#9f0be4",
  spinner.color.background = "#FFFFFF",
  spinner.size = 2
)

# options(
#   shiny.error = function(){
#     shinyalert(text = "An error occurred!")
#   }
# )
# 
# options(shiny.sanitize.errors = FALSE)

# import search engine function
#source("SearchEngine.R")


# Define UI for application that fetches data from Open Alex
ui <- list(navbarPage(title = strong("OpenAlex For Gephi"),
                                   windowTitle = "OpenAlexForGephi",
                                   theme = my_theme,
                                   tabPanel(title = strong("Author To Publication"),icon = icon("bar-chart"),
                                            includeCSS("style.css"),includeScript("code.js"),
                                            sidebarLayout(
                                              sidebarPanel = "",
                                              mainPanel(
                                                   align = "center",
                                                   width = 12,
                                                   fluidRow(column(6,actionLink("info",h6("Info", style = "color:#670e94;text-align:left;"),style = "text-decoration:none;")),
                                                            column(6,a(h6("Maintainer",style = "color:#670e94;text-align:right;"), style = "text-decoration:none;", target = "_blank", href = "https://github.com/Ifeanyi55"))),
                                                   
                                                   
                                                   h6(strong("Select Publication Search Window",style = "color:#670e94;")),
                                                   dateInput("date","From"),dateInput("date2","To"), br(),
                                                   textInput("text",h6(strong("Keyword Search"),style = "color:#670e94"),placeholder = "Enter keyword(s) here",width = "25%"),br(),
                                                   actionButton("search",strong("Search"),icon = icon("search")),
                                                   withSpinner(reactableOutput("table",width = "80%",height = 400),type = 1),
                                                   fluidRow(column(6,downloadButton("down_nodes",strong("Nodes CSV"),icon = icon("download"))),
                                                            column(6,downloadButton("down_edges",strong("Edges CSV"),icon = icon("download")))),
                                                   
                                                   spsGoTop("up", right = "2%", bottom= "8%", icon = icon("arrow-up"), color = "purple"),
                                                   
                                                            )))))    
                                            

  
# Define server logic for application that fetches data from Open Alex
server <- function(input, output, session) {
  
  # import nodes function
  source("OpenAlexNodes.R")
  
  # import edges display function
  source("OpenAlexEdgesDisp.R")
  
  # import edges function
  source("OpenAlexEdges.R")
  
  
# convert functions to a reactive objects
# searchEngine <- reactive({search_engine()})
  
# convert date inputs to a reactive object
fromReactive <- reactive({input$date})
toReactive <- reactive({input$date2})

# convert text input to a reactive object
textReactive <- reactive({input$text})

authorNodes <- reactive({authorPubNodes(
  keywords = c(unlist(strsplit(textReactive(),split = ","))),
  pub_start_date = fromReactive(),
  pub_end_date = toReactive()
)})
authorEdgesDisp <- reactive({authorPubEdgesDisp(
  keywords = c(unlist(strsplit(textReactive(),split = ","))),
  pub_start_date = fromReactive(),
  pub_end_date = toReactive()
)})
authorEdges <- reactive({authorPubEdges(
  keywords = c(unlist(strsplit(textReactive(),split = ","))),
  pub_start_date = fromReactive(),
  pub_end_date = toReactive()
)})


# # run the search engine
# searchReact <- eventReactive(input$search,{
#   searchEngine(keywords = textReactive(),
#                pub_start_date = fromReactive(),
#                pub_end_date = toReactive())
#   
# })

# pass search output to nodes and edges
nodes_df <- eventReactive(input$search,{
  authorNodes()
})

edges_df <- eventReactive(input$search,{
  authorEdges()
})

edges_disp <- eventReactive(input$search,{
  authorEdgesDisp()
})


# render data as a reactable output
output$table <- renderReactable({

      tryCatch(
        {
          reactable(edges_disp(),
                    theme = reactableTheme(highlightColor = "#b615e7",
                                           borderColor = "#670e94",
                                           borderWidth = 3),
                    bordered = T,
                    compact = T,
                    striped = T,
                    highlight = T,
                    searchable = T,
                    filterable = T)
          
          
        },
        
        error = function(e){
          message("An error occurred!")
          print(e)
        }
      )
})

# activate download buttons
output$down_nodes <- downloadHandler(
  filename = function(){
    paste("Node",".csv",sep = "")
  },
  content = function(file){
    write.csv(nodes_df(),file,row.names = F)
  }
)

output$down_edges <- downloadHandler(
  filename = function(){
    paste("Edge",".csv",sep = "")
  },
  content = function(file){
    write.csv(edges_df(),file,row.names = F)
  }
)

# create shiny alert
observeEvent(input$info,{
  shinyalert(
    text = "Please note that the bigger the search window, 
            the more data is collected. The more data is collected, 
            the longer the runtime and the longer it takes to commence file download.",
    title = "Info",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    confirmButtonText = "OK",
    confirmButtonCol = "#9f0be4",
    timer = 30000,
    showCancelButton = TRUE,
    showConfirmButton = TRUE,
    animation = "slide-from-top",
    imageUrl = "info3.png"
    
  )
})

 
}

    

# Run the application 
shinyApp(ui = ui, server = server)
