library(readr)
library(shinythemes)
library(tidyverse)
library(shiny)

top50contry <- read_csv("top50contry.csv")

top10s <- read_csv("top10s.csv")

x_choices <- as.list(names(top50contry)[c(7:15)])

x_choices_names <- c("Beats Per Minute"
                     ,"Energy"
                     ,"Danceability"
                     ,"Loudness - dB"
                     ,"Liveness"
                     ,"Valence"
                     ,"Duration"
                     ,"Acousticness"
                     ,"Speechiness")

names(x_choices_names) <- x_choices_names


y_choices<-as.list(names(top50contry)[c(16)])

y_choice_names <- c( "Popularity")

div_choices <- (unique(top50contry$country))

ui <- fluidPage(
  
  h1("Analysis of Global Music Trends"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "x"
                  , label = "Choose a predictor variable of interest:"
                  , choices = x_choices
                  , selected = "artist"),
      selectInput(inputId = "y"
                  , label = "Choose a outcome variable of interest:"
                  , choices = y_choices),
      selectInput(inputId = "div"
                  , label = "Include Countries:"
                  , choices = c("All", div_choices)
                  , selected = "All")
      
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs"
                  , tabPanel("Histogram", plotOutput(outputId = "hist"))
                  , tabPanel("Scatterplot", plotOutput(outputId = "scatter"))
      )
    )
  )
)

# server
server <- function(input,output){
  use_data <- reactive({
    data<-top50contry
    req(input$div)
    if(input$div!="All"){
      data<-data%>%
        filter(country==input$div)
    }else{
      data<-data
    }
  })
  
  output$hist <- renderPlot({
    ggplot(data = use_data(), aes_string(x = input$x)) +
      geom_histogram(color = "#00215c", fill = "#00215c", alpha = 0.7) +
      labs(x = x_choices_names[x_choices == input$x]
           , y = "Count")
  })
  
  output$scatter <- renderPlot({
    ggplot(data = use_data(), aes_string(x = input$x, y = input$y)) +
      geom_point() +
      labs(x = names(x_choices)[x_choices == input$x]
           , y = y_choices[y_choices == input$y])
  })

}

# call to shinyApp
shinyApp(ui = ui, server = server)
