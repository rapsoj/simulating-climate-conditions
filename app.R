# Load libraries
library(shiny)
library(dplyr)

# Load functions
source('mitigants/reduce-meat-consumption.R')

# UI
ui <- fluidPage(
  titlePanel("Impact of Meat Consumption Reduction"),
  sidebarLayout(
    sidebarPanel(
      selectInput("meat", "Meat Type:", selected = "",
                  choices = c("", sort(setNames(names(meat_print), meat_print)))),
      selectInput("country", "Country:", selected = "",
                  choices = c("", sort(setNames(names(country_print), country_print)))),
      sliderInput("percent_reduction", "Percent Reduction:", 
                  min = 0, max = 100, value = 0),
      actionButton("calculate", "Calculate Impact")
    ),
    mainPanel(
      textOutput("result1"),
      textOutput("result2")
    )
  )
)

server <- function(input, output) {
  
  output$result1 <- renderText({
    req(input$calculate)  # Ensure calculation button is clicked
    
    meat <- input$meat
    country <- input$country
    percent_reduction <- input$percent_reduction
    
    # Call the function and capture the output
    result <- reduce_meat_consumption(meat, country, percent_reduction)
    
    # Return the first string from the result
    result[1]
  })
  
  output$result2 <- renderText({
    req(input$calculate)  # Ensure calculation button is clicked
    
    meat <- input$meat
    country <- input$country
    percent_reduction <- input$percent_reduction
    
    # Call the function and capture the output
    result <- reduce_meat_consumption(meat, country, percent_reduction)
    
    # Return the second string from the result
    result[2]
  })
}

# Run the application
shinyApp(ui = ui, server = server)
