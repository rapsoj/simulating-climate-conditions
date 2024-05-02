#### PREPARE WORKSPACE ####

# Load libraries
library(shiny)
library(dplyr)
library(plotly)

# Load functions
source('mitigants/reduce-meat-consumption.R')
source('mitigants/reduce-plastic-waste.R')

##### USER INTERFACE ####
ui <- fluidPage(
  titlePanel("Impact of Mitigation Strategies"),
  sidebarLayout(
    sidebarPanel(
      selectInput("strategy", "Mitigation Strategy:", selected = "",
                  choices = c("", "Reduce Meat Consumption", "Reduce Plastic Waste")),
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

#### SERVER ####
server <- function(input, output) {
  
  output$result1 <- renderText({
    req(input$calculate)  # Ensure calculation button is clicked
    
    strategy <- input$strategy
    meat <- input$meat
    country <- input$country
    percent_reduction <- input$percent_reduction
    
    # Call the appropriate function based on selected strategy and capture the output
    if (strategy == "Reduce Meat Consumption") {
      result <- reduce_meat_consumption(meat, country, percent_reduction)
    } else if (strategy == "Reduce Plastic Waste") {
      result <- reduce_plastic_waste(country, percent_reduction)
    } else {
      result <- c("No strategy selected", "")
    }
    
    # Return the first string from the result
    result[1]
  })
  
  output$result2 <- renderText({
    req(input$calculate)  # Ensure calculation button is clicked
    
    strategy <- input$strategy
    meat <- input$meat
    country <- input$country
    percent_reduction <- input$percent_reduction
    
    # Call the appropriate function based on selected strategy and capture the output
    if (strategy == "Reduce Meat Consumption") {
      result <- reduce_meat_consumption(meat, country, percent_reduction)
    } else if (strategy == "Reduce Plastic Waste") {
      result <- reduce_plastic_waste(country, percent_reduction)
    } else {
      result <- c("", "No strategy selected")
    }
    
    # Return the second string from the result
    result[2]
  })
}

#### RUN APPLICATION ####
shinyApp(ui = ui, server = server)

### TODO: Selection for mitigant selection
### TODO: Add map from Sophie's code
### TODO: Output final dataframe for each mitigant with per capita value and impact of 1% reduction on emissions
### TODO: Combine this into a single dataframe
### TODO: Enable selecting and combining multiple mitigants
### TODO: Combine selection impacts into single evaluation, show current and new emissions
### TODO: Show worst/best per capita/total countries
### TODO: Group mitigants by area
### TODO: Provide option to set threholds per countries