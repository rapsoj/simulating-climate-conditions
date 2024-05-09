# Load libraries
library(dplyr)
library(shiny)
library(plotly)

# Combine all mitigants into one big dataframe
mitigation_df <- data.frame(Code = ghg_df$Code,
                            Entity = ghg_df$Entity,
                            plastic_impact = reduce_plastic_waste(ghg_df$Code, 100)$impact,
                            cement_impact = reduce_cement_production(ghg_df$Code, 100)$impact,
                            landfill_impact = reduce_landfill_waste(ghg_df$Code, 100)$impact,
                            water_impact = reduce_domestic_water_use(ghg_df$Code, 100)$impact,
                            food_waste_impact = reduce_food_waste(ghg_df$Code, 100)$impact
)

# For now, make all NAs equal to zero 
mitigation_df[is.na(mitigation_df)] <- 0


### APP BUILDING
# Define UI
ui <- fluidPage(
  titlePanel("Mitigation Impact"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variables", "Select Variables to Reduce:",
                  choices = c("Plastic", "Cement", "Landfill", "Water", "Food Waste"),
                  multiple = TRUE),
      uiOutput("sliders")
    ),
    mainPanel(
      plotlyOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observe({
    # Generate sliders based on selected variables
    output$sliders <- renderUI({
      sliders <- lapply(input$variables, function(variable) {
        if(variable == "Plastic") {
          sliderInput("plastic_reduction", "Percentage Reduction in Plastic Waste Production:",
                      min = 0, max = 100, value = 0)
        } else if(variable == "Cement") {
          sliderInput("cement_reduction", "Percentage Reduction in Cement Production",
                      min = 0, max = 100, value = 0)
        } else if(variable == "Landfill") {
          sliderInput("landfill_reduction", "Percentage Reduction in Landfill Waste Production",
                      min = 0, max = 100, value = 0)
        } else if(variable == "Water") {
          sliderInput("water_reduction", "Percentage Reduction in Domestic Water Use",
                      min = 0, max = 100, value = 0)
        } else if(variable == "Food Waste") {
          sliderInput("food_waste_reduction", "Percentage Reduction in Food Waste",
                      min = 0, max = 100, value = 0)
        }
      })
      do.call(tagList, sliders)
    })
  })
  
  output$map <- renderPlotly({
    # Calculate impact of reduction for the selected variables
    reduction_df <- mitigation_df
    if("Plastic" %in% input$variables) {
      reduction_df$plastic_impact <- reduction_df$plastic_impact * (input$plastic_reduction / 100)
    } else {reduction_df$plastic_impact <- reduction_df$plastic_impact * 0}
    if("Cement" %in% input$variables) {
      reduction_df$cement_impact <- reduction_df$cement_impact * (input$cement_reduction / 100)
    } else {reduction_df$cement_impact <- reduction_df$cement_impact * 0}
    if("Landfill" %in% input$variables) {
      reduction_df$landfill_impact <- reduction_df$landfill_impact * (input$landfill_reduction / 100)
    } else {reduction_df$landfill_impact <- reduction_df$landfill_impact * 0}
    if("Water" %in% input$variables) {
      reduction_df$water_impact <- reduction_df$water_impact * (input$water_reduction / 100)
    } else {reduction_df$water_impact <- reduction_df$water_impact * 0}
    if("Food Waste" %in% input$variables) {
      reduction_df$food_waste_impact <- reduction_df$food_waste_impact * (input$food_waste_reduction / 100)
    } else {reduction_df$food_waste_impact <- reduction_df$food_waste_impact * 0}
    reduction_df$total_impact <- rowSums(reduction_df[, c("plastic_impact", "cement_impact", "landfill_impact", "water_impact", "food_waste_impact")], na.rm = TRUE)
    reduction_df$total_impact_million_tonnes <- reduction_df$total_impact / 1000000000
    
    # Create an interactive map
    plot_ly(type = 'choropleth',
            locations = reduction_df$Code,
            z = round(reduction_df$total_impact_million_tonnes, 2),
            text = ~paste(reduction_df$Entity, "<br>", 
                          "Impact (million tonnes CO2eq):", 
                          round(reduction_df$total_impact_million_tonnes, 2)),
            colorscale = "Viridis",
            marker = list(line = list(width = 0.5)),
            colorbar = list(title = "Impact (million tonnes CO2eq)")) %>%
      layout(title = "Reduction in greenhouse gas emissions due to summed mitigation efforts")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
