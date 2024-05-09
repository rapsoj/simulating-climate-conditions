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
                            foodwaste_impact = reduce_food_waste(ghg_df$Code, 100)$impact
)

# For now, make all NAs equal to zero 
mitigation_df[is.na(mitigation_df)] <- 0

# Define variables
variables <- c("Plastic", "Cement", "Landfill", "Water", "FoodWaste")

# Initialise the reduction_df 
# Make sure the columns have the same name and same order as the mitigation_df
# Also important to ensure all the numeric columns name are of the form "mitigationnamewithnospaces_impact"
reduction_df <- data.frame(Code = ghg_df$Code,
                           Entity = ghg_df$Entity,
                           plastic_impact = rep(0, nrow(ghg_df)),
                           cement_impact = rep(0, nrow(ghg_df)),
                           landfill_impact = rep(0, nrow(ghg_df)),
                           water_impact = rep(0, nrow(ghg_df)),
                           foodwaste_impact = rep(0, nrow(ghg_df)))


### APP BUILDING
# Define UI
ui <- fluidPage(
  titlePanel("Mitigation Impact"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variables", "Select Variables to Reduce:",
                  choices = c("Plastic", "Cement", "Landfill", "Water", "FoodWaste"),
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
          sliderInput("Plastic", "Percentage Reduction in Plastic Waste Production:",
                      min = 0, max = 100, value = 0)
        } else if(variable == "Cement") {
          sliderInput("Cement", "Percentage Reduction in Cement Production",
                      min = 0, max = 100, value = 0)
        } else if(variable == "Landfill") {
          sliderInput("Landfill", "Percentage Reduction in Landfill Waste Production",
                      min = 0, max = 100, value = 0)
        } else if(variable == "Water") {
          sliderInput("Water", "Percentage Reduction in Domestic Water Use",
                      min = 0, max = 100, value = 0)
        } else if(variable == "FoodWaste") {
          sliderInput("FoodWaste", "Percentage Reduction in Food Waste",
                      min = 0, max = 100, value = 0)
        }
      })
      do.call(tagList, sliders)
    })
  })
  
  output$map <- renderPlotly({
    
    # Calculate impact of reduction for the selected variables
    for (var in variables) {
      # for any selected variable ...
      if(var %in% input$variables) {
        # find the corresponding column in the mitigation df
        matching_index <- grep(tolower(var), tolower(names(mitigation_df)))
        # reduce by the given percentage
        reduction_df[, matching_index] <- mitigation_df[, matching_index] *
          (input[[var]] / 100)
      }
    }
    
    # Sum the total impact
    reduction_df$total_impact <- rowSums(reduction_df[, grepl("_impact", names(reduction_df))])
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

