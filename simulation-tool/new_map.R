# Load libraries
library(dplyr)

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


#### NOW SHINY APP BUILDING ####################################################

library(shiny)
library(plotly)

# Define UI
ui <- fluidPage(
  titlePanel("Mitigation Impact"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("plastic_reduction", "Percentage Reduction in Plastic Waste Production:",
                  min = 0, max = 100, value = 0),
      sliderInput("cement_reduction", "Percentage Reduction in Cement Production",
                  min = 0, max = 100, value = 0),
      sliderInput("landfill_reduction", "Percentage Reduction in Lanfill Waste Production",
                  min = 0, max = 100, value = 0),
      sliderInput("water_reduction", "Percentage Reduction in Domestic Water Use",
                  min = 0, max = 100, value = 0),
      sliderInput("food_waste_reduction", "Percentage Reduction in Food Waste",
                  min = 0, max = 100, value = 0)
    ),
    mainPanel(
      plotlyOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$map <- renderPlotly({
    # Calculate impact of reduction in plastic waste for each country
    reduction_df <- mitigation_df %>%
      mutate(plastic_impact = plastic_impact * input$plastic_reduction,
             cement_impact = cement_impact * input$cement_reduction,
             landfill_impact = landfill_impact * input$landfill_reduction,
             water_impact = water_impact * input$water_reduction,
             food_waste_impact = food_waste_impact * input$food_waste_reduction,
             total_impact = plastic_impact + cement_impact + landfill_impact + 
               water_impact + food_waste_impact,
             total_impact_million_tonnes = total_impact / 1000000000)
    
    # Create an interactive map
    plot_ly(type = 'choropleth',
            locations = reduction_df$Code,
            z = round(reduction_df$total_impact_million_tonnes,2),
            text = ~paste(reduction_df$Entity, "<br>", 
                          "Impact (million tonnes CO2eq):", 
                          round(reduction_df$total_impact_million_tonnes,2)),
            colorscale = "Viridis",
            marker = list(line = list(width = 0.5)),
            colorbar = list(title = "Impact (million tonnes CO2eq)")) %>%
      layout(title = "Reduction in greenhouse gas emissions due to summed mitigation efforts")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
