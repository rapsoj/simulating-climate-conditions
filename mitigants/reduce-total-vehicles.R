#### PREPARE WORKSPACE ####

# Move working directory to main folder
setwd("C:/Users/fakuz/OneDrive/Dokumente/GitHub/simulating-climate-conditions")

# Import utils
source("data/utils/geography.R")

# Load libraries
library(dplyr)


#### LOAD AND CLEAN DATA ####

# Load global CO2 equivalent emissions data
ghg_df <- read.csv('data/total-ghg-emissions.csv') %>%
  # Rename columns
  dplyr::rename('co2eq' = Annual.greenhouse.gas.emissions.in.CO..equivalents) %>%
  # Convert from tonnes to kilograms
  mutate(co2eq = co2eq * 1000) %>%
  # Filter to most recent year
  filter(Year == max(Year)) %>%
  # Filter to country-level
  filter(Code != '') %>%
  # Link regions
  merge(stack(regions), by.x = "Code", by.y = "values", all.x = T) %>%
  # Rename columns
  dplyr::rename('region' = ind) %>%
  # Link continents
  merge(stack(continents), by.x = "region", by.y = "values", all.x = T) %>%
  # Rename columns
  dplyr::rename('continent' = ind)

# Load population data
pop_df <- read.csv('data/population.csv') %>%
  # Rename columns
  dplyr::rename('pop' = Population..historical.estimates.) %>%
  # Filter to most recent year
  filter(Year == max(Year)) %>%
  # Filter to country-level
  filter(Code != '') %>%
  # Filter to regions that have GHG emissions data 
  filter(Code %in% ghg_df$Code) %>%
  # Link regions
  merge(stack(regions), by.x = "Code", by.y = "values", all.x = T) %>%
  # Rename columns
  dplyr::rename('region' = ind) %>%
  # Link continents
  merge(stack(continents), by.x = "region", by.y = "values", all.x = T) %>%
  # Rename columns
  dplyr::rename('continent' = ind)

# Load data on vehicle emissions
vehicle_emissions_df <- read.csv('data/per-capita-co2-transport.csv')
vehicle_emissions_df <- vehicle_emissions_df %>%
  dplyr::rename(per_capita_emissions = `Per.capita.CO2.emissions`)

# Load data on registered vehicles per 1000 people
vehicles_per_1000_df <- read.csv('data/registered-vehicles-per-1000-people.csv', 
                                 fileEncoding = "UTF-8-BOM", 
                                 check.names = FALSE, 
                                 stringsAsFactors = FALSE)
vehicles_per_1000_df <- vehicles_per_1000_df %>%
  dplyr::rename(vehicles_per_1000 = `Registered.vehicles.per.1000.people`)


#### CALCULATE IMPACT OF MITIGANT #####

# Define valid variables
country_vars <- ghg_df$Code
percent_reduction_vars <- c(0:100)

# Write printable versions of variable names
country_print <- setNames(as.list(ghg_df$Entity), ghg_df$Code)

# Calculate total vehicles with population data
total_vehicles_df <- merge(vehicles_per_1000_df, pop_df, by = "Code") %>%
  mutate(total_vehicles = (vehicles_per_1000 / 1000) * pop)

# Calculate total emissions from vehicles
total_vehicle_emissions <- merge(
  total_vehicles_df,
  vehicle_emissions_df,
  by = "Code",
  all.x = TRUE
) %>%
  mutate(
    total_vehicle_emissions = total_vehicles * per_capita_emissions
  )

# Merge vehicle emissions data with population data
total_vehicle_emissions <- merge(
  pop_df,
  vehicle_emissions_df,
  by = "Code",
  all.x = TRUE
) %>%
  mutate(
    total_vehicle_emissions = pop * per_capita_emissions
  )
    
 
# Write function to calculate impact of vehicle reduction
reduce_vehicles <- function(country, percent_reduction){
  filtered_data <- total_vehicle_emissions[total_vehicle_emissions$Code %in% country,]
  
  # Apply reduction to total vehicle emission data
  old_emissions <- sum(filtered_data$total_vehicle_emissions)
  new_emissions <- old_emissions * (100 - percent_reduction) / 100
  impact <- old_emissions - new_emissions
  impact_million_tonnes <- impact / 100000000
  
  # Compare to current emissions
  country_emissions <- sum(ghg_df[ghg_df$Code %in% country, 'co2eq'])
  global_emissions <- sum(ghg_df[ghg_df$Code == 'OWID_WRL', 'co2eq'])
  percent_impact_country <- impact / country_emissions * 100
  percent_impact_global <- impact / global_emissions * 100
  
  # Print result
  print(paste0(
    'The impact of reducing vehicles by ', percent_reduction, '% in ', country_print[country],
    ' is a reduction of ', round(impact_million_tonnes, 2), ' million tonnes CO2 equivalent or ',
    round(percent_impact_country, 2), '% of total ', country_print[country], ' emissions.'))
  print(paste0(
    'This is ', round(percent_impact_global, 2), '% of global emissions.'))
}

#### SOURCES ####
# total-ghg-emissions.csv: Our World in Data. (2023). "Per capita CO₂ emissions". Retrieved from https://ourworldindata.org/co2-and-greenhouse-gas-emissions.
# population-and-demography.csv: Our World in Data. (2022). "Population". Retrieved from https://ourworldindata.org/population. 
# per-capita-co2-transport.csv: Our World in Data. (2023). "Per capita CO₂ emissions from transport, 2020"Retrieved from  https://ourworldindata.org/co2-emissions-from-transport
# registered-vehicles-per-1000-people.csv: OUr World in Data. (2022). "Registered vehicles per 1,000 people, 2017". Retrieved from https://ourworldindata.org/grapher/registered-vehicles-per-1000-people
