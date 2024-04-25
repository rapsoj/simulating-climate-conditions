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

# Print the column names to verify the structure
print(names(vehicle_emissions_df))
vehicle_emissions_df <- vehicle_emissions_df %>%
  dplyr::rename(per_capita_emissions = Per.capita.carbon.dioxide.emissions.from.transport)

# Load data on registered vehicles per 1000 people
vehicles_per_1000_df <- read.csv('data/registered-vehicles-per-1000-people.csv', 
                                 fileEncoding = "UTF-8-BOM", 
                                 check.names = FALSE, 
                                 stringsAsFactors = FALSE)
# Check the structure and first few entries of the dataframe
str(vehicles_per_1000_df)
head(vehicles_per_1000_df)


#### CALCULATE IMPACT OF MITIGANT #####

# Define valid variables
country_vars <- ghg_df$Code
percent_reduction_vars <- c(0:100)

# Write printable versions of variable names
country_print <- setNames(as.list(ghg_df$Entity), ghg_df$Code)

# Calculate total vehicles with population data
total_vehicles_df <- merge(vehicle_emissions_df, pop_df, by = "Code") %>%
  mutate(total_vehicles = per_capita_emissions * pop)


# Calculate total emissions from vehicles
total_vehicle_emissions <- merge(
  # Merge population and per capita vehicle emission dataframes
  total_vehicle_emissions <- merge(
    x = pop_df,
    y = vehicle_emissions_df,
    by = "Code",
    all.x = TRUE
  ) %>%
    mutate(
      total_vehicle_emissions = pop * per_capita_emissions
    )
    
  # Group by region
  group_by(region) %>%
  # Impute missing per capita values using regional averages
  mutate(
    impute_flag_veh = ifelse(is.na(Car.and.vans.emissions), "*", ""),
    Car.and.vans.emissions = if_else(
      impute_flag_veh == "*", median(Car.and.vans.emissions, na.rm = TRUE), Car.and.vans.emissions)
  ) %>%
  ungroup() %>%
  # Group by continent
  group_by(continent) %>%
  # Impute remaining missing per capita values using continent averages
  mutate(
    impute_flag_veh = ifelse(is.na(Car.and.vans.emissions), "**", impute_flag_veh),
    Car.and.vans.emissions = if_else(
      impute_flag_veh == "**", median(Car.and.vans.emissions, na.rm = TRUE), Car.and.vans.emissions)
  ) %>%
  # Calculate total vehicle emissions
  mutate(
    total_vehicle_co2eq = pop * Car.and.vans.emissions
  )

# Write function to calculate impact of vehicle reduction
reduce_vehicles <- function(country, percent_reduction){
  # Apply reduction to total vehicle emission data
  old_emissions <- total_vehicle_emissions[
    total_vehicle_emissions$Code %in% country, 'total_vehicle_co2eq']
  new_emissions <- old_emissions * (100 - percent_reduction) / 100
  impact <- old_emissions - new_emissions
  impact_million_tonnes <- impact / 100000000
  
  # Compare to current emissions
  country_emissions <- ghg_df[ghg_df$Code %in% country, 'co2eq']
  global_emissions <- ghg_df[ghg_df$Code == 'OWID_WRL', 'co2eq']
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

# https://www.iea.org/data-and-statistics/charts/transport-sector-co2-emissions-by-mode-in-the-sustainable-development-scenario-2000-2030
# https://www.statista.com/statistics/1201189/road-transport-sector-co2-emissions-worldwide-by-country/
# https://ourworldindata.org/co2-emissions-from-transport
# https://ourworldindata.org/grapher/per-capita-co2-transport?time=latest


# total-ghg-emissions.csv: Our World in Data. (2023). "Per capita CO₂ emissions". Retrieved from https://ourworldindata.org/co2-and-greenhouse-gas-emissions.
# population-and-demography.csv: Our World in Data. (2022). "Population". Retrieved from https://ourworldindata.org/population. 
# per-capita-co2-domestic-aviation.csv: Our World in Data. (2019). "Transport". Retrieved from https://ourworldindata.org/transport#:~:text=Global%20aviation%20%E2%80%93%20both%20passenger%20flights,CO2%20emissions%20in%202018.
# per-capita-co2-international-flights-adjusted.csv: Our World in Data. (2019). "Transport". Retrieved from https://ourworldindata.org/transport#:~:text=Global%20aviation%20%E2%80%93%20both%20passenger%20flights,CO2%20emissions%20in%202018.