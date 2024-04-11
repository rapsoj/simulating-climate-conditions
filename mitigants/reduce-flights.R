#### PREPARE WORKSPACE ####

# Move working directory to main folder
setwd("../")

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

# Load data on annual per capita domestic flight emissions
per_cap_flight_dom_df <- read.csv(
  'data/per-capita-co2-domestic-aviation.csv') %>%
  # Rename columns
  dplyr::rename('per_cap_co2adj_dom' = Per.capita.domestic.aviation.CO2)

# Load data on annual per capita international flight emissions
per_cap_flight_int_df <- read.csv(
  'data/per-capita-co2-international-flights-adjusted.csv') %>%
  # Rename columns
  dplyr::rename('per_cap_co2adj_int' = Per.capita.international.CO2...adjusted)
  

#### CALCULATE IMPACT OF MITIGANT #####

# Define valid variables
flight_vars <- c('dom', 'int')
country_vars <- ghg_df$Code
percent_reduction_vars <- c(0:100)

# Write printable versions of variable names
country_print <- setNames(as.list(ghg_df$Entity), ghg_df$Code)
flight_print <- list('dom' = 'domestic',
                     'int' = 'international')

# Calculate total emissions from flights
total_flight_emissions <- merge(
  # Merge population and per capita domestic flight emission dataframes
  pop_df, per_cap_flight_dom_df[,c('Code', 'per_cap_co2adj_dom')],
  by='Code', all.x=T) %>%
  # Merge per capita domestic flight emission dataframe
  merge(per_cap_flight_int_df[,c('Code', 'per_cap_co2adj_int')],
        by='Code', all.x=T) %>%
  # Group by region
  group_by(region) %>%
  # Impute missing per capita values using regional averages
  mutate(
    # Add imputation flags
    impute_flag_dom = ifelse(is.na(per_cap_co2adj_dom), "*", ""),
    impute_flag_int = ifelse(is.na(per_cap_co2adj_int), "*", ""),
    # Impute per capita values using regional averages
    per_cap_co2adj_dom = if_else(
      impute_flag_dom == "*", median(per_cap_co2adj_dom, na.rm = T), per_cap_co2adj_dom),
    per_cap_co2adj_int = if_else(
      impute_flag_int == "*", median(per_cap_co2adj_int, na.rm = T), per_cap_co2adj_int)
  ) %>%
  # Ungroup data
  ungroup() %>%
  # Group by continent
  group_by(continent) %>%
  # Impute remaining missing per capita values using continent averages
  mutate(
    # Add imputation flags
    impute_flag_dom = ifelse(is.na(per_cap_co2adj_dom), "**", impute_flag_dom),
    impute_flag_int = ifelse(is.na(per_cap_co2adj_int), "**", impute_flag_int),
    # Impute per capita values using regional averages
    per_cap_co2adj_dom = if_else(
      impute_flag_dom == "**", median(per_cap_co2adj_dom, na.rm = T), per_cap_co2adj_dom),
    per_cap_co2adj_int = if_else(
      impute_flag_int == "**", median(per_cap_co2adj_int, na.rm = T), per_cap_co2adj_int)
  ) %>%
  # Add columns with total flight emissions
  mutate(
    # Calculate total domestic flight emissions
    total_dom_co2eq = pop * per_cap_co2adj_dom,
    # Calculate total international flight emissions
    total_int_co2eq = pop * per_cap_co2adj_int,
  )

# Write function to calculate impact of mitigant
reduce_flights <- function(flight, country, percent_reduction){
  # Apply reduction to total flight emission data
  old_emissions <- total_flight_emissions[
    total_flight_emissions$Code %in% country, paste0('total_', flight, '_co2eq')]
  new_emissions <- total_flight_emissions[
    total_flight_emissions$Code %in% country, paste0('total_', flight, '_co2eq')] *
    (100 - percent_reduction) / 100
  impact <- old_emissions - new_emissions
  impact_million_tonnes <- impact / 100000000
  # Compare to current emissions
  country_emissions <- ghg_df[ghg_df$Code %in% country, 'co2eq']
  global_emissions <- ghg_df[ghg_df$Code == 'OWID_WRL', 'co2eq']
  percent_impact_country <- impact / country_emissions * 100
  percent_impact_global <- impact / global_emissions * 100
  # Print result
  print(paste0(
    'The impact of reducing ', country_print[country], ' ', flight_print[flight],
    ' flights by ', percent_reduction, '% is a reduction of ',
    round(impact_million_tonnes, 2), ' million tonnes CO2 equivalent or ',
    round(percent_impact_country, 2), '% of total ', country_print[country], 
    ' emissions.'))
  print(paste0(
    'This is ', round(percent_impact_global, 2), '% of global emissions.'))
}


#### SOURCES ####

# total-ghg-emissions.csv: Our World in Data. (2023). "Per capita CO₂ emissions". Retrieved from https://ourworldindata.org/co2-and-greenhouse-gas-emissions.
# population-and-demography.csv: Our World in Data. (2022). "Population". Retrieved from https://ourworldindata.org/population. 
# per-capita-co2-domestic-aviation.csv: Our World in Data. (2019). "Transport". Retrieved from https://ourworldindata.org/transport#:~:text=Global%20aviation%20%E2%80%93%20both%20passenger%20flights,CO2%20emissions%20in%202018.
# per-capita-co2-international-flights-adjusted.csv: Our World in Data. (2019). "Transport". Retrieved from https://ourworldindata.org/transport#:~:text=Global%20aviation%20%E2%80%93%20both%20passenger%20flights,CO2%20emissions%20in%202018.