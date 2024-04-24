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

# Load data on annual agricultural deforestation emissions
deforest_df <- read.csv(
  'data/co2-deforestation-for-food.csv') %>%
  # Rename columns
  dplyr::rename('co2' = total_embodied_emissions) %>%
  # Convert from tonnes to kilograms
  mutate(co2 = co2 * 1000)


#### CALCULATE IMPACT OF MITIGANT #####

# Define valid variables
country_vars <- ghg_df$Code
percent_reduction_vars <- c(0:100)

# Write printable versions of variable names
country_print <- setNames(as.list(ghg_df$Entity), ghg_df$Code)

# Calculate total emissions from deforestation
total_deforest_emissions <- merge(
  # Merge population and deforestation emission dataframes
  pop_df, deforest_df[,c('Code', 'co2')],
  by='Code', all.x=T) %>%
  # Calculate per capita emissions
  mutate(per_cap_co2 = co2 / pop) %>%
  # Group by region
  group_by(region) %>%
  # Impute missing per capita values using regional averages
  mutate(
    # Add imputation flags
    impute_flag = ifelse(is.na(per_cap_co2), "*", ""),
    # Impute per capita values using regional averages
    per_cap_co2 = if_else(
      impute_flag == "*", median(per_cap_co2, na.rm = T), per_cap_co2),
  ) %>%
  # Ungroup data
  ungroup() %>%
  # Group by continent
  group_by(continent) %>%
  # Impute remaining missing per capita values using continent averages
  mutate(
    # Add imputation flags
    impute_flag = ifelse(is.na(per_cap_co2), "**", impute_flag),
    # Impute per capita values using regional averages
    per_cap_co2 = if_else(
      impute_flag == "**", median(per_cap_co2, na.rm = T), per_cap_co2),
  ) %>%
  # Add columns with total deforestation emissions
  mutate(
    # Calculate total domestic deforestation emissions
    total_co2 = pop * per_cap_co2,
  )

# Write function to calculate impact of mitigant
reduce_deforest <- function(country, percent_reduction){
  # Apply reduction to total deforestation emission data
  old_emissions <- total_deforest_emissions[
    total_deforest_emissions$Code %in% country, paste0('total_', 'co2')]
  new_emissions <- old_emissions * (100 - percent_reduction) / 100
  impact <- old_emissions - new_emissions
  impact_million_tonnes <- impact / 1000 / 1000000
  # Compare to current emissions
  country_emissions <- ghg_df[ghg_df$Code %in% country, 'co2eq']
  global_emissions <- ghg_df[ghg_df$Code == 'OWID_WRL', 'co2eq']
  percent_impact_country <- impact / country_emissions * 100
  percent_impact_global <- impact / global_emissions * 100
  # Print result
  print(paste0(
    'The impact of reducing ', country_print[country], ' ',
    'agricultural deforestation by ', percent_reduction, '% is a reduction of ',
    round(impact_million_tonnes, 2), ' million tonnes CO2 equivalent or ',
    round(percent_impact_country, 2), '% of total ', country_print[country], 
    ' emissions.'))
  print(paste0(
    'This is ', round(percent_impact_global, 2), '% of global emissions.'))
}


#### SOURCES ####

# total-ghg-emissions.csv: Our World in Data. (2023). "Per capita CO₂ emissions". Retrieved from https://ourworldindata.org/co2-and-greenhouse-gas-emissions.
# population-and-demography.csv: Our World in Data. (2022). "Population". Retrieved from https://ourworldindata.org/population. 
# co2-deforestation-for-food.csv: Our World in Data. (2019). "Deforestation". Retrieved from https://ourworldindata.org/deforestation.