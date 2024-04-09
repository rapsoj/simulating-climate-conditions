# Load libraries
library(dplyr)

# Move working directory to main folder
setwd("../")


#### LOAD AND CLEAN DATA ####

# Load global CO2 equivalent emissions data
ghg_df <- read.csv('data/total-ghg-emissions.csv') %>%
  # Rename columns
  rename('co2eq' = Annual.greenhouse.gas.emissions.in.CO..equivalents) %>%
  # Convert from tonnes to kilograms
  mutate(co2eq = co2eq * 1000) %>%
  # Filter to most recent year
  filter(Year == max(Year)) %>%
  # Filter to country-level
  filter(Code != '')

# Load population data
pop_df <- read.csv('data/population.csv') %>%
  # Rename columns
  rename('pop' = Population..historical.estimates.) %>%
  # Filter to most recent year
  filter(Year == max(Year)) %>%
  # Filter to country-level
  filter(Code != '')

# Load and clean per capita fossil fuel data
per_cap_ff_emissions_df <- subset(read.csv("data/owid-co2-data.csv"), 
                          select = c("country",
                                     "year",
                                     "iso_code",
                                     "gas_co2_per_capita",
                                     "coal_co2_per_capita",
                                     "oil_co2_per_capita"))
  
per_cap_ff_emissions <- per_cap_ff_emissions_df %>%
  # Rename columns
  rename('per_cap_gas_co2' = gas_co2_per_capita,
         'per_cap_coal_co2' = coal_co2_per_capita,
         'per_cap_oil_co2' = oil_co2_per_capita,
         'Entity' = country,
         "Year" = year,
         "Code" = iso_code) %>%
  # Convert to kg
  mutate(per_cap_gas_co2 = per_cap_gas_co2 * 1000,
         per_cap_oil_co2 = per_cap_oil_co2 * 1000,
         per_cap_coal_co2 = per_cap_coal_co2 * 1000) %>%
  # Filter to most recent year
  filter(Year == max(Year)) %>%
  # Filter to country-level
  filter(Code != "")


#### CALCULATE IMPACT OF MITIGANT #####

# Define valid variables
region_vars <- ghg_df$Code
fuel_vars <- c('coal', 'gas', 'oil')
percent_reduction_vars <- c(0:100)

# Write printable versions of variable names
fuel_print <- list('coal' = 'coal', 'oil' = 'oil', 'gas' = 'gas')
region_print <- setNames(as.list(ghg_df$Entity), ghg_df$Code)


# Calculate total emissions from fossil fuels
total_ff_emissions <- merge(
  # Merge population and per capita ff emission dataframes
  pop_df, per_cap_ff_emissions[,c(
    'Code', 'per_cap_gas_co2', 'per_cap_coal_co2',
    'per_cap_oil_co2')],
  by='Code', all.x=T) %>%
  # Add columns with total ff emissions
  mutate(
    # Calculate total coal emissions
    total_coal_co2 = pop * per_cap_coal_co2,
    # Calculate total gas emissions
    total_gas_co2 = pop * per_cap_gas_co2,
    # Calculate total oil emissions
    total_oil_co2 = pop * per_cap_oil_co2
  )

# Write function to calculate impact of mitigant
reduce_ff_consumption <- function(fuel, region, percent_reduction){
  # Apply reduction to total meat emission data
  old_emissions <- total_ff_emissions[
    total_ff_emissions$Code %in% region, paste0('total_', fuel, '_co2')]
  new_emissions <- total_ff_emissions[
    total_ff_emissions$Code %in% region, paste0('total_', fuel, '_co2')] *
    (100 - percent_reduction) / 100
  impact <- old_emissions - new_emissions
  impact_million_tonnes <- impact / 100000000
  # Compare to current emissions
  country_emissions <- ghg_df[ghg_df$Code %in% region, 'co2eq']
  global_emissions <- ghg_df[ghg_df$Code == 'OWID_WRL', 'co2eq']
  percent_impact_country <- impact / country_emissions * 100
  percent_impact_global <- impact / global_emissions * 100
  # Print result
  print(paste0(
    'The impact of reducing ', region_print[region], ' ', fuel_print[fuel],
    ' consumption by ', percent_reduction, '% is a reduction of ',
    round(impact_million_tonnes, 2), ' million tonnes CO2 equivalent or ',
    round(percent_impact_country, 2), '% of total ', region_print[region], 
    ' emissions.'))
  print(paste0(
    'This is ', round(percent_impact_global, 2), '% of global emissions.'))
}



#### SOURCES ####

# total-ghg-emissions.csv: Our World in Data. (2023). "Per capita CO₂ emissions". Retrieved from https://ourworldindata.org/co2-and-greenhouse-gas-emissions.
# owid-co2-data.csv: Our World in Data. (2023). "CO2 and Greenhouse Gas Emissions". Retrieved from https://github.com/owid/co2-data?tab=readme-ov-file. 
# population.csv: Our World in Data. (2022). "Population". Retrieved from https://ourworldindata.org/population. 


