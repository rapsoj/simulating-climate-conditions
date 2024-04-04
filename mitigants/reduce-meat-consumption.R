#### PREPARE WORKSPACE ####

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

# Load data on annual per capita meat consumption
meat_consumption_df <- read.csv(
  'data/per-capita-meat-consumption-by-type-kilograms-per-year.csv') %>%
  # Rename columns
  rename(
    'beef' = Meat..beef...00002731....Food.available.for.consumption...0645pc....kilograms.per.year.per.capita,
    'pork' = Meat..pig...00002733....Food.available.for.consumption...0645pc....kilograms.per.year.per.capita,
    'sheep_goat' = Meat..sheep.and.goat...00002732....Food.available.for.consumption...0645pc....kilograms.per.year.per.capita,
    'poultry' = Meat..poultry...00002734....Food.available.for.consumption...0645pc....kilograms.per.year.per.capita,
    'other' = Meat..Other...00002735....Food.available.for.consumption...0645pc....kilograms.per.year.per.capita)  %>%
  # Filter to most recent year
  filter(Year == max(Year))

# Load data on emissions per kg of food product
food_emissions_df <- read.csv('data/ghg-per-kg-poore.csv') %>%
  # Rename columns
  rename('co2eq' = GHG.emissions.per.kilogram..Poore...Nemecek..2018.)

# Load population data
pop_df <- read.csv('data/population.csv') %>%
  # Rename columns
  rename('pop' = Population..historical.estimates.) %>%
  # Filter to most recent year
  filter(Year == max(Year)) %>%
  # Filter to country-level
  filter(Code != '')


#### CALCULATE IMPACT OF MITIGANT #####

# Define valid variables
meat_vars <- c('beef', 'pork', 'sheep_goat', 'poultry')
region_vars <- ghg_df$Code
percent_reduction_vars <- c(0:100)

# Write printable versions of variable names
meat_print <- list('beef' = 'beef', 'pork' = 'pork',
                   'sheep_goat' = 'sheep and goat', 'poultry' = 'poultry')
region_print <- setNames(as.list(ghg_df$Entity), ghg_df$Code)

# Calculate emissions from meat consumption per capita
per_capita_meat_emissions <- meat_consumption_df[,c('Entity', 'Code')] %>%
  # Add columns with per kg meat emissions
  mutate(
    # Calculate beef emissions by averaging dairy and beef herd emissions
    per_cap_beef_co2eq = (
      food_emissions_df[
        food_emissions_df$Entity == 'Beef (beef herd)',]$co2eq
      + food_emissions_df[
        food_emissions_df$Entity == 'Beef (dairy herd)',]$co2eq) / 2
    * meat_consumption_df$beef,
    # Calculate pork emissions
    per_cap_pork_co2eq = food_emissions_df[
      food_emissions_df$Entity == 'Pig Meat',]$co2eq 
    * meat_consumption_df$pork,
    # Calculate sheep and goat emissions
    per_cap_sheep_goat_co2eq = food_emissions_df[
      food_emissions_df$Entity == 'Lamb & Mutton',]$co2eq 
    * meat_consumption_df$sheep_goat,
    # Calculate poultry emissions
    per_cap_poultry_co2eq = food_emissions_df[
      food_emissions_df$Entity == 'Poultry Meat',]$co2eq 
    * meat_consumption_df$poultry
  )

# Calculate total emissions from meat consumption
total_meat_emissions <- merge(
  # Merge population and per capita meat emission dataframes
  pop_df, per_capita_meat_emissions[,c(
    'Code', 'per_cap_beef_co2eq', 'per_cap_pork_co2eq',
    'per_cap_sheep_goat_co2eq', 'per_cap_poultry_co2eq')],
  by='Code', all.x=T) %>%
  # Add columns with total meat emissions
  mutate(
    # Calculate total beef emissions
    total_beef_co2eq = pop * per_cap_beef_co2eq,
    # Calculate total pork emissions
    total_pork_co2eq = pop * per_cap_pork_co2eq,
    # Calculate total beef emissions
    total_beef_co2eq = pop * per_cap_beef_co2eq,
    # Calculate total sheep and goat emissions
    total_sheep_goat_co2eq = pop * per_cap_sheep_goat_co2eq,
    # Calculate total poultry emissions
    total_poultry_co2eq = pop * per_cap_poultry_co2eq
  )

# Write function to calculate impact of mitigant
reduce_meat_consumption <- function(meat, region, percent_reduction){
  # Apply reduction to total meat emission data
  old_emissions <- total_meat_emissions[
    total_meat_emissions$Code %in% region, paste0('total_', meat, '_co2eq')]
  new_emissions <- total_meat_emissions[
    total_meat_emissions$Code %in% region, paste0('total_', meat, '_co2eq')] *
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
    'The impact of reducing ', region_print[region], ' ', meat_print[meat],
    ' consumption by ', percent_reduction, '% is a reduction of ',
    round(impact_million_tonnes, 2), ' million tonnes CO2 equivalent or ',
    round(percent_impact_country, 2), '% of total ', region_print[region], 
    ' emissions.'))
  print(paste0(
    'This is ', round(percent_impact_global, 2), '% of global emissions.'))
}


#### SOURCES ####

# total-ghg-emissions.csv: Our World in Data. (2023). "Per capita CO₂ emissions". Retrieved from https://ourworldindata.org/co2-and-greenhouse-gas-emissions.
# per-capita-meat-consumption-by-type-kilograms-per-year.csv: Our World in Data. (2023). "Per capita meat consumption by type". Retrieved from https://ourworldindata.org/grapher/per-capita-meat-consumption-by-type-kilograms-per-year.
# ghg-per-kg-poore.csv: Our World in Data. (2018). "Greenhouse gas emissions per kilogram of food product". Retrieved from https://ourworldindata.org/environmental-impacts-of-food.
# population-and-demography.csv: Our World in Data. (2022). "Population". Retrieved from https://ourworldindata.org/population. 

### TODO: Add fish consumption data
### TODO: Impute missing values for countries based on regional per capita averages
### TODO: Add economy of scale adjustment for per capita meat emissions