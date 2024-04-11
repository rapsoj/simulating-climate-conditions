# Load libraries
library(dplyr)


#### LOAD AND CLEAN DATA ####

# Load global CO2 equivalent emissions data
ghg_df <- read.csv('total-ghg-emissions.csv') %>%
  # Rename columns
  rename('co2eq' = Annual.greenhouse.gas.emissions.in.CO..equivalents) %>%
  # Convert from tonnes to kilograms
  mutate(co2eq = co2eq * 1000) %>%
  # Filter to most recent year
  filter(Year == max(Year)) %>%
  # Filter to country-level
  filter(Code != '')

# Load population data
pop_df <- read.csv('population.csv') %>%
  # Rename columns
  rename('pop' = Population..historical.estimates.) %>%
  # Filter to most recent year
  filter(Year == max(Year)) %>%
  # Filter to country-level
  filter(Code != '')


# Load and filter total plastic waste emissions data
total_plastic_emissions <- subset(read.csv("greenhouse-gas-emissions-from-plastics.csv"),
                                  select = c("Entity", "Year", "All.greenhouse.gases"))
total_plastic_emissions <- total_plastic_emissions %>%
  # filter to most recent year
  filter(Year == max(Year)) %>%
  # filter to just end-of-life (i.e. waste)
  filter(Entity == "End-of-life") %>%
  # convert tonnes into kg
  mutate(All.greenhouse.gases = All.greenhouse.gases * 1000) %>%
  # rename columns
  rename('co2eq' = All.greenhouse.gases)


# Load and filter country per-capita plastic waste data
per_cap_plastic_waste <- read.csv("plastic-waste-per-capita.csv")
per_cap_plastic_waste <- per_cap_plastic_waste %>%
  # rename columns
  rename('per_cap_plastic_waste' = Per.capita.plastic.waste..kg.person.day.) %>%
  # filter to most recent year
  filter(Year == max(Year)) %>%
  # filter to country-level
  filter(Code != "") %>%
  # multiply by 365 to get per-cap plastic waste in kg per year (instead of per day)
  mutate(per_cap_plastic_waste = per_cap_plastic_waste * 365)

# Merge population and per capita plastic waste dataframes
total_plastic_waste <- merge(
  pop_df, per_cap_plastic_waste[, c(
    'Code', 'per_cap_plastic_waste'
  )], by = 'Code', all.x=T) %>%
  # add columns with total plastic emissions
  mutate(total_plastic_waste_kg = pop * per_cap_plastic_waste)


# Calculate the total kg of plastic waste, globally
global_plastic_waste_kg <- sum(total_plastic_waste_kg$total_plastic_waste_kg,
                                na.rm = TRUE)
# Calculate emissions per kg of plastic waste (kgs of co2eq per kg plastic waste)
emissions_per_kg <- total_plastic_emissions$co2eq/global_plastic_waste_kg

# Add emissions to the population and per cap waste dataframe
total_plastic_waste$total_plastic_waste_co2eq <- 
  total_plastic_waste$total_plastic_waste_kg * emissions_per_kg



#### CALCULATE IMPACT OF MITIGANT #####

# Define valid variables
region_vars <- ghg_df$Code
percent_reduction_vars <- c(0:100)

# Write printable versions of variable names
region_print <- setNames(as.list(ghg_df$Entity), ghg_df$Code)

# Write function to calculate impact of mitigant
reduce_plastic_waste <- function(region, percent_reduction){
  # Apply reduction to total plastic waste emission data
  old_emissions <- total_plastic_waste[
    total_plastic_waste$Code %in% region, "total_plastic_waste_co2eq"]
  new_emissions <- total_plastic_waste[
    total_plastic_waste$Code %in% region, "total_plastic_waste_co2eq"] *
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
    'The impact of reducing ', region_print[region], 'plastic waste by ', 
    percent_reduction, '% is a reduction of ',
    round(impact_million_tonnes, 2), ' million tonnes CO2 equivalent or ',
    round(percent_impact_country, 2), '% of total ', region_print[region], 
    ' emissions.'))
  print(paste0(
    'This is ', round(percent_impact_global, 2), '% of global emissions.'))
}                              


#### SOURCES ####

# total-ghg-emissions.csv: Our World in Data. (2023). "Per capita CO₂ emissions". Retrieved from https://ourworldindata.org/co2-and-greenhouse-gas-emissions.
# population.csv: Our World in Data. (2022). "Population". Retrieved from https://ourworldindata.org/population. 
# greenhouse-gas-emissions-from-plastics.csv. (2022). "Greenhouse gas emissions from plastics". Retrieved from https://ourworldindata.org/ghg-emissions-plastics
# plastic-waste-per-capita.csv. (2010). "Per capita plastic waste". Retrieved from https://ourworldindata.org/grapher/plastic-waste-per-capita?tab=table


# To do:
# Find more up-to-date data on plastic waste per capita 
# See if you can separate end of life emissions into incineration, recycling, landfill, etc.
