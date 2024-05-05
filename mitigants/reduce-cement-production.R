# Load libraries
library(dplyr)

# Move working directory to main folder
setwd("../")

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

# kg of CO2 per metric tonne of cement production
co2_per_tonne <- 0.6 * 1000

# Load and clean cement production data
cement_production <- subset(read.csv("data/cement-production-by-country-2024.csv"),
                            select = c("country", "CementProduction2022")) %>%
  # rename columns
  rename("Entity" = country, 
         "cement_tonnes" = CementProduction2022) %>%
  # convert KMT to tonnes
  mutate(cement_tonnes = cement_tonnes * 1000) %>%
  # add co2 column
  mutate(cement_co2 = cement_tonnes * co2_per_tonne)

# Add in country codes for use later on
cement_df <- subset(merge(cement_production, ghg_df, by = "Entity", all.x = T),
                    select = c("Entity", "Code", 
                               "cement_tonnes", "cement_co2")) %>%
  # filter to country level
  filter(Code != "")



#### CALCULATE IMPACT OF MITIGANT #####

# Define valid variables
region_vars <- ghg_df$Code
percent_reduction_vars <- c(0:100)

# Write printable versions of variable names
region_print <- setNames(as.list(ghg_df$Entity), ghg_df$Code)

# Write function to calculate impact of mitigant
reduce_cement_production <- function(region, percent_reduction){
  # Apply reduction to cement production emission data
  old_emissions <- cement_df[
    cement_df$Code %in% region, "cement_co2"]
  new_emissions <- cement_df[
    cement_df$Code %in% region, "cement_co2"] *
    (100 - percent_reduction) / 100
  impact <- old_emissions - new_emissions
  impact_million_tonnes <- impact / 1000000000
  # Compare to current emissions
  country_emissions <- ghg_df[ghg_df$Code %in% region, 'co2eq']
  global_emissions <- ghg_df[ghg_df$Code == 'OWID_WRL', 'co2eq']
  percent_impact_country <- impact / country_emissions * 100
  percent_impact_global <- impact / global_emissions * 100
  # Print result
  print(paste0(
    'The impact of reducing ', region_print[region], ' cement production by ', 
    percent_reduction, '% is a reduction of ',
    round(impact_million_tonnes, 2), ' million tonnes CO2 equivalent or ',
    round(percent_impact_country, 2), '% of total ', region_print[region], 
    ' emissions.'))
  print(paste0(
    'This is ', round(percent_impact_global, 2), '% of global emissions.'))
}   


#### SOURCES ####

# total-ghg-emissions.csv: Our World in Data. (2023). "Per capita CO₂ emissions". Retrieved from https://ourworldindata.org/co2-and-greenhouse-gas-emissions.
# cement-production-by-country-2024.csv: United States Geological Survey (2023). "Cement". Retrieved from https://pubs.usgs.gov/periodicals/mcs2023/mcs2023-cement.pdf
# co2_per_tonne: International Energy Agency. (2023). "Tracking Cement > CO2 emissions". Retrieved from https://www.iea.org/energy-system/industry/cement


# TO DO:
# Find figures for non-major countries



                            
                           


