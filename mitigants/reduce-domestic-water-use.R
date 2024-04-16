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

# Load and clean municipal water withdrawal data
# (make simplifying assumption that all water withdrawn is used)
water_use <- read.csv("municipal-water-withdrawal.csv") %>%
  # rename columns 
  rename('water_use' = Municipal.water.withdrawal) %>%
  # filter to country level
  filter(Code != "") %>%
  # filter to each country's latest year
  group_by(Entity) %>%
  filter(Year == max(Year)) %>%
  ungroup()

# UK Gov Environment Agency household water use emissions figure = 35 Mt co2eq
UK_domestic_water_use_co2eq_kg <- 35000000000 


#### CALCULATE IMPACT OF MITIGANT #####

# Calculate co2eq kg emissions per cubic metre of domestic water
kg_co2eq_per_m3 <- UK_domestic_water_use_co2eq_kg / 
  water_use[which(water_use$Code == "GBR"), ]$water_use

# Define valid variables
region_vars <- ghg_df$Code
percent_reduction_vars <- c(0:100)

# Write printable versions of variable names
region_print <- setNames(as.list(ghg_df$Entity), ghg_df$Code)

# Add emissions data to water_use dataframe
water_use <- data.frame(water_use) %>%
  # add co2eq column
  mutate(co2eq = water_use * kg_co2eq_per_m3)

# Write function to calculate impact of mitigant
# Write function to calculate impact of mitigant
reduce_domestic_water_use <- function(region, percent_reduction){
  # Apply reduction to cement production emission data
  old_emissions <- water_use[
    water_use$Code %in% region, "co2eq"]
  new_emissions <- water_use[
    water_use$Code %in% region, "co2eq"] *
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
    'The impact of reducing ', region_print[region], ' domestic water use by ', 
    percent_reduction, '% is a reduction of ',
    round(impact_million_tonnes, 2), ' million tonnes CO2 equivalent or ',
    round(percent_impact_country, 2), '% of total ', region_print[region], 
    ' emissions.'))
  print(paste0(
    'This is ', round(percent_impact_global, 2), '% of global emissions.'))
}  


#### SOURCES ####

# total-ghg-emissions.csv: Our World in Data. (2023). "Per capita CO₂ emissions". Retrieved from https://ourworldindata.org/co2-and-greenhouse-gas-emissions.
# municipal-water-withdrawal.csv: Our World in Data (2017). "Municipal water withdrawal". Retrieved from https://ourworldindata.org/water-use-stress
# UK_domestic_water_use_co2eq_kg: Environment Agency. (2008). "Greenhouse gas emissions of water supply and demand management options ". Retrieved from https://assets.publishing.service.gov.uk/media/5a7cbfd4e5274a38e5756843/scho0708bofv-e-e.pdf. Page 30.


# TO-DO:
# Get a better/more figures for emissions per m3 of domestic water use 
# Note: I think the current figure includes all emissions from the whole supply-use-disposal system, but check this is the case


