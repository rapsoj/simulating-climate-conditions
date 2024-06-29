#### PREPARE WORKSPACE ####

# Import utils
source("data/utils/geography.R")
source("data/utils/impute_by_geography.R")

# Load libraries
library(dplyr)
library(readxl)


#### LOAD AND CLEAN DATA ####

# Load global CO2 equivalent emissions data
ghg_df <- read.csv('data/total-ghg-emissions.csv') %>%
  # Rename columns
  dplyr::rename('co2eq' = Annual.greenhouse.gas.emissions.in.CO..equivalents) %>%
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

# Load data on electric car stock shares
stock_df <- read.csv('data/share-car-stocks-electric.csv') %>%
  # Group by country
  group_by(Code) %>%
  # Filter to most recent year
  filter(Year == max(Year)) %>%
  # Ungroup data
  ungroup() %>%
  # Select columns of interest
  select(Code, Share.of.car.stocks.that.are.electric)

# Load data on register vehicles
reg_vehicles_df <- read.csv('data/registered-vehicles-per-1000-people.csv') %>%
  # Group by country
  group_by(Code) %>%
  # Filter to most recent year
  filter(Year == max(Year)) %>%
  # Ungroup data
  ungroup() %>%
  # Select columns of interest
  select(Code, Registered.vehicles.per.1.000.people)

# Load data on carbon intensity of electricity
ghg_elect_df <- read.csv('data/carbon-intensity-electricity.csv') %>%
  # Group by country
  group_by(Code) %>%
  # Filter to most recent year
  filter(Year == max(Year)) %>%
  # Ungroup data
  ungroup() %>%
  # Select columns of interest
  select(Code, Carbon.intensity.of.electricity...gCO2.kWh)

# Load data on urban passenger vehicle kilometers
vehicle_km_urb_df <- read.csv('data/ITF_OUTLOOK_2023_URB_PASS_VKM_27062024122044852.csv') %>%
  # Filter to passenger cars
  filter(Transport.mode == 'Passenger cars') %>%
  # Filter to most recent data
  filter(Time == 2022) %>%
  # Filter to current scenario
  filter(SCENARIO == 'CURRENT') %>%
  # Convert to actual units
  mutate(Value = Value * 1e9) %>%
  # Rename value
  rename(vehicle_km_urb = Value) %>%
  # Select variables of interest
  select(Country.group, vehicle_km_urb)

# Load data on regional passenger vehicle kilometers
vehicle_km_reg_df <- read.csv('data/ITF_OUTLOOK_2023_REG_PASS_VKM_28062024134235369.csv') %>%
  # Filter to passenger cars
  filter(Transport.mode == 'Passenger cars') %>%
  # Filter to most recent data
  filter(Time == 2022) %>%
  # Filter to current scenario
  filter(SCENARIO == 'CURRENT') %>%
  # Convert to actual units
  mutate(Value = Value * 1e9) %>%
  # Rename value
  rename(vehicle_km_reg = Value) %>%
  # Select variables of interest
  select(Country.group, vehicle_km_reg)

# Load data on intercity passenger vehicle kilometers
vehicle_km_int_df <- read.csv('data/ITF_OUTLOOK_2023_INT_PASS_VKM_28062024134320949.csv') %>%
  # Filter to passenger cars
  filter(Transport.mode == 'Passenger cars') %>%
  # Filter to most recent data
  filter(Time == 2022) %>%
  # Filter to current scenario
  filter(SCENARIO == 'CURRENT') %>%
  # Convert to actual units
  mutate(Value = Value * 1e9) %>%
  # Rename value
  rename(vehicle_km_int = Value) %>%
  # Select variables of interest
  select(Country.group, vehicle_km_int)

# Add statistics for average vehicle energy consumption in MJ/km
energy_ev <- 0.62

# Add statistics for converting units
mj_to_kwh <- 0.277777778

# Add statistic for average gas/diesel vehicle GHGs in gCO2/km
ghg_gas = 170


#### CALCULATE IMPACT OF MITIGANT #####

# Define valid variables
country_vars <- ghg_df$Code
percent_reduction_vars <- c(0:100)

# Write printable versions of variable names
country_print <- setNames(as.list(ghg_df$Entity), ghg_df$Code)

# Calculate total vehicle kilometers
total_vehicle_km <- vehicle_km_urb_df %>%
  # Merge with region vehicle kilometers
  merge(vehicle_km_reg_df) %>%
  # Merge with intercity vehicle kilometers
  merge(vehicle_km_int_df) %>%
  # Sum total vehicle kilometers
  mutate(total_vehicle_km = vehicle_km_urb + vehicle_km_reg + vehicle_km_int)

# Calculate GHGs produced by an EV
ghg_per_ev <- pop_df %>%
  # Merge number of registered vehicles
  merge(reg_vehicles_df, by='Code', all.x=T) %>%
  # Merge carbon intensity of electricity data
  merge(ghg_elect_df, by='Code', all.x=T) %>%
  # Merge percent stock EV
  merge(stock_df, by='Code', all.x=T) %>%
  # Impute missing columns
  impute_by_geography(c('Registered.vehicles.per.1.000.people',
                        'Carbon.intensity.of.electricity...gCO2.kWh',
                        'Share.of.car.stocks.that.are.electric')) %>%
  # Edit columns
  mutate(
    # Identify OECD region
    oecd_region = case_when(
      continent == 'Europe' ~ 'Europe',
      region == 'Central Asia' ~ 'East and Northeast Asia',
      region == 'East Asia' ~ 'East and Northeast Asia',
      region == 'South America' ~ 'Latin America and the Caribbean',
      region == 'Central America' ~ 'Latin America and the Caribbean',
      Code == 'MEX' ~ 'Latin America and the Caribbean',
      region == 'Caribbean' ~ 'Latin America and the Caribbean',
      region == 'West Asia' ~ 'Middle East and North Africa',
      region == 'North Africa' ~ 'Middle East and North Africa',
      region == 'Southeast Asia' ~ 'Southeast Asia',
      continent == 'Africa' ~ 'Sub-Saharan Africa',
      region == 'South Asia' ~ 'South and Southwest Asia',
      region == 'Oceania Islands' ~ 'Sub-Saharan Africa',
      region == 'Oceania Main' ~ 'United States of America, Canada, Australia and New Zealand',
      Code == 'CAN' ~ 'United States of America, Canada, Australia and New Zealand',
      Code == 'USA' ~ 'United States of America, Canada, Australia and New Zealand'),
    # Calculate total number of vehicles
    total_vehicles = Registered.vehicles.per.1.000.people * pop / 1000) %>%
  # Merge total vehicle kilometers data
  merge(total_vehicle_km, by.x='oecd_region', by.y='Country.group', all.x=T) %>%
  # Group by OECD region
  group_by(oecd_region) %>%
  # Calculate average vehicle kilometers per vehicle
  mutate(
    total_vehicles_oecd = sum(total_vehicles),
    vehicle_km_car = total_vehicle_km / total_vehicles_oecd) %>%
  # Ungroup data
  ungroup() %>%
  # Edit columns
  mutate(
    # Calculate total energy use for EVs
    energy_ev_total_MJ = energy_ev * vehicle_km_car,
    # Calculate GHGs from EVs
    ghg_per_ev = Carbon.intensity.of.electricity...gCO2.kWh * energy_ev_total_MJ * mj_to_kwh, 
    # Calculate GHGs from gas/diesel cars
    ghg_per_gas = ghg_gas * vehicle_km_car,
    # Calculate difference between EV and gas/diesel emissions
    ghg_difference = ghg_per_gas - ghg_per_ev,
    # Set stock share to zero if no data
    Share.of.car.stocks.that.are.electric = ifelse(is.na(Share.of.car.stocks.that.are.electric), 0, Share.of.car.stocks.that.are.electric),
    # Calculate number of electric vehicles
    total_evs = total_vehicles * (Share.of.car.stocks.that.are.electric / 100),
    # Calculate number of gas vehicles
    total_gas = total_vehicles * (1 - Share.of.car.stocks.that.are.electric / 100),
    # Calculate total vehicle emissions
    ghg_vehicle = total_evs * ghg_per_ev + total_gas * ghg_per_gas)

# Write function to calculate impact of mitigant
replace_cars_with_ev <- function(country, percent_replacement){
  # Apply replacement to total vehicle emission data
  impact_tonnes <- ghg_per_ev[ghg_per_ev$Code %in% country, 'total_gas'] *
    (percent_replacement / 100) * ghg_per_ev[ghg_per_ev$Code %in% country, 'ghg_difference'] / 1e6
  impact_million_tonnes <- impact_tonnes / 1e6
  # Compare to current emissions
  country_emissions <- ghg_df[ghg_df$Code %in% country, 'co2eq']
  global_emissions <- ghg_df[ghg_df$Code == 'OWID_WRL', 'co2eq']
  percent_impact_country <- impact_tonnes / country_emissions * 100
  percent_impact_global <- impact_tonnes / global_emissions * 100
  # Print result
  result <- c(
    paste0(
      'The impact of replacing ', country_print[country],
      ' internal combustion engine vehicles with electic vehicles by ',
      percent_replacement, '% is a reduction of ',
      round(impact_million_tonnes, 2), ' million tonnes CO2 equivalent or ',
      round(percent_impact_country, 2), '% of total ', country_print[country], 
      ' emissions.'),
    paste0(
      'This is ', round(percent_impact_global, 2), '% of global emissions.')
  )
  return(result)}

# Output dataframe with total and per capita values by country
write.csv(ghg_per_ev[, c('total_gas', 'ghg_difference)], 'mitigants/output/replace_cars_with_ev.csv', row.names=FALSE)


#### SOURCES ####

# total-ghg-emissions.csv: Our World in Data. (2023). "Per capita CO₂ emissions". Retrieved from https://ourworldindata.org/co2-and-greenhouse-gas-emissions.
# population-and-demography.csv: Our World in Data. (2022). "Population". Retrieved from https://ourworldindata.org/population. 

# - carbon-intensity-electricity.csv: https://ourworldindata.org/grapher/carbon-intensity-electricity [ghg_elec]
# - ITF_OUTLOOK_2023_URB_PASS_VKM_27062024122044852: https://stats.oecd.org/Index.aspx?DataSetCode=ITF_OUTLOOK_2023_URB_PASS_VKM, https://stats.oecd.org/Index.aspx?DataSetCode=ITF_OUTLOOK_2023_REG_PASS_VKM, https://stats.oecd.org/Index.aspx?DataSetCode=ITF_OUTLOOK_2023_INT_PASS_VKM, https://stats.oecd.org/Index.aspx?DataSetCode=ITF_PASSENGER_TRANSPORT [car_km]
# - registered-vehicles-per-1000-people.csv: https://ourworldindata.org/grapher/registered-vehicles-per-1000-people [n_cars]
# - essd_ghg_data.xlsx: https://essd.copernicus.org/articles/13/5213/2021/ [ghg_cars]
# - share-car-stocks-electric.csv: https://ourworldindata.org/electric-car-sales [p_ev]
# https://core.ac.uk/download/pdf/1589782.pdf [energy_ev, energy_gas]
# https://ourworldindata.org/travel-carbon-footprint


### TODO: Use fuel economy data to calculate non-EV emissions instead of emissions