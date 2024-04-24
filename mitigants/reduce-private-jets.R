#### PREPARE WORKSPACE ####

# Move working directory to main folder
setwd("../")

# Import utils
source("data/utils/geography.R")

# Load libraries
library(dplyr)
library(stringr)
library(magrittr)
library(tidyr)

# Define a function to extract countries and values from each row
extract_country_values <- function(text) {
  countries <- str_extract_all(text, "[A-Za-z\\s().]+(?=\\s\\d)") %>%
    lapply(function(x) gsub("^\\s+", "", x)) %>%
    unlist()
  
  jets_owned <- str_extract_all(text, "(?<=\\w|\\))\\s(\\d+)") %>%
    lapply(as.integer) %>%
    unlist()
  jets_owned <- jets_owned[seq(1, length(jets_owned), by = 4)]
  
  data.frame(country = countries, jets_owned = jets_owned)
}


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

# Load private jet ownership by country
ownership_df <- read.csv('data/jet-ownership.csv', header=F) %>%
  # Remove extra text at beginning
  mutate(text = sub("^[^.]*\\. ", "", V1)) %>%
  # Perform row-wise operations
  rowwise() %>%
  # Extract country names from text
  mutate(country_values = list(extract_country_values(text))) %>%
  # Extract total jet registries from text
  unnest(country_values) %>%
  # Select columns of interest
  select(country, jets_owned) %>%
  # Rename non-standard country names
  mutate(country = case_when(
    country == "Czech Republic" ~ "Czechia",
    country == "Dem. Republic of Congo" ~ "Democratic Republic of Congo",
    country == "Ivoire" ~ "Cote d'Ivoire",
    country == "Macedonia" ~ "North Macedonia",
    country == "Macau" ~ "Macao",
    country == "Mali Republic" ~ "Mali",
    country == "Russian Federation" ~ "Russia",
    country == "Seychelles Islands" ~ "Seychelles",
    country == "Slovak Republic" ~ "Slovakia",
    country == "Swaziland" ~ "Eswatini",
    country == "Virgin Islands (British)" ~ "British Virgin Islands",
    TRUE ~ country
  )) %>%
  # Link countries to country codes
  merge(ghg_df[, c('Code', 'Entity')], by.x = "country",
        by.y = "Entity", all.x = T) %>%
  # Link regions
  merge(stack(regions), by.x = "Code", by.y = "values", all.x = T) %>%
  # Rename columns
  dplyr::rename('region' = ind) %>%
  # Link continents
  merge(stack(continents), by.x = "region", by.y = "values", all.x = T) %>%
  # Filter to regions that have GHG emissions data 
  filter(Code %in% ghg_df$Code) %>%
  # Rename columns
  dplyr::rename(
    'Entity' = country,
    'continent' = ind)

# Load private jet flights by country
flights_df <- read.csv('data/jet-flights.csv', dec = ".") %>%
  # Rename columns
  dplyr::rename(
    'annual_high' = 'X52.week.high',
    'annual_low' = 'X52..Week.low') %>%
  # Calculate average annual flights using average of year low and high
  mutate(
    # Convert 52-week high column to integers
    annual_high = as.integer(gsub(",", "", annual_high)),
    # Convert 52-week low column to integers
    annual_low = as.integer(gsub(",", "", annual_low)),
    # Take average of annual weekly high and low and convert to years
    avg_annual_flights = (annual_high + annual_low) / 2 * 52,
    # Create column for country code
    Code = case_when(
      Market == "Global" ~ "OWID_WRL",
      Market == "USA" ~ "USA",
      Market == "UK" ~ "GBR",
      Market == "Germany" ~ "DEU",
      Market == "France" ~ "FRA",
      Market == "Switzerland" ~ "CHE",
      Market == "Italy" ~ "ITA"),
    # Replace non-standard region names
    Market = case_when(
      Market == "Middle East" ~ "West Asia",
      TRUE ~ Market))

# Add statistic for average flight time in minutes
avg_flight_time_stat <- 71.77

# Add statistic for hourly CO2 emissions from private jets in tonnes CO2eq
hourly_emissions_stat <- 2


#### CALCULATE IMPACT OF MITIGANT #####

# Define valid variables
country_vars <- ghg_df$Code
percent_reduction_vars <- c(0:100)

# Write printable versions of variable names
country_print <- setNames(as.list(ghg_df$Entity), ghg_df$Code)

# Calculate total jets owned by region
total_ownership_region <- ownership_df %>%
  # Group by region
  group_by(region) %>%
  summarize(jets_owned = sum(jets_owned, na.rm = TRUE))

# Calculate total jets owned by continent
total_ownership_continent <- ownership_df %>%
  # Group by continent
  group_by(continent) %>%
  summarize(jets_owned = sum(jets_owned, na.rm = TRUE))

# Calculate average departures per private jet by country
flights_per_jet_country <- merge(ownership_df, flights_df) %>%
  # Calculate average flights per jet
  mutate(avg_flights_per_jet_country = avg_annual_flights / jets_owned)

# Calculate average departures per private jet by region
flights_per_jet_region <- merge(
  total_ownership_region, flights_df, by.x = 'region', by.y = 'Market') %>%
  # Calculate average flights per jet
  mutate(avg_flights_per_jet_region = avg_annual_flights / jets_owned)

# Calculate average departures per private jet by continent
flights_per_jet_continent <- merge(
  total_ownership_continent, flights_df, by.x = 'continent', by.y = 'Market') %>%
  # Calculate average flights per jet
  mutate(avg_flights_per_jet_continent = avg_annual_flights / jets_owned)

# Calculate private jets owned per capita
jets_per_capita <- merge(
  pop_df, ownership_df, all.x=TRUE) %>%
  # Calculate private jets owned per capita
  mutate(jets_owned_per_capita = jets_owned / pop) %>%
  # Group by region
  group_by(region) %>%
  # Impute missing per capita values using regional averages
  mutate(
    # Add imputation flags
    impute_flag = ifelse(is.na(jets_owned_per_capita), "*", ""),
    # Impute per capita values using regional averages
    jets_owned_per_capita = if_else(
      impute_flag == "*", median(jets_owned_per_capita, na.rm = T), jets_owned_per_capita),
  ) %>%
  # Ungroup data
  ungroup() %>%
  # Group by continent
  group_by(continent) %>%
  # Impute remaining missing per capita values using continent averages
  mutate(
    # Add imputation flags
    impute_flag = ifelse(is.na(jets_owned_per_capita), "**", impute_flag),
    # Impute per capita values using regional averages
    jets_owned_per_capita = if_else(
      impute_flag == "**", median(jets_owned_per_capita, na.rm = T), jets_owned_per_capita),
    # Recalculate jets owned for imputed values
    jets_owned = ifelse(
      impute_flag != "", floor(jets_owned_per_capita * pop), jets_owned),
    # Round jets per capita to zero if total imputed jets is less than 1
    jets_owned_per_capita = ifelse(
      jets_owned_per_capita * pop < 1, 0, jets_owned_per_capita))

# Calculate total private jet emissions estimates for all countries
total_jet_emissions <- merge(jets_per_capita, flights_per_jet_country[
  , c("Code", "avg_flights_per_jet_country")], by="Code", all.x=TRUE) %>%
  # Merge total departures by region
  merge(flights_per_jet_region[
    , c("region", "avg_flights_per_jet_region")], by="region", all.x=TRUE) %>%
  # Merge total departures by continent
  merge(flights_per_jet_continent[
    , c("continent", "avg_flights_per_jet_continent")], by="continent", all.x=TRUE) %>%
  # Edit columns
  mutate(
    # Calculate best estimates of average flights per jet by country
    avg_flights_per_jet = case_when(
      # Use most specific available estimate estimate
      is.na(avg_flights_per_jet_country) == FALSE ~ avg_flights_per_jet_country,
      is.na(avg_flights_per_jet_region) == FALSE ~ avg_flights_per_jet_region,
      is.na(avg_flights_per_jet_continent) == FALSE ~ avg_flights_per_jet_continent,
      # Use South America average for Caribbean and Central American countries
      (region == 'Caribbean') | (region == 'Central America') ~
        flights_per_jet_region[
          flights_per_jet_region$region == 'South America', 'avg_flights_per_jet_region'],
      # Use North American average for Oceania main countries
      region == 'Oceania Main' ~
        flights_per_jet_region[
          flights_per_jet_region$region == 'North America', 'avg_flights_per_jet_region'],
      # Use Africa average for Oceania island countries
      region == 'Oceania Islands' ~
        flights_per_jet_continent[
          flights_per_jet_continent$continent == 'Africa', 'avg_flights_per_jet_continent']),
    # Calculate total private jet flights using number of jets
    total_jet_flights = jets_owned * avg_flights_per_jet,
    # Calculate total flight hours using statistic
    total_flight_hours = total_jet_flights * avg_flight_time_stat,
    # Calculate total jet emissions using statistic
    total_jet_emissions = total_flight_hours * hourly_emissions_stat)

# Write function to calculate impact of mitigant
reduce_private_jets <- function(country, percent_reduction){
  # Apply reduction to total flight emission data
  old_emissions <- total_jet_emissions[
    total_jet_emissions$Code %in% country, 'total_jet_emissions']
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
    'The impact of reducing ', country_print[country], ' private jet usage by ',
    percent_reduction, '% is a reduction of ', round(impact_million_tonnes, 2),
    ' million tonnes CO2 equivalent or ', round(percent_impact_country, 2),
    '% of total ', country_print[country],  ' emissions.'))
  print(paste0(
    'This is ', round(percent_impact_global, 2), '% of global emissions.'))
}


#### SOURCES ####

# total-ghg-emissions.csv: Our World in Data. (2023). "Per capita CO₂ emissions". Retrieved from https://ourworldindata.org/co2-and-greenhouse-gas-emissions.
# population-and-demography.csv: Our World in Data. (2022). "Population". Retrieved from https://ourworldindata.org/population. 
# jet-ownership.csv: Bart International. (2020). "Fleet Report". Retrieved from https://issuu.com/bartintl/docs/bartintl185/s/10280812.
# jet-flights.csv: Private Jet Card Comparisons. (2023). "Private Jet Flight Activity Analysis – 2023 – Week 1". Retrieved from https://privatejetcardcomparisons.com/2023/01/12/private-jet-flight-activity-analysis-2023-week-1/.
# avg_flight_time_stat: Yard. (2022). "Just Plane Wrong: Celebs with the Worst Private Jet Co2 Emissions". Retrieved from https://weareyard.com/insights/worst-celebrity-private-jet-co2-emission-offenders.
# hourly_emissions_stat: Transport and Environment. (2021). "Private jets: can the super-rich supercharge zero-emission aviation?". Retrieved from https://www.transportenvironment.org/discover/private-jets-can-the-super-rich-supercharge-zero-emission-aviation/.