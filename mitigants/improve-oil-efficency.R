setwd("..//")

library(dplyr)

source("data/utils/geography.R")

pop_df <- read.csv('data/population.csv') %>%
  # Rename columns
  dplyr::rename('Population' = Population..historical.estimates.) %>%
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
  dplyr::rename('continent' = ind) %>%
  dplyr::rename('Country' = Entity)

ghg_df <- read.csv('data/total-ghg-emissions.csv') %>%
  # Rename columns
  rename('co2eq' = Annual.greenhouse.gas.emissions.in.CO..equivalents) %>%
  # Convert from tonnes to kilograms
  mutate(co2eq = co2eq * 1000) %>%
  rename('Country' = Entity) %>%
  # Filter to most recent year
  filter(Year == max(Year)) %>%
  # Filter to country-level
  filter(Code != '') %>%
  merge(stack(regions), by.x = "Code", by.y = "values", all.x = T) %>%
  # Rename columns
  dplyr::rename('region' = ind) %>%
  # Link continents
  merge(stack(continents), by.x = "region", by.y = "values", all.x = T) %>%
  # Rename columns
  dplyr::rename('continent' = ind)

Numcars <- read.csv('data/Number of Cars Per Country.csv') %>%
  mutate(Total_Car = gsub(",","", Total_Car))

EU_dieselandpetrol <- read.csv('data/road_eqs_carpda_page_spreadsheet.csv') %>%
  dplyr::rename('Country' = GEO..Labels.) %>%
  dplyr::rename('percent_Petroleum' = X..Petroleum) %>%
  dplyr::rename('percent_Diesel' = X..Diesel)

oil_Eff <- read.csv('data/Improving Oil Efficency - Average Fuel Consumption.csv') %>%
  dplyr::rename('Country' = X ) %>%
  dplyr::rename('km_per_letre' = X.2)

distancetravelled <- read.csv('data/average_distance_by_car.csv') %>%
  dplyr::rename('km_by_car' = km.travelled.by.car.per.annum)

EV_share <- read.csv('data/IEA-EV-dataEV sales shareHistoricalCars.csv') %>%
  filter(year == max(year)) %>%
  filter(parameter == 'EV stock share') %>%
  dplyr::rename('Percent_EV' = value) %>%
  mutate(region = case_when(
    region == 'Korea' ~ 'South Korea',
    region == 'Turkiye' ~ 'Turkey',
    TRUE ~ region))

country_var <- ghg_df$Code
km_increase_in_efficiency <- c(0:20)

total_increase_in_efficiency <- merge(
  ghg_df, Numcars[c('Country', 'Total_Car')], by.x='Country', by.y='Country', all.x = T) %>%
  merge(oil_Eff[,c('Country', 'km_per_letre')], by.x='Country', by.y='Country', all.x = T) %>%
  merge(EV_share[,c('region', 'Percent_EV')], by.x='Country', by.y='region', all.x = T) %>%
  merge(distancetravelled[,c('Country', 'km_by_car')], by.x='Country', by.y='Country', all.x = T) %>%
  merge(pop_df[,c('Country','Population')],by='Country', by.y='Country', all.x = T) %>%
  mutate(Total_Car = as.integer(Total_Car)) %>%
  mutate(car_per_capita = Total_Car/Population) %>%
  group_by(region)%>%
  mutate(impute_flag_car = ifelse(is.na(car_per_capita), "*", ''),
         km_per_car = if_else(
           impute_flag_car == "*", mean(car_per_capita, na.rm = T), car_per_capita)) %>%
  mutate(impute_flag_total_car = ifelse(is.na(Total_Car), "*", ''),
         Total_Car = if_else(
           impute_flag_total_car == "*", mean(Total_Car, na.rm = T), Total_Car)) %>%
  mutate(impute_flag_EV = ifelse(is.na(Percent_EV), "*", ''),
         Percent_EV = if_else(
           impute_flag_EV == "*", mean(Percent_EV, na.rm = T), Percent_EV)) %>%
  ungroup() %>%
  mutate(impute_flag_EV = ifelse(is.na(Percent_EV), "**", '*'),
         Percent_EV = if_else(impute_flag_EV == "**", 'EV_Share$Rest of the World', Percent_EV)) %>%
  group_by(region) %>%
  mutate(
    impute_flag_kmperletre = ifelse(is.na(km_per_letre), "*", ''),
    km_per_letre = if_else(
      impute_flag_kmperletre == "*", mean(km_per_letre, na.rm = T), km_per_letre)) %>%
  ungroup() %>%
  group_by(continent) %>%
  mutate(
    impute_flag_kmperletre = ifelse(is.na(km_per_letre), "**", impute_flag_kmperletre),
    km_per_letre = if_else(
      impute_flag_kmperletre == "**", mean(km_per_letre, na.rm = T), km_per_letre))

  

