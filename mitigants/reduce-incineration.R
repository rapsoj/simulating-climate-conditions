setwd("..//")

library(dplyr)

source("data/utils/geography.R")

pop_df <- read.csv('data/population.csv') %>%
  dplyr::rename('population' = Population..historical.estimates.) %>%
  filter(Year == max(Year)) %>%
  filter(Code != '') %>%
  filter(Code %in% ghg_df$Code) %>%
  merge(stack(regions), by.x = "Code", by.y = "values", all.x = T) %>%
  dplyr::rename('region' = ind) %>%
  merge(stack(continents), by.x = "region", by.y = "values", all.x = T) %>%
  dplyr::rename('continent' = ind)

ghg_df <- read.csv('data/total-ghg-emissions.csv') %>%
  dplyr::rename('co2eq' = Annual.greenhouse.gas.emissions.in.CO..equivalents) %>%
  mutate(co2eq = co2eq * 1000) %>%
  filter(Year == max(Year)) %>%
  filter(Code != '') %>%
  merge(stack(regions), by.x = "Code", by.y = "values", all.x = T) %>%
  dplyr::rename('region' = ind) %>%
  merge(stack(continents), by.x = "region", by.y = "values", all.x = T) %>%
  dplyr::rename('continent' = ind)

inceneration <- read.csv('data/global_waste_index_2022.csv')

inceneration_df = subset(inceneration, select = -c(Rank, Rank.2019, Waste.Generated, Recycling, Landfill, Open.Dump, Unaccounted.Waste, Recycled...Generated, Final.Score)) %>%
  dplyr::rename('Incinerated_waste_per_Person' = Incineration) %>%
  dplyr::rename('Entity' = Country)

country_vars <- ghg_df$Code
percent_reduction_vars <- c(0:100)

Total_inceneration <- merge(
  pop_df, ghg_df[,c('Code',  'co2eq' )]) %>%
  merge(inceneration_df[,c('Entity', 'Incinerated_waste_per_Person')], by='Entity', all.x=T) %>%
  mutate(
    Incinerated_waste_per_Person = substr(Incinerated_waste_per_Person, 1, nchar(Incinerated_waste_per_Person) - 3)
  ) %>%
  mutate(
    Incinerated_waste_per_Person = as.integer(Incinerated_waste_per_Person)) %>%
  group_by(region) %>%
  mutate(
    impute_flag_inc = ifelse(is.na(Incinerated_waste_per_Person), "*"," "),
    Incinerated_waste_per_Person = if_else(impute_flag_inc == "*", median(Incinerated_waste_per_Person, na.rm = T), Incinerated_waste_per_Person)) %>%
  ungroup() %>%
  group_by(continent) %>%
  mutate(
    impute_flag_inc = ifelse(is.na(Incinerated_waste_per_Person), "**",Incinerated_waste_per_Person),
    Incinerated_waste_per_Person = if_else(impute_flag_inc == "**", median(Incinerated_waste_per_Person, na.rm = T), Incinerated_waste_per_Person))


reduce_incineration <- function(Total_inceneration, percent_reduction_vars){
  country_emissions <- ghg_df[ghg_df$Code %in% 'Entity', 'co2eq']
  global_emissions <- ghg_df[ghg_df$Code == 'OWID_WRL', 'co2eq']
  percent_nat_reduction <- (country_emissions - (population * Incinerated_waste_per_Person * percent_reduction_vars)) / country_emissions 
  total_nat_reduction <- (country_emissions - (population * Incinerated_waste_per_Person * percent_reduction_vars))/ 1000
  total_global_reduction <- global_emissions - (population * Incinerated_waste_per_Person * percent_reduction_vars))/ 1000
  percent_global_reduction <- (global_emissions - (population * Incinerated_waste_per_Person * percent_reduction_vars)) / global_emissions
}  

# Write printable versions of variable names
country_print <- setNames(as.list(ghg_df$Entity), ghg_df$Code)

# Write function to calculate impact of mitigant
reduce_incineration <- function(country, percent_reduction){
  # Apply reduction to total flight emission data
  old_emissions <- Total_inceneration[
    Total_inceneration$Code %in% country, 'co2eq']
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
    'The impact of reducing ', country_print[country], ' incineration by ',
    percent_reduction, '% is a reduction of ',
    round(impact_million_tonnes, 2), ' million tonnes CO2 equivalent or ',
    round(percent_impact_country, 2), '% of total ', country_print[country], 
    ' emissions.'))
  print(paste0(
    'This is ', round(percent_impact_global, 2), '% of global emissions.'))
}




