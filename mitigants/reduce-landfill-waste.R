# Load libraries
library(dplyr)


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

# These are data taken from the Global Waste Index 2022, all figures are in 
# kg per capita
landfill_per_cap <- subset(read.csv("data/global_waste_index_2022.csv"),
                             select = c("Country", "Landfill"))
landfill_per_cap <- landfill_per_cap %>% 
  rename('Entity' = Country)
# Remove "kg" from the Landfill column
landfill_per_cap$Landfill <- gsub(" kg", "", landfill_per_cap$Landfill)
# Convert Landfill column to numeric
landfill_per_cap$Landfill <- as.numeric(landfill_per_cap$Landfill)
landfill_per_cap <- landfill_per_cap %>%
  rename('landfill_kg_per_cap' = Landfill)

# Rename the countries that contradict names in ghg_df
# USA = United States
# Slovak Republic = Slovakia
# Czech Republic = Czechia
landfill_per_cap <- landfill_per_cap %>%
  mutate(Entity = case_when(
    Entity == "USA" ~ "United States",
    Entity == "Slovak Republic" ~ "Slovakia",
    Entity == "Czech Republic" ~ "Czechia",
    TRUE ~ Entity
  ))


# GOV UK experimental statistics:
# Residual waste emissions from landfill have decreased from 1.8 million tonnes
# CO2e in 2016 (from 3.7 million tonnes of waste) to 0.9 million tonnes in 2020 
# (from 1.7 million tonnes of waste).

# Calculating kg of CO2-eq per kg of landfill waste (most recent figures)
co2eq_per_kg <- (0.9 * 1000 * 1000000) / (1.7 * 1000 * 1000000)






#### CALCULATE IMPACT OF MITIGANT #####

# Define valid variables
region_vars <- ghg_df$Code
percent_reduction_vars <- c(0:100)

# Write printable versions of variable names
region_print <- setNames(as.list(ghg_df$Entity), ghg_df$Code)

# Merge landfill and population dataframes
total_landfill <- merge(
  pop_df, landfill_per_cap, 
  by = 'Entity', all.x=T)
# Filter to just those countries included in Global Waste Index 2022
total_landfill = total_landfill[!is.na(total_landfill$landfill_kg_per_cap), ] %>%
  # append column with total landfill waste
  mutate(total_landfill_waste = pop * landfill_kg_per_cap) %>%
  # append column with total landfill emissions
  mutate(landfill_co2eq = total_landfill_waste * co2eq_per_kg)

# Filter dataframe to just Entity, Code, and emissions
total_landfill <- subset(total_landfill,
                    select = c("Entity", "Code", "landfill_co2eq"))
# Add in missing countries to match the ghg_df 
total_landfill <- subset(merge(ghg_df, total_landfill, by = c("Entity", "Code"), all.x = TRUE),
                    select = c("Entity", "Code", "landfill_co2eq"))


# Write function to calculate impact of mitigant
reduce_landfill_waste <- function(region, percent_reduction){
  # Apply reduction to total plastic waste emission data
  old_emissions <- total_landfill[
    total_landfill$Code %in% region, "landfill_co2eq"]
  new_emissions <- total_landfill[
    total_landfill$Code %in% region, "landfill_co2eq"] *
    (100 - percent_reduction) / 100
  impact <- old_emissions - new_emissions
  impact_million_tonnes <- impact / 1000000000
  # Compare to current emissions
  country_emissions <- ghg_df[ghg_df$Code %in% region, 'co2eq']
  global_emissions <- ghg_df[ghg_df$Code == 'OWID_WRL', 'co2eq']
  percent_impact_country <- impact / country_emissions * 100
  percent_impact_global <- impact / global_emissions * 100
  # Organise outputs
  output_df <- data.frame("Code" = region,
                          "Entity" = ghg_df[
                            ghg_df$Code %in% region,
                          ]$Entity,
                          "old_emissions" = old_emissions,
                          "new_emissions" = new_emissions,
                          "impact" = impact,
                          "impact_million_tonnes" = impact_million_tonnes)
  return(output_df)
}   


#### SOURCES ####

# total-ghg-emissions.csv: Our World in Data. (2023). "Per capita CO₂ emissions". Retrieved from https://ourworldindata.org/co2-and-greenhouse-gas-emissions.
# population.csv: Our World in Data. (2022). "Population". Retrieved from https://ourworldindata.org/population. 
# global_waste_index_2022.csv: Sensoneo. (2022). "Global Waste Index 2022". Retrieved from https://sensoneo.com/global-waste-index/
# GovUK experimental statistics: https://assets.publishing.service.gov.uk/media/63974500e90e077c329444f0/Statistics_on_carbon_emmisions_Waste_Households_England_v8_2018.pdf. Page 4. 



# TO DO:
# Find better figure for co2equivalent per kg of landfill
# Countries lost in the merge: "Czech Republic", "Slovak Republic", "USA"
