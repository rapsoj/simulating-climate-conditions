# Disappointingly, this method takes us closer to expected ghg emissions for food waste
# and here we only account for the co2 released, not methane which is quite a significant omissions

# Load libraries
library(dplyr)


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


# Load and clean food waste data (measured in kgs)
food_waste <- read.csv("data/food-waste-per-capita.csv") %>%
  # Filter to country level
  filter(Code != "") %>%
  # Filter to most recent year
  filter(Year == max(Year)) %>%
  # Rename columns
  rename('retail' = X12.3.1...Food.waste.per.capita..KG....AG_FOOD_WST_PC...Retail,
         'out_of_home' = X12.3.1...Food.waste.per.capita..KG....AG_FOOD_WST_PC...Out.of.home.consumption,
         'household' = X12.3.1...Food.waste.per.capita..KG....AG_FOOD_WST_PC...Households) %>%
  # Add total column
  mutate(total = retail + out_of_home + household)
food_waste <- subset(food_waste,
                     select = c("Entity", "Code", "total")) %>%
  rename('food_waste_per_cap' = total)



#### CALCULATE IMPACT OF MITIGANT #####

# Define valid variables
region_vars <- ghg_df$Code
percent_reduction_vars <- c(0:100)

# Write printable versions of variable names
region_print <- setNames(as.list(ghg_df$Entity), ghg_df$Code)

# Add population and emissions data
total_food_waste <- merge(
  pop_df, food_waste[, c('Code', 'food_waste_per_cap')], 
  by = "Code", all.x=T) %>%
  # add calculate total foodwaste 
  mutate(total_food_waste = pop * food_waste_per_cap)

# Calculate emissions per kg of food waste
co2eq_per_kg_food <- (sum(ghg_df$co2eq, na.rm = TRUE) * 0.06) /
  sum(total_food_waste$total_food_waste, na.rm = TRUE)
# Add in emissions data 
total_food_waste$food_co2eq <- total_food_waste$total_food_waste * co2_per_kg_food


# Filter dataframe to just Entity, Code, and emissions
total_food_waste <- subset(total_food_waste,
                         select = c("Entity", "Code", "food_co2eq"))
# Add in missing countries to match the ghg_df 
total_food_waste <- subset(merge(ghg_df, total_food_waste, 
                                 by = c("Entity", "Code"), all.x = TRUE),
                         select = c("Entity", "Code", "food_co2eq"))


# Write function to calculate impact of mitigant
reduce_food_waste <- function(region, percent_reduction){
  # Apply reduction to cement production emission data
  old_emissions <- total_food_waste[
    total_food_waste$Code %in% region, "food_co2eq"]
  new_emissions <- total_food_waste[
    total_food_waste$Code %in% region, "food_co2eq"] *
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
# greenhouse-gas-emissions-from-plastics.csv. (2022). "Greenhouse gas emissions from plastics". Retrieved from https://ourworldindata.org/ghg-emissions-plastics
# food-waste-per-capita.csv: Our World in Data. (2023). "Food waste per capita, 2019". Retrieved from https://ourworldindata.org/grapher/food-waste-per-capita
# Food waste responsible for 6% of total GHG emissions. Our World in Data. (2020). Retrieved from https://ourworldindata.org/food-waste-emissions#:~:text=Food%20waste%20accounts%20for%20around,6%25%20of%20total%20global%20emissions.&text=Food%20production%20accounts%20for%20around,of%20global%20greenhouse%20gas%20emissions.&text=This%20is%20a%20lot%2C%20but,is%20a%20basic%20human%20need.

# TO DO:
# Split food waste emissions by retail/out-of-home consumption/household food waste (this is already included in the data, summed to get total)




