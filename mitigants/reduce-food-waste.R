# NB: The following considers the impact of a reduction of all types of food 
#     waste that is sent to landfill or incinerated.
#     It does not consider the distinction between different types of food waste
#     or the emissions from other forms of waste management.

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

# Load population data
pop_df <- read.csv('data/population.csv') %>%
  # Rename columns
  rename('pop' = Population..historical.estimates.) %>%
  # Filter to most recent year
  filter(Year == max(Year)) %>%
  # Filter to country-level
  filter(Code != '')

# Load continent data
continents <- subset(read.csv("data/continents-according-to-our-world-in-data.csv"),
                     select = c("Entity", "Continent"))


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

# Merge continent and food waste dataframes
food_waste_df <- merge(
  food_waste, continents,
  by = "Entity", all.x = T)


# Create food waste management dataframe
# NB: In the absence of regional specific data on food waste management, regional 
#     data on mixed municipal waste management has been used. In the absence of 
#     regional data on municipal waste management, global municipal waste 
#     management figures have been used.
management <- data.frame("Region" = c("United States",
                                      "United Kindgom",
                                      "Europe",
                                      "Africa",
                                      "Asia",
                                      "Other"),
                         "percent_landfill" = c(56, 40, 23, 90, 63, 37),
                         "percent_incinerated" = c(12, 11, 27, 6, 12, 11))

# Add co2eq per waste management method data (measured in kg per kg food waste)
co2eq_per_kg_food <-  data.frame("landfill" = 0.004,
                                 "incineration" = 0.016)




#### CALCULATE IMPACT OF MITIGANT #####

# Define valid variables
region_vars <- ghg_df$Code
percent_reduction_vars <- c(0:100)

# Write printable versions of variable names
region_print <- setNames(as.list(ghg_df$Entity), ghg_df$Code)

# The next bit is really ugly:
# For ease of merging with management dataframe, want to change the UK and US
# continent names to equal their entity names, and make all regions that aren't
# UK, US, Europe, Africa or Asia to have continent value "Other"
food_waste_df$Continent <- ifelse(food_waste_df$Continent %in% management$Region, 
                                  food_waste_df$Continent, 
                                  "Other")
food_waste_df[which(food_waste_df$Entity == "United States"), ]$Continent <- "United States"
food_waste_df[which(food_waste_df$Entity == "United Kingdom"), ]$Continent <- "United Kingdom"
food_waste_df <- food_waste_df %>%
  rename('Region' = Continent)

# Now merge the food_waste and waste management dataframes
food_waste_df <- merge(food_waste_df, management, by = "Region", all.x = TRUE) %>%
  # Calculate kg of landfilled/incinerated waste
  mutate(landfill_kg = percent_landfill * total / 100,
         incineration_kg = percent_incinerated * total / 100)

# Subset to relevant columns
food_waste_df <- subset(food_waste_df,
                        select = c("Entity", "Code", "total", 
                                   "landfill_kg", "incineration_kg"))

# Merge with population dataframe
total_food_waste <- merge(
  food_waste_df, pop_df[, c(
    'Code', 'pop'
  )], by = 'Code', all.x=T) %>%
  # calculate total landfill and incineration kgs
  mutate(total_landfill = landfill_kg * pop,
         total_incineration = incineration_kg * pop) %>%
  # add emissions data
  mutate(co2eq_kg = total_landfill * co2eq_per_kg_food$landfill +
           total_incineration * co2eq_per_kg_food$incineration)




# Write function to calculate impact of mitigant
reduce_food_waste <- function(region, percent_reduction){
  # Apply reduction to cement production emission data
  old_emissions <- total_food_waste[
    total_food_waste$Code %in% region, "co2eq_kg"]
  new_emissions <- total_food_waste[
    total_food_waste$Code %in% region, "co2eq_kg"] *
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
    'The impact of reducing ', region_print[region], ' food waste by ', 
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
# continents-according-to-our-world-in-data.csv: Our World in Data. (2015). "Continents according to Our World in Data". Retrieved from https://ourworldindata.org/world-region-map-definitions
# food-waste-per-capita.csv: Our World in Data. (2023). "Food waste per capita, 2019". Retrieved from https://ourworldindata.org/grapher/food-waste-per-capita?country=OWID_WRL~CHN~USA~IDN~RUS~BRA~ZAF~AFG~GBR

# muncipal waste management figures: https://bolt.eu/en/blog/what-happens-to-food-waste/#:~:text=90%25%2B%20disposed%20at%20landfills,~6%25%20incinerated%20and%20composted.
# UK waste manegment figures: https://assets.publishing.service.gov.uk/media/5a78d35940f0b6324769a753/pb13540-waste-policy-review110614.pdf
# co2eq per waste management method figures: https://www.sciencedirect.com/science/article/pii/S0306919217309168


# TO DO:
# Properly source the waste management percentages and co2eq figures
# See if you can find more region specific data on waste management for major countries like China, India, Brazil, etc. 
# Clean up the ugly data processing and merging halfway through 
