#### PREPARE WORKSPACE ####

# Moving working directory to main folder
setwd("../")

# Read in data
meat_consumption_df <- read.csv(
  'data/per-capita-meat-consumption-by-type-kilograms-per-year.csv')