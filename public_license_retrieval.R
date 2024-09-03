# Author: Anna Cobb
# Date: 08/09/2024
# Description: retrieving and potentially processing Chicago public passenger
# vehicle licenses data using the RSocrata package (which apparently the city 
# of Chicago helped write!)

library(RSocrata)
library(tidyverse)
library(dplyr)

# reading in the historical public passenger license data (takes a while)
# df = read.socrata(url = "https://data.cityofchicago.org/resource/tx35-q6ia.json",
#                   app_token = "NRZAZqgR2lywoiW7H1fALVgDa")

write.csv(df,"public_passenger_license_data.csv")
