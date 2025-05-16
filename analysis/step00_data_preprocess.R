# Script for pre-processing the MRoS data
# Data here are from the Jennings et al. 2025 manuscript
# Jennings, K.S., Collins, M., Hatchett, B.J. et al. 
# Machine learning shows a limit to rain-snow partitioning accuracy when using 
# near-surface meteorology. Nat Commun 16, 2929 (2025). 
# https://doi.org/10.1038/s41467-025-58234-2

# Data citation:
# Jennings, Keith; Arienzo, Monica; Collins, Meghan; Hur, Nayoung (2024), 
# “Mountain Rain or Snow Crowdsourced Precipitation Phase Data 2020-2023”, 
# Mendeley Data, V1, doi: 10.17632/x84hy7yky4.1

# Load packages 
library(tidyverse)

# Use the URL from the Mendeley Data link
# If URL broken, go to https://doi.org/10.17632/x84hy7yky4.1
fname = "https://data.mendeley.com/public-files/datasets/x84hy7yky4/files/d4f56dec-1634-462c-8a53-821ccf767029/file_downloaded"

# Read in the data file
df <- read.csv(fname)

# Downselect and rename the variables we want
df <- df %>% 
  select(phase, latitude, longitude, datetime_utc, elevation.m,
         tair_degC = tair,
         twet_degC = twet,
         tdew_degC = tdew,
         rh)

# Add a unique ID column for joining data later on
df <- df %>% 
  mutate(id = row_number())

# Export data
write.csv(x = df,
          file = "data/mros_obs.csv",
          quote = F, row.names = F)
