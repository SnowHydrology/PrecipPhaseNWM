# Script for pre-processing the MRoS data
# The original dataset is from https://rainorsnowmaps.com/
# Spatial subset: CONUS
# Full period of record

# Downloaded file is quite large for GitHub, so it's stored locally
# The processed file, posted to GitHub, is used for all subsequent analyses

# Load packages 
library(tidyverse)

# ID the local data file
# Rename this if you are using your own version of MRoS data
fname = "../../data/mros_obs_2020-01-01_2025-04-24.csv"

# Read in the local data file
df <- read.csv(fname)

# Filter to the obs passing QC checks
# And remove duplicate entries
df2 <- df %>% 
  filter(temp_air_flag == "Pass",
         rh_flag == "Pass",
         phase_flag == "Pass",
         nstation_temp_air_flag == "Pass") %>% 
  group_by(latitude, longitude, datetime_utc) %>% 
  mutate(n_obs = n()) %>% 
  filter(n_obs == 1) %>% 
  ungroup()

# Downselect and rename the variables we want
df3 <- df2 %>% 
  select(phase, latitude, longitude, datetime_utc,
         elevation_m, eco_level3, eco_level4, state, plp,
         tair_degC = temp_air_idw_lapse_var,
         twet_degC = temp_wet,
         tdew_degC = temp_dew_idw_lapse_var,
         rh)

# Store data on the two datasets
df_meta <- data.frame(
  source = c("full", "processed"),
  n_obs = c(nrow(df), nrow(df3))
  )

# Export data
write.csv(x = df3,
          file = "data/mros_obs.csv",
          quote = F, row.names = F)
write.csv(x = df_meta,
          file = "data/mros_obs_meta.csv",
          quote = F, row.names = F)
