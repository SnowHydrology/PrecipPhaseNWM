# Script for predicting precipitation phase in the Mountain Rain or Snow dataset
# Using the non-optimized NWM v3 and NextGen methods

# Load packages
library(tidyverse)

# Source the phase partitioning functions
source("analysis/step01_partitioning_functions.R")

# Data directory and file
data_dir = "data/"
input_file = "mros_obs.csv"

# Read in the data
# And rename cols for simpler text
df <- read.csv(paste0(data_dir, input_file)) %>% 
  rename(tair = tair_degC,
         twet = twet_degC)

# Define the methods
# For reference these include:
# jordan: NWM default from SNTHERM (Noah-MP option)
# thresh_tair_0: 0°C air temperature threshold (Noah-MP option)
# thresh_tair_2.2: 2.2°C air temperature threshold (Noah-MP option)
# thresh_twet_0.5: 0.5°C wet bulb temperature threshold (previous best from MRoS, newly implemented in NOM)
# thresh_twet_1.0: 1.0°C wet bulb temperature threshold(previous best from synoptic data, newly implemented in NOM)
# range_tair_0.0: air temperature dual-threshold (-0.5°C to 0.5°C) VIC
# range_tair_1.0: air temperature dual-threshold (-1°C to 3°C) UEB 
# binlog: binary logistic regression model, f(tair, rh) (newly implemented in NOM)
ppms <- 
  data.frame(
    method = c("jordan", rep("thresh", 4), rep("range", 2), "binlog"),
    temp_type = c(rep("tair", 3), rep("twet", 2), rep("tair", 2), NA),
    thresh = c(NA, 0, 2.2, 0.5, 1.0, rep(NA, 3)), # for single-value thresholds
    threshLo = c(rep(NA, 5), -0.5, -1.0, NA), # for dual-threshold ranges
    threshHi = c(rep(NA, 5), 0.5, 3.0, NA) # for dual-threshold ranges
  )

# Specify the phase determination thresholds
# We'll revisit this later in optimization
# The way it is set now, any snow fraction return that != 0 | 1 will be mixed
rain_thresh = 0
snow_thresh = 1

# Define the analysis scenario
# We will add "optimized" later when we explore further approaches
tmp.scenario = "current"

# Empty data frame to store predictions
preds <- data.frame()

# Loop through the methods and estimate precipitation phase
for(i in 1:length(ppms$method)){
  
  # Temporary vars for methods and types
  tmp.method    = ppms[i, "method"]
  tmp.temp_type = ppms[i, "temp_type"]
  
  # If-else statements for methods
  if(tmp.method == "thresh"){
    tmp.thresh = ppms[i, "thresh"]
    tmp.pred <- df %>% 
     mutate(snow_frac_pred = phaseThresh(.[[tmp.temp_type]], tmp.thresh),
            ppm = paste(tmp.method, tmp.temp_type, tmp.thresh, sep = "_"))
  } else if(tmp.method == "range") {
    tmp.threshLo = ppms[i, "threshLo"]
    tmp.threshHi = ppms[i, "threshHi"]
    tmp.pred <- df %>% 
      mutate(snow_frac_pred = phaseRange(.[[tmp.temp_type]], tmp.threshLo, tmp.threshHi),
             ppm = paste(tmp.method, tmp.temp_type, tmp.thresh, sep = "_"))
  } else if(tmp.method == "binlog"){
    tmp.pred <- df %>% 
      mutate(snow_frac_pred = phaseBinlog(tair, rh),
             ppm = tmp.method)
  } else if(tmp.method == "jordan"){
    tmp.pred <- df %>% 
      mutate(snow_frac_pred = phaseJordan(tair),
             ppm = tmp.method)
  }
  
  # Estimate the phase from the snow fraction
  tmp.pred <- tmp.pred %>% 
    mutate(phase_pred = phaseConversion(snow_frac_pred))

  # Add the scenario to the data
  tmp.pred$scenario <- tmp.scenario
  
  # Select the data we want
  tmp.pred <- tmp.pred %>% 
    select(id:scenario)
  
  # Bind the predictions to the others
  preds <- bind_rows(preds, tmp.pred)
}

# Export the predictions
write.csv(x = preds,
          file = paste0(data_dir, "preds_current_methods.csv"),
          row.names = F, quote = F)
