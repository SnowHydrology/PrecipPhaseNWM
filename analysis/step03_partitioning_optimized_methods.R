# Script for predicting precipitation phase in the Mountain Rain or Snow dataset
# By optimizing various methods

# Load packages
library(tidyverse)

# Source the phase partitioning functions
source("analysis/step01_partitioning_functions.R")

# Data directory and file
data_dir = "data/"
input_file = "mros_obs.csv"

# Read in the data
df <- read.csv(paste0(data_dir, input_file))  %>% 
  rename(tair = tair_degC,
         twet = twet_degC)

# Identify the temperature types
temp_types <- c("tair", "twet")

################################################################################

# Optimize the rain-snow thresholds

################################################################################

# Create the threshold optimization parameters
lower_thresh = -5. # minimum rain-snow threshold value, °C
upper_thresh = 10  # maximum rain-snow threshold value, °C
iteration_step_thresh = 0.25 # step between threshold values to optimize, °C
thresh_vec <- seq(lower_thresh, upper_thresh, iteration_step_thresh) # vector of rain-snow thresholds, °C

# Empty dataframe to store predictions
preds <- data.frame()

# Loop through the temperature types and then the thresholds
for(j in 1:length(temp_types)){
  
  # Identify optimization paramaters
  tmp.temp_type <- temp_types[j]
  tmp.method <- "thresh"
  tmp.scenario <- "optim"
  
  for(i in 1:length(thresh_vec)){
    
    # Get temporary threshold
    tmp.thresh = thresh_vec[i]
    
    # Make predictions
    tmp.pred <- df %>% 
      mutate(snow_frac_pred = phaseThresh(.[[tmp.temp_type]], tmp.thresh),
             ppm = paste(tmp.method, tmp.temp_type, tmp.thresh, sep = "_"))
    
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
}

# Export the predictions
# Save as RDS to minimize data size
saveRDS(object = select(preds, id, ppm, phase_pred), 
        file = paste0(data_dir, "preds_optim_thresh.RDS"))

################################################################################

# Optimize the dual-threshold ranges

################################################################################

# Create the range optimization parameters
lower_rain_thresh = 0  # minimum value for all-rain threshold, °C
upper_rain_thresh = 10 # maximum value for all-rain threshold, °C
iteration_step = 0.5   # step between threshold values to optimize, °C
rain_thresh_vec <- 
  seq(lower_rain_thresh, upper_rain_thresh, iteration_step) # vector of all-rain thresholds, °C
lower_range = 0  # minimum value for mixed range (defines difference b/w all-snow and all-rain), °C
upper_range = 10 # maximum value for mixed range (defines difference b/w all-snow and all-rain), °C
range_vec = 
  seq(lower_range, upper_range, by = iteration_step) # vector of mixed ranges, °C
# data frame of all optimization params 
mixed_range_matrix <- data.frame(
  rain_thresh = rep(rain_thresh_vec, each = length(range_vec)),
  range = rep(range_vec, length(rain_thresh_vec))
) %>% 
  mutate(snow_thresh = rain_thresh - range)

# Make an empty df for predictions
preds <- data.frame()

# Loop through the temperature types and then the ranges
for(j in 1:length(temp_types)){
  
  # Identify optimization paramaters
  tmp.temp_type <- temp_types[j]
  tmp.method <- "range"
  tmp.scenario <- "optim"
  
  for(i in 1:length(mixed_range_matrix$rain_thresh)){
    
    # Get temporary all-snow and all-rain thresholds
    tmp.threshLo = mixed_range_matrix[i, "snow_thresh"]
    tmp.threshHi = mixed_range_matrix[i, "rain_thresh"]
    
    # Make predictions
    tmp.pred <- df %>% 
      mutate(snow_frac_pred = phaseRange(.[[tmp.temp_type]], tmp.threshLo, tmp.threshHi),
             ppm = paste(tmp.method, tmp.temp_type, tmp.threshLo, tmp.threshHi, sep = "_"))
    
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
}

# Export the predictions
# Save as RDS to minimize data size
saveRDS(object = select(preds, id, ppm, phase_pred), 
        file = paste0(data_dir, "preds_optim_range.RDS"))





################################################################################

# Using the R optim() function

################################################################################

# I explored using the optim function but came across a couple challenges

# Single parameter (rain-snow threshold) optimization was effective, but optim 
# struggled in multi-parameter optimization seemingly finding local minima or
# erroring out

# And there is no straightforward way to see the predictions produced by the 
# method to explore the sensitivity of different methods to changing param values


# # The R optim function optimizes parameter values to minimize an objective function
# # Here we want to optimize accuracy (1 = all predictions correct), so we will use 
# # a 1-accuracy objective function
# objectiveThresh <- function(thresh, temp, obs_phase) {
#   preds <- phaseThresh(temp, thresh) %>% 
#     phaseConversion()
#     (1 - mean(preds == obs_phase))
# }
# 
# # Choose temperature column
# temp_col <- "tdew_degC"
# 
# # Make an initial threshold guess
# thresh_init = 0
# 
# # Add upper and lower bounds for the threshold search
# thresh_bound_lower = -1
# thresh_bound_upper = 6
# 
# # Run optimization 
# opt_result <- optim(
#   par = thresh_init,
#   fn = objectiveThresh,
#   temp = df[[temp_col]],
#   obs_phase = df$phase,
#   method = "Brent",
#   lower = thresh_bound_lower,
#   upper = thresh_bound_upper
# )
# 
# # Extract best threshold and accuracy
# best_thresh_params <- opt_result$par
# best_thresh_accuracy <- 1 - opt_result$value
# 
# cat("Best threshold:", best_thresh_params, "\n")
# cat("Best accuracy:", best_thresh_accuracy, "\n") 
# 
# 
# # The R optim function optimizes parameter values to minimize an objective function
# # Here we want to optimize accuracy (1 = all predictions correct), so we will use 
# # a 1-accuracy objective function
# objectiveRange <- function(range, temp, obs_phase) {
#   # Assign threshold values from range vector
#   snow_thresh <- range[1]
#   rain_thresh <- range[2]
#   
#   # Constraints: snow_thresh must be <= rain_thresh
#   #if (snow_thresh > rain_thresh) return(Inf)
#   
#   # Make predictions 
#   preds <- phaseRange(temp, snow_thresh, rain_thresh) %>% 
#     phaseConversion()
#   
#   # Compute 1-accuracy
#   (1 - mean(preds == obs_phase))
# }
# 
# 
# # Choose temperature column
# temp_col <- "tdew_degC"
# 
# # Make an initial range guess
# range_init = c(-1, 3)
# 
# # Add upper and lower bounds for the range search
# range_bound_lower = c(-10, 0)
# range_bound_upper = c(1, 15)
# 
# # Run optimization 
# opt_result <- optim(
#   par = range_init,
#   fn = objectiveRange,
#   temp = df[[temp_col]],
#   obs_phase = df$phase,
#   method = "L-BFGS-B",
#   lower = range_bound_lower,
#   upper = range_bound_lower
# )
# 
# # Extract best threshold and accuracy
# best_range_params <- opt_result$par
# best_range_accuracy <- 1 - opt_result$value


