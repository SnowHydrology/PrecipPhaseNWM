# Script for summarizing the prediction data

# Load packages
library(tidyverse)

# Data directory and files
data_dir = "data/"
obs_file = "mros_obs.csv"
baseline_file = "preds_current_methods.csv"
optim_thresh_file = "preds_optim_thresh.RDS"
optim_range_file = "preds_optim_range.RDS"

# Import data
obs <- read.csv(paste0(data_dir, obs_file))
baseline <- read.csv(paste0(data_dir, baseline_file)) %>% 
  select(-snow_frac_pred)
optim_thresh <- readRDS(paste0(data_dir, optim_thresh_file)) %>% 
  mutate(scenario = "optim")
optim_range <- readRDS(paste0(data_dir, optim_range_file)) %>% 
  mutate(scenario = "optim")

# Bind predictions for analysis
preds <- bind_rows(baseline, optim_thresh, optim_range)
  
# Add observed phase
preds <- preds %>% 
  left_join(., 
            select(obs, id, phase),
            by = "id")

################################################################################

# Make performance metric functions

################################################################################

#' Function to compute accuracy
#'
#' @param obs    # observations
#' @param pred   # predictions
#'
#' @returns Accuracy as a percent where 0% = none correct and 100% = all correct
#' @export
#'
#' @examples
perfAccuracy <- function(obs, pred){
  (sum(obs == pred) / n()) * 100
}

#' Function to compute relative bias of predictions
#'
#' @param obs   # observations
#' @param pred  # predictions
#' @param phase # phase to compute bias for (rain, snow, mix)
#'
#' @returns Percent bias
#' @export
#'
#' @examples
perfBias <- function(obs, pred, phase){
  ((sum(pred == phase) - sum(obs == phase)) / 
    sum(obs == phase)) * 100
}

#' Function to compute score that combines accuracy and biases
#'
#' @param acc        # accuracy %
#' @param bias_snow  # snow bias %
#' @param bias_rain  # rain bias %
#' @param bias_mix   # mix bias %
#'
#' @returns Score from -infinity to 100, where 100 = perfect
#' @export
#'
#' @examples
perfScore <- function(acc, bias_snow, bias_rain, bias_mix){
  100 - ((100 - acc) + abs(bias_snow) + abs(bias_rain) + abs(bias_mix))
}

################################################################################

# Compute performance for each method and scenario

################################################################################

summary <- preds %>% 
  group_by(ppm, scenario) %>% 
  summarize(accuracy = perfAccuracy(phase, phase_pred),
            rain_bias = perfBias(phase, phase_pred, "Rain"),
            snow_bias = perfBias(phase, phase_pred, "Snow"),
            mix_bias = perfBias(phase, phase_pred, "Mix"),
            score = perfScore(accuracy, snow_bias, rain_bias, mix_bias)) 

# Add information on the ppm type, humidity inclusion, and threshold values
# Code probably a bit wonky
summary <- summary %>% 
  mutate(ppm_type = ifelse(ppm %in% c("jordan", "binlog"),
                           ppm,
                           ifelse(str_starts(ppm, "thresh"),
                                  "thresh",
                                  "range")),
         temp_type = ifelse(str_detect(ppm, "_twet_"),
                            "twet",
                            "tair"),
         humidity = ifelse(ppm == "jordan" | str_detect(ppm, "_tair_"),
                            "no",
                            "yes"),
         thresh = ifelse(ppm_type == "thresh",
                         as.numeric(str_match(ppm, "^[^_]+_([^_]+)_([-0-9.]+)")[,3]),
                         NA),
         snow_thresh = ifelse(ppm_type == "range",
                              as.numeric(str_match(ppm, "^[^_]+_([^_]+)_([-0-9.]+)_([-0-9.]+)")[,3]),
                              NA),
         rain_thresh = ifelse(ppm_type == "range",
                              as.numeric(str_match(ppm, "^[^_]+_([^_]+)_([-0-9.]+)_([-0-9.]+)")[,4]),
                              NA)
  )

