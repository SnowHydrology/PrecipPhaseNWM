# Functions for rain-snow prediction

# Load packages 
library(tidyverse)


#' Single-value threshold for precipitation phase partitioning
#'
#' @param temp_degC Input temperature (air, wet bulb, dew point) in degrees Celsius
#' @param thresh_degC Temperature threshold for rain-snow partitioning in degrees Celsius
#'
#' @returns The fraction of solid precipitation where 1 = all snow and 0 = all rain
#' @export
#'
#' @examples phaseThresh(1, 2)
phaseThresh <- function(temp_degC, thresh_degC){
  ifelse(temp_degC > thresh_degC,
         0,
         1)
}

#' Dual-threshold range for precipitation phase partitioning
#'
#' @param temp_degC Input temperature (air, wet bulb, dew point) in degrees Celsius
#' @param thresh_low_degC Lower temperature threshold in degrees Celsius (below = all snow, above = mix)
#' @param thresh_high_degC Upper temperature threshold in degrees Celsius (above = all rain, below = mix)
#'
#' @returns The fraction of solid precipitation where 1 = all snow and 0 = all rain
#' @export
#'
#' @examples phaseThresh(1, -1, 3)
phaseRange <- function(temp_degC, thresh_low_degC, thresh_high_degC){
  ifelse(temp_degC >= thresh_high_degC,
         0,
         ifelse(temp_degC <= thresh_low_degC,
                1,
                (temp_degC - thresh_low_degC) / (thresh_high_degC - thresh_low_degC)))
}



#' SNTHERM (Jordan, 1990) precipitation phase partitioning method
#'
#' @param tair_degC Input air temperature in degrees Celsius
#'
#' @returns The fraction of solid precipitation where 1 = all snow and 0 = all rain
#' @export
#'
#' @examples phaseJordan(1)
phaseJordan <- function(tair_degC){
  ifelse(tair_degC > 2.5,
         0,
         ifelse(tair_degC <= 0.5,
                1,
                ifelse(tair_degC <= 2,
                       1 - (-54.632 + 0.2 * (tair_degC + 273.15)),
                       0.6)))
}

#' Binary logistic regression model (bivariate) for precipitation phase partitioning
#'
#' @param tair_degC Input air temperature in degrees Celsius
#' @param rh_pct Input relative humidity in percent
#'
#' @returns The fraction of solid precipitation where 1 = all snow and 0 = all rain
#' @export
#'
#' @examples phaseBinlog(1, 80)
phaseBinlog <- function(tair_degC, rh_pct){
  snow_prob = 1/(1 + exp(-10.04 + 1.41 * tair_degC + 0.09 * rh_pct))
  ifelse(snow_prob > 0.5,
         1,
         0)
}


#' Function to convert numeric phase (or snow fraction using the convention in this script)
#'
#' @param phase_numeric Input numeric phase from 0 (all rain) to 1 (all snow)
#' @param all_snow_thresh Numeric parameter above which precipitation is all snow (default = 1)
#' @param all_rain_thresh Numeric parameter below which precipitation is all rain (default = 0)
#'
#' @returns Character string corresponding to the phase ("Rain", "Mix", "Snow")
#' @export
#'
#' @examples phaseConversion(1) # Returns "Snow"
phaseConversion <- function(phase_numeric, all_snow_thresh = 1, all_rain_thresh = 0){
  ifelse(phase_numeric >= all_snow_thresh,
         "Snow",
         ifelse(phase_numeric <= all_rain_thresh,
                "Rain",
                "Mix"))
}