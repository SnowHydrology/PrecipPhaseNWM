# Script to analyze and plot predictions

# Load packages
library(tidyverse)
library(cowplot); theme_set(theme_cowplot())
library(scales)

# Add directory paths
data_dir <- "data/"
plot_dir <- "figures/"

# Import the summarized data
summary <- readRDS(paste0(data_dir, "preds_summary.RDS"))

# Create a new temp type for facet labels
summary <- summary %>% 
  mutate(temp_type2 = ifelse(temp_type == "tair",
                             "Air Temp.",
                             ifelse(temp_type == "twet",
                                    "Wet Bulb Temp.",
                                    NA)))

# Normalize score for easier interpretation
summary <- summary %>% 
  ungroup() %>% 
  mutate(score_normal = ((score - min(score)) /(100 - min(score))) * 100)

################################################################################

# Main figure 1
# 5-panel plot for the optimized ranges

################################################################################

# Subset the data to just what we need
plot_data <- summary %>% 
  filter(scenario == "optim" & ppm_type == "range")

# Plot in a grid
range_optim_plot_5panel <- 
  cowplot::plot_grid(
    ggplot(plot_data, aes(rain_thresh, snow_thresh, fill = accuracy)) + 
      geom_raster() + 
      scale_fill_viridis_c(name = "Accuracy\n(%)") + 
      facet_wrap(~temp_type2) +
      theme(legend.position = "top",
            legend.key.width = unit(1, "cm")) +
      labs(x = "Rain Threshold (°C)", y = "Snow Threshold (°C)"),
    ggplot(plot_data, aes(rain_thresh, snow_thresh, fill = rain_bias)) + 
      geom_raster() + 
      scale_fill_gradientn(limits = c(-100,100), oob = squish, na.value = "white", 
                           colors = hcl.colors(3, "Blue-Red2", rev = T),
                           name = "Rain Bias\n(%)") +
      facet_wrap(~temp_type2)+
      theme(legend.position = "top", 
            legend.key.width = unit(1, "cm"),
            axis.title.y = element_blank(), axis.text.y = element_blank()) +
      labs(x = "Rain Threshold (°C)"),
    ggplot(plot_data, aes(rain_thresh, snow_thresh, fill = snow_bias)) + 
      geom_raster() + 
      scale_fill_gradientn(limits = c(-100,100), oob = squish, na.value = "white", 
                           colors = hcl.colors(3, "Blue-Red2", rev = T),
                           name = "Snow Bias\n(%)") +
      facet_wrap(~temp_type2)+
      theme(legend.position = "top", 
            legend.key.width = unit(1, "cm"),
            axis.title.y = element_blank(), axis.text.y = element_blank()) +
      labs(x = "Rain Threshold (°C)"),
    ggplot(plot_data, aes(rain_thresh, snow_thresh, fill = mix_bias)) + 
      geom_raster() + 
      scale_fill_gradientn(limits = c(-100,100), oob = squish, na.value = "white", 
                           colors = hcl.colors(3, "Blue-Red2", rev = T),
                           name = "Mix Bias\n(%)") +
      facet_wrap(~temp_type2)+
      theme(legend.position = "top", 
            legend.key.width = unit(1, "cm"),
            axis.title.y = element_blank(), axis.text.y = element_blank()) +
      labs(x = "Rain Threshold (°C)"),
    ggplot(plot_data, aes(rain_thresh, snow_thresh, fill = score_normal)) + 
      geom_raster() + 
      scale_fill_viridis_c(name = "Normalized\nScore") + 
      facet_wrap(~temp_type2)+
      theme(legend.position = "top", 
            legend.key.width = unit(1, "cm"),
            axis.title.y = element_blank(), axis.text.y = element_blank()) +
      labs(x = "Rain Threshold (°C)"),
    ncol = 5, rel_widths = c(1, rep(0.85, 4))
  )
save_plot(filename = paste0(plot_dir, "range_optim_plot_5panel.pdf"), 
          plot = range_optim_plot_5panel,
          base_width = 20, base_height = 10)

################################################################################

# Main figure 2
# 5-panel plot for the optimized thresholds

################################################################################

# Subset the data to just what we need
plot_data <- summary %>% 
  filter(scenario == "optim" & ppm_type == "thresh")

# Plot in a grid
thresh_optim_plot_5panel <- 
  cowplot::plot_grid(
    ggplot(plot_data, aes(thresh, accuracy, color = temp_type2)) + 
      geom_line(lwd = 1) +
      labs(x = "Rain-Snow Threshold (°C)", y = "Accuracy (%)") +
      scale_color_manual(name = "Temperature\nType", values = hcl.colors(2, "Blue-Red2",rev = T)) +
      theme(legend.position = c(0.6,0.2)),
    ggplot(plot_data, aes(thresh, rain_bias, color = temp_type2)) + 
      geom_line(lwd = 1) +
      labs(x = "Rain-Snow Threshold (°C)", y = "Rain Bias (%)")+
      scale_color_manual(name = "Temperature\nType", values = hcl.colors(2, "Blue-Red2",rev = T)) +
      theme(legend.position = "none"),
    ggplot(plot_data, aes(thresh, snow_bias, color = temp_type2)) + 
      geom_line(lwd = 1) +
      labs(x = "Rain-Snow Threshold (°C)", y = "Snow Bias (%)")+
      scale_color_manual(name = "Temperature\nType", values = hcl.colors(2, "Blue-Red2",rev = T)) +
      theme(legend.position = "none"),
    ggplot(plot_data, aes(thresh, mix_bias, color = temp_type2)) + 
      geom_line(lwd = 1) +
      labs(x = "Rain-Snow Threshold (°C)", y = "Mix Bias (%)")+
      scale_color_manual(name = "Temperature\nType", values = hcl.colors(2, "Blue-Red2",rev = T)) +
      theme(legend.position = "none"),
    ggplot(plot_data, aes(thresh, score_normal, color = temp_type2)) + 
      geom_line(lwd = 1) +
      labs(x = "Rain-Snow Threshold (°C)", y = "Normalized Score")+
      scale_color_manual(name = "Temperature\nType", values = hcl.colors(2, "Blue-Red2",rev = T)) +
      theme(legend.position = "none"),
    ncol = 5
  )
save_plot(filename = paste0(plot_dir, "thresh_optim_plot_5panel.pdf"), 
          plot = thresh_optim_plot_5panel,
          base_width = 20, base_height = 7)


################################################################################

# Main table
# Performance for all baseline and optimized methods

################################################################################

# Identify the baseline and other model PPMs to access
ppms_to_get <- 
  c("jordan",
    "thresh_tair_0",
    "thresh_tair_2.2",
    "binlog",
    "range_tair_-0.5_0.5",
    "range_tair_-1_3")

# Subset to optimized data to find best methods
optim_data <- summary %>% 
  filter(scenario == "optim") %>% 
  ungroup()

# Find the best optimized ranges
best_ta_range_accuracy <- optim_data %>% 
  filter(ppm_type == "range" & temp_type == "tair" & ((rain_thresh - snow_thresh) > 0)) %>% 
  arrange(-accuracy) %>% 
  slice(1)
best_ta_range_score <- optim_data %>% 
  filter(ppm_type == "range" & temp_type == "tair" & ((rain_thresh - snow_thresh) > 0)) %>% 
  arrange(-score) %>% 
  slice(1)
best_tw_range_accuracy <- optim_data %>% 
  filter(ppm_type == "range" & temp_type == "twet" & ((rain_thresh - snow_thresh) > 0)) %>% 
  arrange(-accuracy) %>% 
  slice(1)
best_tw_range_score <- optim_data %>% 
  filter(ppm_type == "range" & temp_type == "twet" & ((rain_thresh - snow_thresh) > 0)) %>% 
  arrange(-score) %>% 
  slice(1)

# Find the best optimized thresholds
best_ta_thresh_accuracy <- optim_data %>% 
  filter(ppm_type == "thresh" & temp_type == "tair") %>% 
  arrange(-accuracy) %>% 
  slice(1)
best_ta_thresh_score <- optim_data %>% 
  filter(ppm_type == "thresh" & temp_type == "tair") %>% 
  arrange(-score) %>% 
  slice(1)
best_tw_thresh_accuracy <- optim_data %>% 
  filter(ppm_type == "thresh" & temp_type == "twet") %>% 
  arrange(-accuracy) %>% 
  slice(1)
best_tw_thresh_score <- optim_data %>% 
  filter(ppm_type == "thresh" & temp_type == "twet") %>% 
  arrange(-score) %>% 
  slice(1)

# Put all the baseline and best optimized methods into one df
comparison_data <- 
  bind_rows(
    summary %>% filter(scenario == "current" & ppm %in% ppms_to_get),
    best_ta_range_accuracy, best_ta_range_score,
    best_tw_range_accuracy, best_tw_range_score,
    best_ta_thresh_accuracy, best_ta_thresh_score,
    best_tw_thresh_accuracy, best_tw_thresh_score
  )

# Make a table
comparison_data %>% 
  arrange(-score_normal) %>% 
  select(ppm, score_normal, accuracy:mix_bias) %>% 
  mutate(across(where(is.numeric), round, digits = 1)) %>% 
  write.csv(paste0(data_dir, "ppm_analysis_table.csv"), row.names = F, quote = F)

