################################################################################
#' Author: Thomas Michael Fj??rvik
#' This script does the following:
#' Creates histograms showing histograms of hourly wind speeds in area 19
#' before and after applying a Box-Cox transformation
################################################################################

################################################################################
#' Load packages
################################################################################
library(tidyverse)
library(tikzDevice)
library(fs)
################################################################################
#' Functions
################################################################################

source("scripts/functions/helper_functions.R")

################################################################################
#' Load data
################################################################################

# Hourly data

wind <- readRDS("data/NVE_areas_windspeeds/19.rds") %>% 
  mutate(datetime =as.POSIXct((time-1)*60*60,origin='1970-01-01 00:00:01')) %>% 
  rename(windspeed = value,
         region = name) %>% 
  mutate(windspeed = ifelse(windspeed == 0, 1e-08, windspeed)) %>% 
  filter(!((day(datetime) == 29) & (month(datetime) == 2)))

################################################################################
#' Histogram before Box-Cox
################################################################################

hist_wind <- wind %>% 
  ggplot(., aes(x = windspeed)) + 
  geom_histogram(aes(y= after_stat(density)), color = "white", fill = "blue") +
  labs(y = "Relative frequency") +
  # Theme. Same as for "calculate_cross_correlations_of_innovations.R"
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

################################################################################
#' Apply Box-Cox transformation
################################################################################

wind <- wind %>% 
  mutate(ws = box_cox_forecast(windspeed))

################################################################################
#' Histogram after Box-Cox
################################################################################

hist_wind_bc <- wind %>% 
  ggplot(., aes(x = ws)) + # Only changed this
  geom_histogram(aes(y= after_stat(density)), color = "white", fill = "blue") +
  labs(y = "Relative frequency") +
  # Theme. Same as for "calculate_cross_correlations_of_innovations.R"
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

################################################################################
#' Create combined histogram
################################################################################

# Create long-form data to used facet_wrap

wind_long <- wind %>% 
  select(windspeed, ws) %>% 
  rename(before = windspeed, after = ws) %>% 
  pivot_longer(cols = c(before, after), 
               names_to = "variable", 
               values_to = "value") %>% 
  mutate(variable = factor(variable, levels = c("before", "after"))) %>% 
  arrange(variable, value)

# Create figure

hist_combined <- wind_long %>% 
  ggplot(., aes(x = value)) + # Only changed this
  geom_histogram(aes(y= after_stat(density)), color = "white", fill = "lightblue") +
  facet_wrap(~ variable, scales = "free") +
  labs(y = "Relative frequency") +
  # Theme. Same as for "calculate_cross_correlations_of_innovations.R"
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

hist_combined

################################################################################
#' Create tikz-figure
################################################################################

#' Run entire block in one go

tikzDevice::tikz(file = path("_figures/hist_a19_before_after_box_cox", 
                             ext = "tex"), 
                 width    = 6.4,  # Good dimensions for use in Overleaf
                 height   = 3.54, # Good dimensions for use in Overleaf 
                 sanitize = TRUE)
hist_combined
dev.off() 