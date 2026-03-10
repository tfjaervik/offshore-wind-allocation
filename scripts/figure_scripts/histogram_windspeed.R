################################################################################
#' Author: Thomas Michael Fj??rvik
#' This script does the following:
#' Creates histograms of the windspeed of selected locations
################################################################################

################################################################################
#' Load packages
################################################################################

library(tidyverse)
library(tikzDevice)

################################################################################################
#' Load data
################################################################################

wind <- readRDS("data/NVE_windspeeds_1996-97.rds")
wind_daily <- wind %>% 
  mutate(YMD = as.Date(datetime)) %>% 
  group_by(locID, YMD) %>% 
  summarise(daily_mean = mean(windspeed)) %>% 
  ungroup()

################################################################################
#' Create histogram plot
################################################################################

wind_hist <- wind %>% 
  ggplot(., mapping = aes(x = windspeed)) + 
  geom_histogram(aes(y = after_stat(density))) + 
  geom_density(fill = "lightblue", alpha = 0.5) + 
  facet_wrap(~ locID)

wind_hist <- wind %>% 
  ggplot(., mapping = aes(x = windspeed)) +
  geom_histogram(color = "darkblue", fill = "darkblue") + 
  theme_classic() +
  facet_wrap(~locID)

wind_hist_daily <- wind_daily %>% 
  ggplot(., mapping = aes(x = daily_mean)) +
  geom_histogram(color = "darkblue", fill = "darkblue") +
  theme_classic() +
  facet_wrap(~locID)

# Create hourly histograms for all locations
tikz("_figures/hourly_wind_histograms.tex", width = 6, height = 4, standAlone = FALSE)

wind %>% 
  ggplot(., mapping = aes(x = windspeed)) +
  geom_histogram(color = "darkblue", fill = "darkblue") + 
  theme_classic() +
  facet_wrap(~locID) %>% 
  print()

dev.off()

# Create daily histograms for all locations
tikz("_figures/daily_wind_histograms.tex", width = 6, height = 4, standAlone = FALSE)

wind_daily %>% 
  ggplot(., mapping = aes(x = daily_mean)) +
  geom_histogram(color = "darkblue", fill = "darkblue") + 
  theme_classic() +
  facet_wrap(~locID) +
  labs(x = "Daily mean wind speed") %>% 
  print()

dev.off()
