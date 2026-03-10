################################################################################
#' Author: Thomas Michael Fj??rvik
#' This script does the following:
#' Creates plot of winds speeds for area 19 for different time frequencies.
#' Filters on years so the plots become readable.
################################################################################

################################################################################
#' Load packages
################################################################################
library(tidyverse)
library(tikzDevice)
library(patchwork)
################################################################################
#' Load data
################################################################################

wind <- readRDS("data/NVE_windspeeds_1996-97.rds") %>% 
  filter(locID == 19)

wind_daily <- wind %>% 
  group_by(day = date(datetime)) %>% 
  summarise(windspeed = mean(windspeed))

wind_weekly <- wind %>% 
  group_by(week = floor_date(datetime, "week")) %>% 
  summarise(windspeed = mean(windspeed)) 

wind_monthly <- wind %>% 
  group_by(month = floor_date(datetime, "month")) %>% 
  summarise(windspeed = mean(windspeed))


################################################################################
#' Create plots
################################################################################

create_plot <- function(df, x_col, y_col, xlabel){
  p <- ggplot(data = df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_line() +
    xlab(xlabel) +
  # Theme
  theme_minimal() +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      strip.background = element_rect(fill = "lightgrey", color = "black"),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5)
    )
  return(p)
}

p1 <- create_plot(wind, "datetime", "windspeed", "Hourly")
p2 <- create_plot(wind_daily, "day", "windspeed", "Daily")
p3 <- create_plot(wind_weekly, "week", "windspeed", "Weekly")
p4 <- create_plot(wind_monthly, "month", "windspeed", "Monthly")

speed_plot <- (p1|p2)/(p3|p4)

################################################################################
#' Save plot
################################################################################

#' Run entire block in one go

tikzDevice::tikz(file = path("_figures/speed_plot_2", 
                             ext = "tex"), 
                 width    = 6.4,  # Good dimensions for use in Overleaf
                 height   = 3.54, # Good dimensions for use in Overleaf 
                 sanitize = TRUE)
speed_plot
dev.off() 

# End of block
