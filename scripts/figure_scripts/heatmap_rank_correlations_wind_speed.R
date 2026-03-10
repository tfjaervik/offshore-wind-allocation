################################################################################
#' Author: Thomas Michael Fj??rvik
#' This script does the following:
#' Creates a heatmap of rank correlations of the wind speed of the 20 different
#' NVE locations.
################################################################################

################################################################################
#' Load packages
################################################################################

library(tidyverse)
library(viridis)
library(ggplot2)
library(reshape2)
library(tikzDevice)
################################################################################
#' Load data
################################################################################

true_data <- paste0("data/NVE_areas_windspeeds/", seq(1,20, 1), ".rds") %>% 
  map(., readRDS) %>% 
  map(., ~{.x %>% 
      mutate(datetime =as.POSIXct((time-1)*60*60,origin='1970-01-01 00:00:01')) %>% 
      rename(windspeed = value,
             region = name) %>% 
      mutate(windspeed = ifelse(windspeed == 0, 1e-08, windspeed)) %>% 
      filter(!((day(datetime) == 29) & (month(datetime) == 2))) %>% 
      group_by(day = date(datetime)) %>% 
      summarise(windspeed = mean(windspeed)) %>% 
      ungroup()
  }) %>% 
  imap(., ~{nm <- paste0("ws_", .y)
  .x %>% 
    rename_with(~nm, .cols = last_col())}) %>% 
  plyr::join_all(., type = "left") %>% 
  select(-day)

################################################################################
#' Create heatmap
################################################################################
# Step 1: Compute Spearman rank correlation
corr_matrix <- cor(true_data, method = "spearman", use="complete.obs")


# Step 2: Convert correlation matrix to long format for ggplot
corr_long <- melt(corr_matrix)
colnames(corr_long) <- c("Region1", "Region2", "Correlation")
corr_long <- corr_long %>% 
  mutate(Region1 = str_replace(Region1, "ws_", ""),
         Region2 = str_replace(Region2, "ws_", ""),
         Region1 = as.numeric(Region1),
         Region2 = as.numeric(Region2))


# Step 3: Plot heatmap

heatmap <- ggplot(corr_long, aes(x = Region1, y = Region2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "viridis", direction = 1,
                     name="Spearman\nRank\nCorrelation") +
  scale_x_continuous(breaks = 1:20) +
  scale_y_continuous(breaks = 1:20) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.grid.major = element_blank(),   # remove major grid
        panel.grid.minor = element_blank(),   # remove minor grid
        panel.grid = element_blank(),         # ensures all gridlines gone
        panel.border = element_blank()) +        # remove optional border) 
  coord_fixed()

# Save map
ggsave("_figures/heatmap_rank_correlations_wind_speed.png", heatmap, width = 7, height=6, dpi=300)
