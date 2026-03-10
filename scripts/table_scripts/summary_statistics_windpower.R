################################################################################
#' Author: Thomas Michael Fj??rvik
#' This script does the following:
#' Calculates summary statistics for both real and simulated windpower data
################################################################################

################################################################################
#' Load packages
################################################################################

library(tidyverse)
library(parameters)
library(kableExtra)

################################################################################
#' Load data
################################################################################

# True data

windspeed <- paste0("data/NVE_areas_windspeeds/", seq(1,20, 1), ".rds") %>% 
  map(., readRDS) %>% 
  map(., ~{.x %>% 
      mutate(datetime =as.POSIXct((time-1)*60*60,origin='1970-01-01 00:00:01')) %>% 
      rename(windspeed = value) %>% 
      select(datetime, windspeed) %>% 
      filter(!((day(datetime) == 29) & (month(datetime) == 2))) %>% 
      group_by(day = date(datetime)) %>% 
      summarise(windspeed = mean(windspeed)) %>% 
      ungroup()
  }) %>% 
  imap(., ~{nm <- paste0("ws_", .y)
  .x %>% 
    rename_with(~nm, .cols = last_col())}) %>% 
  plyr::join_all(., by = "day", type = "left") %>% 
  select(-day)


# Convert windspeed to windpower

speed_to_power <- function(x){
  
  C_r <- 15
  power <- vector()
  
  for(i in 1:length(x)){
    if(x[i] < 3 | x[i] > 25){
      power[i] <- 0
    } else if(x[i] >= 3 & x[i] <= 10.59){
      power[i] <- (x[i]^3 - 3^3)/(10.59^3 - 3^3)
    } else{
      power[i] <- 1
    }
  }
  output <- C_r*power
  return(output)
}

true_data <- apply(windspeed, 2, FUN = speed_to_power)


# Load simulated data

# 100 years of daily data
# sim_data <- readRDS("pipeline/vine_copula/simulate_windpower/windpower.rds")

# 10 000 years of daily data
sim_data <- readRDS("pipeline/vine_copula/simulate_windpower/windpower_10000_daily.rds")

colnames(sim_data) <- paste0("ws_", seq(1, 20, by = 1))


################################################################################
#' Summary statistics table for true data
################################################################################

cvar12 <- function(x){
  mean(x[x <= quantile(x, probs = 0.12)])
}

summary_function <- function(x){
  c("Mean" = mean(x), "Median" = median(x), "StD" = sd(x), 
    "Skewness" = skewness(x, type = "1")$Skewness, "Kurtosis" = kurtosis(x, type = "1")$Kurtosis,
    "CVaR" = cvar12(x))
}

true_summary <- t(apply(true_data, 2, FUN = summary_function))
sim_summary <- t(apply(sim_data, 2, FUN = summary_function))


true_summary <- true_summary %>% 
  as_tibble() %>% 
  mutate_all(., round, digits = 2) %>% 
  mutate(Region = seq(1, nrow(true_summary), 1), .before = Mean)

sim_summary <- sim_summary %>% 
  as_tibble() %>% 
  mutate_all(., round, digits = 2) %>%
  mutate(Region = seq(1, nrow(true_summary), 1), .before = Mean)


################################################################################
#' Create combined summary table with True/Simulated side by side
################################################################################


# Combine summaries column-wise
combined_summary <- cbind(
  Region = true_summary$Region,
  Mean_true = true_summary$Mean, Mean_sim = sim_summary$Mean,
  Median_true = true_summary$Median, Median_sim = sim_summary$Median,
  StD_true = true_summary$StD, StD_sim = sim_summary$StD,
  Skewness_true = true_summary$Skewness, Skewness_sim = sim_summary$Skewness,
  Kurtosis_true = true_summary$Kurtosis, Kurtosis_sim = sim_summary$Kurtosis,
  CVaR_true = true_summary$CVaR, CVaR_sim = sim_summary$CVaR
) %>% as_tibble()

# Rename columns for display
colnames(combined_summary) <- c(
  "Region",
  "Raw", "Sim",    # Mean
  "Raw", "Sim",    # Median
  "Raw", "Sim",    # StD
  "Raw", "Sim",    # Skewness
  "Raw", "Sim",    # Kurtosis
  "Raw", "Sim"     # CVaR
)

# Define top header (statistics)
top_header <- c(
  " " = 1,        # Region column
  "Mean" = 2,
  "Median" = 2,
  "StD" = 2,
  "Skewness" = 2,
  "Kurtosis" = 2,
  "CVaR" = 2
)

# Create LaTeX table
combined_table <- combined_summary %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    caption = "Summary statistics of raw and simulated windpower side by side for each region.",
    label = "combined_wpower_summary",
    align = "c",
    escape = FALSE
  ) %>%
  kable_styling(latex_options = c("scale_down", "hold_position")) %>%
  add_header_above(top_header) %>%   # Only top-level statistic names
  TexTools::ltx_caption(tbl_note = "Summary statistics of raw and simulated windpower side by side for each region.")

# Replace [!h] with [H] in the generated table environment for exact placement by quarto/Latex.
combined_table <- gsub("\\[!h\\]", "[H]", combined_table)

# Save table
# Note: TexTools::write_tex_simplified does not change table contents.
# It only makes slight layout adjustments, some of which have been overwritten later anyway.
# The table and its contents are not affected.
TexTools::write_tex_simplified(combined_table, path = "_tables/combined_windpower_summary_table_with_CVaR")
