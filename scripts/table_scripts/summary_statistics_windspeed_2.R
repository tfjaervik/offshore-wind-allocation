################################################################################
#' Author: Thomas Michael Fj??rvik
#' This script does the following:
#' Compare distributions of true and simulated data
################################################################################

################################################################################
#' Load packages
################################################################################
library(tidyverse)
library(parameters)
library(kableExtra)
#library(kable)
################################################################################
#' Load data
################################################################################
# True data

# Hourly data converted to daily data

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

# Simulated data
windspeed <- readRDS("pipeline/vine_copula/simulate_windspeed_2/windspeed_10000_daily.rds")

################################################################################
#' Calculate summary statistics for both areas and plot some histograms
################################################################################


summary_function <- function(x){
  c("Mean" = mean(x), "Median" = median(x), "StD" = sd(x), 
    "Skewness" = skewness(x, type = "1")$Skewness, "Kurtosis" = kurtosis(x, type = "1")$Kurtosis)
}

true_summary <- t(apply(true_data, 2, FUN = summary_function))
sim_summary <- t(apply(windspeed, 2, FUN = summary_function))


true_summary <- true_summary %>% 
  as_tibble() %>% 
  mutate_all(., round, digits = 2) %>% 
  mutate(Region = seq(1, nrow(true_summary), 1), .before = Mean)

sim_summary <- sim_summary %>% 
  as_tibble() %>% 
  mutate_all(., round, digits = 2) %>%
  mutate(Region = seq(1, nrow(true_summary), 1), .before = Mean)


################################################################################
#' Create combined summary table
################################################################################

# Add suffixes to distinguish True vs Simulated
true_summary2 <- true_summary %>% rename_with(~paste0(., "_true"), -Region)
sim_summary2  <- sim_summary  %>% rename_with(~paste0(., "_sim"), -Region)

# Join by Region
combined_summary <- left_join(true_summary2, sim_summary2, by = "Region")

# Reorder columns for side-by-side comparison
combined_summary <- combined_summary %>%
  select(
    Region,
    Mean_true, Mean_sim,
    Median_true, Median_sim,
    StD_true, StD_sim,
    Skewness_true, Skewness_sim,
    Kurtosis_true, Kurtosis_sim
  )

colnames(combined_summary) <- c(
  "Region",
  "True", "Simulated",    # Mean
  "True", "Simulated",    # Median
  "True", "Simulated",    # StD
  "True", "Simulated",    # Skewness
  "True", "Simulated"     # Kurtosis
)

# Define the two-level header
top_header <- c(" " = 1, "Mean" = 2, "Median" = 2, "StD" = 2, "Skewness" = 2, "Kurtosis" = 2)

# Create the table
combined_table <- combined_summary %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    caption = "Comparison of True and Simulated Daily Windspeeds by Region",
    label = "combined_summary",
    align = "c",
    escape = TRUE
  ) %>%
  kable_styling(latex_options = c("scale_down", "hold_position")) %>%
  add_header_above(top_header) %>%     # Statistic names on top
  TexTools::ltx_caption(tbl_note = "This table displays the true and simulated summary statistics side by side for each region.
                        Note that the true data consists of 24 years of hourly data aggregated to daily data by taking the mean.
                        The simulated data consists of 10 000 years of daily data. Hence, some deviations in shape statistics 
                        are to be expected, as they could be affected by tail observations.")

# Replace [!h] with [H] in the generated table environment for exact placement by quarto/Latex.
combined_table <- gsub("\\[!h\\]", "[H]", combined_table)

# Note: TexTools::write_tex_simplified does not change table contents.
# It only makes slight layout adjustments, some of which have been overwritten later anyway.
# The table and its contents are not affected.
TexTools::write_tex_simplified(combined_table, path = "_tables/combined_windspeed_summary_table")

