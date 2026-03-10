################################################################################
#' Author: Thomas Michael FjC&rvik
#' This script does the following:
#' Calculates cross-correlation of lagged wind speed after transforming the
#' marginal distributions into white noise.
################################################################################

library(tidyverse)
library(tikzDevice)
library(patchwork)

################################################################################
#' Load filtered residuals
################################################################################

# Read filtered residuals
# Gets the directories of the files, then the full path, reads the files,
# binds the rows and selects the relevant columns.

hourly <- list.dirs(path="pipeline/marginal_distributions") %>% 
  { .[grepl("region_.+$", basename(.))]} %>% 
  lapply(function(dir) {
    list.files(
      path = dir,
      pattern = "^wind_.+_hourly\\.rds$",
      full.names = TRUE
    )
  }) %>% 
  map(., readRDS) %>% 
  bind_rows() %>% 
  select(datetime, region, locID, filtered_residuals)

daily <- readRDS("pipeline/marginal_distributions/all_regions_daily/wind_list.rds") %>% 
  bind_rows(.id = "locID") %>% 
  rename(datetime = date)

df_list <- list("hourly" = hourly,
                "daily"  = daily) %>% 
  map(~ select(.x, locID, datetime, filtered_residuals)) %>% 
  map(~ arrange(.x, locID)) %>% 
  map(~ pivot_wider(.x, names_from = locID, values_from = filtered_residuals))

################################################################################
#' Calculate cross-correlations on hourly and daily timescale
################################################################################

locations <- names(df_list[[1]])[-1] # exclude 'datetime'

cross_corrs <- 
  imap(df_list, function(df, dataset_name){
    combn(locations, 2, function(x){
      tibble(
        dataset = dataset_name,
        loc1 = x[1],
        loc2 = x[2],
        ccf = list(ccf(df[[x[1]]], df[[x[2]]], plot = FALSE))
      )
    }, simplify = FALSE) %>% 
      bind_rows()
  }) %>% 
  bind_rows()

################################################################################
#' Tidy the data
################################################################################

cross_corrs_tidy <- cross_corrs %>% 
  mutate(
    ccf_df = map(ccf, ~ {
      n <- .x$n.used
      ci <- qnorm(0.975) / sqrt(n) # CI used by plot.ccf()
      
      tibble(
      lag = .x$lag,
      acf = .x$acf,
      ci_upper = ci,
      ci_lower = -ci
    )
      })
  ) %>% 
  select(dataset, loc1, loc2, ccf_df) %>% 
  unnest(ccf_df)

################################################################################
#' Plot the data
################################################################################

plot_cross_correlations <- function(df,
                                    loc1_string,
                                    loc2_string,
                                    frequencies,
                                    max_lag = 10,
                                    ylim = 1) {
  
  # vertical buffer for top/bottom of plot
  buffer <- 0.1 * ylim  # 10% extra space
  
  # Prepare data
  df_filtered <- df %>%
    # ensure numeric columns
    mutate(lag = as.numeric(lag),
           acf = as.numeric(acf),
           ci_upper = as.numeric(ci_upper),
           ci_lower = as.numeric(ci_lower)) %>%
    filter(dataset %in% frequencies,
           loc1 == loc1_string,
           loc2 == loc2_string,
           abs(lag) <= max_lag) %>%
    mutate(
      # flag truncated points
      is_truncated = acf < -ylim | acf > ylim,
      # clamp values for plotting
      acf_clamped = pmax(pmin(acf, ylim), -ylim)
    )
  
  # Plot
  ggplot(df_filtered, aes(x = lag, y = acf_clamped)) +
    
    # ACF lines
    geom_line() +
    
    # Points, colored by truncated status
    geom_point(aes(color = is_truncated), size = 2, shape = 3) +
    
    # CI lines
    geom_hline(aes(yintercept = ci_upper), linetype = "dashed", color = "blue") +
    geom_hline(aes(yintercept = ci_lower), linetype = "dashed", color = "blue") +
    
    # Legend for truncated points
    scale_color_manual(
      name = "Point type",
      values = c("FALSE" = "black", "TRUE" = "red"),
      labels = c("Normal", "Truncated")
    ) +
    
    # Facets
    facet_grid(~dataset, scales = "fixed") +
    
    # Theme
    theme_minimal() +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      strip.background = element_rect(fill = "lightgrey", color = "black"),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5)
    ) +
    
    # Add vertical buffer so points at edges are visible
    coord_cartesian(ylim = c(-ylim - buffer, ylim + buffer)) +
    
    # Labels
    labs(
      title = paste0("Cross-correlations for locations ", loc1_string, " and ", loc2_string),
      y = "Cross-correlation (ACF)",
      x = "Lag"
    )
}



p1 <- plot_cross_correlations(cross_corrs_tidy,
                        "1",
                        "19",
                        c("hourly", "daily"),
                        max_lag=10,
                        ylim = 0.1)

p2 <- plot_cross_correlations(cross_corrs_tidy,
                        "15",
                        "16",
                        c("hourly", "daily"),
                        max_lag=10,
                        ylim = 0.1)

p1 / p2

################################################################################
#' Create figure
################################################################################

# loc1_string = "2"
# loc2_string = "4"
# tikz(
#   paste0("_figures/calculate_cross_correlations_of_innovations_",
#          loc1_string,
#          "_",
#          loc2_string,
#          ".tex"),
#   width = 6,
#   height = 4,
#   standAlone = FALSE)
# 
# plot_cross_correlations(cross_corrs_tidy,
#                         loc1_string,
#                         loc2_string,
#                         c("hourly", "daily"),
#                         max_lag=10)
# 
# dev.off()

tikz(
  "_figures/calculate_cross_correlations_of_innovations_1_19_15_16.tex",
  width = 6,
  height = 4,
  standAlone = FALSE)

p1 / p2

dev.off()
