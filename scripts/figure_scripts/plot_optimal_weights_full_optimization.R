################################################################################
#' Author: Thomas Michael Fj??rvik
#' This script does the following:
#' Finds the optimal locations of offshore wind power production by finding
#' the weight vector that maximizes the empirical q-quantile of the wind portfolio
################################################################################

################################################################################
#' Load packages
################################################################################

library(tidyverse)
library(mgcv)
library(tikzDevice)
library(fs)


################################################################################
#' Load data
################################################################################

optimal_cvar <- readRDS("pipeline/optimization/optimal_locations_VaR_and_CVaR/optimal_cVaR_benchmark_case.rds")
optimal_var12 <- readRDS("pipeline/optimization/optimal_locations_VaR_and_CVaR/optimal_VaR_12_percent_benchmark_case.rds")
optimal_var5 <- readRDS("pipeline/optimization/optimal_locations_VaR_and_CVaR/optimal_VaR_5_percent_benchmark_case.rds")
min_var <- readRDS("pipeline/optimization/optimal_locations_VaR_and_CVaR/minimum_variance_benchmark_case.rds")

################################################################################
#' Create tibble with optimal weights for each optimization criterion
################################################################################


cbind("12 percent VaR" = optimal_var12$value, "5 percent VaR" = optimal_var5$value,
      "12 Percent CVaR" = optimal_cvar$value)


opt_weights <- cbind("VaR_12_pct" = delta.w2n.adapted(m = 20, optimal_var12$par), "VaR_5_pct" = delta.w2n.adapted(m = 20, optimal_var5$par),
                     "CVaR_12_pct" = delta.w2n.adapted(m = 20, optimal_cvar$par),
                     "Minimum_Variance" = delta.w2n.adapted(m = 20, min_var$par)#,
                     #"Maximum_Mean" = delta.w2n.adapted(m = 20, optimal_mean$par
                                                        ) %>% 
  as_tibble()


################################################################################
#' Plot optimal weights 
################################################################################

colors <- c("VaR 12%" = "blue", "VaR 5%" = "green", "CVaR 12%" = "red", "Minimum Variance" = "black") #, "Maximum_Mean" = "orange")


weight_location_plot <- opt_weights %>% 
  ggplot(aes(x = seq(1, 20, 1))) + 
  geom_point(aes(y = VaR_12_pct, color = "VaR 12%"), size = 3) + 
  geom_point(aes(y = VaR_5_pct, color = "VaR 5%"), size = 2) +
  geom_point(aes(y = CVaR_12_pct, color = "CVaR 12%"), size = 1) + 
  geom_point(aes(y = Minimum_Variance, color = "Minimum Variance"), size = 1) +
  # geom_point(aes(y = Maximum_Mean, color = "Maximum_Mean"), size = 1) + # Do not include this, does not make sense
  labs(y = "Optimal weights", x = "Locations", color = "Optimality Criterion") + 
  scale_color_manual(values = colors) +
  #geom_hline(yintercept = 1/2000) +
  #annotate("text", x = 3, y = 0, label = "One turbine limit") + 
  scale_y_continuous(limits = c(0, 0.31)) +
  theme_bw()


################################################################################
#' Save plot to .tex file
################################################################################

#' Run entire block in one go

# tikzDevice::tikz(file = path("_figures/plot_optimal_weights_full_optimization", 
#                              ext = "tex"), 
#                  width    = 6.4,  # Good dimensions for use in Overleaf
#                  height   = 3.54, # Good dimensions for use in Overleaf 
#                  sanitize = TRUE)
# weight_location_plot
# dev.off()  

tikzDevice::tikz(file = path("_figures/plot_optimal_weights_full_optimization_10000_daily", 
                             ext = "tex"), 
                 width    = 6.4,  # Good dimensions for use in Overleaf
                 height   = 3.54, # Good dimensions for use in Overleaf 
                 sanitize = TRUE)
weight_location_plot
dev.off()  
