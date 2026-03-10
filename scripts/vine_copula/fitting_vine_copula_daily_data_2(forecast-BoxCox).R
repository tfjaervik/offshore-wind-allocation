################################################################################
#' Author: Thomas Michael Fj??rvik
#' This script does the following:
#' Creates a dataset of filtered residuals of daily wind speed 
#' and estimates a vine copula object that is stored. 
################################################################################

################################################################################
#' Load packages
################################################################################

library(tidyverse)
library(VineCopula)
library(forecast)
library(parameters)

################################################################################
#' Load data
################################################################################

wind_list <- readRDS("pipeline/marginal_distributions/all_regions_daily/wind_list.rds")

df <- wind_list %>% 
  map(., ~{.x %>% 
      select(date, filtered_residuals)}) %>% 
  plyr::join_all(by = "date", type = "left")

df_names <- c("date", paste0("area_", seq(1,20,1)))
colnames(df) <- df_names

################################################################################
#' Investigate distribution of daily residuals
################################################################################

# Fit normal distribution to all columns of df_daily

fit_normal <- function(x){
  fit <- MASS::fitdistr(x, "normal")
  para <- fit$estimate
  
  return(para)
}

normal_estimates <- apply(df[, -1], 2, fit_normal)
ks <- apply(df[, -1], 2, ks.test, y = "pnorm") %>% 
  map_dbl(., ~{.x$p.value})
sw <- apply(df[1:5000, -1], 2, shapiro.test) %>% 
  map_dbl(., ~{.x$p.value})

# All regions pass both the Kolmogorov-Smirnov test and the Shapiro-Wilks test
# for normality.


################################################################################
#' Fit vine copula to data
################################################################################


df_matrix <- apply(as.matrix(df[, -1]), 2, pnorm)

# Fit a vine copula to the data

daily_copula <- RVineStructureSelect(data = df_matrix,
                                     familyset = c(1, 3, 4, 5, 13, 14), # Gaussian, clayton, gumbel, frank, rot_clayton, rot_gumbel
                                     indeptest = TRUE,
                                     progress = TRUE,
                                     presel = TRUE)

# Save copula object

saveRDS(daily_copula, file = "pipeline/vine_copula/fitting_vine_copula_daily_data_2(forecast-BoxCox)/daily_copula.rds")
