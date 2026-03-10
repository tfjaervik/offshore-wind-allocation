################################################################################
#' Author: Thomas Michael Fj??rvik
#' This script does the following:
#' Filters the time series of wind speed for all areas 
################################################################################

################################################################################
#' Load packages
################################################################################
library(tidyverse)
library(purrr)
################################################################################
#' Load data
################################################################################

# Hourly data

wind_list <- paste0("data/NVE_areas_windspeeds/", seq(1,20, 1), ".rds") %>% 
  map(., readRDS) %>% 
  map(., ~{.x %>% 
      mutate(datetime =as.POSIXct((time-1)*60*60,origin='1970-01-01 00:00:01')) %>% 
      rename(windspeed = value,
             region = name) %>% 
      mutate(windspeed = ifelse(windspeed == 0, 1e-08, windspeed)) %>% 
      filter(!((day(datetime) == 29) & (month(datetime) == 2)))
    })
  

wind_daily_list <- wind_list %>% 
  map(., ~{.x %>% 
      group_by(day = date(datetime)) %>% 
      summarise(windspeed = mean(windspeed)) %>% 
      ungroup()
      })


################################################################################
#' Functions
################################################################################

source("scripts/functions/helper_functions.R")

################################################################################
#' Apply Box-Cox transformation
################################################################################
# Map over all the datasets and then map the list objects to the global environment
# Do box_cox transformation and store all the lambdas, which are stored as vector
# attributes.

df_list <- wind_daily_list %>% 
  map(., ~ .x %>% 
        mutate(ws = box_cox_forecast(windspeed)))

lambda <- df_list %>% 
  map_dbl(., ~{.x$ws %>% 
      attr(., "lambda")
    })


################################################################################
#' Fit seasonal function using ordinary least squares
################################################################################


# We have to add the time t to all our datasets, and so we simply add a sequence
# starting at 1 and ending at nrow(df). We also have to add the period scaling factor and
# the different harmonic terms. Having done this, we only need to estimate, 
# the a-coefficients, which can be done by ordinary least squares as the expression
# is then linear. 

df_list <- df_list %>% 
  map(., ~{
    aux = 365
    .x %>% 
      mutate(t = seq(1, nrow(.x), by = 1),
             scale = aux,
             a_1 = cos((2*pi*t)/scale), 
             a_2 = sin((2*pi*t)/scale),
             a_3 = cos((4*pi*t)/scale),
             a_4 = sin((4*pi*t)/scale))
  })
    


seasonal_regressions <- df_list %>% 
  map(., ~lm(ws ~ a_1 + a_2 + a_3 + a_4, data = .x))


################################################################################
#' Estimate ARMA model for the deseasonalized data
################################################################################

# Get residuals from seasonal regressions

deseasonalized_series <- seasonal_regressions %>% 
  map(., ~{summary(.x)$residuals
  })


# Add residuals to wind data

df_list <- df_list %>% 
  imap(., ~{.x %>% 
      mutate(deseasonalized = deseasonalized_series[[.y]])
  }) 

# Estimate ARMA models using auto.arima() to select the best model

arma_models <- df_list %>% 
  map(., ~{forecast::auto.arima(.x$deseasonalized, 
                                d = 0, # Force no differencing
                                D = 0, # No seasonal differencing
                                approximation = FALSE, 
                                seasonal = FALSE, # Restrict search to non-seasonal models
                                allowmean = FALSE)}) # No mean


# Extract residuals and merge back into the respective datasets

arma_residuals <- arma_models %>% 
  map(., ~{residuals(.x)})

df_list <- df_list %>% 
  imap(., ~{
    .x %>% 
      mutate(arma_residuals = arma_residuals[[.y]])
  }) 

################################################################################
#' Estimate seasonal volatility function and normalize residuals.
#' First we adjust the skewness and kurtosis to about 0 and 3, then we divide
#' by the sample standard deviation (which leaves skewness and kurtosis unchanged)
################################################################################

# Values for kurtosis transformation. Gives kurtosis approximately equal to 3 

delta_vec <- c(0.02825, 0.0385, 0.0306, 0.0297, 0.0364, 0.0308, 0.0249, 0.0234, 0.0263, 0.0281,
               0.0273, 0.0406, 0.0404, 0.0292, 0.0320, 0.0259, 0.0208, 0.0274, 0.0211, 0.0318)

# Create Yeo Johnson objects

yt_objects <- df_list %>%
  map(., ~{.x$arma_residuals %>% 
      bestNormalize::yeojohnson()})

df_list <- df_list %>% 
  imap(., ~{.x %>% 
      mutate(arma_residuals = adjust_skewness_kurtosis(delta = delta_vec[.y], yt_objects[[.y]])) 
    }) %>% 
  map(., ~{.x %>% 
      mutate(date = day,
             variance_residuals = var(arma_residuals),
             filtered_residuals = arma_residuals/sqrt(variance_residuals)) %>% 
      select(date, windspeed, ws, deseasonalized, arma_residuals, variance_residuals, filtered_residuals)
    })

# kurt_vec <- df_list_2 %>% 
#   map(., ~{.x$arma_residuals %>% 
#       LambertW::kurtosis()
#     }) %>% 
#   unlist()
# 
# skew_vec <- df_list_2 %>% 
#   map(., ~{.x$arma_residuals %>% 
#       LambertW::skewness()
#   }) %>% 
#   unlist()
# skew_vec_2 <- df_list %>% 
#   map(., ~{.x$arma_residuals %>% 
#       LambertW::skewness()
#   }) %>% 
#   unlist()
# 
# mean_vec <- df_list_2 %>% 
#   map(., ~{.x$arma_residuals %>% 
#       mean()
#   }) %>% 
#   unlist()
# 
# mean_vec_2 <- df_list %>% 
#   map(., ~{.x$arma_residuals %>% 
#       mean()
#   }) %>% 
#   unlist()
# 
# # See if skewness is affected by kurtosis transformation
# 
# test_1 <- predict(yt_objects[[1]]) %>% LambertW::skewness()
# test_2 <- predict(yt_objects[[1]]) %>% LambertW::G_delta(., delta_vec[1]) %>% LambertW::skewness()

# It increases slightly, but is still not far from zero

# Dividing by the empirical standard deviation is not so well-founded, given that
# we only have 25 datapoints to estimate them for each day. 
# Recall that we estimate one standard deviation per date, for example for 01.01.
# This means we only have one sample per year. Hence, instead we
# normalize residuals by dividing by the sample standard deviations for each region

# df_list <- df_list %>% 
#   map(., ~{
#     .x %>% 
#       mutate(date = day,
#              variance_residuals = var(arma_residuals),
#              filtered_residuals = arma_residuals/sqrt(variance_residuals)) %>% 
#       select(date, windspeed, ws, deseasonalized, arma_residuals, variance_residuals, filtered_residuals)
#   })


# There seems to be no particular seasonality in the volatility, so instead
# we normalize the residuals by divind them by their empirical standard
# devation

# df_list <- df_list %>% 
#   map(., ~{
#     .x %>% 
#       mutate(date = day) %>% 
#       group_by(month = month(day), 
#                day = day(day)) %>%
#       mutate(variance_residuals = mean(arma_residuals^2)) %>% 
#       ungroup() %>% 
#       mutate(filtered_residuals = arma_residuals/sqrt(variance_residuals)) %>% 
#       select(date, windspeed, ws, deseasonalized, arma_residuals, variance_residuals, filtered_residuals)
#   })



# First we need to estimate the daily volatility. This is done by averaging 
# the squared residual for the same date and hour across years. Note that when 
# we do this on such a small test data set, the estimates are pretty imprecise 
# (and also when we do it on an hourly level)

# 
# df_list <- df_list %>% 
#   map(., ~{
#       .x %>% 
#         mutate(date = day) %>% 
#         group_by(month = month(day), 
#                  day = day(day)) %>%
#         mutate(variance_residuals = mean(arma_residuals^2)) %>% 
#         ungroup()
#   })
# 
# # Can either fit a seasonal variance function to the variance estimates, or
# # if there is no seasonality in the squared residuals, but remaining hetero-
# # skedasticity, we can divide the residuals by the empirical standard deviations
# 
# # Estimate seasonal variance function and divide residuals by standard deviation
# 
# volatility_list <- df_list %>% 
#   map(., ~{ 
#       .x %>% 
#         select(month, day, variance_residuals)
#   }) %>%
#   map(., ~{
#     aux = 365
#     .x %>% 
#       distinct() %>% 
#       mutate(t = row_number(),
#              c_1 = cos(2*pi*t/aux),
#              c_2 = cos(4*pi*t/aux),
#              c_3 = cos(6*pi*t/aux))
#   })
# 
# volatility_estimates <- volatility_list %>% 
#   map(., ~lm(variance_residuals ~ c_1 + c_2 + c_3, data = .x)$fitted.values)
# 
# # Merge estimates with datasets in df_list and normalize arma_residuals by 
# # dividing them by the square root of volatility_estimates
# 
# volatility_list <- volatility_list %>% 
#   imap(., ~{.x %>% 
#       mutate(seasonal_volatility = volatility_estimates[[.y]])}) %>% 
#   map(., ~{
#       .x %>% 
#         select(month, day, seasonal_volatility)
#   })
# 
# df_list <- df_list %>% 
#   imap(., ~{.x %>% 
#       left_join(volatility_list[[.y]]) # Perform the natural join, i.e. by all common columns
#   }) %>% 
#   map(., ~{
#       .x %>% 
#         mutate(filtered_residuals = arma_residuals/sqrt(seasonal_volatility)) %>% 
#         select(date, windspeed, ws, deseasonalized, arma_residuals, filtered_residuals)
#   })

layout(matrix(c(1,2),ncol = 2))
hist(df_list[[1]]$filtered_residuals, breaks = 100)
hist(rnorm(8760), breaks = 100)
################################################################################
#' Save daily filtered wind data
################################################################################


wind_list <- df_list %>% 
  map(., ~{.x %>% 
      select(date, windspeed, filtered_residuals)
      })
  
volatility_list <- df_list %>% 
  map(., ~{.x$variance_residuals[1]})

# If we estimated seasonal volatility
# volatility_list <- volatility_estimates 

seasonality_list <-  seasonal_regressions %>% 
  map(., ~{.x$fitted.values[1:365]})


saveRDS(wind_list, file = "pipeline/marginal_distributions/all_regions_daily/wind_list.rds")
saveRDS(arma_models, file = "pipeline/marginal_distributions/all_regions_daily/arma_models.rds")
saveRDS(volatility_list, file = "pipeline/marginal_distributions/all_regions_daily/volatility_list.rds")
saveRDS(seasonality_list, file = "pipeline/marginal_distributions/all_regions_daily/seasonality_list.rds")
saveRDS(lambda, file = "pipeline/marginal_distributions/all_regions_daily/lambda.rds")
saveRDS(delta_vec, file = "pipeline/marginal_distributions/all_regions_daily/delta_vec.rds")
saveRDS(yt_objects, file = "pipeline/marginal_distributions/all_regions_daily/yt_objects.rds")
