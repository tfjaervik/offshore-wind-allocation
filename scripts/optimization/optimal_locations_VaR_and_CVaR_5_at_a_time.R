################################################################################
#' Author: Thomas Michael Fj??rvik
#' This script does the following:
#' Calculates optimal maximum (positive) CVaR of windpower for all 
#' unique combination of five locations
################################################################################

################################################################################
#' Load packages
################################################################################

library(tidyverse)
library(mgcv)
library(furrr)


################################################################################
#' Load data
################################################################################

windpower <- readRDS("pipeline/vine_copula/simulate_windpower/windpower.rds")

colnames(windpower) <- paste0("loc_", seq(1, 20, 1))

combination_list <- combn(paste0("loc_", seq(1, 20, 1)), m = 5, simplify = FALSE)

################################################################################
#' Functions
################################################################################

# Used for intial guess of weights, with log function adapted to numerical optimization

delta.n2w.adapted <- function(m, delta){
  
  foo <- notLog(delta/delta[1])
  tdelta <- as.vector(tail(foo, m - 1))
  return(tdelta)
}

# Normalize weights so they sum to 1 with exp function adapted to numerical optimization

delta.w2n.adapted <- function(m, tdelta){
  
  # set first element to one and fill in the last m - 1 elements with working parameters and take exp
  delta <- c(1, notExp(tdelta))
  
  # normalize
  delta = delta/sum(delta)
  
  return(delta)
}


# Returns negative of CVaR, to be used for minimization

negative_CVaR <- function(weights, data){
  weights <- delta.w2n.adapted(m = length(weights), tdelta = weights)
  
  output <- data%*%weights
  
  mean_q_output <- mean(output[output <= quantile(output, p = 0.12)])
  
  return(-mean_q_output)
}

negative_VaR <- function(weights, data, quantile){
  
  weights <- delta.w2n.adapted(m = length(weights), tdelta = weights)
  
  output <- data%*%weights
  
  q_output <- quantile(output, p = quantile)
  
  return(-q_output)
  
}

result_CVaR <- function(columns, data){
  
  data <- data[, columns]
  tryCatch(
    {expr = 
  res <- optim(delta.n2w.adapted(m = 5, delta = rep(1/5, times = 5)),
        fn = negative_CVaR,
        data = data,
        control = list(trace = FALSE, maxit = 20000))
  
  res_list <- list("weights" = delta.w2n.adapted(m = 5, res$par),
                   "CVaR" = res$value)
  
  return(res_list)
    },
  error = function(e){
    return(list("weights" = rep(NA, 5),
                "CVaR" = NA))
  }
  )
}

################################################################################
#' Optimization
################################################################################

# We run the optimization if parallel using furrr.

plan(multisession, workers = parallel::detectCores() - 1)

start_time <- Sys.time()

results <- future_map(
  combination_list,
  result_CVaR,
  data = windpower,
  .progress = TRUE,
  .options = furrr_options(seed = TRUE) # Controls seed when parallelizing
)

end_time <- Sys.time()

end_time - start_time



################################################################################
#' Save results
################################################################################


saveRDS(results, file = "pipeline/optimization/optimal_locations_VaR_and_CVaR_5_at_a_time/20_choose_5_optimization_results_10000_daily.rds")

################################################################################
#' Investigate results
################################################################################

results <- readRDS("pipeline/optimization/optimal_locations_VaR_and_CVaR_5_at_a_time/20_choose_5_optimization_results_10000_daily.rds")

combination_list <- combn(paste0("loc_", seq(1, 20, 1)), m = 5, simplify = FALSE)

# Investigate results

values <- results %>% 
  map(., ~{.x$CVaR}) %>% 
  unlist()

weights <- results %>% 
  map(., ~{.x$weights}) %>% 
  do.call(rbind, .)


# Want to find the 10 best solutions

values <- cbind(values, "index" = seq(1, length(values), 1)) # Add row numbers to CVaR values

top_10 <- head(values[order(values[, "values"], decreasing = FALSE), ], n = 10)

opt_solutions <- list()

for(i in 1:nrow(top_10)){
  
  opt_solutions[[i]] <- results[[top_10[i, "index"]]]
  opt_solutions[[i]]$locations <- combination_list[[top_10[i, "index"]]]
}


# Let's create a matrix of the 10 best locations

best_loc_matrix <- opt_solutions %>% 
  map(., ~{.x$locations}) %>% 
  do.call(rbind, .)

colnames(best_loc_matrix) <- paste0("best_", seq(1, ncol(best_loc_matrix), 1))

best_weights_matrix <- opt_solutions %>% 
  map(., ~{.x$weights}) %>% 
  do.call(rbind, .)

colnames(best_weights_matrix) <- paste0("weight_", seq(1, ncol(best_weights_matrix), 1))

# Let's also collect the 10 best CVaR numbers

best_CVaR <- opt_solutions %>% 
  map(., ~{.x$CVaR}) %>% 
  do.call(rbind, .) %>% 
  cbind(c("Best", "2nd Best", "3rd Best", "4th Best", "5th Best", 
             "6th Best", "7th Best", "8th Best", "9th Best", "10th Best"),
        .) %>% 
  as_tibble() %>% 
  rename(Ranking = V1,
         CVaR = V2) %>% 
  mutate(CVaR = abs(round(as.numeric(CVaR), digits = 2)))

################################################################################
#' Calculate average wind production for each of the top 10 best locations
#' to get a feeling of potential trade-offs.
################################################################################

# Write function to calculate mean wind production

mean_production <- function(weights, locations, data){
  
  data <- data[, locations]
  
  production <- data%*%weights
  
  return(mean(production))
  
}

mean_matrix <- matrix(nrow = nrow(best_loc_matrix), ncol = 2)
colnames(mean_matrix) <- c("mean_production", "CVaR_ranking")


windpower <- readRDS("pipeline/vine_copula/simulate_windpower/windpower_10000_daily.rds")
colnames(windpower) <- paste0("loc_", seq(1, 20, 1))

for(i in 1:nrow(mean_matrix)){
  mean_matrix[i, ] <- c(mean_production(weights = best_weights_matrix[i, ],
                                        locations = best_loc_matrix[i, ],
                                        data = windpower),
                        i)
}

ordered_mean_matrix <- mean_matrix[order(mean_matrix[, "mean_production"], decreasing = TRUE), ]


################################################################################
#' Save results
################################################################################

saveRDS(best_loc_matrix, file = "pipeline/optimization/optimal_locations_VaR_and_CVaR_5_at_a_time/best_loc_matrix_10000_daily.rds")
saveRDS(best_weights_matrix, file = "pipeline/optimization/optimal_locations_VaR_and_CVaR_5_at_a_time/best_weights_matrix_10000_daily.rds")
saveRDS(ordered_mean_matrix, file = "pipeline/optimization/optimal_locations_VaR_and_CVaR_5_at_a_time/ordered_mean_matrix_10000_daily.rds")
saveRDS(best_CVaR, file = "pipeline/optimization/optimal_locations_VaR_and_CVaR_5_at_a_time/best_CVaR_10000_daily.rds")
