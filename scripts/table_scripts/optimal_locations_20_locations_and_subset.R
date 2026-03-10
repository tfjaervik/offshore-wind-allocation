################################################################################
#' Author: Thomas Michael Fj??rvik
#' This script does the following:
#' Create results tables for the optimal allocations of wind farms. The setup
#' is that we build at a maximum of 5 locations at a time. Panel A displays
#' the results when any combination of 5 locations is permitted and panel B
#' displays the results when only location 8-20 is under consideration.
################################################################################

################################################################################
#' Load packages
################################################################################

library(tidyverse)
library(kableExtra)

################################################################################
#' Panel A
################################################################################

best_loc_matrix <- readRDS(file = "pipeline/optimization/optimal_locations_VaR_and_CVaR_5_at_a_time/best_loc_matrix_10000_daily.rds")
best_weights_matrix <- readRDS(file = "pipeline/optimization/optimal_locations_VaR_and_CVaR_5_at_a_time/best_weights_matrix_10000_daily.rds")
ordered_mean_matrix <- readRDS(file = "pipeline/optimization/optimal_locations_VaR_and_CVaR_5_at_a_time/ordered_mean_matrix_10000_daily.rds")
best_CVaR <- readRDS(file = "pipeline/optimization/optimal_locations_VaR_and_CVaR_5_at_a_time/best_CVaR_10000_daily.rds")


colnames(best_loc_matrix) <- c("1st", "2nd", "3rd", "4th", "5th")
colnames(best_weights_matrix) <- colnames(best_loc_matrix) 

best_loc_matrix <- apply(best_loc_matrix, 2, str_extract, pattern = "\\d+") # Keep only digits in locations


best_weights_tibble <- best_weights_matrix %>% 
  round(., digits = 2) %>% 
  as_tibble() %>% 
  rename_with(~ paste0(.x, "_weight"))

best_loc_tibble <- best_loc_matrix %>% 
  as_tibble() %>% 
  rename_with(~ paste0(.x, "_loc")) #%>% 


colnames(ordered_mean_matrix) <- c("Mean Production", "Ranking")
ordered_mean_matrix <- ordered_mean_matrix[, c("Ranking", "Mean Production")]
ordered_mean_matrix[, 2] <- round(ordered_mean_matrix[, 2], digits = 2) 
ordered_mean_matrix <- ordered_mean_matrix %>% 
  as_tibble() %>% 
  mutate(Ranking = case_when(Ranking == 1 ~ "Best",
                             Ranking == 2 ~ "2nd Best",
                             Ranking == 3 ~ "3rd Best",
                             Ranking == 4 ~ "4th Best",
                             Ranking == 5 ~ "5th Best",
                             Ranking == 6 ~ "6th Best",
                             Ranking == 7 ~ "7th Best",
                             Ranking == 8 ~ "8th Best",
                             Ranking == 9 ~ "9th Best",
                             Ranking == 10~ "10th Best"))


best_locations_A <- best_loc_tibble %>% 
  cbind(best_weights_tibble) %>% 
  as_tibble() %>% 
  mutate(Ranking = c("Best", "2nd Best", "3rd Best", "4th Best", "5th Best",
                     "6th Best", "7th Best", "8th Best", "9th Best", "10th Best"),
         .before = '1st_loc') %>% 
  left_join(best_CVaR) %>% 
  left_join(ordered_mean_matrix) %>% 
  add_row() %>% 
  mutate_all(~replace(., is.na(.), ""))

colnames(best_locations_A) <- c("Ranking", "1st", "2nd", "3rd", "4th", "5th",
                                "1st", "2nd", "3rd", "4th", "5th", "CVaR", "Mean")

################################################################################
#' Panel B
################################################################################

locs <- readRDS("pipeline/optimization/optimal_locations_VaR_and_CVaR_5_at_a_time/20_choose_5_optimization_results_10000_daily.rds")
combination_list <- combn(paste0("loc_", seq(1, 20, 1)), m = 5, simplify = FALSE)

keep_idx <- sapply(combination_list, function(x) {
  nums <- as.numeric(sub("loc_", "", x))
  all(nums >= 8 & nums <= 20)
})

keep_locs <- locs[keep_idx]
keep_comb <- combination_list[keep_idx]

values <- keep_locs %>% 
  map(., ~{.x$CVaR}) %>% 
  unlist()

weights <- keep_locs %>% 
  map(., ~{.x$weights}) %>% 
  do.call(rbind, .)

values <- cbind(values, "index" = seq(1, length(values), 1)) # Add row numbers to CVaR values

top_10 <- head(values[order(values[, "values"], decreasing = FALSE), ], n = 10)

opt_solutions <- list()

for(i in 1:nrow(top_10)){
  
  opt_solutions[[i]] <- keep_locs[[top_10[i, "index"]]]
  opt_solutions[[i]]$locations <- keep_comb[[top_10[i, "index"]]]
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


colnames(best_loc_matrix) <- c("1st", "2nd", "3rd", "4th", "5th")
colnames(best_weights_matrix) <- colnames(best_loc_matrix) 

best_loc_matrix <- apply(best_loc_matrix, 2, str_extract, pattern = "\\d+") # Keep only digits in locations


best_weights_tibble <- best_weights_matrix %>% 
  round(., digits = 2) %>% 
  as_tibble() %>% 
  rename_with(~ paste0(.x, "_weight"))

best_loc_tibble <- best_loc_matrix %>% 
  as_tibble() %>% 
  rename_with(~ paste0(.x, "_loc"))#%>% 


ordered_mean_tibble <- ordered_mean_matrix %>% 
  as_tibble() %>% 
  mutate(CVaR_ranking = as.character(CVaR_ranking),
         join_column = case_when(
           CVaR_ranking == 1 ~ "Best",
           CVaR_ranking == 2 ~ "2nd Best",
           CVaR_ranking == 3 ~ "3rd Best",
           CVaR_ranking == 4 ~ "4th Best",
           CVaR_ranking == 5 ~ "5th Best",
           CVaR_ranking == 6 ~ "6th Best",
           CVaR_ranking == 7 ~ "7th Best",
           CVaR_ranking == 8 ~ "8th Best",
           CVaR_ranking == 9 ~ "9th Best",
           CVaR_ranking == 10~ "10th Best"
         )) %>% 
  left_join(best_CVaR, join_by(join_column==Ranking)) %>% 
  select(join_column, mean_production) %>% 
  rename(Mean = mean_production)

best_locations_B <- best_loc_tibble %>% 
  cbind(best_weights_tibble) %>% 
  as_tibble() %>% 
  mutate(Ranking = c("Best", "2nd Best", "3rd Best", "4th Best", "5th Best",
                     "6th Best", "7th Best", "8th Best", "9th Best", "10th Best"),
         .before = '1st_loc') %>% 
  left_join(best_CVaR) %>% 
  left_join(ordered_mean_tibble, join_by(Ranking==join_column)) %>% 
  mutate(Mean = round(Mean, 2))



colnames(best_locations_B) <- c("Ranking", "1st", "2nd", "3rd", "4th", "5th",
                              "1st", "2nd", "3rd", "4th", "5th", "CVaR", "Mean")

################################################################################
#' Create table
################################################################################

top_header <- c(" " = 1, "Location" = 5, 
                "Corresponding weight" = 5,
                "Production metrics" = 2)

locations <- rbind(best_locations_A, best_locations_B)

table_tex <- kable(locations,
                          format = "latex",
                          align = c("c"),
                          booktabs = T,
                          escape = F,
                          caption = "Best 5 wind farm locations.",
                          label = "best_5_locations_10000_daily_combined") %>% 
  kable_styling() %>% 
  add_header_above(top_header, escape = FALSE) %>%     # Statistic names on top
  group_rows(., group_label = "Panel A. Best allocations considering all 20 regions.", start_row = 0, end_row = 11,
             bold = F, italic = T, indent = F,
             underline = F, 
             escape = FALSE) %>%
  group_rows(., group_label = "Panel B. Best allocations considering regions 8 to 20.", start_row = 12, end_row = 20,
             bold = F, italic = T, indent = F,
             underline = F, 
             escape = FALSE) %>%
  TexTools::ltx_caption(., tbl_note = 
                          "This table displays the optimal wind farm locations 
                        given that it is only possible to build at 5 locations. Panel A displays the results for the case when any combination 
                        of the 20 regions under consideration is permitted. Panel B displays the results for the case when only
                        combinations of regions 8 to 20 are permitted." , 
                        print_tbl = TRUE) %>% 
  TexTools::ltx_placement(tbl_placement = "h")

# Write table to latex file
# Note: TexTools::write_tex_simplified does not change table contents.
# It only makes slight layout adjustments, some of which have been overwritten later anyway.
# The table and its contents are not affected.
TexTools::write_tex_simplified(table_tex, path = "_tables/best_5_locations_10000_daily_combined")
