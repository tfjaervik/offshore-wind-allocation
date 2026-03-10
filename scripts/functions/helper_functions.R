################################################################################
#' Author: Thomas Michael Fj??rvik
#' This script does the following:
#' Contains useful functions used in the analysis
################################################################################

################################################################################
#' Box-Cox transformation
################################################################################

# x is a numeric vector

box_cox <- function(z){

  reg <- lm(z ~ 1, x = TRUE, y = TRUE) # boxcox() needs the model.matrix x and the response y, which defaults to FALSE
  bc <- MASS::boxcox(reg, objective.name = "Log-Likelihood")
  lambda <- bc$x[which.max(bc$y)]
  
  new_values <- (z^lambda - 1)/lambda
  
  return(new_values)
}

box_cox_lambda <- function(z){
  reg <- lm(z ~ 1, x = TRUE, y = TRUE) # boxcox() needs the model.matrix x and the response y, which defaults to FALSE
  bc <- MASS::boxcox(reg, objective.name = "Log-Likelihood")
  lambda <- bc$x[which.max(bc$y)]
  return(lambda)
}

box_cox_forecast <- function(z){
  transformed_data <- forecast::BoxCox(z, lambda = "auto")
}


# adjust_skewness_kurtosis <- function(delta = 0.025, yj_object = NULL){
#   
#   predict(yj_object) %>% LambertW::G_delta(., delta = delta)
# }

adjust_skewness_kurtosis <- function(delta = 0.025, yj_object = NULL){
  
  x <- predict(yj_object)
  x <- as.numeric(x)
  as.numeric(LambertW::G_delta(x, delta = delta))
}

# adjust_skewness_kurtosis_inverse <- function(x, delta = 0.025, yj_object = NULL){
#   
#   LambertW::W_delta(x, delta = delta) %>% predict(yj_object, newdata = ., inverse = TRUE)
# }


adjust_skewness_kurtosis_inverse <- function(x, delta = 0.025, yj_object = NULL){
  
  x <- as.numeric(x)
  
  x_trans <- as.numeric(LambertW::W_delta(x, delta = delta))
  pred <- predict(yj_object, newdata =x_trans, inverse = TRUE)
  
  as.numeric(pred)
}
