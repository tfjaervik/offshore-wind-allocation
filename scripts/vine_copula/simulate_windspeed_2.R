################################################################################
#' Author: Thomas Michael Fj??rvik
#' This script does the following:
#' Simulates data from a vine copula fitted on daily windspeed data and 
#' transforms the data to artificial wind speed data. Note: This script
#' uses data that earlier has been Box-Cox transformed before fitting ARMA
#' models using forecast::BoxCox instead of the earlier MASS:boxcox
################################################################################

################################################################################
#' Load packages
################################################################################

library(tidyverse)
library(VineCopula)
library(forecast)

################################################################################
#' Functions
################################################################################

# Add the following two functions to be able to simulate from the estimated
# ARIMA objects that has been estimated with forecast::auto.arima(). For some 
# reason, these two functions are not in the package "forecast", though they 
# should be. They have been taken directly from the package author's 
# github, so should be correct. 

simulate.Arima <- function(object, nsim = length(object$x), seed = NULL, xreg = NULL, future = TRUE, bootstrap = FALSE, innov = NULL, lambda = object$lambda, ...) {
  # Error check:
  if (object$arma[7] < 0) {
    stop("Value for seasonal difference is < 0. Must be >= 0")
  } else if ((sum(object$arma[c(3, 4, 7)]) > 0) && (object$arma[5] < 2)) {
    stop("Invalid value for seasonal period")
  }
  
  # Check if data is included
  x <- object$x <- getResponse(object)
  if (is.null(x)) {
    n <- 0
    future <- FALSE
    if (nsim == 0L) {
      nsim <- 100
    }
  } else {
    if (is.null(tsp(x))) {
      x <- ts(x, frequency = 1, start = 1)
    }
    if (!is.null(lambda)) {
      x <- BoxCox(x, lambda)
      lambda <- attr(x, "lambda")
    }
    n <- length(x)
  }
  
  # Check xreg
  if (!is.null(xreg)) {
    xreg <- as.matrix(xreg)
    nsim <- nrow(xreg)
  }
  use.drift <- is.element("drift", names(object$coef))
  usexreg <- (!is.null(xreg) | use.drift | !is.null(object$xreg))
  xm <- oldxm <- 0
  if (use.drift) {
    # Remove existing drift column
    if (NCOL(xreg) == 1 && all(diff(xreg) == 1)) {
      xreg <- NULL
    } else if (!is.null(colnames(xreg))) {
      xreg <- xreg[, colnames(xreg) != "drift", drop = FALSE]
    }
    # Create new drift column
    xreg <- cbind(drift = as.matrix(seq(nsim) + n * future), xreg)
  }
  # Check xreg has the correct dimensions
  if (usexreg) {
    if (is.null(xreg)) {
      stop("xreg argument missing")
    } else if (is.null(object$xreg)) {
      stop("xreg not required")
    } else if (NCOL(xreg) != NCOL(object$xreg)) {
      stop("xreg has incorrect dimension.")
    }
  }
  
  ######## Random Seed Code
  if (is.null(innov)) {
    if (!exists(".Random.seed", envir = .GlobalEnv)) {
      runif(1)
    }
    if (is.null(seed)) {
      RNGstate <- .Random.seed
    } else {
      R.seed <- .Random.seed
      set.seed(seed)
      RNGstate <- structure(seed, kind = as.list(RNGkind()))
      on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    }
  } else {
    nsim <- length(innov)
  }
  ######## End Random seed code
  
  # Check for seasonal ARMA components and set flag accordingly. This will be used later in myarima.sim()
  flag.s.arma <- (sum(object$arma[c(3, 4)]) > 0)
  # Check for Seasonality in ARIMA model
  if (sum(object$arma[c(3, 4, 7)]) > 0) {
    # return(simulateSeasonalArima(object, nsim=nsim, seed=seed, xreg=xreg, future=future, bootstrap=bootstrap, ...))
    if (sum(object$model$phi) == 0) {
      ar <- NULL
    } else {
      ar <- as.double(object$model$phi)
    }
    if (sum(object$model$theta) == 0) {
      ma <- NULL
    } else {
      ma <- as.double(object$model$theta)
    }
    order <- c(length(ar), object$arma[6], length(ma))
    
    if (future) {
      model <- list(
        order = order, ar = ar, ma = ma, sd = sqrt(object$sigma2), residuals = residuals(object),
        seasonal.difference = object$arma[7], seasonal.period = object$arma[5], flag.seasonal.arma = flag.s.arma,
        seasonal.order = object$arma[c(3, 7, 4)]
      )
    } else {
      model <- list(order = order, ar = ar, ma = ma, sd = sqrt(object$sigma2), residuals = residuals(object))
    }
    flag.seasonal.diff <- (object$arma[7] > 0)
  } else {
    #### Non-Seasonal ARIMA specific code: Set up the model
    order <- object$arma[c(1, 6, 2)]
    if (order[1] > 0) {
      ar <- object$model$phi[1:order[1]]
    } else {
      ar <- NULL
    }
    if (order[3] > 0) {
      ma <- object$model$theta[1:order[3]]
    } else {
      ma <- NULL
    }
    if (object$arma[2] != length(ma)) {
      stop("MA length wrong")
    } else if (object$arma[1] != length(ar)) {
      stop("AR length wrong")
    }
    
    if (future) {
      model <- list(
        order = object$arma[c(1, 6, 2)], ar = ar, ma = ma, sd = sqrt(object$sigma2), residuals = residuals(object),
        seasonal.difference = 0, flag.seasonal.arma = flag.s.arma, seasonal.order = c(0, 0, 0), seasonal.period = 1
      )
    } else {
      model <- list(order = object$arma[c(1, 6, 2)], ar = ar, ma = ma, sd = sqrt(object$sigma2), residuals = residuals(object))
    }
    flag.seasonal.diff <- FALSE
    ### End non-seasonal ARIMA specific code
  }
  
  if (bootstrap) {
    res <- na.omit(c(model$residuals) - mean(model$residuals, na.rm = TRUE))
    e <- sample(res, nsim, replace = TRUE)
  } else if (is.null(innov)) {
    e <- rnorm(nsim, 0, model$sd)
  } else if (length(innov) == nsim) {
    e <- innov
  } else {
    stop("Length of innov must be equal to nsim")
  }
  
  narma <- sum(object$arma[1L:4L])
  if (length(object$coef) > narma) {
    if (names(object$coef)[narma + 1L] == "intercept") {
      xreg <- cbind(intercept = rep(1, nsim), xreg)
      if (future) {
        object$xreg <- cbind(intercept = rep(1, n), object$xreg)
      }
    }
    if (!is.null(xreg)) {
      xm <- if (narma == 0) {
        drop(as.matrix(xreg) %*% object$coef)
      } else {
        drop(as.matrix(xreg) %*% object$coef[-(1L:narma)])
      }
      if (future) {
        oldxm <- if (narma == 0) {
          drop(as.matrix(object$xreg) %*% object$coef)
        } else {
          drop(as.matrix(object$xreg) %*% object$coef[-(1L:narma)])
        }
      }
    }
  }
  if (future) {
    sim <- myarima.sim(model, nsim, x - oldxm, e = e) + xm
  } else {
    if (flag.seasonal.diff) {
      zeros <- object$arma[5] * object$arma[7]
      sim <- arima.sim(model, nsim, innov = e)
      sim <- diffinv(sim, lag = object$arma[5], differences = object$arma[7])[-(1:zeros)]
      sim <- tail(sim, nsim) + xm
    } else {
      sim <- tail(arima.sim(model, nsim, innov = e), nsim) + xm
    }
    if (!is.null(x)) {
      sim <- ts(sim, start = tsp(x)[1], frequency = tsp(x)[3])
    } else {
      sim <- ts(sim, frequency = object$frequency)
    }
    
    # If model is non-stationary, then condition simulated data on first observation
    if (!is.null(x) & (model$order[2] > 0 || flag.seasonal.diff)) {
      sim <- sim - sim[1] + x[1]
    }
  }
  if (!is.null(lambda)) {
    sim <- InvBoxCox(sim, lambda)
  }
  
  return(sim)
}

myarima.sim <- function(model, n, x, e, ...) {
  start.innov <- residuals(model)
  innov <- e
  data <- x
  # Remove initial NAs
  first.nonmiss <- which(!is.na(x))[1]
  if (first.nonmiss > 1) {
    tsp.x <- tsp(x)
    start.x <- tsp.x[1] + (first.nonmiss - 1) / tsp.x[3]
    x <- window(x, start = start.x)
    start.innov <- window(start.innov, start = start.x)
  }
  model$x <- x
  n.start <- length(x)
  x <- ts(c(start.innov, innov), start = 1 - n.start, frequency = model$seasonal.period)
  flag.noadjust <- FALSE
  if (is.null(tsp(data))) {
    data <- ts(data, frequency = 1, start = 1)
  }
  if (!is.list(model)) {
    stop("'model' must be list")
  }
  if (n <= 0L) {
    stop("'n' must be strictly positive")
  }
  p <- length(model$ar)
  q <- length(model$ma)
  d <- 0
  D <- model$seasonal.difference
  m <- model$seasonal.period
  if (!is.null(ord <- model$order)) {
    if (length(ord) != 3L) {
      stop("'model$order' must be of length 3")
    }
    if (p != ord[1L]) {
      stop("inconsistent specification of 'ar' order")
    }
    if (q != ord[3L]) {
      stop("inconsistent specification of 'ma' order")
    }
    d <- ord[2L]
    if (d != round(d) || d < 0) {
      stop("number of differences must be a positive integer")
    }
  }
  if (p) {
    minroots <- min(Mod(polyroot(c(1, -model$ar))))
    if (minroots <= 1) {
      stop("'ar' part of model is not stationary")
    }
  }
  if (length(model$ma)) {
    # MA filtering
    x <- stats::filter(x, c(1, model$ma), method = "convolution", sides = 1L)
    x[seq_along(model$ma)] <- 0
  }
  ## AR "filtering"
  len.ar <- length(model$ar)
  
  if (length(model$ar) && (len.ar <= length(data))) {
    if ((D != 0) && (d != 0)) {
      diff.data <- diff(data, lag = 1, differences = d)
      diff.data <- diff(diff.data, lag = m, differences = D)
    } else if ((D != 0) && (d == 0)) {
      diff.data <- diff(data, lag = model$seasonal.period, differences = D)
    } else if ((D == 0) && (d != 0)) {
      diff.data <- diff(data, lag = 1, differences = d)
    } else {
      diff.data <- data
    }
    
    x.new.innovations <- x[(length(start.innov) + 1):length(x)]
    x.with.data <- c(diff.data, x.new.innovations)
    
    for (i in (length(diff.data) + 1):length(x.with.data))
    {
      lagged.x.values <- x.with.data[(i - len.ar):(i - 1)]
      ar.coefficients <- model$ar[length(model$ar):1]
      sum.multiplied.x <- sum((lagged.x.values * ar.coefficients)[abs(ar.coefficients) > .Machine$double.eps])
      x.with.data[i] <- x.with.data[i] + sum.multiplied.x
    }
    
    x.end <- x.with.data[(length(diff.data) + 1):length(x.with.data)]
    x <- ts(x.end, start = 1, frequency = model$seasonal.period)
    flag.noadjust <- TRUE
  } else if (length(model$ar)) # but data too short
  {
    # AR filtering for all other cases where AR is used.
    x <- stats::filter(x, model$ar, method = "recursive")
  }
  if ((d == 0) && (D == 0) && (flag.noadjust == FALSE)) # Adjust to ensure end matches approximately
  {
    # Last 20 diffs
    if (n.start >= 20) {
      xdiff <- (model$x - x[1:n.start])[n.start - (19:0)]
    } else {
      xdiff <- model$x - x[1:n.start]
    }
    # If all same sign, choose last
    if (all(sign(xdiff) == 1) || all(sign(xdiff) == -1)) {
      xdiff <- xdiff[length(xdiff)]
    } else { # choose mean.
      xdiff <- mean(xdiff)
    }
    x <- x + xdiff
  }
  if ((n.start > 0) && (flag.noadjust == FALSE)) {
    x <- x[-(1:n.start)]
  }
  
  ######## Undo all differences
  
  if ((D > 0) && (d == 0)) {
    # Seasonal undifferencing, if there is no regular differencing
    i <- length(data) - D * m + 1
    seasonal.xi <- data[i:length(data)]
    length.s.xi <- length(seasonal.xi)
    x <- diffinv(x, lag = m, differences = D, xi = seasonal.xi)[-(1:length.s.xi)]
  } else if ((d > 0) && (D == 0)) {
    # Regular undifferencing, if there is no seasonal differencing
    x <- diffinv(x, differences = d, xi = data[length(data) - (d:1) + 1])[-(1:d)]
  } else if ((d > 0) && (D > 0)) {
    # Undifferencing for where the differencing is both Seasonal and Non-Seasonal
    # Regular first
    delta.four <- diff(data, lag = m, differences = D)
    regular.xi <- delta.four[(length(delta.four) - D):length(delta.four)]
    x <- diffinv(x, differences = d, xi = regular.xi[length(regular.xi) - (d:1) + 1])[-(1:d)]
    
    # Then seasonal
    i <- length(data) - D * m + 1
    seasonal.xi <- data[i:length(data)]
    length.s.xi <- length(seasonal.xi)
    x <- diffinv(x, lag = m, differences = D, xi = seasonal.xi)
    x <- x[-(1:length.s.xi)]
  }
  
  x <- ts(x[1:n], frequency = frequency(data), start = tsp(data)[2] + 1 / tsp(data)[3])
  return(x)
}


################################################################################
#' Load data
################################################################################

# Load copula

daily_copula <- readRDS("pipeline/vine_copula/fitting_vine_copula_daily_data_2(forecast-BoxCox)/daily_copula.rds")

# Load Yeo-Johnson objects (used to be loaded from same script as copula, but changed 
# so that we first adjust for skewness and kurtosis before standardizing)

yj_objects <- readRDS("pipeline/marginal_distributions/all_regions_daily/yt_objects.rds")
delta_vec <- readRDS("pipeline/marginal_distributions/all_regions_daily/delta_vec.rds")

# Load seasonal volatility

volatility_list <- readRDS("pipeline/marginal_distributions/all_regions_daily/volatility_list.rds")

# Load seasonal levels

seasonality_list <- readRDS("pipeline/marginal_distributions/all_regions_daily/seasonality_list.rds")

# Load ARIMA objects

arma_models <- readRDS("pipeline/marginal_distributions/all_regions_daily/arma_models.rds")

# Load lambda values

lambda <- readRDS("pipeline/marginal_distributions/all_regions_daily/lambda.rds")


################################################################################
#' Simulate from copula
################################################################################
set.seed(69420)

# Run code below if simulated data has not already been saved.
# Note: This can take a long time.

# n_years <- 10000
# sim_data <- RVineSim(n_years*365, RVM = daily_copula)
# 
# saveRDS(
#   sim_data, file = "pipeline/vine_copula/simulate_windspeed_2/sim_data.rds"
# )

sim_data <- readRDS("pipeline/vine_copula/simulate_windspeed_2/sim_data.rds")

################################################################################
#' Create artificial windspeed data
################################################################################

# Transform residuals using qnorm

df <- apply(sim_data, 2, qnorm)

# Multiply residuals with corresponding seasonal volatility/empirical standard
# deviations, depending on what has been estimated earlier.

for(i in 1:ncol(df)){
  df[, i] <- df[, i]*sqrt(volatility_list[[i]])
}

source("scripts/functions/helper_functions.R")

# Inverse skewness and kurtosis transformations

for(i in 1:ncol(df)){
  df[, i] <- adjust_skewness_kurtosis_inverse(df[, i], delta = delta_vec[i], yj_object = yj_objects[[i]])
}

# Simulate from ARMA models using the columns of df as innovations for the 20
# different ARMA simulations
# Add seasonal levels to get simulated windspeed data
# Lastly, apply inverse Box-Cox transformation

deseasonalized_df <- matrix(nrow = nrow(df), ncol = ncol(df))
windspeed <- matrix(nrow = nrow(df), ncol = ncol(df))


for(i in 1:ncol(deseasonalized_df)){
  deseasonalized_df[, i] <- simulate.Arima(arma_models[[i]], innov = df[, i])
  windspeed[, i] <- forecast::InvBoxCox(deseasonalized_df[, i] + seasonality_list[[i]], lambda = lambda[i])
}

# Check summary for some windspeed columns

windspeed_summary <- summary(windspeed[, 1])

for(i in 2:ncol(windspeed)){
  windspeed_summary <- rbind(windspeed_summary, summary(windspeed[, i]))
}



################################################################################
#' Save simulated windspeed data
################################################################################

saveRDS(windspeed, file = "pipeline/vine_copula/simulate_windspeed_2/windspeed_10000_daily.rds")
