################################################################################
#' Load packages
################################################################################
library(tidyverse)
library(patchwork)
library(tikzDevice)
library(fs)
library(kableExtra)

################################################################################
#' Load data
################################################################################

wind <- readRDS("data/NVE_windspeeds_1996-97.rds") %>% 
  filter(locID == 19)

################################################################################
#' Create data on different frequencies and apply box-cox to adjust for skewness
################################################################################

wind_daily <- wind %>% 
  group_by(day = date(datetime)) %>% 
  summarise(windspeed = mean(windspeed))

wind_weekly <- wind %>% 
  group_by(week = floor_date(datetime, "week")) %>% 
  summarise(windspeed = mean(windspeed)) 

wind_monthly <- wind %>% 
  group_by(month = floor_date(datetime, "month")) %>% 
  summarise(windspeed = mean(windspeed))

source("scripts/functions/helper_functions.R")

# Map over all the datasets and then map the list objects to the global environment

list("wind" = wind, "wind_daily" = wind_daily, "wind_weekly" = wind_weekly, "wind_monthly" = wind_monthly) %>% 
  map(., ~ .x %>% 
        mutate(ws = box_cox(windspeed))) %>% 
  list2env(., globalenv())  

################################################################################
#' Create plot of wind speeds on different frequencies
################################################################################


# Plot windspeed on different frequencies

p1 <- ggplot(data = wind, aes(x = datetime, y = ws)) +
  geom_line() +
  # Theme
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )


p2 <- ggplot(data = wind_daily, aes(x = day, y = ws)) +
  geom_line() +
  # Theme
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

p3 <- ggplot(data = wind_weekly, aes(x = week, y = ws)) +
  geom_line() +
  # Theme
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

p4 <- ggplot(data = wind_monthly, aes(x = month, y = ws)) +
  geom_line() + 
  # Theme
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

# speed_plot <- gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)


speed_plot <- (p1|p2)/(p3|p4)



#' Run entire block in one go

tikzDevice::tikz(file = path("_figures/speed_plot", 
                             ext = "tex"), 
                 width    = 6.4,  # Good dimensions for use in Overleaf
                 height   = 3.54, # Good dimensions for use in Overleaf 
                 sanitize = TRUE)
speed_plot
dev.off() 

################################################################################
#' Create plot of acf
################################################################################

acf_hourly <- wind %>% 
  pull(ws) %>% 
  forecast::ggAcf(., lag.max = 1000) + 
  labs(title = "Hourly") +
  # Theme
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

acf_daily <- wind_daily %>% 
  pull(ws) %>% 
  forecast::ggAcf(., lag.max = 1000) + 
  labs(title = "Daily") +
  # Theme
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

acf_weekly <- wind_weekly %>% 
  pull(ws) %>% 
  forecast::ggAcf(., lag.max = 1000) + 
  labs(title = "Weekly") + 
  # Theme
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

acf_monthly <- wind_monthly %>% 
  pull(ws) %>% 
  forecast::ggAcf(., lag.max = 1000) + 
  labs(title = "Monthly") +
  # Theme
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

acf_plot <- (acf_hourly|acf_daily)/(acf_weekly|acf_monthly)

tikzDevice::tikz(file = path("_figures/acf_plot", 
                             ext = "tex"), 
                 width    = 6.4,  # Good dimensions for use in Overleaf
                 height   = 3.54, # Good dimensions for use in Overleaf 
                 sanitize = TRUE)
acf_plot
dev.off() 

################################################################################
#' Fit seasonal function using ordinary least squares
################################################################################


# We have to add the time t to all our datasets, and so we simply add a sequence
# starting at 1 and ending at nrow(df). We also have to add the period scaling factor and
# the different harmonic terms. Having done this, we only need to estimate, 
# the a-coefficients, which can be done by ordinary least squares as the expression
# is then linear. 

list("wind" = wind, "wind_daily" = wind_daily, "wind_weekly" = wind_weekly, "wind_monthly" = wind_monthly) %>% 
  imap(., ~{
    if(.y %in% c("wind")){
      aux = 365*24
    } else if(.y %in% c("wind_daily")){
      aux = 365
    } else if(.y %in% c("wind_weekly")){
      aux = 52
    } else if(.y %in% c("wind_monthly")){
      aux = 12
    }
    .x %>% 
        mutate(t = seq(1, nrow(.x), by = 1),
               scale = aux,
               a_1 = cos((2*pi*t)/scale), 
               a_2 = sin((2*pi*t)/scale),
               a_3 = cos((4*pi*t)/scale),
               a_4 = sin((4*pi*t)/scale))
    }) %>% 
  list2env(., globalenv())


seasonal_regressions <- list("wind" = wind, "wind_daily" = wind_daily, 
                             "wind_weekly" = wind_weekly, 
                             "wind_monthly" = wind_monthly) %>% 
  map(., ~lm(ws ~ a_1 + a_2 + a_3 + a_4, data = .x))

################################################################################
#' Create table summarising coefficients and R-squared for the different models
################################################################################

# Get coefficient estimates from all regressions and put on table form

coef_table <- c("wind", "wind_daily", "wind_weekly", "wind_monthly") %>% 
  map(., ~{summary(
    seasonal_regressions[[.x]])$coefficients
    }) %>% 
  map(., ~{.x %>% 
      as_tibble() %>%
      mutate(coef_names = c("$a_{0}$", "$a_{1}$", "$a_{2}$",
                            "$a_{3}$", "$a_{4}$")) %>% 
      select(coef_names, Estimate) 
      }) %>% 
  purrr::reduce(., dplyr::left_join, by = "coef_names")

colnames(coef_table) <- c("Parameter", "Hourly", "Daily", "Weekly", "Monthly")


# Must add R-squared estimates

r_2 <- c("wind", "wind_daily", "wind_weekly", "wind_monthly") %>% 
  map(., ~{summary(
    seasonal_regressions[[.x]])$r.squared
  }) %>% 
  unlist(.)

r_2 <- c("$R^{2}$ (\\%)", 100*r_2)

# Add r-squared as bottom row to our coefficients table

coef_table <- coef_table %>% 
  rbind(., r_2) %>% 
  mutate_at(vars(-c("Parameter")), ~round(as.numeric(.x), digits = 2))

# Create and export latex table

seasonal_regressions_table <- kable(coef_table, 
                       format = "latex",
                       booktabs = T, 
                       caption = "Estimates for seasonal function $S_{t}$",
                       label = "seasonal_reg_table", 
                       escape = F,
                       align = "c",) %>% 
  kable_styling() %>% 
  # Note: TexTools::write_tex_simplified does not change table contents.
  # It only makes slight layout adjustments, some of which have been overwritten later anyway.
  # The table and its contents are not affected.
  TexTools::ltx_caption(tbl_note = "This table displays coefficient estimates for
                        the seasonal component of the time series of windspeed in
                        area 19 at different frequencies.") 

# Note: TexTools::write_tex_simplified does not change table contents.
# It only makes slight layout adjustments, some of which have been overwritten later anyway.
# The table and its contents are not affected.  
TexTools::write_tex_simplified(seasonal_regressions_table, path = "_tables/seasonal_regression_table_a19")

################################################################################
#' Create deasesonalized series and plot acf and pacf
################################################################################

# Get residuals from seasonal regressions

deseasonalized_series <- c("wind", "wind_daily", "wind_weekly" ,"wind_monthly") %>% 
  map(., ~{summary(
    seasonal_regressions[[.x]])$residuals
  })

names(deseasonalized_series) <- c("wind", "wind_daily", "wind_weekly" ,"wind_monthly")


list("wind" = wind, "wind_daily" = wind_daily, "wind_weekly" = wind_weekly, "wind_monthly" = wind_monthly) %>% 
  imap(., ~{.x %>% 
      mutate(deseasonalized = deseasonalized_series[[.y]])
    }) %>% 
  list2env(., globalenv())


# Plot acf and pacf of the deseasonalized series


acf_ds_hourly <- wind %>% 
  pull(deseasonalized) %>% 
  forecast::ggAcf(., lag.max = 1000) + 
  labs(title = "Hourly") +
  # Theme
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

acf_ds_daily <- wind_daily %>% 
  pull(deseasonalized) %>% 
  forecast::ggAcf(., lag.max = 1000) + 
  labs(title = "Daily") +
  # Theme
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

acf_ds_weekly <- wind_weekly %>% 
  pull(deseasonalized) %>% 
  forecast::ggAcf(., lag.max = 1000) + 
  labs(title = "Weekly") +
  # Theme
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

acf_ds_monthly <- wind_monthly %>% 
  pull(deseasonalized) %>% 
  forecast::ggAcf(., lag.max = 1000) + 
  labs(title = "Monthly") +
  # Theme
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

acf_ds_plot <- (acf_ds_hourly|acf_ds_daily)/(acf_ds_weekly|acf_ds_monthly)


#' Run entire block in one go

tikzDevice::tikz(file = path("_figures/acf_plot_deseasonalized_a19", 
                             ext = "tex"), 
                 width    = 6.4,  # Good dimensions for use in Overleaf
                 height   = 3.54, # Good dimensions for use in Overleaf 
                 sanitize = TRUE)
acf_ds_plot
dev.off() 

# Pacf

pacf_ds_hourly <- wind %>% 
  pull(deseasonalized) %>% 
  forecast::ggPacf(., lag.max = 1000) + 
  labs(title = "Hourly") +
  # Theme
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

pacf_ds_daily <- wind_daily %>% 
  pull(deseasonalized) %>% 
  forecast::ggPacf(., lag.max = 1000) + 
  labs(title = "Daily") +
  # Theme
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

pacf_ds_weekly <- wind_weekly %>% 
  pull(deseasonalized) %>% 
  forecast::ggPacf(., lag.max = 1000) + 
  labs(title = "Weekly") +
  # Theme
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

pacf_ds_monthly <- wind_monthly %>% 
  pull(deseasonalized) %>% 
  forecast::ggPacf(., lag.max = 1000) + 
  labs(title = "Monthly") +
  # Theme
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )


pacf_ds_plot <- (pacf_ds_hourly|pacf_ds_daily)/(pacf_ds_weekly|pacf_ds_monthly)

# Run block in one go
tikzDevice::tikz(file = path("_figures/pacf_plot_deseasonalized_a19", 
                             ext = "tex"), 
                 width    = 6.4,  # Good dimensions for use in Overleaf
                 height   = 3.54, # Good dimensions for use in Overleaf 
                 sanitize = TRUE)
pacf_ds_plot
dev.off() 