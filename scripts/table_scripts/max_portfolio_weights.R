################################################################################
#' Author: Thomas Michael Fj??rvik
#' This script does the following:
#' Creates a table showing NVE candidate areas and corresponding maximum 
#' portfolio weights
################################################################################

################################################################################
#' Load packages
################################################################################

library(tidyverse)
library(readxl)
library(kableExtra)
################################################################################
#' Load data
################################################################################

area_path <- "data/NVE_areal.xlsx"
areas <- read_excel(area_path)

one_turbine <- 15
max_nt <- 2000

areas <- areas %>% 
  mutate(pot_cap_3_33 = `area(km2)` * 3.5 * 0.33,
         pot_cap_3_67 = pot_cap_3_33 * (0.67/0.33),
         pot_cap_3_100= pot_cap_3_33 / 0.33,
         pot_cap_5_33 = pot_cap_3_33 * 5/3.5,
         pot_cap_5_67 = pot_cap_3_67 * 5/3.5,
         pot_cap_5_100= pot_cap_3_100 * 5/3.5,
         pot_cap_7_33 = pot_cap_3_33 * 7.5/3.5,
         pot_cap_7_67 = pot_cap_3_67 * 7.5/3.5,
         pot_cap_7_100= pot_cap_3_100 * 7.5/3.5
         ) %>% 
  mutate(max_turbines_3_33 = floor(pot_cap_3_33 / one_turbine),
         max_turbines_3_67 = floor(pot_cap_3_67 / one_turbine),
         max_turbines_3_100= floor(pot_cap_3_100/ one_turbine),
         max_turbines_5_33 = floor(pot_cap_5_33 / one_turbine),
         max_turbines_5_67 = floor(pot_cap_5_67 / one_turbine),
         max_turbines_5_100= floor(pot_cap_5_100/ one_turbine),
         max_turbines_7_33 = floor(pot_cap_7_33 / one_turbine),
         max_turbines_7_67 = floor(pot_cap_7_67 / one_turbine),
         max_turbines_7_100= floor(pot_cap_7_100/ one_turbine)
         ) %>% 
  mutate(max_w_3_33 = round(max_turbines_3_33 / max_nt * 100, 1),
         max_w_3_67 = round(max_turbines_3_67 / max_nt * 100, 1),
         max_w_3_100= round(max_turbines_3_100/ max_nt * 100, 1),
         max_w_5_33 = round(max_turbines_5_33 / max_nt * 100, 1),
         max_w_5_67 = round(max_turbines_5_67 / max_nt * 100, 1),
         max_w_5_100= round(max_turbines_5_100/ max_nt * 100, 1),
         max_w_7_33 = round(max_turbines_7_33 / max_nt * 100, 1),
         max_w_7_67 = round(max_turbines_7_67 / max_nt * 100, 1),
         max_w_7_100= round(max_turbines_7_100/ max_nt * 100, 1)
         ) %>% 
  mutate(pot_cap_3_33 = round(pot_cap_3_33 ),
         pot_cap_3_67 = round(pot_cap_3_67 ),
         pot_cap_3_100= round(pot_cap_3_100),
         pot_cap_5_33 = round(pot_cap_5_33 ),
         pot_cap_5_67 = round(pot_cap_5_67 ),
         pot_cap_5_100= round(pot_cap_5_100),
         pot_cap_7_33 = round(pot_cap_7_33 ),
         pot_cap_7_67 = round(pot_cap_7_67 ),
         pot_cap_7_100= round(pot_cap_7_100)
         ) %>% 
  mutate(
    max_w_3_33 = paste0(max_w_3_33 , "\\%"),
    max_w_3_67 = paste0(max_w_3_67 , "\\%"),
    max_w_3_100= paste0(max_w_3_100, "\\%"),
    max_w_5_33 = paste0(max_w_5_33 , "\\%"),
    max_w_5_67 = paste0(max_w_5_67 , "\\%"),
    max_w_5_100= paste0(max_w_5_100, "\\%"),
    max_w_7_33 = paste0(max_w_7_33 , "\\%"),
    max_w_7_67 = paste0(max_w_7_67 , "\\%"),
    max_w_7_100= paste0(max_w_7_100, "\\%")
  ) %>% 
  mutate(ID = seq(1, 20, 1)) %>% 
  select(ID, name,`area(km2)`, 
         pot_cap_3_100, max_turbines_3_100, max_w_3_100,
         pot_cap_5_100, max_turbines_5_100, max_w_5_100,
         pot_cap_7_100, max_turbines_7_100, max_w_7_100
  ) %>% 
  mutate(across(everything(), as.character))

#areas2 <- bind_rows(setNames(as.list(colnames(areas)),colnames(areas)) %>% as_tibble_row(), areas)
#areas2

################################################################################
#' Create table
################################################################################

colnames(areas) <- c(
  "ID",
  "Location",
  "Area",
  "Potential Capacity (MW)", "Max NoT", "Max portfolio weight",    # 3
  "Potential Capacity (MW)", "Max NoT", "Max portfolio weight",    # 5
  "Potential Capacity (MW)", "Max NoT", "Max portfolio weight"    # 7
)

# Define the two-level header
top_header <- c(" " = 3, "Capacity density 3.5 MW $km^{-2}$" = 3, 
                "Capacity density 5 MW $km^{-2}$" = 3, 
                "Capacity density 7.5 MW $km^{-2}$" = 3)

# Create the table
combined_table <- areas %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    caption = "Power production capacity by location",
    label = "max_portfolio_weights",
    align = "c",
    escape = FALSE
  ) %>%
  kable_styling(latex_options = c("scale_down", "hold_position", "rotate")) %>%
  add_header_above(top_header, escape = FALSE) %>%     # Statistic names on top
  group_rows("N", 1, 4) %>% 
  group_rows("NW", 5, 7) %>% 
  group_rows("W", 8, 13) %>% 
  group_rows("SW", 14, 19) %>% 
  group_rows("SE", 20, 20) %>% 
  TexTools::ltx_caption(
    tbl_note = "This table displays the potential capacity, maximum number of turbines
    and maximum portfolio weights for the different locations in this study. They have been
    calculated assuming a 100\\% utilization rate and assuming a maximum production of 15 MW per turbine")

# Replace [!h] with [H] in the generated table environment for exact placement by quarto/Latex.
#combined_table <- gsub("\\[!h\\]", "[H]", combined_table)

# 1. Combine all lines into one string
combined_table <- paste(combined_table, collapse = "\n")

# 2. Replace \begin{table}[!h] or \begin{table} with \begin{sidewaystable}
combined_table_sideways <- gsub(
  "\\\\begin\\{table\\}(\\[.*\\])?",       # pattern matches optional [!h]
  "\\\\begin{sidewaystable}",              # replacement
  combined_table,
  perl = TRUE
)

# 3. Replace \end{table} with \end{sidewaystable}
combined_table_sideways <- gsub(
  "\\\\end\\{table\\}",
  "\\\\end{sidewaystable}",
  combined_table_sideways
)

# Note: TexTools::write_tex_simplified does not change table contents.
# It only makes slight layout adjustments, some of which have been overwritten later anyway.
# The table and its contents are not affected.
TexTools::write_tex_simplified(combined_table_sideways, path = "_tables/max_portfolio_weights_table")
