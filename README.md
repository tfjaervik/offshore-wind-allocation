# Managing Downside Risk and Spatial Allocation of Offshore Wind: Evidence from Norway's 30GW Expansion

## Authors

Thomas Fjærvik<sup>1</sup> and Sondre Hølleland<sup>1</sup>

1: Norwegian School of Economics, Bergen, Norway.

## Abstract

This paper investigates the optimal allocation of offshore wind farms along the Norwegian coast in light of
the Norwegian Government’s objective to develop 30
GW of offshore wind capacity by 2040. Because wind
power is inherently variable and expensive to store, we
focus on maximizing base load production by identifying portfolios of wind farm locations that perform best
in low-wind scenarios. Using wind speed data from
the high-resolution NORA3-WP dataset for 20 candidate regions, we model marginal wind speed dynamics
through seasonal ARMA processes and capture spatial
dependence using a vine copula. This framework enables the simulation of 10 000 years of synthetic windspeed and corresponding power-output data. We then
solve a constrained portfolio optimization problem that
maximizes expected production in the lower 12% quantile of the joint power distribution, subject to sparsity
constraints that limit development to five sites. The results show that spatial diversification can substantially
stabilize base load wind power output. This paper thus
adds to an existing quantitative foundation for offshore
wind planning under production variability and spatial
dependence.

## Reproducibility

This repository contains the necessary code for reproducing results presented in the abovementioned paper. 

- Data is found in the data folder. Specifically, in "data/NVE_areas_windspeeds/" one can find all the extracted data for each region under consideration in the paper. 

- The scripts are organised in folders with names that should provide a sense of the order in which they are run. The pipeline folder contains subfolders where intermediate data files are stored if this project is cloned the code is run. The pipeline structure can be used to figure out the order in which to run necessary preceding scripts. This is illustrated in the example below.

Example: The script "optimization/optimal_locations_VaR_and_CVaR_5_at_a_time.R" loads data from ""pipeline/vine_copula/simulate_windpower/windpower.rds". Hence, to run this script one first needs to run "vine_copula/simulate_windpower.R". That script loads data from "pipeline/vine_copula/simulate_windspeed_2/windspeed_10000_daily.rds", which means that one must run the script "vine_copula/simulate_windspeed_2.R", and so on. 