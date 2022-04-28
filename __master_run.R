# Define global analysis parameters ---------------------------------------

rerun_imputations <- FALSE
rerun_analysis <- TRUE

# Packages and helper functions -------------------------------------------

source("1_code/0_packages.R")

source("1_code/1_helpers.R")


# Load and clean data -----------------------------------------------------

source("1_code/2_load_data.R")

source("1_code/3_cleaning.R")

source("1_code/4_exclusion.R")

source("1_code/5_imputations.R")

source("1_code/6_create_variables.R")


# Main analysis -----------------------------------------------------------

source("1_code/7_analysis.R")

source("1_code/8_plots.R")

source("1_code/9_regressions.R")


# Appendix ----------------------------------------------------------------

source("1_code/X1_data_sources.R")

source("1_code/X2_calibration.R")

source("1_code/X3_outliers.R")

#source("1_code/X4_sensitivity.R")

source("1_code/X5_linear_trends.R")

source("1_code/X6_country_by_country.R")
