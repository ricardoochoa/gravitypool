# data-raw/process_data.R
# Script to process raw JSON data and compile package internal datasets (R/sysdata.rda)
# and exported example datasets.

library(jsonlite)
library(usethis)

base_data_path <- file.path("inst", "extdata", "gravitypool_base_data.json")
cancun_path <- file.path("inst", "extdata", "vignette_cancun_case.json")

if (!file.exists(base_data_path)) {
  stop("Base data JSON not found at: ", base_data_path)
}

base_data <- jsonlite::fromJSON(base_data_path, simplifyVector = TRUE)

pipes_data <- as.data.frame(base_data$hydraulics$pipes)
filters_data <- as.data.frame(base_data$hydraulics$filters)
fluids_data <- base_data$hydraulics$fluids
lca_factors <- base_data$lca_factors
financial_params <- base_data$financial_parameters_usd

# Save internal data to R/sysdata.rda
usethis::use_data(
  pipes_data,
  filters_data,
  fluids_data,
  lca_factors,
  financial_params,
  internal = TRUE,
  overwrite = TRUE
)

# Exported Cancun case study dataset for convenience
if (file.exists(cancun_path)) {
  cancun_case_study <- jsonlite::fromJSON(cancun_path, simplifyVector = TRUE)
  usethis::use_data(cancun_case_study, overwrite = TRUE)
}

message("Internal sysdata.rda and exported datasets created successfully.")
