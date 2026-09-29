# R/globals.R
# Declare global variables used in non-standard evaluation (ggplot2 aes)

utils::globalVariables(c(
  "Componente",
  "Costo_USD",
  "Emisiones_kg_co2e",
  "Escenario",
  "Flujo_Neto_USD",
  "Sistema",
  "group",
  "x",
  "y",
  "year",
  "filters_data",
  "financial_params",
  "fluids_data",
  "lca_factors",
  "pipes_data"
))

#' @importFrom dplyr %>%
#' @importFrom plotly ggplotly
#' @importFrom bslib page_sidebar
NULL
