#' Create a Pool Scenario S3 Object
#'
#' Constructor and initializer for the `pool_scenario` S3 class, representing
#' the physical, hydraulic, and economic definition of a swimming pool facility.
#'
#' @param project_name Character. Name of the project or facility.
#' @param location Character. Geographic location.
#' @param length_m Numeric. Pool length in meters.
#' @param width_m Numeric. Pool width in meters.
#' @param avg_depth_m Numeric. Pool average depth in meters.
#' @param target_turnovers_per_day Numeric. Target water volume turnovers per 24 hours. Default is 4.0.
#' @param peak_sun_hours Numeric. Peak Sun Hours (HSP/PSH) at location. Default is 5.2.
#' @param topography_drop_m Numeric. Available natural gravitational drop in meters. Default is 0.0.
#' @param ambient_temp_c Numeric. Average ambient/water temperature in Celsius. Default is 28.0.
#' @param pipe_material Character. Pipe material from internal database. Default is "PVC_Sch40".
#' @param pipe_nominal_inch Numeric. Pipe nominal trade diameter in inches (e.g. 2.0, 3.0, 4.0). Default is 3.0.
#' @param pipe_length_m Numeric. Total equivalent linear pipe length in meters. Default is 45.0.
#' @param filter_type Character. Filter technology ("sand_high_rate", "cartridge", "biofilter_natural"). Default is "sand_high_rate".
#' @param pumps_qty Integer. Number of operational pumps in current baseline system. Default is 3.
#' @param pump_power_hp_each Numeric. Rated mechanical horsepower per pump. Default is 3.0.
#' @param pump_power_kw_total Numeric. Total electrical power consumption of pumping system in kW. Default is 6.71.
#' @param daily_operation_hours Numeric. Operating hours per day for baseline pumps. Default is 24.0.
#' @param estimated_tdh_m Numeric. Estimated baseline Total Dynamic Head in meters of water column (m.c.a.). Default is 18.0.
#' @param energy_tariff Character. Name of the utility electrical tariff. Default is "CFE_GDMTH_Peninsular".
#' @param energy_cost_kwh_avg_mxn Numeric. Blended average electricity cost per kWh in MXN. Default is 3.15.
#' @param capacity_charge_kw_month_mxn Numeric. Monthly peak capacity charge per kW in MXN. Default is 446.0.
#' @param exchange_rate_usd_mxn Numeric. Exchange rate MXN per USD. Default is 18.5.
#' @param day_filter_turnovers Numeric. Volume turnovers processed through sand filter during daytime. Default is 3.25.
#' @param day_filter_hours Numeric. Daytime active filtration hours. Default is 8.0.
#' @param night_bypass_hours Numeric. Nighttime bypass circulation hours. Default is 14.0.
#' @param night_buffer_tank_m3 Numeric. Capacity of elevated buffer tank for night bypass in m3. Default is 35.0.
#' @param night_tank_height_m Numeric. Elevation height of night buffer tank in meters. Default is 3.2.
#' @param night_flow_m3_per_hour Numeric. Low-pressure bypass circulation flow in m3/h. Default is 2.5.
#'
#' @return An object of class `pool_scenario`.
#' @export
#'
#' @examples
#' scenario <- create_pool_scenario(
#'   project_name = "Cancun Semi-Olympic",
#'   length_m = 25.0,
#'   width_m = 12.5,
#'   avg_depth_m = 1.35
#' )
#' print(scenario)
create_pool_scenario <- function(
    project_name = "Piscina Modelo",
    location = "Cancun, Quintana Roo",
    length_m = 25.0,
    width_m = 12.5,
    avg_depth_m = 1.35,
    target_turnovers_per_day = 4.0,
    peak_sun_hours = 5.2,
    topography_drop_m = 0.0,
    ambient_temp_c = 28.0,
    pipe_material = "PVC_Sch40",
    pipe_nominal_inch = 3.0,
    pipe_length_m = 45.0,
    filter_type = "sand_high_rate",
    pumps_qty = 3,
    pump_power_hp_each = 3.0,
    pump_power_kw_total = 6.71,
    daily_operation_hours = 24.0,
    estimated_tdh_m = 18.0,
    energy_tariff = "CFE_GDMTH_Peninsular",
    energy_cost_kwh_avg_mxn = 3.15,
    capacity_charge_kw_month_mxn = 446.0,
    exchange_rate_usd_mxn = 18.5,
    day_filter_turnovers = 3.25,
    day_filter_hours = 8.0,
    night_bypass_hours = 14.0,
    night_buffer_tank_m3 = 35.0,
    night_tank_height_m = 3.2,
    night_flow_m3_per_hour = 2.5
) {
  # Input validations
  if (!is.numeric(length_m) || length_m <= 0) stop("length_m must be a positive number.")
  if (!is.numeric(width_m) || width_m <= 0) stop("width_m must be a positive number.")
  if (!is.numeric(avg_depth_m) || avg_depth_m <= 0) stop("avg_depth_m must be a positive number.")
  if (!is.numeric(target_turnovers_per_day) || target_turnovers_per_day <= 0) {
    stop("target_turnovers_per_day must be positive.")
  }
  if (!is.numeric(peak_sun_hours) || peak_sun_hours <= 0) stop("peak_sun_hours must be positive.")
  if (!is.numeric(exchange_rate_usd_mxn) || exchange_rate_usd_mxn <= 0) {
    stop("exchange_rate_usd_mxn must be positive.")
  }

  pool_volume_m3 <- round(length_m * width_m * avg_depth_m, 3)
  target_flow_m3_per_day <- round(pool_volume_m3 * target_turnovers_per_day, 3)
  daily_energy_kwh <- round(pump_power_kw_total * daily_operation_hours, 2)

  obj <- list(
    project_metadata = list(
      name = as.character(project_name),
      location = as.character(location),
      peak_sun_hours = as.numeric(peak_sun_hours),
      topography_drop_m = as.numeric(topography_drop_m),
      ambient_temp_avg_c = as.numeric(ambient_temp_c),
      exchange_rate_usd_mxn = as.numeric(exchange_rate_usd_mxn)
    ),
    pool_specs = list(
      length_m = as.numeric(length_m),
      width_m = as.numeric(width_m),
      avg_depth_m = as.numeric(avg_depth_m),
      total_volume_m3 = pool_volume_m3,
      target_turnovers_per_day = as.numeric(target_turnovers_per_day),
      target_flow_m3_per_day = target_flow_m3_per_day,
      pipe_material = as.character(pipe_material),
      pipe_nominal_inch = as.numeric(pipe_nominal_inch),
      pipe_length_m = as.numeric(pipe_length_m),
      filter_type = as.character(filter_type)
    ),
    current_system = list(
      pumps_qty = as.integer(pumps_qty),
      pump_power_hp_each = as.numeric(pump_power_hp_each),
      pump_power_kw_total = as.numeric(pump_power_kw_total),
      daily_operation_hours = as.numeric(daily_operation_hours),
      daily_energy_consumption_kwh = daily_energy_kwh,
      estimated_tdh_m = as.numeric(estimated_tdh_m),
      energy_tariff = as.character(energy_tariff),
      energy_cost_kwh_avg_mxn = as.numeric(energy_cost_kwh_avg_mxn),
      capacity_charge_kw_month_mxn = as.numeric(capacity_charge_kw_month_mxn)
    ),
    hybrid_bypass_specs = list(
      day_filter_turnovers = as.numeric(day_filter_turnovers),
      day_filter_hours = as.numeric(day_filter_hours),
      night_bypass_hours = as.numeric(night_bypass_hours),
      night_buffer_tank_m3 = as.numeric(night_buffer_tank_m3),
      night_tank_height_m = as.numeric(night_tank_height_m),
      night_flow_m3_per_hour = as.numeric(night_flow_m3_per_hour)
    )
  )

  class(obj) <- c("pool_scenario", "list")
  validate_pool_scenario(obj)
  return(obj)
}

#' Validate a Pool Scenario Object
#'
#' @param x An object of class `pool_scenario`.
#' @return The validated `pool_scenario` object invisibly.
#' @keywords internal
validate_pool_scenario <- function(x) {
  if (!inherits(x, "pool_scenario")) {
    stop("Object is not of class 'pool_scenario'")
  }

  required_top_levels <- c("project_metadata", "pool_specs", "current_system", "hybrid_bypass_specs")
  missing_levels <- setdiff(required_top_levels, names(x))
  if (length(missing_levels) > 0) {
    stop("Missing required scenario components: ", paste(missing_levels, collapse = ", "))
  }

  # Validate filter type exists in filters_data
  if (exists("filters_data", envir = asNamespace("gravitypool"), inherits = FALSE)) {
    valid_filters <- filters_data$type
    if (!x$pool_specs$filter_type %in% valid_filters) {
      warning("Filter type '", x$pool_specs$filter_type, "' is not in standard database (",
              paste(valid_filters, collapse = ", "), ").")
    }
  }

  invisible(x)
}

#' Load a Pool Scenario from JSON
#'
#' Helper to initialize a `pool_scenario` instance from a JSON file.
#'
#' @param json_path Character. Path to the JSON file. If omitted, loads the bundled Cancun case study.
#'
#' @return An object of class `pool_scenario`.
#' @export
load_pool_scenario <- function(json_path = NULL) {
  if (is.null(json_path)) {
    json_path <- system.file("extdata", "vignette_cancun_case.json", package = "gravitypool")
    if (json_path == "" || !file.exists(json_path)) {
      # Fallback to local inst if testing without installation
      json_path <- file.path("inst", "extdata", "vignette_cancun_case.json")
    }
  }

  if (!file.exists(json_path)) {
    stop("JSON file not found at: ", json_path)
  }

  data <- jsonlite::fromJSON(json_path, simplifyVector = TRUE)

  pm <- data$project_metadata
  ps <- data$pool_specs
  cs <- data$current_system
  hb <- data$hybrid_bypass_specs

  create_pool_scenario(
    project_name = pm$name %||% "Proyecto Cargado",
    location = pm$location %||% "Ubicacion Desconocida",
    length_m = ps$length_m,
    width_m = ps$width_m,
    avg_depth_m = ps$avg_depth_m,
    target_turnovers_per_day = ps$target_turnovers_per_day %||% 4.0,
    peak_sun_hours = pm$peak_sun_hours %||% 5.2,
    topography_drop_m = pm$topography_drop_m %||% 0.0,
    ambient_temp_c = pm$ambient_temp_avg_c %||% 28.0,
    pipe_material = ps$pipe_material %||% "PVC_Sch40",
    pipe_nominal_inch = ps$pipe_nominal_inch %||% 3.0,
    pipe_length_m = ps$pipe_length_m %||% 45.0,
    filter_type = ps$filter_type %||% "sand_high_rate",
    pumps_qty = cs$pumps_qty %||% 3,
    pump_power_hp_each = cs$pump_power_hp_each %||% 3.0,
    pump_power_kw_total = cs$pump_power_kw_total %||% 6.71,
    daily_operation_hours = cs$daily_operation_hours %||% 24.0,
    estimated_tdh_m = cs$estimated_tdh_m %||% 18.0,
    energy_tariff = cs$energy_tariff %||% "CFE_GDMTH_Peninsular",
    energy_cost_kwh_avg_mxn = cs$energy_cost_kwh_avg_mxn %||% 3.15,
    capacity_charge_kw_month_mxn = cs$capacity_charge_kw_month_mxn %||% 446.0,
    exchange_rate_usd_mxn = pm$exchange_rate_usd_mxn %||% 18.5,
    day_filter_turnovers = hb$day_filter_turnovers %||% 3.25,
    day_filter_hours = hb$day_filter_hours %||% 8.0,
    night_bypass_hours = hb$night_bypass_hours %||% 14.0,
    night_buffer_tank_m3 = hb$night_buffer_tank_m3 %||% 35.0,
    night_tank_height_m = hb$night_tank_height_m %||% 3.2,
    night_flow_m3_per_hour = hb$night_flow_m3_per_hour %||% 2.5
  )
}

# Infix null fallback operator
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Print method for pool_scenario
#'
#' @param x An object of class `pool_scenario`.
#' @param ... Additional arguments.
#' @export
print.pool_scenario <- function(x, ...) {
  cat("=========================================================\n")
  cat("  gravitypool :: ESCENARIO DE INSTALACION ACUATICA\n")
  cat("=========================================================\n")
  cat(sprintf("  Proyecto:          %s (%s)\n", x$project_metadata$name, x$project_metadata$location))
  cat(sprintf("  Dimensiones:       %.1f m x %.1f m x %.2f m\n",
              x$pool_specs$length_m, x$pool_specs$width_m, x$pool_specs$avg_depth_m))
  cat(sprintf("  Volumen Total:     %.2f m3 (%.0f litros)\n",
              x$pool_specs$total_volume_m3, x$pool_specs$total_volume_m3 * 1000))
  cat(sprintf("  Recambios Diarios: %.1f turnovers/dia (%.2f m3/dia)\n",
              x$pool_specs$target_turnovers_per_day, x$pool_specs$target_flow_m3_per_day))
  cat(sprintf("  Red Hidraulica:    %s %.1f\" (L = %.1f m) | Filtro: %s\n",
              x$pool_specs$pipe_material, x$pool_specs$pipe_nominal_inch,
              x$pool_specs$pipe_length_m, x$pool_specs$filter_type))
  cat("---------------------------------------------------------\n")
  cat(sprintf("  Bombeo Actual:     %d bombas x %.1f HP (Total: %.2f kW)\n",
              x$current_system$pumps_qty, x$current_system$pump_power_hp_each,
              x$current_system$pump_power_kw_total))
  cat(sprintf("  Consumo Electrico: %.1f kWh/dia (TDH est.: %.1f m.c.a.)\n",
              x$current_system$daily_energy_consumption_kwh, x$current_system$estimated_tdh_m))
  cat(sprintf("  Tarifa:            %s ($%.2f MXN/kWh)\n",
              x$current_system$energy_tariff, x$current_system$energy_cost_kwh_avg_mxn))
  cat(sprintf("  Solar & Clima:     %.1f HSP | Temp: %.1f  deg C | TC: $%.2f MXN/USD\n",
              x$project_metadata$peak_sun_hours, x$project_metadata$ambient_temp_avg_c,
              x$project_metadata$exchange_rate_usd_mxn))
  cat("=========================================================\n")
  invisible(x)
}
