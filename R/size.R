#' Size Elevated Gravitational Buffer / Storage Tank
#'
#' Evaluates the required volumetric capacity, water mass, structural reinforced concrete
#' requirements, and structural viability for an elevated storage tank.
#'
#' @param volume_m3 Numeric. Effective usable water volume required in m3.
#' @param required_height_m Numeric. Elevation head required to drive the gravitational flow (m).
#' @param freeboard_ratio Numeric. Reserve safety volume fraction above active storage (default 0.10 = 10 percent).
#' @param weight_alert_tons Numeric. Threshold in metric tons to trigger structural warning (default 60 tons).
#' @param height_alert_m Numeric. Threshold in meters to trigger structural warning (default 5 meters).
#'
#' @return A list with volumetric, weight, civil materials, and feasibility metrics:
#' \itemize{
#'   \item \code{total_tank_volume_m3}: Gross volume including freeboard.
#'   \item \code{water_mass_tons}: Mass of stored water in metric tons.
#'   \item \code{concrete_est_m3}: Estimated volume of reinforced concrete for tank walls, slab and columns.
#'   \item \code{structural_mass_tons}: Estimated dead load of the civil concrete structure.
#'   \item \code{total_gross_load_tons}: Total load on soil foundations (water + structure).
#'   \item \code{elevation_height_m}: Tower height from ground to tank invert.
#'   \item \code{feasibility_status}: Character: "Viable", "Precaucion", or "Inviable Estructural".
#' }
#' @export
#'
#' @examples
#' # Compact bypass tank
#' t1 <- size_tank_volume(volume_m3 = 35, required_height_m = 3.2)
#'
#' # Extreme pure gravity tank
#' t2 <- size_tank_volume(volume_m3 = 450, required_height_m = 15.0)
size_tank_volume <- function(
    volume_m3,
    required_height_m,
    freeboard_ratio = 0.10,
    weight_alert_tons = 60.0,
    height_alert_m = 5.0
) {
  if (volume_m3 <= 0) stop("volume_m3 must be positive.")
  if (required_height_m < 0) stop("required_height_m cannot be negative.")

  gross_volume_m3 <- round(volume_m3 * (1 + freeboard_ratio), 2)
  water_mass_tons <- round(gross_volume_m3 * 0.996, 2) # at ~28 deg C

  # Civil structural estimate:
  # Concrete volume index: ~0.42 m3 of concrete per m3 of elevated storage
  # Plus elevated substructure scaling with height: (1 + 0.08 * height)
  height_scaling <- 1 + (0.08 * max(0, required_height_m - 2))
  concrete_vol_m3 <- round(gross_volume_m3 * 0.35 * height_scaling, 2)
  concrete_mass_tons <- round(concrete_vol_m3 * 2.40, 2) # density ~2400 kg/m3

  # Reinforcement steel (~90 kg/m3 of concrete)
  steel_rebar_tons <- round((concrete_vol_m3 * 90) / 1000, 2)

  total_load_tons <- round(water_mass_tons + concrete_mass_tons + steel_rebar_tons, 2)

  # Feasibility classification & Warnings
  is_inviable <- (total_load_tons > weight_alert_tons && required_height_m >= height_alert_m) ||
    (total_load_tons > 150) || (required_height_m > 12.0)

  is_caution <- (total_load_tons > 40 && required_height_m >= 3.5) && !is_inviable

  feasibility <- if (is_inviable) {
    warning(sprintf(
      "ALERTA ESTRUCTURAL: El diseno requiere un tanque elevado de %.1f toneladas a %.1f metros de altura. Inviable para recintos deportivos/recreativos estandar.",
      total_load_tons, required_height_m
    ), call. = FALSE)
    "Inviable Estructural"
  } else if (is_caution) {
    "Precaucion"
  } else {
    "Viable"
  }

  list(
    active_volume_m3 = volume_m3,
    total_tank_volume_m3 = gross_volume_m3,
    water_mass_tons = water_mass_tons,
    concrete_est_m3 = concrete_vol_m3,
    steel_rebar_tons = steel_rebar_tons,
    structural_mass_tons = round(concrete_mass_tons + steel_rebar_tons, 2),
    total_gross_load_tons = total_load_tons,
    elevation_height_m = round(required_height_m, 2),
    feasibility_status = feasibility
  )
}

#' Size Solar Photovoltaic Pumping System
#'
#' Sizes the required photovoltaic peak array capacity and solar direct DC pumping
#' specifications to meet daily hydraulic volume targets.
#'
#' @param daily_energy_kwh Numeric. Pumping energy required during daytime operation in kWh.
#' @param peak_sun_hours Numeric. Peak Sun Hours (HSP) available on site (default 5.2).
#' @param system_derate Numeric. System derate factor accounting for temperature, dust, and wiring (default 0.80).
#' @param operating_hours Numeric. Solar operating window in hours (default 8.0).
#'
#' @return A list with solar power sizing metrics:
#' \describe{
#'   \item{pv_array_kwp}{Required photovoltaic module capacity in kWp.}
#'   \item{pump_shaft_power_kw}{Nominal shaft pumping power required in kW.}
#'   \item{pump_shaft_power_hp}{Nominal pump power in horsepower (HP).}
#'   \item{solar_collecting_area_m2}{Estimated photovoltaic roof/ground area in m2.}
#' }
#' @export
#'
#' @examples
#' solar <- size_solar_array(daily_energy_kwh = 53.7, peak_sun_hours = 5.2)
#' solar$pv_array_kwp
size_solar_array <- function(
    daily_energy_kwh,
    peak_sun_hours = 5.2,
    system_derate = 0.80,
    operating_hours = 8.0
) {
  if (daily_energy_kwh <= 0) stop("daily_energy_kwh must be positive.")
  if (peak_sun_hours <= 0) stop("peak_sun_hours must be positive.")

  # Solar peak power
  kwp_required <- round(daily_energy_kwh / (peak_sun_hours * system_derate), 2)

  # Pumping continuous power during sun window
  pump_kw <- round(daily_energy_kwh / operating_hours, 2)
  pump_hp <- round(pump_kw / 0.7457, 1)

  # PV Area ~5.0 m2 per kWp (approx 200 W/m2 efficiency)
  area_m2 <- round(kwp_required * 5.0, 1)

  list(
    pv_array_kwp = kwp_required,
    pump_shaft_power_kw = pump_kw,
    pump_shaft_power_hp = pump_hp,
    solar_collecting_area_m2 = area_m2
  )
}

#' Size Stationary Battery Energy Storage System (BESS)
#'
#' Calculates required nameplate battery storage capacity and weight for nighttime
#' pool recirculation using Lithium Iron Phosphate (LiFePO4 / LFP) chemistry.
#'
#' @param night_energy_kwh Numeric. Energy required for nighttime pumping in kWh.
#' @param depth_of_discharge Numeric. Recommended maximum DoD for 6000-cycle life (default 0.85 = 85 percent).
#' @param round_trip_efficiency Numeric. Inverter and chemical round-trip efficiency (default 0.90 = 90 percent).
#'
#' @return A list with battery sizing details:
#' \itemize{
#'   \item \code{useful_energy_kwh}: Net energy required at pump terminals.
#'   \item \code{nameplate_capacity_kwh}: Total nominal battery storage needed.
#'   \item \code{battery_rack_modules_5kwh}: Estimated number of standard 5.12 kWh 48V/100Ah rack modules.
#'   \item \code{battery_weight_kg}: Estimated total battery rack mass in kilograms.
#' }
#' @export
#'
#' @examples
#' bess <- size_battery_bank(night_energy_kwh = 93.9)
#' bess$nameplate_capacity_kwh
size_battery_bank <- function(
    night_energy_kwh,
    depth_of_discharge = 0.85,
    round_trip_efficiency = 0.90
) {
  if (night_energy_kwh <= 0) stop("night_energy_kwh must be positive.")

  nameplate_kwh <- round(night_energy_kwh / (depth_of_discharge * round_trip_efficiency), 2)
  modules_5kwh <- ceiling(nameplate_kwh / 5.12)
  weight_kg <- round(nameplate_kwh * 9.5, 1) # approx 9.5 kg per kWh for commercial LFP

  list(
    useful_energy_kwh = round(night_energy_kwh, 2),
    nameplate_capacity_kwh = nameplate_kwh,
    battery_rack_modules_5kwh = as.integer(modules_5kwh),
    battery_weight_kg = weight_kg
  )
}
