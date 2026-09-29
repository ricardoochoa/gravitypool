#' Simulate Gravitational Recirculation System
#'
#' Evaluates the hydrodynamic, structural, and capital requirements for either
#' a "pure_gravity" (brute-force elevation) or a "hybrid_bypass" (solar day filtration
#' + nocturnal gravity bypass) scenario.
#'
#' @param scenario An object of class `pool_scenario`.
#' @param mode Character. "hybrid_bypass" (recommended) or "pure_gravity".
#'
#' @return A list of class `gravity_simulation` containing hydraulic, structural,
#'   and financial sizing outputs.
#' @export
#'
#' @examples
#' scen <- create_pool_scenario()
#' sim_hybrid <- simulate_gravity_system(scen, mode = "hybrid_bypass")
simulate_gravity_system <- function(scenario, mode = c("hybrid_bypass", "pure_gravity")) {
  validate_pool_scenario(scenario)
  mode <- match.arg(mode)

  ps <- scenario$pool_specs
  pm <- scenario$project_metadata
  cs <- scenario$current_system
  hb <- scenario$hybrid_bypass_specs
  lca_const <- get("lca_factors", envir = asNamespace("gravitypool"))
  fin_const <- get("financial_params", envir = asNamespace("gravitypool"))

  pipe_specs <- hydro_pipe_specs(ps$pipe_material, ps$pipe_nominal_inch)

  if (mode == "pure_gravity") {
    # Nighttime circulation: 14 hours driving full turnover rate through the sand filter
    night_hours <- 14.0
    night_volume_m3 <- ps$total_volume_m3 * ps$target_turnovers_per_day * (night_hours / 24.0)
    night_flow_m3_h <- night_volume_m3 / night_hours

    # Head loss through high-rate sand filter
    tdh_night <- hydro_calculate_tdh(
      flow_m3_h = night_flow_m3_h,
      static_lift_m = 0,
      pipe_length_m = ps$pipe_length_m,
      inner_diameter_mm = pipe_specs$inner_diameter_mm,
      filter_type = ps$filter_type,
      filter_state = "dirty",
      temp_c = pm$ambient_temp_avg_c
    )

    required_head_m <- max(2.0, tdh_night$total_tdh_m - pm$topography_drop_m)

    tank_size <- size_tank_volume(
      volume_m3 = night_volume_m3,
      required_height_m = required_head_m,
      weight_alert_tons = 60.0,
      height_alert_m = 5.0
    )

    # Solar array for daytime pumping
    day_energy_kwh <- cs$pump_power_kw_total * (24.0 - night_hours)
    solar_size <- size_solar_array(day_energy_kwh, peak_sun_hours = pm$peak_sun_hours)

    # Costs
    tank_cost_usd <- tank_size$total_tank_volume_m3 * fin_const$concrete_elevated_tank_per_m3
    solar_pv_cost_usd <- solar_size$pv_array_kwp * fin_const$solar_pv_per_kwp
    solar_pumps_cost_usd <- solar_size$pump_shaft_power_hp * fin_const$solar_pump_dc_per_hp
    total_capex_usd <- tank_cost_usd + solar_pv_cost_usd + solar_pumps_cost_usd

    res <- list(
      mode = mode,
      scenario = scenario,
      tdh_breakdown = tdh_night,
      tank_sizing = tank_size,
      solar_sizing = solar_size,
      capex_usd = list(
        civil_tank = round(tank_cost_usd, 2),
        solar_pv = round(solar_pv_cost_usd, 2),
        solar_pumps = round(solar_pumps_cost_usd, 2),
        automation = 0,
        total = round(total_capex_usd, 2)
      ),
      operational_flow_m3_h = round(night_flow_m3_h, 2),
      feasibility = tank_size$feasibility_status
    )

  } else {
    # HYBRID BYPASS MODE
    # Daytime (8h): High-rate filtration driven by direct solar DC pumps
    day_volume_m3 <- ps$total_volume_m3 * hb$day_filter_turnovers
    day_flow_m3_h <- day_volume_m3 / hb$day_filter_hours

    tdh_day <- hydro_calculate_tdh(
      flow_m3_h = day_flow_m3_h,
      static_lift_m = hb$night_tank_height_m, # lift to buffer tank while filtering
      pipe_length_m = ps$pipe_length_m,
      inner_diameter_mm = pipe_specs$inner_diameter_mm,
      filter_type = ps$filter_type,
      filter_state = "dirty",
      temp_c = pm$ambient_temp_avg_c
    )

    # Solar array sized for daytime filtration + filling tank
    day_pumping_kw <- cs$pump_power_kw_total
    day_energy_kwh <- day_pumping_kw * hb$day_filter_hours
    solar_size <- size_solar_array(day_energy_kwh, peak_sun_hours = pm$peak_sun_hours)

    # Nighttime (14h): Bypass mode (low head: skimming & chemical sanitization)
    tdh_night_bypass <- hydro_calculate_tdh(
      flow_m3_h = hb$night_flow_m3_per_hour,
      static_lift_m = 0,
      pipe_length_m = ps$pipe_length_m,
      inner_diameter_mm = pipe_specs$inner_diameter_mm,
      filter_type = "bypass_skimmer",
      filter_state = "clean",
      temp_c = pm$ambient_temp_avg_c
    )

    tank_size <- size_tank_volume(
      volume_m3 = hb$night_buffer_tank_m3,
      required_height_m = hb$night_tank_height_m,
      weight_alert_tons = 60.0,
      height_alert_m = 5.0
    )

    # Costs
    tank_cost_usd <- tank_size$total_tank_volume_m3 * fin_const$concrete_elevated_tank_per_m3
    solar_pv_cost_usd <- solar_size$pv_array_kwp * fin_const$solar_pv_per_kwp
    solar_pumps_cost_usd <- solar_size$pump_shaft_power_hp * fin_const$solar_pump_dc_per_hp
    automation_cost_usd <- fin_const$bypass_valve_automation_package
    total_capex_usd <- tank_cost_usd + solar_pv_cost_usd + solar_pumps_cost_usd + automation_cost_usd

    res <- list(
      mode = mode,
      scenario = scenario,
      tdh_day = tdh_day,
      tdh_night = tdh_night_bypass,
      tank_sizing = tank_size,
      solar_sizing = solar_size,
      capex_usd = list(
        civil_tank = round(tank_cost_usd, 2),
        solar_pv = round(solar_pv_cost_usd, 2),
        solar_pumps = round(solar_pumps_cost_usd, 2),
        automation = round(automation_cost_usd, 2),
        total = round(total_capex_usd, 2)
      ),
      operational_flow_m3_h = round(day_flow_m3_h, 2),
      feasibility = tank_size$feasibility_status
    )
  }

  class(res) <- c("gravity_simulation", "list")
  return(res)
}

#' Simulate Battery Storage (BESS) Recirculation System
#'
#' Models the traditional alternative of maintaining full 24/7 pressurized filtration
#' with solar PV powering daytime operations and charging a Lithium Iron Phosphate
#' (LiFePO4) battery system for 14 hours of nighttime pumping.
#'
#' @param scenario An object of class `pool_scenario`.
#'
#' @return A list of class `battery_simulation`.
#' @export
#'
#' @examples
#' scen <- create_pool_scenario()
#' sim_bess <- simulate_battery_system(scen)
simulate_battery_system <- function(scenario) {
  validate_pool_scenario(scenario)

  ps <- scenario$pool_specs
  pm <- scenario$project_metadata
  cs <- scenario$current_system
  fin_const <- get("financial_params", envir = asNamespace("gravitypool"))

  night_hours <- 14.0
  day_hours <- 24.0 - night_hours

  # Night energy required
  night_energy_kwh <- cs$pump_power_kw_total * night_hours
  day_energy_kwh <- cs$pump_power_kw_total * day_hours

  bess_size <- size_battery_bank(night_energy_kwh)

  # Solar PV must cover direct daytime load + battery recharge requirement
  total_daily_generation_kwh <- day_energy_kwh + bess_size$nameplate_capacity_kwh
  solar_size <- size_solar_array(total_daily_generation_kwh, peak_sun_hours = pm$peak_sun_hours)

  # Capital costs
  battery_cost_usd <- bess_size$nameplate_capacity_kwh * fin_const$lithium_battery_system_per_kwh
  solar_pv_cost_usd <- solar_size$pv_array_kwp * fin_const$solar_pv_per_kwp
  inverter_pumps_cost_usd <- cs$pumps_qty * 1200.0 # Commercial hybrid inverters & variable drive pumps
  total_capex_usd <- battery_cost_usd + solar_pv_cost_usd + inverter_pumps_cost_usd

  res <- list(
    scenario = scenario,
    battery_sizing = bess_size,
    solar_sizing = solar_size,
    capex_usd = list(
      battery_system = round(battery_cost_usd, 2),
      solar_pv = round(solar_pv_cost_usd, 2),
      inverters_pumps = round(inverter_pumps_cost_usd, 2),
      total = round(total_capex_usd, 2)
    ),
    night_energy_kwh = night_energy_kwh
  )

  class(res) <- c("battery_simulation", "list")
  return(res)
}

#' Life Cycle Carbon Assessment (LCA) Evaluation
#'
#' Computes cumulative greenhouse gas emissions (in kg CO2e) over a 20-year horizon,
#' capturing the cyclic replacement of lithium batteries every 6 years versus the
#' upfront civil embodied carbon of elevated concrete tanks.
#'
#' @param gravity_sim A `gravity_simulation` object (typically hybrid bypass).
#' @param battery_sim A `battery_simulation` object.
#' @param horizon_years Integer. Analysis period in years (default 20).
#'
#' @return A data frame with annual cumulative emissions per scenario.
#' @export
eval_lca_emissions <- function(gravity_sim, battery_sim, horizon_years = 20) {
  lca <- get("lca_factors", envir = asNamespace("gravitypool"))
  years <- 0:horizon_years

  # 1. Gravitational System (Hybrid Bypass)
  # Upfront: concrete, steel, solar PV
  grav_tank <- gravity_sim$tank_sizing
  grav_pv <- gravity_sim$solar_sizing$pv_array_kwp

  co2_concrete <- grav_tank$concrete_est_m3 * lca$concrete_kg_co2e_per_m3_est
  co2_steel <- (grav_tank$steel_rebar_tons * 1000) * lca$steel_rebar_kg_co2e_per_kg
  co2_pv_grav <- grav_pv * lca$solar_module_mono_kg_co2e_per_kwp
  initial_grav_co2 <- co2_concrete + co2_steel + co2_pv_grav

  # Constant flat curve (concrete lifespan 50+ years, zero ongoing carbon)
  grav_cumulative <- rep(initial_grav_co2, length(years))

  # 2. Battery Storage System (LFP)
  # Upfront: solar PV + initial battery pack
  bess_kwh <- battery_sim$battery_sizing$nameplate_capacity_kwh
  bess_pv <- battery_sim$solar_sizing$pv_array_kwp

  co2_pv_bess <- bess_pv * lca$solar_module_mono_kg_co2e_per_kwp
  co2_bess_unit <- bess_kwh * lca$lithium_battery_kg_co2e_per_kwh
  initial_bess_co2 <- co2_pv_bess + co2_bess_unit

  # Replacement every 6 years (Year 6, 12, 18)
  bess_cumulative <- numeric(length(years))
  current_bess_total <- initial_bess_co2

  for (i in seq_along(years)) {
    yr <- years[i]
    if (yr > 0 && yr %% lca$battery_lifespan_years == 0) {
      current_bess_total <- current_bess_total + co2_bess_unit
    }
    bess_cumulative[i] <- current_bess_total
  }

  # 3. BAU (Grid-tied 100% CFE fossil mix)
  annual_kwh <- battery_sim$scenario$current_system$daily_energy_consumption_kwh * 365
  annual_grid_co2 <- annual_kwh * lca$grid_kg_co2e_per_kwh_mexico
  bau_cumulative <- years * annual_grid_co2

  data.frame(
    year = years,
    gravity_hybrid_kg_co2e = round(grav_cumulative, 1),
    battery_system_kg_co2e = round(bess_cumulative, 1),
    grid_bau_kg_co2e = round(bau_cumulative, 1),
    savings_vs_battery_kg = round(bess_cumulative - grav_cumulative, 1),
    savings_vs_grid_kg = round(bau_cumulative - grav_cumulative, 1)
  )
}

#' Financial Payback and Net Present Value Evaluation
#'
#' Analyzes 15-year multi-year cash flows, operating expense avoidance under
#' CFE GDMTH tariffs, and payback crossover for Gravitational Hybrid vs. BESS.
#'
#' @param gravity_sim A `gravity_simulation` object.
#' @param battery_sim A `battery_simulation` object.
#' @param discount_rate Numeric. Annual discount rate for NPV (default 0.08 = 8 percent).
#' @param horizon_years Integer. Financial projection horizon (default 15 years).
#'
#' @return A list containing cash flow data frame, payback years, and NPV metrics.
#' @export
eval_financial_roi <- function(gravity_sim, battery_sim, discount_rate = 0.08, horizon_years = 15) {
  scen <- gravity_sim$scenario
  cs <- scen$current_system
  pm <- scen$project_metadata
  fin_const <- get("financial_params", envir = asNamespace("gravitypool"))
  tc <- pm$exchange_rate_usd_mxn

  # Annual BAU OPEX in MXN and USD
  annual_energy_kwh <- cs$daily_energy_consumption_kwh * 365
  energy_cost_mxn_yr <- annual_energy_kwh * cs$energy_cost_kwh_avg_mxn
  capacity_charge_mxn_yr <- cs$pump_power_kw_total * cs$capacity_charge_kw_month_mxn * 12
  total_bau_opex_mxn <- energy_cost_mxn_yr + capacity_charge_mxn_yr
  total_bau_opex_usd <- total_bau_opex_mxn / tc

  years <- 0:horizon_years

  # 1. BAU Cash Flow (Cumulative spent)
  bau_cash_flow <- -1 * (years * total_bau_opex_usd)

  # 2. Gravity System Cash Flow
  # CAPEX at year 0, OPEX maintenance 1% of capex/year, 0 grid energy
  grav_capex <- gravity_sim$capex_usd$total
  grav_maint_annual <- grav_capex * 0.015

  grav_net_annual_savings <- total_bau_opex_usd - grav_maint_annual
  grav_cumulative_cf <- numeric(length(years))
  grav_cumulative_cf[1] <- -grav_capex

  for (i in 2:length(years)) {
    grav_cumulative_cf[i] <- grav_cumulative_cf[i - 1] + grav_net_annual_savings
  }

  # 3. Battery System Cash Flow
  # CAPEX at year 0, OPEX maint 2.5%, full battery replacement at yr 6 and yr 12
  bess_capex <- battery_sim$capex_usd$total
  bess_maint_annual <- bess_capex * 0.02
  bess_replace_usd <- battery_sim$capex_usd$battery_system

  bess_cumulative_cf <- numeric(length(years))
  bess_cumulative_cf[1] <- -bess_capex

  for (i in 2:length(years)) {
    yr <- years[i]
    annual_net <- total_bau_opex_usd - bess_maint_annual
    if (yr %in% c(6, 12)) {
      annual_net <- annual_net - bess_replace_usd
    }
    bess_cumulative_cf[i] <- bess_cumulative_cf[i - 1] + annual_net
  }

  # Calculate simple payback (interpolated year crossing zero)
  calc_payback <- function(cf, yrs) {
    if (all(cf < 0)) return(NA_real_)
    idx <- which(cf >= 0)[1]
    if (idx == 1) return(0)
    prev_cf <- cf[idx - 1]
    curr_cf <- cf[idx]
    round(yrs[idx - 1] + (-prev_cf) / (curr_cf - prev_cf), 1)
  }

  payback_gravity <- calc_payback(grav_cumulative_cf, years)
  payback_battery <- calc_payback(bess_cumulative_cf, years)

  # Net Present Value (NPV)
  npv_gravity <- -grav_capex + sum(rep(grav_net_annual_savings, horizon_years) / ((1 + discount_rate)^(1:horizon_years)))

  bess_annual_flows <- rep(total_bau_opex_usd - bess_maint_annual, horizon_years)
  if (horizon_years >= 6) bess_annual_flows[6] <- bess_annual_flows[6] - bess_replace_usd
  if (horizon_years >= 12) bess_annual_flows[12] <- bess_annual_flows[12] - bess_replace_usd
  npv_battery <- -bess_capex + sum(bess_annual_flows / ((1 + discount_rate)^(1:horizon_years)))

  cf_table <- data.frame(
    year = years,
    bau_spent_usd = round(bau_cash_flow, 0),
    gravity_hybrid_net_usd = round(grav_cumulative_cf, 0),
    battery_system_net_usd = round(bess_cumulative_cf, 0),
    gravity_vs_battery_gain_usd = round(grav_cumulative_cf - bess_cumulative_cf, 0)
  )

  list(
    cash_flow_table = cf_table,
    payback_gravity_years = payback_gravity,
    payback_battery_years = payback_battery,
    npv_gravity_usd = round(npv_gravity, 0),
    npv_battery_usd = round(npv_battery, 0),
    annual_bau_opex_usd = round(total_bau_opex_usd, 0),
    annual_bau_opex_mxn = round(total_bau_opex_mxn, 0)
  )
}

#' Comprehensive Multi-Criteria Scenario Comparison
#'
#' Top-level orchestrator that simulates gravitational (pure & hybrid) and battery
#' options, computing hydraulic, environmental (LCA), and financial ROI metrics.
#'
#' @param scenario An object of class `pool_scenario`.
#' @param horizon_years Integer. Projection period in years (default 20).
#'
#' @return An object of class `gravitypool_comparison`.
#' @export
#'
#' @examples
#' scen <- create_pool_scenario()
#' comp <- compare_scenarios(scen)
#' print(comp)
compare_scenarios <- function(scenario, horizon_years = 20) {
  validate_pool_scenario(scenario)

  # Suppress warnings for pure gravity to allow clean comparison capture
  sim_pure_grav <- suppressWarnings(simulate_gravity_system(scenario, mode = "pure_gravity"))
  sim_hybrid_grav <- simulate_gravity_system(scenario, mode = "hybrid_bypass")
  sim_battery <- simulate_battery_system(scenario)

  lca_df <- eval_lca_emissions(sim_hybrid_grav, sim_battery, horizon_years = horizon_years)
  fin_eval <- eval_financial_roi(sim_hybrid_grav, sim_battery, horizon_years = min(horizon_years, 15))

  res <- list(
    scenario = scenario,
    sim_pure_gravity = sim_pure_grav,
    sim_hybrid_gravity = sim_hybrid_grav,
    sim_battery = sim_battery,
    lca_trajectory = lca_df,
    financial_eval = fin_eval,
    horizon_years = horizon_years
  )

  class(res) <- c("gravitypool_comparison", "list")
  return(res)
}

#' Print method for gravitypool_comparison
#'
#' @param x An object of class `gravitypool_comparison`.
#' @param ... Additional arguments.
#' @export
print.gravitypool_comparison <- function(x, ...) {
  pm <- x$scenario$project_metadata
  fin <- x$financial_eval
  lca_end <- utils::tail(x$lca_trajectory, 1)

  fmt_n <- function(v) format(round(v), big.mark = ",")

  cat("========================================================================\n")
  cat("  gravitypool :: COMPARATIVA ESTRATEGICA DE ESCENARIOS\n")
  cat("========================================================================\n")
  cat(sprintf("  Proyecto: %s | Ubicacion: %s\n", pm$name, pm$location))
  cat("------------------------------------------------------------------------\n")
  cat("  1. COMPARATIVA DE INVERSION INICIAL (CAPEX):\n")
  cat(sprintf("     - Red Convencional (BAU):        $       0 USD (OPEX actual: $%s USD/ano)\n", fmt_n(fin$annual_bau_opex_usd)))
  cat(sprintf("     - Almacenamiento Baterias (LFP): $%s USD (Capacidad: %.1f kWh)\n",
              fmt_n(x$sim_battery$capex_usd$total), x$sim_battery$battery_sizing$nameplate_capacity_kwh))
  cat(sprintf("     - Bypass Gravitacional Hibrido:  $%s USD (Tanque: %.1f m3 @ %.1f m)\n",
              fmt_n(x$sim_hybrid_gravity$capex_usd$total),
              x$sim_hybrid_gravity$tank_sizing$total_tank_volume_m3,
              x$sim_hybrid_gravity$tank_sizing$elevation_height_m))
  cat(sprintf("     - Gravedad Pura (Fuerza Bruta):  $%s USD [ESTADO: %s]\n",
              fmt_n(x$sim_pure_gravity$capex_usd$total), x$sim_pure_gravity$feasibility))
  cat("------------------------------------------------------------------------\n")
  cat("  2. EVALUACION FINANCIERA (15 Anos):\n")
  cat(sprintf("     - Retorno de Inversion (Payback) Bypass:  %.1f anos\n", fin$payback_gravity_years))
  cat(sprintf("     - Retorno de Inversion (Payback) Bateria: %.1f anos\n", fin$payback_battery_years))
  cat(sprintf("     - Valor Actual Neto (NPV 8%%) Bypass:     $%s USD\n", fmt_n(fin$npv_gravity_usd)))
  cat(sprintf("     - Valor Actual Neto (NPV 8%%) Bateria:    $%s USD\n", fmt_n(fin$npv_battery_usd)))
  cat("------------------------------------------------------------------------\n")
  cat("  3. EVALUACION DE CICLO DE VIDA (LCA 20 Anos):\n")
  cat(sprintf("     - Emisiones Acumuladas Red CFE:           %s kg CO2e\n", fmt_n(lca_end$grid_bau_kg_co2e)))
  cat(sprintf("     - Emisiones Acumuladas Baterias (LFP):    %s kg CO2e\n", fmt_n(lca_end$battery_system_kg_co2e)))
  cat(sprintf("     - Emisiones Acumuladas Bypass Hibrido:    %s kg CO2e\n", fmt_n(lca_end$gravity_hybrid_kg_co2e)))
  cat(sprintf("     - CO2e Ahorrado Bypass vs. Baterias:      %s kg CO2e (%.1f Ton)\n",
              fmt_n(lca_end$savings_vs_battery_kg), lca_end$savings_vs_battery_kg / 1000))
  cat("========================================================================\n")
  invisible(x)
}
