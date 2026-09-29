#' Retrieve Pipe Specifications from Database
#'
#' @param material Character. Pipe material, e.g. "PVC_Sch40" or "PVC_Sch80".
#' @param nominal_inch Numeric. Nominal trade size in inches (e.g. 2.0, 3.0, 4.0).
#'
#' @return A list containing `material`, `nominal_inch`, `inner_diameter_mm`,
#'   `inner_diameter_m`, `outer_diameter_mm`, and `hazen_williams_c`.
#' @export
#'
#' @examples
#' specs <- hydro_pipe_specs("PVC_Sch40", 3.0)
#' specs$inner_diameter_mm
hydro_pipe_specs <- function(material = "PVC_Sch40", nominal_inch = 3.0) {
  # Query internal dataset
  pipes <- get("pipes_data", envir = asNamespace("gravitypool"))

  matched <- pipes[pipes$material == material & abs(pipes$nominal_inch - nominal_inch) < 0.05, ]

  if (nrow(matched) > 0) {
    row <- matched[1, ]
    return(list(
      material = row$material,
      nominal_inch = row$nominal_inch,
      outer_diameter_mm = row$outer_diameter_mm,
      inner_diameter_mm = row$inner_diameter_mm,
      inner_diameter_m = row$inner_diameter_mm / 1000,
      wall_thickness_mm = row$wall_thickness_mm,
      hazen_williams_c = row$hazen_williams_c
    ))
  }

  # Fallback for unlisted nominal sizes
  warning("Exact match for ", material, " ", nominal_inch, "\" not found in catalog. Using standard ASTM D1785 estimate.")
  id_est_mm <- nominal_inch * 25.4 * 0.90 # ~90% of nominal ID
  list(
    material = material,
    nominal_inch = nominal_inch,
    outer_diameter_mm = nominal_inch * 25.4 * 1.15,
    inner_diameter_mm = id_est_mm,
    inner_diameter_m = id_est_mm / 1000,
    wall_thickness_mm = 4.0,
    hazen_williams_c = 150
  )
}

#' Retrieve Fluid Thermophysical Properties
#'
#' @param temp_c Numeric. Fluid temperature in Celsius (typically 20.0 to 30.0  deg C).
#'
#' @return A list containing `temperature_c`, `density_kg_m3`, `kinematic_viscosity_cst`,
#'   and `kinematic_viscosity_m2_s`.
#' @export
#'
#' @examples
#' props <- hydro_fluid_properties(28.0)
#' props$density_kg_m3
hydro_fluid_properties <- function(temp_c = 28.0) {
  # Linear interpolation between 20 deg C, 28 deg C, and 30 deg C
  temps <- c(20.0, 28.0, 30.0)
  densities <- c(998.20, 996.23, 995.65)
  viscosities_cst <- c(1.0034, 0.8355, 0.8007)

  dens <- stats::approx(temps, densities, xout = temp_c, rule = 2)$y
  nu_cst <- stats::approx(temps, viscosities_cst, xout = temp_c, rule = 2)$y

  list(
    temperature_c = temp_c,
    density_kg_m3 = dens,
    kinematic_viscosity_cst = nu_cst,
    kinematic_viscosity_m2_s = nu_cst * 1e-6
  )
}

#' Calculate Friction Head Loss Using Hazen-Williams
#'
#' Computes head loss in pipes using the empirical Hazen-Williams formulation
#' in International System (SI) units:
#' \deqn{h_f = \frac{10.67 \cdot L \cdot Q^{1.852}}{C^{1.852} \cdot D^{4.87}}}
#'
#' @param flow_m3_h Numeric. Volumetric flow rate in cubic meters per hour (\eqn{m^3/h}).
#' @param pipe_length_m Numeric. Total pipe length in meters (\eqn{m}).
#' @param inner_diameter_mm Numeric. Pipe actual inner diameter in millimeters (\eqn{mm}).
#' @param c_factor Numeric. Hazen-Williams roughness coefficient. Default is 150 for PVC.
#' @param temp_c Numeric. Fluid temperature in Celsius. Default is 28.0.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{head_loss_m}: Head loss in meters of water column (m.c.a.).
#'   \item \code{head_loss_psi}: Head loss in pounds per square inch (psi).
#'   \item \code{velocity_m_s}: Flow velocity inside pipe in m/s.
#'   \item \code{reynolds_number}: Dimensionless Reynolds number.
#'   \item \code{flow_regime}: Character indication ("Laminar", "Transitional", "Turbulent").
#' }
#' @export
#'
#' @examples
#' loss <- hydro_pipe_friction(flow_m3_h = 70.3, pipe_length_m = 45, inner_diameter_mm = 77.3)
#' loss$head_loss_m
hydro_pipe_friction <- function(
    flow_m3_h,
    pipe_length_m,
    inner_diameter_mm,
    c_factor = 150,
    temp_c = 28.0
) {
  if (flow_m3_h <= 0) {
    return(list(
      head_loss_m = 0,
      head_loss_psi = 0,
      velocity_m_s = 0,
      reynolds_number = 0,
      flow_regime = "Static"
    ))
  }

  # SI conversions
  q_m3_s <- flow_m3_h / 3600.0
  d_m <- inner_diameter_mm / 1000.0
  area_m2 <- pi * (d_m / 2)^2
  velocity_m_s <- q_m3_s / area_m2

  # Hazen-Williams Head Loss (meters)
  hf_m <- (10.67 * pipe_length_m * (q_m3_s^1.852)) / ((c_factor^1.852) * (d_m^4.87))
  hf_psi <- hf_m * 1.42233 # 1 m.c.a. approx 1.422 psi

  # Reynolds Number
  fluid <- hydro_fluid_properties(temp_c)
  reynolds <- (velocity_m_s * d_m) / fluid$kinematic_viscosity_m2_s

  regime <- if (reynolds < 2100) "Laminar" else if (reynolds < 4000) "Transitional" else "Turbulent"

  list(
    head_loss_m = round(hf_m, 4),
    head_loss_psi = round(hf_psi, 4),
    velocity_m_s = round(velocity_m_s, 3),
    reynolds_number = round(reynolds, 0),
    flow_regime = regime
  )
}

#' Retrieve Filter Head Loss from Empirical Database
#'
#' @param filter_type Character. Filter technology: "sand_high_rate", "cartridge",
#'   "biofilter_natural", or "bypass_skimmer".
#' @param state Character. Operational fouling condition: "dirty" or "clean".
#'
#' @return Numeric. Head loss in meters of water column (m.c.a.).
#' @export
#'
#' @examples
#' hydro_filter_head_loss("sand_high_rate", "dirty") # ~14.06 m
#' hydro_filter_head_loss("bypass_skimmer", "clean") # ~0.14 m
hydro_filter_head_loss <- function(filter_type = "sand_high_rate", state = c("dirty", "clean")) {
  state <- match.arg(state)
  filters <- get("filters_data", envir = asNamespace("gravitypool"))

  matched <- filters[filters$type == filter_type, ]
  if (nrow(matched) == 0) {
    warning("Filter type '", filter_type, "' not found. Defaulting to 'sand_high_rate'.")
    matched <- filters[filters$type == "sand_high_rate", ]
  }

  col_name <- if (state == "dirty") "m_h2o_equivalent_dirty" else "m_h2o_equivalent_clean"
  return(as.numeric(matched[[col_name]]))
}

#' Calculate Total Dynamic Head (TDH)
#'
#' Combines static elevation difference, pipe friction losses, minor fitting
#' losses, and media filter restriction into the composite Total Dynamic Head.
#'
#' @param flow_m3_h Numeric. Operating flow rate in m3/h.
#' @param static_lift_m Numeric. Geometric elevation difference in meters.
#' @param pipe_length_m Numeric. Linear pipe run in meters.
#' @param inner_diameter_mm Numeric. Pipe inner diameter in mm.
#' @param filter_type Character. Filter technology.
#' @param filter_state Character. Filter state: "dirty" (worst-case design) or "clean".
#' @param minor_loss_ratio Numeric. Factor for valves/elbows relative to pipe friction (default 0.20 = 20 percent).
#' @param c_factor Numeric. Hazen-Williams roughness coefficient.
#' @param temp_c Numeric. Fluid temperature.
#'
#' @return A list with the TDH breakdown in meters of water column (m.c.a.).
#' @export
#'
#' @examples
#' tdh <- hydro_calculate_tdh(flow_m3_h = 70.3, static_lift_m = 0, pipe_length_m = 45,
#'                            inner_diameter_mm = 77.3, filter_type = "sand_high_rate")
#' tdh$total_tdh_m
hydro_calculate_tdh <- function(
    flow_m3_h,
    static_lift_m = 0.0,
    pipe_length_m = 45.0,
    inner_diameter_mm = 77.3,
    filter_type = "sand_high_rate",
    filter_state = "dirty",
    minor_loss_ratio = 0.20,
    c_factor = 150,
    temp_c = 28.0
) {
  friction <- hydro_pipe_friction(
    flow_m3_h = flow_m3_h,
    pipe_length_m = pipe_length_m,
    inner_diameter_mm = inner_diameter_mm,
    c_factor = c_factor,
    temp_c = temp_c
  )

  pipe_loss_m <- friction$head_loss_m
  minor_loss_m <- pipe_loss_m * minor_loss_ratio
  filter_loss_m <- hydro_filter_head_loss(filter_type, filter_state)

  total_tdh_m <- static_lift_m + pipe_loss_m + minor_loss_m + filter_loss_m

  list(
    static_lift_m = round(static_lift_m, 2),
    pipe_friction_loss_m = round(pipe_loss_m, 2),
    minor_losses_m = round(minor_loss_m, 2),
    filter_head_loss_m = round(filter_loss_m, 2),
    total_tdh_m = round(total_tdh_m, 2),
    velocity_m_s = friction$velocity_m_s,
    reynolds = friction$reynolds_number
  )
}
