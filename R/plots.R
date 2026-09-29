#' Plot 2D Physical Topography and Hydraulic Scheme
#'
#' Generates an architectural 2D cross-section diagram displaying the pool basin,
#' machine pump room, and elevated storage tank with height callouts.
#'
#' @param sim_result A `gravity_simulation` object (from `simulate_gravity_system`).
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#' scen <- create_pool_scenario()
#' sim <- simulate_gravity_system(scen, mode = "hybrid_bypass")
#' p <- plot_topography(sim)
plot_topography <- function(sim_result) {
  if (!inherits(sim_result, "gravity_simulation")) {
    stop("Argument sim_result must be of class 'gravity_simulation'")
  }

  scen <- sim_result$scenario
  ps <- scen$pool_specs
  tank <- sim_result$tank_sizing
  h_elev <- tank$elevation_height_m
  vol <- tank$total_tank_volume_m3
  feasibility <- tank$feasibility_status

  # Geometry points
  # Pool: x from -15 to 0, depth from -1.35 to 0
  pool_df <- data.frame(
    x = c(-15, 0, 0, -15),
    y = c(0, 0, -ps$avg_depth_m, -ps$avg_depth_m)
  )

  # Pump room: x from 2 to 6, y from 0 to 2.5
  room_df <- data.frame(
    x = c(2, 6, 6, 2),
    y = c(0, 0, 2.5, 2.5)
  )

  # Tank: x from 8 to 12, y from h_elev to h_elev + 2.5
  tank_df <- data.frame(
    x = c(8, 12, 12, 8),
    y = c(h_elev, h_elev, h_elev + 2.5, h_elev + 2.5)
  )

  # Tower columns: x=8.5 and 11.5 from y=0 to y=h_elev
  tower_df <- data.frame(
    x = c(8.5, 8.5, 11.5, 11.5),
    y = c(0, h_elev, 0, h_elev),
    group = c(1, 1, 2, 2)
  )

  # Color by feasibility
  tank_fill <- if (feasibility == "Viable") "#2ecc71" else if (feasibility == "Precaucion") "#f39c12" else "#e74c3c"
  border_col <- if (feasibility == "Viable") "#27ae60" else if (feasibility == "Precaucion") "#d35400" else "#c0392b"

  p <- ggplot2::ggplot() +
    # Ground line
    ggplot2::geom_hline(yintercept = 0, color = "#7f8c8d", linewidth = 1.2, linetype = "dashed") +
    # Pool water
    ggplot2::geom_polygon(data = pool_df, ggplot2::aes(x = x, y = y), fill = "#3498db", alpha = 0.6) +
    ggplot2::annotate("text", x = -7.5, y = -ps$avg_depth_m / 2,
                      label = sprintf("Alberca (%.0f m3)\nProf: %.2f m", ps$total_volume_m3, ps$avg_depth_m),
                      color = "white", fontface = "bold", size = 3.8) +
    # Machine room
    ggplot2::geom_polygon(data = room_df, ggplot2::aes(x = x, y = y), fill = "#95a5a6", alpha = 0.8) +
    ggplot2::annotate("text", x = 4, y = 1.25, label = "Cuarto de\nMaquinas", color = "white", size = 3.2, fontface = "bold") +
    # Tower columns
    ggplot2::geom_line(data = tower_df, ggplot2::aes(x = x, y = y, group = group), color = "#34495e", linewidth = 2.0) +
    # Tank body
    ggplot2::geom_polygon(data = tank_df, ggplot2::aes(x = x, y = y), fill = tank_fill, color = border_col, linewidth = 1.0) +
    ggplot2::annotate("text", x = 10, y = h_elev + 1.25,
                      label = sprintf("Tanque\n%.1f m3\n(%.1f Ton)", vol, tank$total_gross_load_tons),
                      color = "white", fontface = "bold", size = 3.4) +
    # Height dimension arrow & label
    ggplot2::annotate("segment", x = 13.5, xend = 13.5, y = 0, yend = h_elev,
                      arrow = ggplot2::arrow(ends = "both", length = ggplot2::unit(0.2, "cm")),
                      color = "#2c3e50", linewidth = 0.8) +
    ggplot2::annotate("text", x = 15.2, y = h_elev / 2,
                      label = sprintf("Altura:\n%.1f m", h_elev), color = "#2c3e50", fontface = "bold", size = 3.5) +
    ggplot2::labs(
      title = sprintf("Esquema Hidraulico-Topografico: %s", sim_result$mode),
      subtitle = sprintf("Estado Estructural: %s | Carga total a cimentacion: %.1f Ton", feasibility, tank$total_gross_load_tons),
      x = "Distancia horizontal relativa (m)",
      y = "Cota de elevacion relativa (m)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(color = if (feasibility == "Viable") "#27ae60" else "#c0392b", face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  return(p)
}

#' Plot 20-Year Cumulative Carbon Emissions (LCA)
#'
#' Displays 20-year cumulative greenhouse gas emissions comparing Grid BAU,
#' Battery Energy Storage System (with 6-year replacement bumps), and Gravitational Hybrid Bypass.
#'
#' @param comparison_result A `gravitypool_comparison` object (from `compare_scenarios`).
#'
#' @return A `ggplot2` object ready for `plotly::ggplotly()`.
#' @export
#'
#' @examples
#' scen <- create_pool_scenario()
#' comp <- compare_scenarios(scen)
#' p <- plot_lca_emissions(comp)
plot_lca_emissions <- function(comparison_result) {
  if (!inherits(comparison_result, "gravitypool_comparison")) {
    stop("Argument comparison_result must be of class 'gravitypool_comparison'")
  }

  df <- comparison_result$lca_trajectory

  # Convert to long format for ggplot2
  df_long <- tidyr::pivot_longer(
    df,
    cols = c("gravity_hybrid_kg_co2e", "battery_system_kg_co2e", "grid_bau_kg_co2e"),
    names_to = "Escenario",
    values_to = "Emisiones_kg_co2e"
  )

  df_long$Escenario <- factor(
    df_long$Escenario,
    levels = c("grid_bau_kg_co2e", "battery_system_kg_co2e", "gravity_hybrid_kg_co2e"),
    labels = c("Red Electrica (CFE BAU)", "Almacenamiento Baterias (LFP)", "Bypass Gravitacional Hibrido")
  )

  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = year, y = Emisiones_kg_co2e / 1000, color = Escenario, group = Escenario)) +
    ggplot2::geom_line(linewidth = 1.3) +
    ggplot2::geom_point(size = 2.2) +
    # Annotate battery replacement bumps at year 6, 12, 18
    ggplot2::geom_vline(xintercept = c(6, 12, 18), linetype = "dotted", color = "#e67e22", alpha = 0.7) +
    ggplot2::scale_color_manual(
      values = c(
        "Red Electrica (CFE BAU)" = "#e74c3c",
        "Almacenamiento Baterias (LFP)" = "#e67e22",
        "Bypass Gravitacional Hibrido" = "#27ae60"
      )
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, comparison_result$horizon_years, by = 2)) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(
      title = "Analisis de Ciclo de Vida (LCA): Emisiones Acumuladas de CO2e",
      subtitle = "Impacto del reemplazo de celdas de litio cada 6 anos vs. infraestructura civil de larga vida",
      x = "Ano de Operacion",
      y = "Toneladas Metricas de CO2e Acumuladas",
      color = "Tecnologia"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      panel.grid.minor = ggplot2::element_blank()
    )

  return(p)
}

#' Plot 15-Year Cumulative Financial Cash Flow and Payback
#'
#' Displays annual cumulative financial performance and crossover payback point
#' against the continuing operating expenses of baseline grid pumping.
#'
#' @param comparison_result A `gravitypool_comparison` object.
#'
#' @return A `ggplot2` object ready for `plotly::ggplotly()`.
#' @export
#'
#' @examples
#' scen <- create_pool_scenario()
#' comp <- compare_scenarios(scen)
#' p <- plot_financial_payback(comp)
plot_financial_payback <- function(comparison_result) {
  if (!inherits(comparison_result, "gravitypool_comparison")) {
    stop("Argument comparison_result must be of class 'gravitypool_comparison'")
  }

  cf <- comparison_result$financial_eval$cash_flow_table
  fin <- comparison_result$financial_eval

  df_long <- tidyr::pivot_longer(
    cf,
    cols = c("gravity_hybrid_net_usd", "battery_system_net_usd"),
    names_to = "Escenario",
    values_to = "Flujo_Neto_USD"
  )

  df_long$Escenario <- factor(
    df_long$Escenario,
    levels = c("gravity_hybrid_net_usd", "battery_system_net_usd"),
    labels = c("Bypass Gravitacional Hibrido", "Almacenamiento Baterias (LFP)")
  )

  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = year, y = Flujo_Neto_USD, color = Escenario, group = Escenario)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#7f8c8d", linewidth = 1.0) +
    ggplot2::geom_line(linewidth = 1.3) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_color_manual(
      values = c(
        "Bypass Gravitacional Hibrido" = "#27ae60",
        "Almacenamiento Baterias (LFP)" = "#e67e22"
      )
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, 15, by = 1)) +
    ggplot2::scale_y_continuous(labels = scales::dollar) +
    ggplot2::labs(
      title = "Flujo de Caja Acumulado y Retorno de Inversion (Payback)",
      subtitle = sprintf("Retorno Gravitacional: %.1f anos | Retorno Baterias: %.1f anos",
                         fin$payback_gravity_years, fin$payback_battery_years),
      x = "Ano",
      y = "Flujo Acumulado Neto (USD)",
      color = "Tecnologia"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      panel.grid.minor = ggplot2::element_blank()
    )

  return(p)
}

#' Plot Initial CAPEX Investment Breakdown
#'
#' @param comparison_result A `gravitypool_comparison` object.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#' scen <- create_pool_scenario()
#' comp <- compare_scenarios(scen)
#' p <- plot_capex_breakdown(comp)
plot_capex_breakdown <- function(comparison_result) {
  if (!inherits(comparison_result, "gravitypool_comparison")) {
    stop("Argument comparison_result must be of class 'gravitypool_comparison'")
  }

  cg <- comparison_result$sim_hybrid_gravity$capex_usd
  cb <- comparison_result$sim_battery$capex_usd

  df <- data.frame(
    Sistema = c(
      rep("Bypass Gravitacional", 4),
      rep("Sistema Baterias LFP", 3)
    ),
    Componente = c(
      "Tanque / Estructura Civil", "Modulos Fotovoltaicos", "Motobombas Solares DC", "Valvulas y Automatizacion",
      "Bancos de Baterias LFP", "Modulos Fotovoltaicos", "Inversores y Bombas"
    ),
    Costo_USD = c(
      cg$civil_tank, cg$solar_pv, cg$solar_pumps, cg$automation,
      cb$battery_system, cb$solar_pv, cb$inverters_pumps
    )
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = Sistema, y = Costo_USD, fill = Componente)) +
    ggplot2::geom_col(width = 0.55, color = "white") +
    ggplot2::scale_y_continuous(labels = scales::dollar) +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::labs(
      title = "Desglose de Inversion Inicial de Capital (CAPEX)",
      subtitle = sprintf("Bypass Total: $%s USD | Baterias Total: $%s USD",
                         format(round(cg$total), big.mark = ","),
                         format(round(cb$total), big.mark = ",")),
      x = "",
      y = "Inversion Inicial (USD)",
      fill = "Rubro de Inversion"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      panel.grid.minor = ggplot2::element_blank()
    )

  return(p)
}
