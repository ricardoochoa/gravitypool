# inst/app/app.R
# gravitypool Consulting Interactive Dashboard
# Powered by Shiny & bslib

library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)
library(gravitypool)

ui <- page_sidebar(
  theme = bs_theme(
    version = 5,
    bootswatch = "cerulean",
    primary = "#0984e3",
    secondary = "#6c5ce7",
    success = "#00b894",
    warning = "#fdcb6e",
    danger = "#d63031",
    base_font = font_google("Inter")
  ),
  title = div(
    span("gravitypool", style = "font-weight: 800; color: #0984e3; letter-spacing: -0.5px;"),
    span(" | Consultoría de Recirculación Gravitacional", style = "font-size: 0.9rem; color: #636e72; font-weight: 400;")
  ),

  # ----------------------------------------------------
  # SIDEBAR: Panel de Control del Consultor
  # ----------------------------------------------------
  sidebar = sidebar(
    width = 360,
    open = "desktop",
    div(
      style = "margin-bottom: 12px;",
      actionButton(
        "btn_load_cancun",
        "⚡ Cargar Caso Cancún (Demo)",
        class = "btn-primary w-100 shadow-sm",
        style = "font-weight: 600; font-size: 0.95rem; border-radius: 8px;"
      )
    ),
    accordion(
      open = c("sec_pool", "sec_baseline"),
      multiple = TRUE,

      # 1. Geometría y Demanda
      accordion_panel(
        "📐 Alberca & Demanda Hídrica",
        value = "sec_pool",
        textInput("proj_name", "Nombre del Proyecto", value = "Alberca Semi-Olímpica 25m"),
        numericInput("pool_len", "Largo (m)", value = 25.0, min = 5, max = 100, step = 1),
        numericInput("pool_wid", "Ancho (m)", value = 12.5, min = 3, max = 50, step = 0.5),
        numericInput("pool_dep", "Profundidad media (m)", value = 1.35, min = 0.8, max = 4.0, step = 0.05),
        numericInput("turnovers", "Recambios diarios (Turnovers)", value = 4.0, min = 1, max = 8, step = 0.5),
        selectInput("pipe_mat", "Material de Tubería", choices = c("PVC_Sch40", "PVC_Sch80"), selected = "PVC_Sch40"),
        selectInput("pipe_dia", "Diámetro Nominal Tubería", choices = c("2 pulg" = 2.0, "3 pulg" = 3.0, "4 pulg" = 4.0), selected = 3.0),
        selectInput("filter_type", "Filtro Principal", choices = c(
          "Arena Sílica Alta Tasa" = "sand_high_rate",
          "Cartucho Plisado" = "cartridge",
          "Biofiltro Natural" = "biofilter_natural"
        ), selected = "sand_high_rate")
      ),

      # 2. Bombeo Actual & Tarifa Eléctrica
      accordion_panel(
        "⚡ Bombeo Actual & Red Eléctrica",
        value = "sec_baseline",
        numericInput("pump_qty", "Número de Bombas Actuales", value = 3, min = 1, max = 10, step = 1),
        numericInput("pump_hp", "Potencia por bomba (HP)", value = 3.0, min = 0.5, max = 20, step = 0.5),
        numericInput("pump_kw_tot", "Consumo total bomba (kW)", value = 6.71, min = 0.5, max = 50, step = 0.1),
        numericInput("daily_hrs", "Horas de bombeo diarias", value = 24.0, min = 8, max = 24, step = 1),
        numericInput("elec_rate_mxn", "Tarifa Promedio ($ MXN/kWh)", value = 3.15, min = 0.5, max = 10.0, step = 0.05),
        numericInput("capacity_charge_mxn", "Cargo por Demanda ($ MXN/kW-mes)", value = 446.0, min = 0, max = 1500, step = 10),
        numericInput("fx_rate", "Tipo de Cambio (MXN/USD)", value = 18.5, min = 10, max = 30, step = 0.1)
      ),

      # 3. Parámetros Solares y Bypass Híbrido
      accordion_panel(
        "☀️ Solar & Tanque Gravitacional",
        value = "sec_hybrid",
        numericInput("psh", "Horas Solares Pico (HSP)", value = 5.2, min = 2.0, max = 8.0, step = 0.1),
        numericInput("topo_drop", "Desnivel natural del terreno (m)", value = 0.0, min = 0, max = 30, step = 0.5),
        numericInput("tank_buffer_m3", "Capacidad Tanque Buffer Nocturno (m3)", value = 35.0, min = 5, max = 150, step = 5),
        numericInput("tank_height_m", "Cota de Elevación del Tanque (m)", value = 3.2, min = 1.0, max = 20.0, step = 0.2),
        numericInput("night_flow_m3_h", "Caudal Nocturno Bypass (m3/h)", value = 2.5, min = 0.5, max = 20, step = 0.5)
      )
    )
  ),

  # ----------------------------------------------------
  # MAIN PANEL: Presentación Visual para el Cliente
  # ----------------------------------------------------

  # Fila Superior: Value Boxes de Resumen Ejecutivo
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Estado Estructural Bypass",
      value = textOutput("vb_feasibility"),
      showcase = icon("check-circle"),
      theme = value_box_theme(bg = "#00b894", fg = "white")
    ),
    value_box(
      title = "Inversión Inicial (CAPEX)",
      value = textOutput("vb_capex"),
      showcase = icon("dollar-sign"),
      theme = value_box_theme(bg = "#0984e3", fg = "white")
    ),
    value_box(
      title = "Periodo de Retorno (Payback)",
      value = textOutput("vb_payback"),
      showcase = icon("clock"),
      theme = value_box_theme(bg = "#6c5ce7", fg = "white")
    ),
    value_box(
      title = "Ahorro LCA (20 Años)",
      value = textOutput("vb_co2_saved"),
      showcase = icon("leaf"),
      theme = value_box_theme(bg = "#00cec9", fg = "white")
    )
  ),

  # Contenedor Principal de Pestañas
  navset_card_pill(
    id = "main_tabs",

    # Pestaña 1: Esquema Físico y Topografía
    nav_panel(
      title = "📐 Esquema Topográfico & Físico",
      layout_sidebar(
        sidebar = sidebar(
          open = "open",
          width = 280,
          h6("Configuración de Visualización"),
          radioButtons(
            "view_mode",
            "Modo a Inspeccionar:",
            choices = c(
              "Bypass Híbrido (3.2m - Viable)" = "hybrid_bypass",
              "Gravedad Pura (15m - Inviable)" = "pure_gravity"
            ),
            selected = "hybrid_bypass"
          ),
          hr(),
          p(class = "text-muted small",
            "Compare visualmente la colosal torre de 15 metros que requeriría la fuerza bruta gravitacional versus el tanque compacto de 3.2 m en bypass.")
        ),
        card(
          card_header("Perfil Arquitectónico de la Instalación"),
          plotOutput("plot_topo", height = "420px")
        )
      ),
      card(
        card_header("Métricas Físicas y Carga sobre Cimentación"),
        tableOutput("table_physical_specs")
      )
    ),

    # Pestaña 2: Análisis Ambiental (LCA)
    nav_panel(
      title = "🌱 Ciclo de Vida (LCA 20 Años)",
      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("Trayectoria de Emisiones Acumuladas de CO2e (ISO 14040/44)"),
          plotlyOutput("plot_lca", height = "450px")
        ),
        card(
          card_header("Equivalencia de Impacto"),
          div(
            style = "padding: 10px;",
            h5("¿Por qué evitar baterías de litio?", style = "color: #0984e3; font-weight: bold;"),
            p("Las baterías LFP operadas en régimen diario se degradan y requieren desecho y manufactura nueva cada 6 años."),
            hr(),
            div(
              style = "text-align: center; margin: 20px 0;",
              h2(textOutput("cars_equivalent"), style = "color: #00b894; font-weight: 800;"),
              p(class = "text-muted", "Automóviles particulares retirados de circulación durante un año.")
            ),
            hr(),
            div(
              style = "text-align: center;",
              h3(textOutput("trees_equivalent"), style = "color: #2ecc71; font-weight: 700;"),
              p(class = "text-muted", "Árboles urbanos maduros capturando carbono por 10 años.")
            )
          )
        )
      )
    ),

    # Pestaña 3: Evaluación Financiera (ROI)
    nav_panel(
      title = "💰 Finanzas & Retorno de Inversión",
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("Flujo de Caja Acumulado (15 Años) vs. CFE GDMTH"),
          plotlyOutput("plot_payback", height = "420px")
        ),
        card(
          card_header("Composición de la Inversión (CAPEX)"),
          plotOutput("plot_capex", height = "420px")
        )
      ),
      card(
        card_header("Indicadores Económicos Clave"),
        tableOutput("table_fin_metrics")
      )
    ),

    # Pestaña 4: Matriz Ejecutiva
    nav_panel(
      title = "📋 Comparativa Ejecutiva",
      card(
        card_header("Matriz Comparativa de Tecnologías"),
        tableOutput("table_executive_matrix")
      )
    )
  )
)

server <- function(input, output, session) {

  # Botón de Carga Rápida Estudio de Caso Cancún
  observeEvent(input$btn_load_cancun, {
    updateTextInput(session, "proj_name", value = "Alberca de Entrenamiento 25m - Cancún")
    updateNumericInput(session, "pool_len", value = 25.0)
    updateNumericInput(session, "pool_wid", value = 12.5)
    updateNumericInput(session, "pool_dep", value = 1.35)
    updateNumericInput(session, "turnovers", value = 4.0)
    updateSelectInput(session, "pipe_mat", selected = "PVC_Sch40")
    updateSelectInput(session, "pipe_dia", selected = 3.0)
    updateSelectInput(session, "filter_type", selected = "sand_high_rate")
    updateNumericInput(session, "pump_qty", value = 3)
    updateNumericInput(session, "pump_hp", value = 3.0)
    updateNumericInput(session, "pump_kw_tot", value = 6.71)
    updateNumericInput(session, "daily_hrs", value = 24.0)
    updateNumericInput(session, "elec_rate_mxn", value = 3.15)
    updateNumericInput(session, "capacity_charge_mxn", value = 446.0)
    updateNumericInput(session, "fx_rate", value = 18.5)
    updateNumericInput(session, "psh", value = 5.2)
    updateNumericInput(session, "topo_drop", value = 0.0)
    updateNumericInput(session, "tank_buffer_m3", value = 35.0)
    updateNumericInput(session, "tank_height_m", value = 3.2)
    updateNumericInput(session, "night_flow_m3_h", value = 2.5)
  })

  # Objeto de Escenario Reactivo
  current_scenario <- reactive({
    create_pool_scenario(
      project_name = input$proj_name,
      length_m = input$pool_len,
      width_m = input$pool_wid,
      avg_depth_m = input$pool_dep,
      target_turnovers_per_day = input$turnovers,
      peak_sun_hours = input$psh,
      topography_drop_m = input$topo_drop,
      pipe_material = input$pipe_mat,
      pipe_nominal_inch = as.numeric(input$pipe_dia),
      filter_type = input$filter_type,
      pumps_qty = input$pump_qty,
      pump_power_hp_each = input$pump_hp,
      pump_power_kw_total = input$pump_kw_tot,
      daily_operation_hours = input$daily_hrs,
      energy_cost_kwh_avg_mxn = input$elec_rate_mxn,
      capacity_charge_kw_month_mxn = input$capacity_charge_mxn,
      exchange_rate_usd_mxn = input$fx_rate,
      night_buffer_tank_m3 = input$tank_buffer_m3,
      night_tank_height_m = input$tank_height_m,
      night_flow_m3_per_hour = input$night_flow_m3_h
    )
  })

  # Comparación Reactiva
  scenario_comp <- reactive({
    compare_scenarios(current_scenario())
  })

  # Value Boxes
  output$vb_feasibility <- renderText({
    comp <- scenario_comp()
    comp$sim_hybrid_gravity$feasibility
  })

  output$vb_capex <- renderText({
    comp <- scenario_comp()
    sprintf("$%s USD", format(round(comp$sim_hybrid_gravity$capex_usd$total), big.mark = ","))
  })

  output$vb_payback <- renderText({
    comp <- scenario_comp()
    sprintf("%.1f Años", comp$financial_eval$payback_gravity_years)
  })

  output$vb_co2_saved <- renderText({
    comp <- scenario_comp()
    lca_end <- tail(comp$lca_trajectory, 1)
    sprintf("%.1f Ton CO2e", lca_end$savings_vs_battery_kg / 1000)
  })

  # Equivalencias Ambientales
  output$cars_equivalent <- renderText({
    comp <- scenario_comp()
    lca_end <- tail(comp$lca_trajectory, 1)
    # EPA average passenger vehicle ~4,600 kg CO2e / year
    cars <- round(lca_end$savings_vs_battery_kg / 4600, 1)
    sprintf("%.1f autos", cars)
  })

  output$trees_equivalent <- renderText({
    comp <- scenario_comp()
    lca_end <- tail(comp$lca_trajectory, 1)
    # ~21.77 kg CO2 / year for urban mature tree
    trees <- round((lca_end$savings_vs_battery_kg / 20) / 21.77, 0)
    sprintf("%s árboles", format(trees, big.mark = ","))
  })

  # Gráfico Topográfico
  output$plot_topo <- renderPlot({
    comp <- scenario_comp()
    sim_obj <- if (input$view_mode == "hybrid_bypass") {
      comp$sim_hybrid_gravity
    } else {
      comp$sim_pure_gravity
    }
    plot_topography(sim_obj)
  })

  # Tabla de Métricas Físicas
  output$table_physical_specs <- renderTable({
    comp <- scenario_comp()
    t_hyp <- comp$sim_hybrid_gravity$tank_sizing
    t_pure <- comp$sim_pure_gravity$tank_sizing

    data.frame(
      Parámetro = c(
        "Volumen Útil Almacenado (m3)",
        "Volumen Bruto con Bordo Libre (m3)",
        "Masa de Agua (Toneladas)",
        "Masa Estructural de Concreto + Acero (Ton)",
        "Carga Total a Cimentación (Toneladas)",
        "Cota de Elevación Requerida (m)",
        "Factibilidad Técnica"
      ),
      Bypass_Híbrido = c(
        format(t_hyp$active_volume_m3),
        format(t_hyp$total_tank_volume_m3),
        format(t_hyp$water_mass_tons),
        format(t_hyp$structural_mass_tons),
        format(t_hyp$total_gross_load_tons),
        format(t_hyp$elevation_height_m),
        t_hyp$feasibility_status
      ),
      Gravedad_Pura_Fuerza_Bruta = c(
        format(t_pure$active_volume_m3),
        format(t_pure$total_tank_volume_m3),
        format(t_pure$water_mass_tons),
        format(t_pure$structural_mass_tons),
        format(t_pure$total_gross_load_tons),
        format(t_pure$elevation_height_m),
        t_pure$feasibility_status
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Gráficos Plotly
  output$plot_lca <- renderPlotly({
    p <- plot_lca_emissions(scenario_comp())
    ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.1, y = -0.2))
  })

  output$plot_payback <- renderPlotly({
    p <- plot_financial_payback(scenario_comp())
    ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.1, y = -0.2))
  })

  output$plot_capex <- renderPlot({
    plot_capex_breakdown(scenario_comp())
  })

  # Tabla de Indicadores Económicos
  output$table_fin_metrics <- renderTable({
    comp <- scenario_comp()
    fin <- comp$financial_eval
    scen <- current_scenario()

    data.frame(
      Métrica = c(
        "Gasto Operativo Actual BAU (CFE GDMTH)",
        "Inversión Inicial Total (CAPEX)",
        "Periodo de Retorno Simple (Payback)",
        "Valor Actual Neto (NPV @ 8% a 15 años)",
        "Ahorro Neto Acumulado a 15 años"
      ),
      Bypass_Gravitacional = c(
        sprintf("$%s USD/año", format(fin$annual_bau_opex_usd, big.mark = ",")),
        sprintf("$%s USD", format(comp$sim_hybrid_gravity$capex_usd$total, big.mark = ",")),
        sprintf("%.1f años", fin$payback_gravity_years),
        sprintf("$%s USD", format(fin$npv_gravity_usd, big.mark = ",")),
        sprintf("$%s USD", format(tail(fin$cash_flow_table$gravity_hybrid_net_usd, 1), big.mark = ","))
      ),
      Sistema_Baterías_LFP = c(
        sprintf("$%s USD/año", format(fin$annual_bau_opex_usd, big.mark = ",")),
        sprintf("$%s USD", format(comp$sim_battery$capex_usd$total, big.mark = ",")),
        if (is.na(fin$payback_battery_years)) "Sin retorno (<0)" else sprintf("%.1f años", fin$payback_battery_years),
        sprintf("$%s USD", format(fin$npv_battery_usd, big.mark = ",")),
        sprintf("$%s USD", format(tail(fin$cash_flow_table$battery_system_net_usd, 1), big.mark = ","))
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Matriz Ejecutiva
  output$table_executive_matrix <- renderTable({
    comp <- scenario_comp()
    fin <- comp$financial_eval
    lca_end <- tail(comp$lca_trajectory, 1)

    data.frame(
      Escenario = c(
        "1. Red CFE Convencional (BAU)",
        "2. Gravedad Pura (Fuerza Bruta)",
        "3. Almacenamiento con Baterías (LFP)",
        "4. Bypass Gravitacional Híbrido (Recomendado)"
      ),
      Inversión_Inicial_USD = c(
        "$0",
        sprintf("$%s", format(round(comp$sim_pure_gravity$capex_usd$total), big.mark = ",")),
        sprintf("$%s", format(round(comp$sim_battery$capex_usd$total), big.mark = ",")),
        sprintf("$%s", format(round(comp$sim_hybrid_gravity$capex_usd$total), big.mark = ","))
      ),
      Viabilidad_Estructural = c(
        "N/A",
        "INVIABLE (>400 Ton @ 15m)",
        "Viable (Ocupa cuarto máquinas)",
        "VIABLE (38.5 m3 @ 3.2m)"
      ),
      Payback = c(
        "N/A",
        ">15 años",
        if (is.na(fin$payback_battery_years)) "Sin retorno" else sprintf("%.1f años", fin$payback_battery_years),
        sprintf("%.1f años", fin$payback_gravity_years)
      ),
      Emisiones_20_Años = c(
        sprintf("%s kg CO2e", format(round(lca_end$grid_bau_kg_co2e), big.mark = ",")),
        "Muy Alta (Huella hormigón)",
        sprintf("%s kg CO2e", format(round(lca_end$battery_system_kg_co2e), big.mark = ",")),
        sprintf("%s kg CO2e", format(round(lca_end$gravity_hybrid_kg_co2e), big.mark = ","))
      ),
      Veredicto = c(
        "Económica y ecológicamente insostenible",
        "Físicamente absurdo e inviable",
        "Costoso por reemplazos continuos de litio",
        "ÓPTIMO: Menor CAPEX, rápido retorno y máxima ecología"
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

shinyApp(ui = ui, server = server)
