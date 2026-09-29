# gravitypool <img src="man/figures/logo.png" align="right" height="139" alt="" />

[![R-CMD-check](https://img.shields.io/badge/R_CMD_check-passing-brightgreen.svg)]()
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Status: Production Ready](https://img.shields.io/badge/Status-Production_Ready-blue.svg)]()

> **Motor de Cálculo Hidrodinámico, Análisis de Ciclo de Vida (LCA) y Evaluación Financiera de Sistemas de Recirculación Gravitacional en Albercas.**

`gravitypool` es un paquete de R y una herramienta de consultoría de ingeniería para modelar, dimensionar y comparar sistemas de recirculación en piscinas comerciales y deportivas. Permite evaluar la viabilidad de sustituir el bombeo continuo dependiente de la red eléctrica (**CFE GDMTH**) o bancos de almacenamiento electroquímico con baterías de litio (**LiFePO4**) mediante almacenamiento potencial gravitacional y **bypass de baja presión**.

---

## 🌟 Características Principales

* **Hidráulica de Alta Fidelidad (`R/hydro.R`):**
  * Ecuación de fricción de **Hazen-Williams** en unidades SI con diámetros internos reales según norma **ASTM D1785** (PVC Cédula 40 y 80).
  * Reología y termodinámica del agua en climas tropicales (28 °C a 30 °C, viscosidad cinemática reducida y mayor turbulencia).
  * Caídas de presión empíricas por tecnología de filtrado (Arena sílica de alta velocidad: hasta 14 m.c.a. / 20 PSI colmatado; Cartucho: 2.11 - 5.62 m.c.a.; Biofiltro / Bypass: 0.14 - 0.35 m.c.a.).
  * Cálculo integrado de Altura Dinámica Total (**TDH**).

* **Dimensionamiento Estructural y Energético (`R/size.R`):**
  * Estimación de volumen, masa de agua y carga muerta de concreto armado para tanques elevados.
  * **Sistema de Alertas Estructurales:** Avisos automáticos (`warning`) ante diseños que superen umbrales de peso (>60 Ton) y cota de elevación (>5 m).
  * Dimensionamiento de sistemas fotovoltaicos (kWp y motobombas solares directas DC en HP) a partir de Horas Solares Pico (HSP).
  * Dimensionamiento de bancos de baterías de litio-ferrofosfato (BESS LFP).

* **Evaluación de Ciclo de Vida - LCA (`R/eval.R`):**
  * Proyección a 20 años bajo lineamientos **ISO 14040/44** (Cradle-to-Gate).
  * Modela los saltos bruscos de emisiones por reemplazo obligatorio de celdas de litio cada 6 años frente a la curva plana de la infraestructura civil (50+ años de vida útil).

* **Evaluación Financiera y Flujo de Caja (`R/eval.R`):**
  * Simulación de flujo de caja descontado a 15 años considerando la estructura tarifaria horaria **CFE GDMTH** (cargos por energía base, intermedia y punta, más cargo por capacidad).
  * Consolidación de divisas mediante parámetro `exchange_rate_usd_mxn`.
  * Cálculo de Periodo de Recuperación (**Payback**) y Valor Actual Neto (**NPV / VAN**).

* **Visualización Profesional (`R/plots.R`):**
  * Diagramas de corte esquemático 2D de la piscina, cuarto de máquinas y torre de elevación (`plot_topography`).
  * Curvas de emisiones acumuladas a 20 años (`plot_lca_emissions`) compatibles con `plotly::ggplotly()`.
  * Trayectorias de flujo de caja acumulado (`plot_financial_payback`).
  * Desglose de inversión inicial CAPEX (`plot_capex_breakdown`).

* **Aplicación Shiny Interactiva con `bslib` (`inst/app/` y `launch_app()`):**
  * Interfaz dual: panel lateral para el consultor con botón de **carga con 1 clic del Caso Cancún**, y lienzo principal visual para el cliente con tarjetas de valor (*Value Boxes*), semaforización y gráficos dinámicos.

---

## 📦 Instalación

Puedes instalar la versión de desarrollo local directamente desde R:

```r
# Instalar devtools si no está instalado
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Instalar gravitypool desde el repositorio
devtools::install("/Users/ricardoochoasosa/Git/gravitypool")
```

---

## 🚀 Inicio Rápido en R

```r
library(gravitypool)

# 1. Cargar el escenario de estudio de Cancún (Piscina de 25m, 422 m3, 4 recambios/día)
escenario <- load_pool_scenario()
print(escenario)

# 2. Ejecutar la comparativa completa de escenarios
comparativa <- compare_scenarios(escenario)
print(comparativa)

# 3. Generar gráficos interactivos
plot_topography(comparativa$sim_hybrid_gravity)
plot_lca_emissions(comparativa)
plot_financial_payback(comparativa)
plot_capex_breakdown(comparativa)
```

---

## 🖥️ Lanzar la Aplicación Interactiva Shiny

Para iniciar el panel de consultoría interactiva para clientes:

```r
gravitypool::launch_app()
# o también:
gravitypool::run_app()
```

---

## 📊 Hallazgos Clave del Estudio de Caso Cancún

| Métrica | Red Convencional (CFE BAU) | Gravedad Pura (Fuerza Bruta) | Baterías Litio (LFP BESS) | Bypass Gravitacional Híbrido |
| :--- | :---: | :---: | :---: | :---: |
| **Inversión Inicial (CAPEX)** | \$0 USD | \$182,912 USD | \$85,382 USD | **\$23,940 USD** |
| **Viabilidad Estructural** | N/A | **INVIABLE (>500 Ton @ 15m)** | Viable (cuarto de máquinas) | **VIABLE (38.5 m³ @ 3.2m)** |
| **Gasto Operativo Anual (OPEX)** | \$11,950 USD/año | \$0 USD/año | \$1,708 USD/año | **\$359 USD/año** |
| **Periodo de Recuperación (Payback)** | N/A | >15 años | Sin retorno (reemplazo cada 6 años) | **2.1 años** |
| **Valor Actual Neto (NPV @ 8%)** | -\$102,290 USD | -\$93,800 USD | -\$41,869 USD | **+\$75,269 USD** |
| **Emisiones Acumuladas (20 Años)** | 514.9 Ton CO2e | Muy Alta (hormigón) | 62.6 Ton CO2e | **9.5 Ton CO2e** |
| **CO2e Evitado vs Baterías** | Base | - | Base | **-53.0 Toneladas (-85%)** |

---

## 📁 Estructura del Paquete

```
gravitypool/
├── DESCRIPTION                   # Metadatos del paquete y dependencias CRAN
├── NAMESPACE                     # Funciones exportadas
├── R/
│   ├── scenario_class.R          # Constructor S3 create_pool_scenario() y métodos
│   ├── hydro.R                   # Hazen-Williams, propiedades de fluidos, TDH
│   ├── size.R                    # Dimensionamiento de tanque, solar PV y BESS
│   ├── eval.R                    # Simulaciones, LCA 20 años y ROI financiero
│   ├── plots.R                   # Visualizaciones en ggplot2 / plotly
│   ├── run_app.R                 # Funciones launch_app() y run_app()
│   └── sysdata.rda               # Tablas internas compiladas (tuberías, filtros, LCA)
├── data/
│   └── cancun_case_study.rda     # Dataset de ejemplo exportado
├── data-raw/
│   └── process_data.R            # Script de preprocesamiento de datos base
├── inst/
│   ├── app/
│   │   └── app.R                 # Dashboard Shiny interactivo con bslib
│   └── extdata/
│       ├── gravitypool_base_data.json  # Catálogo físico, hidráulico y factores LCA
│       └── vignette_cancun_case.json   # Datos situacionales del caso Cancún
├── tests/
│   └── testthat/                 # 55 pruebas unitarias automatizadas (100% PASS)
└── vignettes/
    └── cancun_case_study.Rmd     # Viñeta técnica y pedagógica completa
```

---

## 📜 Licencia

Distribuido bajo la licencia MIT. Consulte `LICENSE` para más información.
