test_that("create_pool_scenario validates parameters and initializes properly", {
  expect_error(create_pool_scenario(length_m = -10))
  expect_error(create_pool_scenario(target_turnovers_per_day = 0))

  scen <- create_pool_scenario(
    project_name = "Test Semi-Olympic",
    length_m = 25.0,
    width_m = 12.5,
    avg_depth_m = 1.35
  )
  expect_s3_class(scen, "pool_scenario")
  expect_equal(scen$pool_specs$total_volume_m3, 421.875)
  expect_equal(scen$pool_specs$target_flow_m3_per_day, 1687.5)
})

test_that("load_pool_scenario loads Cancun bundled case study", {
  cancun_scen <- load_pool_scenario()
  expect_s3_class(cancun_scen, "pool_scenario")
  expect_equal(cancun_scen$project_metadata$location, "Cancún, Quintana Roo")
  expect_equal(cancun_scen$current_system$energy_tariff, "CFE_GDMTH_Peninsular")
})

test_that("simulate_gravity_system handles pure gravity and hybrid bypass modes", {
  scen <- load_pool_scenario()

  # Pure gravity should warn of structural inviability
  expect_warning(
    sim_pure <- simulate_gravity_system(scen, mode = "pure_gravity"),
    "ALERTA ESTRUCTURAL"
  )
  expect_equal(sim_pure$feasibility, "Inviable Estructural")
  expect_gt(sim_pure$tank_sizing$elevation_height_m, 10)

  # Hybrid bypass should be viable and require compact elevation
  sim_hybrid <- simulate_gravity_system(scen, mode = "hybrid_bypass")
  expect_equal(sim_hybrid$feasibility, "Viable")
  expect_lte(sim_hybrid$tank_sizing$elevation_height_m, 4.0)
})

test_that("eval_lca_emissions tracks 20-year battery replacements", {
  scen <- load_pool_scenario()
  sim_hybrid <- simulate_gravity_system(scen, mode = "hybrid_bypass")
  sim_bess <- simulate_battery_system(scen)

  lca_df <- eval_lca_emissions(sim_hybrid, sim_bess, horizon_years = 20)
  expect_equal(nrow(lca_df), 21)

  # Battery cumulative emissions should jump at year 6, 12, 18
  jump_yr6 <- lca_df$battery_system_kg_co2e[lca_df$year == 6] - lca_df$battery_system_kg_co2e[lca_df$year == 5]
  expect_gt(jump_yr6, 5000)

  # Gravity curve should be flat after year 0
  expect_equal(
    lca_df$gravity_hybrid_kg_co2e[lca_df$year == 1],
    lca_df$gravity_hybrid_kg_co2e[lca_df$year == 20]
  )
})

test_that("eval_financial_roi calculates positive NPV and quick payback for hybrid bypass", {
  scen <- load_pool_scenario()
  sim_hybrid <- simulate_gravity_system(scen, mode = "hybrid_bypass")
  sim_bess <- simulate_battery_system(scen)

  fin <- eval_financial_roi(sim_hybrid, sim_bess, horizon_years = 15)
  expect_gt(fin$npv_gravity_usd, 0)
  expect_lt(fin$payback_gravity_years, 5.0)
})

test_that("compare_scenarios generates complete multi-criteria comparison", {
  scen <- load_pool_scenario()
  comp <- compare_scenarios(scen)
  expect_s3_class(comp, "gravitypool_comparison")
  expect_true(is.data.frame(comp$lca_trajectory))
  expect_true(is.list(comp$financial_eval))
})
