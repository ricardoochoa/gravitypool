test_that("plotting functions return ggplot objects and execute cleanly", {
  scen <- load_pool_scenario()
  comp <- compare_scenarios(scen)

  p_topo <- plot_topography(comp$sim_hybrid_gravity)
  expect_s3_class(p_topo, "ggplot")

  p_lca <- plot_lca_emissions(comp)
  expect_s3_class(p_lca, "ggplot")

  p_pay <- plot_financial_payback(comp)
  expect_s3_class(p_pay, "ggplot")

  p_capex <- plot_capex_breakdown(comp)
  expect_s3_class(p_capex, "ggplot")
})
