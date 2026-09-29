test_that("hydro_pipe_specs returns correct dimensions for standard PVC", {
  sch40_3in <- hydro_pipe_specs("PVC_Sch40", 3.0)
  expect_equal(sch40_3in$inner_diameter_mm, 77.3)
  expect_equal(sch40_3in$hazen_williams_c, 150)

  sch80_3in <- hydro_pipe_specs("PVC_Sch80", 3.0)
  expect_equal(sch80_3in$inner_diameter_mm, 72.8)
  expect_lt(sch80_3in$inner_diameter_mm, sch40_3in$inner_diameter_mm)
})

test_that("hydro_fluid_properties interpolates correctly around 28C", {
  fluid28 <- hydro_fluid_properties(28.0)
  expect_equal(fluid28$density_kg_m3, 996.23, tolerance = 1e-2)
  expect_equal(fluid28$kinematic_viscosity_cst, 0.8355, tolerance = 1e-3)
})

test_that("hydro_pipe_friction calculates Hazen-Williams accurately", {
  # For 70 m3/h in 3" pipe over 45m
  fric <- hydro_pipe_friction(flow_m3_h = 70.3, pipe_length_m = 45.0, inner_diameter_mm = 77.3)
  expect_gt(fric$head_loss_m, 0.5)
  expect_lt(fric$head_loss_m, 10.0)
  expect_gt(fric$velocity_m_s, 2.0)
  expect_equal(fric$flow_regime, "Turbulent")

  # Zero flow
  zero <- hydro_pipe_friction(0, 45, 77.3)
  expect_equal(zero$head_loss_m, 0)
})

test_that("hydro_filter_head_loss queries clean and dirty states", {
  sand_dirty <- hydro_filter_head_loss("sand_high_rate", "dirty")
  sand_clean <- hydro_filter_head_loss("sand_high_rate", "clean")
  expect_equal(sand_dirty, 14.06)
  expect_equal(sand_clean, 7.03)

  bypass_clean <- hydro_filter_head_loss("bypass_skimmer", "clean")
  expect_equal(bypass_clean, 0.14)
})

test_that("hydro_calculate_tdh integrates friction, fittings and filter loss", {
  tdh <- hydro_calculate_tdh(
    flow_m3_h = 70.3,
    static_lift_m = 2.0,
    pipe_length_m = 45.0,
    inner_diameter_mm = 77.3,
    filter_type = "sand_high_rate",
    filter_state = "dirty"
  )
  expect_gt(tdh$total_tdh_m, 16.0)
  expect_equal(tdh$filter_head_loss_m, 14.06)
  expect_equal(tdh$static_lift_m, 2.0)
})
