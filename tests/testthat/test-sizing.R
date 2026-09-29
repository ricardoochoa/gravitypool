test_that("size_tank_volume correctly sizes buffer tanks and detects viable footprint", {
  # Modest bypass buffer tank: 35 m3 at 3.2 m
  tank_bypass <- size_tank_volume(volume_m3 = 35, required_height_m = 3.2)
  expect_equal(tank_bypass$active_volume_m3, 35)
  expect_equal(tank_bypass$feasibility_status, "Viable")
  expect_gt(tank_bypass$total_gross_load_tons, 30)
  expect_lt(tank_bypass$total_gross_load_tons, 80)
})

test_that("size_tank_volume triggers warning and flags structural inviability on extreme designs", {
  expect_warning(
    tank_extreme <- size_tank_volume(volume_m3 = 450, required_height_m = 15.0),
    "ALERTA ESTRUCTURAL"
  )
  expect_equal(tank_extreme$feasibility_status, "Inviable Estructural")
  expect_gt(tank_extreme$total_gross_load_tons, 400)
})

test_that("size_solar_array calculates required PV capacity and pump HP", {
  solar <- size_solar_array(daily_energy_kwh = 53.7, peak_sun_hours = 5.2)
  expect_gt(solar$pv_array_kwp, 10.0)
  expect_lt(solar$pv_array_kwp, 16.0)
  expect_gt(solar$pump_shaft_power_hp, 5.0)
})

test_that("size_battery_bank sizes LFP storage and rack units accurately", {
  bess <- size_battery_bank(night_energy_kwh = 93.9, depth_of_discharge = 0.85, round_trip_efficiency = 0.90)
  expect_gt(bess$nameplate_capacity_kwh, 110)
  expect_lt(bess$nameplate_capacity_kwh, 135)
  expect_gt(bess$battery_rack_modules_5kwh, 20)
})
