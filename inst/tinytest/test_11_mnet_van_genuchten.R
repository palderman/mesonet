library(tinytest)

expect_equal(
  mesonet::mnet_van_genuchten(0, 0.1, 0.5, 1, 2),
  0.5
)

expect_equal(
  mesonet::mnet_van_genuchten(-Inf, 0.1, 0.5, 1, 2),
  0.1
)

expect_error(
  mesonet::mnet_van_genuchten(0, 0.1, 0.5, 0, 2),
  info = "a < 1"
)

expect_error(
  mesonet::mnet_van_genuchten(0, 0.1, 0.5, 1, 0),
  info = "n < 1"
)

expect_error(
  mesonet::mnet_van_genuchten(0, 0.1, 0.5, 1, 1),
  info = "n == 1"
)

expect_error(
  mesonet::mnet_van_genuchten(0, 0.5, 0.1, 1, 2),
  info = "WCr higher than WCs"
)

##################
# Tests with units
##################

expect_equal(
  mesonet::mnet_van_genuchten(units::set_units(0, "kPa"),
                              units::set_units(0.1, "cm3/cm3"),
                              units::set_units(0.5, "cm3/cm3"),
                              units::set_units(1, "1/kPa"),
                              units::set_units(2, "1")),
  units::set_units(0.5, "cm3/cm3"),
  info = "using units"
)
