library(tinytest)

expect_equal(
  mesonet::mnet_calc_mp(data.frame(TR05 = Inf)),
  data.frame(TR05 = Inf,
             MP05 = -2083)
)

expect_equal(
  mesonet::mnet_calc_mp(data.frame(TR05 = 3.17)),
  data.frame(TR05 = 3.17,
             MP05 = -2083/2)
)

expect_equal(
  mesonet::mnet_calc_mp(data.frame(TR05 = log(2)/-3.35 + 3.17)),
  data.frame(TR05 = log(2)/-3.35 + 3.17,
             MP05 = -2083/3)
)

expect_equal(
  mesonet::mnet_calc_mp(data.frame(TR05 = log(3)/-3.35 + 3.17)),
  data.frame(TR05 = log(3)/-3.35 + 3.17,
             MP05 = -2083/4)
)


expect_equal(
  mesonet::mnet_calc_mp(data.frame(TR05 = 3.17,
                                   TR25 = 3.17,
                                   TR60 = log(2)/-3.35 + 3.17,
                                   TR75 = log(3)/-3.35 + 3.17)),
  data.frame(TR05 = 3.17,
             TR25 = 3.17,
             TR60 = log(2)/-3.35 + 3.17,
             TR75 = log(3)/-3.35 + 3.17,
             MP05 = -2083/2,
             MP25 = -2083/2,
             MP60 = -2083/3,
             MP75 = -2083/4))

##################
# Tests with units
##################


expect_equal(
  mesonet::mnet_calc_mp(data.frame(TR05 = units::set_units(Inf, "Celsius"))),
  data.frame(TR05 = units::set_units(Inf, "Celsius"),
             MP05 = units::set_units(-2083, "kPa"))
)

expect_equal(
  mesonet::mnet_calc_mp(data.frame(TR05 = units::set_units(3.17, "Celsius"))),
  data.frame(TR05 = units::set_units(3.17, "Celsius"),
             MP05 = units::set_units(-2083/2, "kPa"))
)

expect_equal(
  mesonet::mnet_calc_mp(data.frame(TR05 = units::set_units(log(2)/-3.35 + 3.17, "Celsius"))),
  data.frame(TR05 = units::set_units(log(2)/-3.35 + 3.17, "Celsius"),
             MP05 = units::set_units(-2083/3, "kPa"))
)

expect_equal(
  mesonet::mnet_calc_mp(data.frame(TR05 = units::set_units(log(3)/-3.35 + 3.17, "Celsius"))),
  data.frame(TR05 = units::set_units(log(3)/-3.35 + 3.17, "Celsius"),
             MP05 = units::set_units(-2083/4, "kPa"))
)


expect_equal(
  mesonet::mnet_calc_mp(data.frame(TR05 = units::set_units(3.17, "Celsius"),
                                   TR25 = units::set_units(3.17, "Celsius"),
                                   TR60 = units::set_units(log(2)/-3.35 + 3.17, "Celsius"),
                                   TR75 = units::set_units(log(3)/-3.35 + 3.17, "Celsius"))),
  data.frame(TR05 = units::set_units(3.17, "Celsius"),
             TR25 = units::set_units(3.17, "Celsius"),
             TR60 = units::set_units(log(2)/-3.35 + 3.17, "Celsius"),
             TR75 = units::set_units(log(3)/-3.35 + 3.17, "Celsius"),
             MP05 = units::set_units(-2083/2, "kPa"),
             MP25 = units::set_units(-2083/2, "kPa"),
             MP60 = units::set_units(-2083/3, "kPa"),
             MP75 = units::set_units(-2083/4, "kPa")))

