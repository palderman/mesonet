library(tinytest)

test_site_info <- data.frame(
  stid = "TEST",
  wcr05 = 0.10,
  wcs05 = 0.50,
  a05 = 0.12,
  n05 = 1.40,
  wcr10 = 0.10,
  wcs10 = 0.50,
  a10 = 0.12,
  n10 = 1.40,
  wcr25 = 0.10,
  wcs25 = 0.50,
  a25 = 0.12,
  n25 = 1.40,
  wcr60 = 0.10,
  wcs60 = 0.50,
  a60 = 0.12,
  n60 = 1.40,
  wcr75 = 0.10,
  wcs75 = 0.50,
  a75 = 0.12,
  n75 = 1.40
)

expect_error(
  mesonet::mnet_calc_vwc(data.frame(TR05 = Inf),
                         site_info = test_site_info)
)

expect_equal(
  mesonet::mnet_calc_vwc(data.frame(STID = "TEST", TR05 = Inf),
                         site_info = test_site_info),
  data.frame(
    STID = "TEST",
    TR05 = Inf,
    VWC05 = with(test_site_info, {
      mesonet::mnet_van_genuchten(
        MP = -2083,
        WCr = wcr05,
        WCs = wcs05,
        a = a05,
        n = n05
        )}))
)

expect_equal(
  mesonet::mnet_calc_vwc(data.frame(STID = "TEST", TR05 = 3.17),
                         site_info = test_site_info),
  data.frame(
    STID = "TEST",
    TR05 = 3.17,
    VWC05 = with(test_site_info, {
      mesonet::mnet_van_genuchten(
        MP = -2083/2,
        WCr = wcr05,
        WCs = wcs05,
        a = a05,
        n = n05
      )}))
)


expect_equal(
  mesonet::mnet_calc_vwc(data.frame(STID = "TEST",
                                    TR05 = 3.17,
                                    TR25 = 3.17,
                                    TR60 = log(2)/-3.35 + 3.17,
                                    TR75 = log(3)/-3.35 + 3.17),
                         site_info = test_site_info),
  data.frame(STID = "TEST",
             TR05 = 3.17,
             TR25 = 3.17,
             TR60 = log(2)/-3.35 + 3.17,
             TR75 = log(3)/-3.35 + 3.17,
             VWC05 = with(test_site_info, {
               mesonet::mnet_van_genuchten(
                 MP = -2083/2,
                 WCr = wcr05,
                 WCs = wcs05,
                 a = a05,
                 n = n05)}),
             VWC25 = with(test_site_info, {
               mesonet::mnet_van_genuchten(
                 MP = -2083/2,
                 WCr = wcr25,
                 WCs = wcs25,
                 a = a25,
                 n = n25)}),
             VWC60 = with(test_site_info, {
               mesonet::mnet_van_genuchten(
                 MP = -2083/3,
                 WCr = wcr60,
                 WCs = wcs60,
                 a = a60,
                 n = n60)}),
             VWC75 = with(test_site_info, {
               mesonet::mnet_van_genuchten(
                 MP = -2083/4,
                 WCr = wcr75,
                 WCs = wcs75,
                 a = a75,
                 n = n75)}))
)

#################
# Test with units
#################

test_site_info <-
  test_site_info |>
  mesonet:::set_mts_units()

test_tr_df <-
  data.frame(STID = "TEST",
             TR05 = 3.17,
             TR25 = 3.17,
             TR60 = log(2)/-3.35 + 3.17,
             TR75 = log(3)/-3.35 + 3.17) |>
  mesonet:::set_mts_units()

expect_equal(
  mesonet::mnet_calc_vwc(test_tr_df,
                         site_info = test_site_info),
  data.frame(STID = "TEST",
             TR05 = units::set_units(3.17, "Celsius"),
             TR25 = units::set_units(3.17, "Celsius"),
             TR60 = units::set_units(log(2)/-3.35 + 3.17, "Celsius"),
             TR75 = units::set_units(log(3)/-3.35 + 3.17, "Celsius"),
             VWC05 = with(test_site_info, {
               mesonet::mnet_van_genuchten(
                 MP = units::set_units(-2083/2, "kPa"),
                 WCr = wcr05,
                 WCs = wcs05,
                 a = a05,
                 n = n05)}),
             VWC25 = with(test_site_info, {
               mesonet::mnet_van_genuchten(
                 MP = units::set_units(-2083/2, "kPa"),
                 WCr = wcr25,
                 WCs = wcs25,
                 a = a25,
                 n = n25)}),
             VWC60 = with(test_site_info, {
               mesonet::mnet_van_genuchten(
                 MP = units::set_units(-2083/3, "kPa"),
                 WCr = wcr60,
                 WCs = wcs60,
                 a = a60,
                 n = n60)}),
             VWC75 = with(test_site_info, {
               mesonet::mnet_van_genuchten(
                 MP = units::set_units(-2083/4, "kPa"),
                 WCr = wcr75,
                 WCs = wcs75,
                 a = a75,
                 n = n75)}))
)

