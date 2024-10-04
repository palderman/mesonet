library(tinytest)

# Test sat_vap_pres()

# Validation data from Table 4-1 at https://geo.libretexts.org/Bookshelves/Meteorology_and_Climate_Science/Practical_Meteorology_(Stull)/04%3A_Water_Vapor/4.00%3A_Vapor_Pressure_at_Saturation

digits <- c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2)

Tair <- c(-40, -35, -30, -25, -20, -15, -10, -5, 0,
          5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60) |>
  units::set_units("Celsius")

es_expected <- c(0.0203, 0.033, 0.0528, 0.0827, 0.1274, 0.1929, 0.2875,
                 0.4222, 0.6113, 0.8735, 1.232, 1.718, 2.369, 3.23, 4.36,
                 5.829, 7.72, 10.13, 13.19, 17.04, 21.83) |>
  units::set_units("kPa")

es_actual <- mesonet:::sat_vap_pres(Tair) |>
  round(digits = digits)

expect_equal(es_actual, es_expected)

# Validation data taken from:
# https://nvlpubs.nist.gov/nistpubs/jres/74c/jresv74cn3-4p117_a1b.pdf
#

e_s_ref <- c(0.151608619428067, 0.239756315736143, 0.371148194981374,
             0.563190294775112, 0.838706874553256, 1.22722724325958,
             1.76634031480187, 2.50308302739445, 3.49576899342402,
             4.81510600770087, 6.54724379723529, 8.79377402123751,
             11.6752521153037, 15.3315359302798, 19.9261880374945) |>
  units::set_units("kPa")

# Invert saturated vapor pressure provided in resource for more accurate
# Tair

Tair <- e_s_ref |>
  mesonet:::T_sat() |>
  rep(times = 9)

RH <- seq(10, 90, by = 10) |>
  rep(each = 15) |>
  units::set_units("percent")

T_dew_expected <- c(-44.2, -35.8, -27.9, -20.1, -12.2,
                    -4.4, 3.3, 11, 18.6, 26.2, 33.7,
                    41.2, 48.7, 56.1, 63.4, -31.6,
                    -23.1, -14.6, -6.2, 2.2, 10.5,
                    18.8, 27.1, 35.4, 43.6, 51.8,
                    59.9, 68, 76, 84, -24.1, -15.3,
                    -6.5, 2.3, 11.1, 19.8, 28.5, 37.2,
                    45.8, 54.4, 63, 71.5, 80.1,
                    88.5, 97, -18.6, -9.5, -0.4,
                    8.6, 17.6, 26.7, 35.6, 44.6, 53.5,
                    62.4, 71.3, 80.2, 89, 97.8,
                    106.6, -14.2, -4.9, 4.4, 13.6,
                    22.9, 32.1, 41.3, 50.5, 59.7, 68.9,
                    78, 87.1, 96.2, 105.3, 114.4,
                    -10.6, -1.1, 8.4, 17.8, 27.3, 36.7,
                    46.1, 55.5, 64.9, 74.2, 83.6,
                    92.9, 102.3, 111.6, 120.9, -7.5,
                    2.2, 11.8, 21.4, 31, 40.6, 50.2,
                    59.8, 69.3, 78.9, 88.4, 98,
                    107.5, 117, 126.5, -4.7, 5.1,
                    14.8, 24.6, 34.3, 44.1, 53.8, 63.6,
                    73.3, 83, 92.7, 102.4, 112.1,
                    121.8, 131.5, -2.2, 7.7, 17.5,
                    27.4, 37.3, 47.2, 57.1, 66.9,
                    76.8, 86.7, 96.5, 106.4, 116.2,
                    126.1, 135.9
                    ) |>
  units::set_units("Fahrenheit")

T_dew_actual <- mesonet:::calc_tdew(Tair, RH) |>
  units::set_units("Fahrenheit")

expect_equal(T_dew_actual, T_dew_expected, tolerance = 0.1)

tmp_mts_file <-
  tempfile(fileext = ".mts")

c("  101 ! (c) 1994 Oklahoma Climatological Survey and the Oklahoma Mesonet - all rights reserved",
  "  18 1994 01 01 00 00 00",
  " STID  STNM  TIME   RELH   TAIR   WSPD   WVEC  WDIR   WDSD   WSSD   WMAX    RAIN     PRES  SRAD   TA9M   WS2M   TS10   TB10   TS05   TB05   TS30",
  " STIL    89     0     31   -999    4.6    4.5   182   12.6    1.3    8.7    0.00   979.39     0   14.1    4.0    5.5    7.9    6.1    8.7    5.7"
) |>
  write(tmp_mts_file)

expected_mts <-
  structure(list(RELH = structure(31, units = structure(list(numerator = "percent",
                                                             denominator = character(0)), class = "symbolic_units"), class = "units"),
                 TAIR = structure(NA_real_, units = structure(list(numerator = "°C",
                                                                   denominator = character(0)), class = "symbolic_units"), class = "units"),
                 WSPD = structure(4.6, units = structure(list(numerator = "m",
                                                              denominator = "s"), class = "symbolic_units"), class = "units"),
                 WVEC = structure(4.5, units = structure(list(numerator = "m",
                                                              denominator = "s"), class = "symbolic_units"), class = "units"),
                 WDIR = 182, WDSD = 12.6, WSSD = structure(1.3, units = structure(list(
                   numerator = "m", denominator = "s"), class = "symbolic_units"), class = "units"),
                 WMAX = structure(8.7, units = structure(list(numerator = "m",
                                                              denominator = "s"), class = "symbolic_units"), class = "units"),
                 RAIN = structure(0, units = structure(list(numerator = "mm",
                                                            denominator = character(0)), class = "symbolic_units"), class = "units"),
                 PRES = structure(97.939, units = structure(list(numerator = "kPa",
                                                                 denominator = character(0)), class = "symbolic_units"), class = "units"),
                 SRAD = structure(0, units = structure(list(numerator = "W",
                                                            denominator = c("m", "m")), class = "symbolic_units"), class = "units"),
                 TA9M = structure(14.1, units = structure(list(numerator = "°C",
                                                               denominator = character(0)), class = "symbolic_units"), class = "units"),
                 WS2M = structure(4, units = structure(list(numerator = "m",
                                                            denominator = "s"), class = "symbolic_units"), class = "units"),
                 TS10 = structure(5.5, units = structure(list(numerator = "°C",
                                                              denominator = character(0)), class = "symbolic_units"), class = "units"),
                 TB10 = structure(7.9, units = structure(list(numerator = "°C",
                                                              denominator = character(0)), class = "symbolic_units"), class = "units"),
                 TS05 = structure(6.1, units = structure(list(numerator = "°C",
                                                              denominator = character(0)), class = "symbolic_units"), class = "units"),
                 TB05 = structure(8.7, units = structure(list(numerator = "°C",
                                                              denominator = character(0)), class = "symbolic_units"), class = "units"),
                 TS30 = structure(5.7, units = structure(list(numerator = "°C",
                                                              denominator = character(0)), class = "symbolic_units"), class = "units"),
                 STID = "STIL", STNM = 89L, TIME = structure(0, units = structure(list(
                   numerator = "min", denominator = character(0)), class = "symbolic_units"), class = "units"),
                 TS25 = structure(NA_real_, units = structure(list(numerator = "°C",
                                                                   denominator = character(0)), class = "symbolic_units"), class = "units"),
                 TS60 = structure(NA_real_, units = structure(list(numerator = "°C",
                                                                   denominator = character(0)), class = "symbolic_units"), class = "units"),
                 TR05 = structure(NA_real_, units = structure(list(numerator = "°C",
                                                                   denominator = character(0)), class = "symbolic_units"), class = "units"),
                 TR25 = structure(NA_real_, units = structure(list(numerator = "°C",
                                                                   denominator = character(0)), class = "symbolic_units"), class = "units"),
                 TR60 = structure(NA_real_, units = structure(list(numerator = "°C",
                                                                   denominator = character(0)), class = "symbolic_units"), class = "units"),
                 TR75 = structure(NA_real_, units = structure(list(numerator = "°C",
                                                                   denominator = character(0)), class = "symbolic_units"), class = "units"),
                 TS45 = structure(NA_real_, units = structure(list(numerator = "°C",
                                                                   denominator = character(0)), class = "symbolic_units"), class = "units"),
                 VW05 = structure(NA_real_, units = structure(list(numerator = c("cm",
                                                                                 "cm", "cm"), denominator = c("cm", "cm", "cm")), class = "symbolic_units"), class = "units"),
                 VW25 = structure(NA_real_, units = structure(list(numerator = c("cm",
                                                                                 "cm", "cm"), denominator = c("cm", "cm", "cm")), class = "symbolic_units"), class = "units"),
                 VW45 = structure(NA_real_, units = structure(list(numerator = c("cm",
                                                                                 "cm", "cm"), denominator = c("cm", "cm", "cm")), class = "symbolic_units"), class = "units"),
                 VDEF = structure(NA_real_, units = structure(list(numerator = c("kPa",
                                                                                 "percent"), denominator = character(0)), class = "symbolic_units"), class = "units"),
                 TDEW = structure(NA_real_, units = structure(list(numerator = "°C",
                                                                   denominator = character(0)), class = "symbolic_units"), class = "units"),
                 DATE = structure(757382400, tzone = "UTC", class = c("POSIXct",
                                                                      "POSIXt"))), row.names = 1L, class = "data.frame")

actual_mts <-
  tmp_mts_file |>
  mesonet::mnet_read_mts()

expect_equal(actual_mts,
             expected_mts)

