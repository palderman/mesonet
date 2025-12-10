library(tinytest)

site_info_path <-
  tempdir() |>
  file.path("tests", "site_info.csv")

site_info_path |>
  dirname() |>
  mesonet::mnet_test_cache(site_info = TRUE)

file_cache <-
  tempdir() |>
  file.path(".mesonet_cache")

actual <-
  paste0("file://", site_info_path) |>
  mesonet::mnet_site_info(file_cache = file_cache)

expected <-
  structure(list(
    stnm = c(110L, 1L, 2L, 116L, 3L),
    stid = c("ACME", "ADAX", "ALTU", "ALV2", "ALVA"),
    name = c("Acme", "Ada", "Altus", "Alva", "Alva"),
    city = c("Rush Springs", "Ada", "Altus", "Alva", "Alva"),
    rang = c(4, 2, 3, 7.2, 2),
    cdir = c("WNW", "NNE", "S", "SSW", "S"),
    cnty = c("Grady", "Pontotoc", "Jackson", "Woods", "Woods"),
    nlat = units::set_units(c(34.80833, 34.79851, 34.58722, 36.70823, 36.7797),
                            "degrees"),
    elon = units::set_units(c(-98.02325, -96.66909, -99.33808, -98.70974,
                              -98.6717),
                            "degrees"),
    elev = units::set_units(c(397, 295, 416, 439, 450), "m"),
    cdiv = c("Central", "South Central", "Southwest", "North Central",
             "North Central"),
    clas = c("STANDARD", "STANDARD", "STANDARD", "STANDARD", "STANDARD"),
    wcr05 = units::set_units(c(0.034, 0.05, 0.089, 0.086, NA), "cm3/cm3"),
    wcs05 = units::set_units(c(0.41, 0.483, 0.384, 0.365, NA), "cm3/cm3"),
    a05 = units::set_units(c(0.273, 0.272, 0.078, 0.037, NA), "1/kPa"),
    n05 = units::set_units(c(1.39, 1.36, 1.29, 1.34, NA), "1"),
    bulk5 = units::set_units(c(1.27, 1.01, 1.57, 1.66, NA), "g/cm3"),
    grav5 = units::set_units(c(0.1, 0, 0, 0, NA), "percent"),
    sand5 = units::set_units(c(73, 61.1, 23.8, 33.3, NA), "percent"),
    silt5 = units::set_units(c(19.7, 22, 40, 31.4, NA), "percent"),
    clay5 = units::set_units(c(7.2, 16.9, 36.2, 35.3, NA), "percent"),
    text5 = c("Sandy Loam", "Sandy Loam", "Clay Loam", "Clay Loam", NA),
    wcr10 = units::set_units(c(0.037, 0.055, 0.092, 0.089, NA), "cm3/cm3"),
    wcs10 = units::set_units(c(0.41, 0.445, 0.387, 0.372, NA), "cm3/cm3"),
    a10 = units::set_units(c(0.253, 0.216, 0.076, 0.044, NA), "1/kPa"),
    n10 = units::set_units(c(1.38, 1.35, 1.28, 1.31, NA), "1"),
    bulk10 = units::set_units(c(1.29, 1.21, 1.57, 1.64, NA), "g/cm3"),
    grav10 = units::set_units(c(0L, 0L, 0L, 0L, NA), "percent"),
    sand10 = units::set_units(c(69.9, 58.6, 22.3, 30.3, NA), "percent"),
    silt10 = units::set_units(c(20.8, 22.9, 41.3, 31, NA), "percent"),
    clay10 = units::set_units(c(9.4, 18.6, 36.4, 38.7, NA), "percent"),
    text10 = c(NA_character_, NA_character_, NA_character_, NA_character_,
               NA_character_),
    wcr25 = units::set_units(c(0.05, 0.069, 0.103, 0.1, NA), "cm3/cm3"),
    wcs25 = units::set_units(c(0.411, 0.324, 0.396, 0.396, NA), "cm3/cm3"),
    a25 = units::set_units(c(0.206, 0.095, 0.071, 0.076, NA), "1/kPa"),
    n25 = units::set_units(c(1.36, 1.26, 1.26, 1.25, NA), "1"),
    bulk25 = units::set_units(c(1.33, 1.79, 1.56, 1.58, NA), "g/cm3"),
    grav25 = units::set_units(c(0.1, 0, 0, 0, NA), "percent"),
    sand25 = units::set_units(c(60.3, 51.1, 17.8, 21.2, NA), "percent"),
    silt25 = units::set_units(c(23.9, 25.4, 45.2, 29.7, NA), "percent"),
    clay25 = units::set_units(c(15.7, 23.5, 37, 49.1, NA), "percent"),
    text25 = c("Sandy Loam", "Sandy Clay Loam", "Silty Clay Loam", "Clay", NA),
    wcr60 = units::set_units(c(0.068, 0.08, 0.118, 0.115, NA), "cm3/cm3"),
    wcs60 = units::set_units(c(0.361, 0.317, 0.373, 0.382, NA), "cm3/cm3"),
    a60 = units::set_units(c(0.192, 0.06, 0.073, 0.13, NA), "1/kPa"),
    n60 = units::set_units(c(1.34, 1.21, 1.17, 1.2, NA), "1"),
    bulk60 = units::set_units(c(1.6, 1.85, 1.69, 1.64, NA), "g/cm3"),
    grav60 = units::set_units(c(0.1, 0, 0, 0, NA), "percent"),
    sand60 = units::set_units(c(52.5, 47.6, 14.3, 19.8, NA), "percent"),
    silt60 = units::set_units(c(25.9, 24.6, 40.9, 29.8, NA), "percent"),
    clay60 = units::set_units(c(21.5, 27.8, 44.8, 50.5, NA), "percent"),
    text60 = c("Sandy Clay Loam", "Sandy Clay Loam", "Silty Clay", "Clay", NA),
    wcr75 = units::set_units(c(0.07, 0.086, 0.118, NA, NA), "cm3/cm3"),
    wcs75 = units::set_units(c(0.347, 0.318, 0.346, NA, NA), "cm3/cm3"),
    a75 = units::set_units(c(0.178, 0.053, 0.059, NA, NA), "1/kPa"),
    n75 = units::set_units(c(1.32, 1.2, 1.16, NA, NA), "1"),
    bulk75 = units::set_units(c(1.68, 1.86, 1.73, NA, NA), "g/cm3"),
    grav75 = units::set_units(c(0, 0, 0, NA, NA), "percent"),
    sand75 = units::set_units(c(53.2, 43.6, 15, NA, NA), "percent"),
    silt75 = units::set_units(c(24.1, 26.6, 40.9, NA, NA), "percent"),
    clay75 = units::set_units(c(22.8, 29.8, 44.1, NA, NA), "percent"),
    text75 = c("Sandy Clay Loam", "Clay Loam", "Silty Clay", NA, NA),
    datc = structure(c(757382400, 757382400, 757382400, 913852800, 757382400),
                     class = c("POSIXct", "POSIXt"), tzone = "UTC"),
    datd = structure(c(4102358400, 4102358400, 4102358400, 4102358400,
                       913766400),
                     class = c("POSIXct", "POSIXt"), tzone = "UTC")),
    row.names = c(NA, -5L),
    class = "data.frame")

expect_equal(actual, expected)

# Check cached csv file

cached_csv_path <-
  tempdir() |>
  file.path(".mesonet_cache", "site_info.csv")

expect_true(file.exists(cached_csv_path))

csv_actual <-
  readLines(cached_csv_path)

csv_expected <-
  readLines(site_info_path)

expect_equal(csv_actual, csv_expected)

unlink(cached_csv_path)

# Check cached rds file

cached_rds_path <-
  cached_csv_path |>
  gsub("csv$", "rds", x = _)

expect_true(file.exists(cached_rds_path))

rds_actual <-
  readRDS(cached_rds_path)

expect_equal(rds_actual, expected)

unlink(cached_rds_path)

site_info_path |>
  dirname() |>
  unlink(recursive = TRUE)

unlink(file_cache, recursive = TRUE)
