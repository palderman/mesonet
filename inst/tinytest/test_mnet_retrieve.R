library(tinytest)

# Create test mesonet root url

test_scenario <-
  data.frame(
    stid = c("ACME", "ALTU"),
    start = c("1994-02-01", "1994-02-02"),
    end = c("1994-02-03", "1994-02-04"))

test_site_info <-
  data.frame(
    stid = c("ACME", "ALTU"),
    datc = as.POSIXct("1994-01-01", tz = "UTC"),
    datd = as.POSIXct("2099-12-31", tz = "UTC")
  )

# Create remote cache for testing

test_remote_cache <-
  tempdir() |>
  file.path(".test_remote_cache")

test_remote_cache |>
  dir.create(recursive = TRUE,
             showWarnings = FALSE)

# Write test files to test_remote_cache

test_files <-
  test_scenario |>
  within({
    stid = lapply(stid, \(.x) rep(.x, 3))
    dates = mapply(\(.start, .end){
      seq(as.POSIXct(.start, tz = "UTC"),
          as.POSIXct(.end, tz = "UTC"),
          by = "days")
    }, .start = start, .end = end, SIMPLIFY = FALSE)
  }) |>
  with({
    data.frame(
      row.names = NULL,
      stid = unlist(stid),
      dates = unlist(dates) |> as.POSIXct(tz = "UTC")
    )
  }) |>
  within({
    year_dir = format(dates, "%Y")
    month_dir = format(dates, "%m")
    day_dir = format(dates, "%d")
    file_name = format(dates, "%Y%m%d") |>
      paste0(stid, ".mts") |>
      tolower()
    rel_path = file.path("mts",
                         year_dir, month_dir, day_dir,
                         file_name)
    file_path = file.path(test_remote_cache, rel_path)
  })

test_files |>
  with({
    mapply(\(.path, .stid = stid, .date){
      dirname(.path) |>
        dir.create(recursive = TRUE,
                   showWarnings = FALSE)

      c("  101 ! (c) 1994 Oklahoma Climatological Survey and the Oklahoma Mesonet - all rights reserved",
        format(.date, "  18 %Y %m %d 00 00 00"),
        " STID  STNM  TIME   RELH   TAIR   WSPD   WVEC  WDIR   WDSD   WSSD   WMAX    RAIN     PRES  SRAD   TA9M   WS2M   TS10   TB10   TS05   TB05   TS30",
        paste0(" ", toupper(.stid),
               "    89",
               sprintf("%6i", seq(0, 1435, by = 5)),
               "     31   -999    4.6    4.5   182   12.6    1.3    8.7",
               sprintf("%9.2f", 1:288),
               "   979.39     0   14.1    4.0    5.5    7.9    6.1    8.7    5.7")
      ) |>
      write(.path)
    }, .path = file_path, .stid = stid, .date = dates)
  })

# Set up test_root_url

test_root_url <-
  test_remote_cache |>
  gsub("\\\\", "/", x = _) |>
  gsub("^/*", "///", x = _) |>
  paste0("file:", .x = _)

# Create local cache for testing

test_local_cache <-
  tempdir() |>
  file.path(".mesonet_cache")

dir.create(test_local_cache,
           showWarnings = FALSE,
           recursive = TRUE)

mesonet:::create_test_site_info(test_local_cache)

#########################################
# test mnet_retrieve() all from download
#########################################

# Construct reference dataset
ref_df <- data.frame(
    TIME = seq.POSIXt(from = as.POSIXct("1994-02-01 00:00", tz = "UTC"),
                      to = as.POSIXct("1994-02-03 23:55", tz = "UTC"),
                      by = as.difftime(5, units = "mins"))
  ) |>
  within({
    RELH = units::set_units(31, units = "percent")
    TAIR = units::set_units(NA_real_, units = "°C")
    WSPD = units::set_units(4.6, units = "m/s")
    WVEC = units::set_units(4.5, units = "m/s")
    WDIR = 182
    WDSD = 12.6
    WSSD = units::set_units(1.3, units = "m/s")
    WMAX = units::set_units(8.7, units = "m/s")
    RAIN = units::set_units(1, units = "mm")
    PRES = units::set_units(97.939, units = "kPa")
    SRAD = units::set_units(0, units = "W/m^2")
    TA9M = units::set_units(14.1, units = "°C")
    WS2M = units::set_units(4, units = "m/s")
    TS10 = units::set_units(5.5, units = "°C")
    TB10 = units::set_units(7.9, units = "°C")
    TS05 = units::set_units(6.1, units = "°C")
    TB05 = units::set_units(8.7, units = "°C")
    TS30 = units::set_units(5.7, units = "°C")
    STID = "ACME"
    STNM = 89L
    TS25 = units::set_units(NA_real_, units = "°C")
    TS60 = units::set_units(NA_real_, units = "°C")
    TR05 = units::set_units(NA_real_, units = "°C")
    TR25 = units::set_units(NA_real_, units = "°C")
    TR60 = units::set_units(NA_real_, units = "°C")
    TR75 = units::set_units(NA_real_, units = "°C")
    TS45 = units::set_units(NA_real_, units = "°C")
    VW05 = units::set_units(NA_real_, units = "cm^3/cm^3")
    VW25 = units::set_units(NA_real_, units = "cm^3/cm^3")
    VW45 = units::set_units(NA_real_, units = "cm^3/cm^3")
    VDEF = units::set_units(NA_real_, units = "kPa")
    TDEW = units::set_units(NA_real_, units = "°C")
  })

ref_df$RAIN[1] <- 288

expected_subdaily <-
  ref_df |>
  within({
    STID = "ALTU"
    TIME = TIME + as.difftime(1, units = "days")
  }) |>
  rbind.data.frame(ref_df, .x = _)

row.names(expected_subdaily) <- 1:nrow(expected_subdaily)

actual_subdaily <-
  test_scenario |>
  with({
    mesonet::mnet_retrieve(stid = stid,
                           start_date = start,
                           end_date = end,
                           delay = 0,
                           root_url = test_root_url,
                           file_cache = test_local_cache,
                           silent = TRUE)
  })

expect_equal(actual_subdaily,
             expected_subdaily)

###############################################
# test mnet_retrieve() all from local mts cache
###############################################


###############################################
# test mnet_retrieve() all from local rds cache
###############################################
