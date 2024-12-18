library(tinytest)

########################
# test mts_to_rds_list()
########################
expect_equal(
  mesonet:::mts_to_rds_list("/tmp/RtmpFovMA2/.mesonet_cache/mts/1994/01/01/19940101stil.mts"),
  "/tmp/RtmpFovMA2/.mesonet_cache/rds/1994/01/01/19940101stil.rds")

expect_equal(
  mesonet:::mts_to_rds_list(r"(C:\TMP\RtmpFovMA2\.mesonet_cache\mts\1994\01\01\19940101stil.mts)"),
  r"(C:\TMP\RtmpFovMA2\.mesonet_cache\rds\1994\01\01\19940101stil.rds)")

#####################################
# test mnet_concatenate() single STID
#####################################

# Construct reference dataset
ref_df <- data.frame(
    DATE = seq.POSIXt(from = as.POSIXct("1994-01-01 00:00", tz = "UTC"),
                      to = as.POSIXct("1994-01-05 23:55", tz = "UTC"),
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
    RAIN = units::set_units(c(288,1:287), units = "mm")
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

# Write reference dataset to temporary .mesonet_cache

test_cache <-
  tempdir() |>
  file.path(".mesonet_cache")

test_cache |>
  dir.create(recursive = TRUE, showWarnings = FALSE)

all_dates <-
  ref_df$DATE |>
  as.Date() |>
  unique()

for(i in seq_along(all_dates)){

  date_file_name <-
    all_dates[i] |>
    format("rds/%Y/%m/%d/%Y%m%dacme.rds") |>
    paste0(test_cache, "/", .x = _) |>
    gsub("/", .Platform$file.sep, x = _)

  date_file_name |>
    dirname() |>
    dir.create(recursive = TRUE, showWarnings = FALSE)

  ref_df |>
    with({
      ref_df[as.Date(DATE) == all_dates[i],]
    }) |>
    saveRDS(date_file_name)

}

# Convert reference dataset to expected concatenated
expected_subdaily <-
  ref_df |>
  within({
    Date = as.Date(DATE - 1)
    for(date in unique(Date)){
      RAIN[Date == as.Date(date)] <-
        RAIN[Date == as.Date(date)] |>
        c(0, .x = _) |>
        diff()
    }
    date = NULL
    Date = NULL
  })

# Use mnet_concatenate() to read reference data from .mesonet_cache
mesonet:::create_test_site_info(test_cache)

actual_subdaily <- mesonet::mnet_concatenate(stid = "ACME",
                                             start_date = "1994-01-01",
                                             end_date = "1994-01-05",
                                             file_cache = test_cache)

expect_equal(actual_subdaily,
             expected_subdaily)

####################################
# test mnet_concatenated() two STIDs
####################################

altu_df <-
  ref_df |>
  within({
    STID = "ALTU"
    DATE = DATE + as.difftime(5, units = "days")
    RAIN = RAIN*2
  })

all_dates <-
  altu_df$DATE |>
  as.Date() |>
  unique()

for(i in seq_along(all_dates)){

  date_file_name <-
    all_dates[i] |>
    format("rds/%Y/%m/%d/%Y%m%daltu.rds") |>
    paste0(test_cache, "/", .x = _) |>
    gsub("/", .Platform$file.sep, x = _)

  date_file_name |>
    dirname() |>
    dir.create(recursive = TRUE, showWarnings = FALSE)

  altu_df |>
    with({
      altu_df[as.Date(DATE) == all_dates[i],]
    }) |>
    saveRDS(date_file_name)

}

expected_subdaily <-
  expected_subdaily |>
  within({
    STID = "ALTU"
    DATE = DATE + as.difftime(5, units = "days")
    RAIN = RAIN*2
  }) |>
  rbind.data.frame(expected_subdaily, .x = _)
row.names(expected_subdaily) <- 1:nrow(expected_subdaily)

actual_subdaily <- mesonet::mnet_concatenate(stid = c("ACME", "ALTU"),
                                             start_date = c("1994-01-01","1994-01-06"),
                                             end_date = c("1994-01-05","1994-01-10"),
                                             file_cache = test_cache)

expect_equal(actual_subdaily,
             expected_subdaily)

unlink(test_cache, recursive = TRUE)
