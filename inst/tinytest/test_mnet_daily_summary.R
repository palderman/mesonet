library(tinytest)
# Test wdir_to_cardinal()

expect_equal(mesonet:::wdir_to_cardinal(c(     0,  22.5, 45.0,  67.5,
                                          90.0, 112.5, 135.0, 157.5,
                                          180.0, 202.5, 225.0, 247.5,
                                          270.0, 292.5, 315.0, 337.5)),
                   c("N", "NNE", "NE", "ENE",
                     "E", "ESE", "SE", "SSE",
                     "S", "SSW", "SW", "WSW",
                     "W", "WNW", "NW", "NNW"),
             info = "wdir_to_cardinal()")

expect_equal(mesonet:::wdir_to_cardinal(c(348.75,  11.25,  33.75,  56.25,
                                           78.75, 101.25, 123.75, 146.25,
                                          168.75, 191.25, 213.75, 236.25,
                                          258.75, 281.25, 303.75, 326.25)),
                   c("N", "NNE", "NE", "ENE",
                     "E", "ESE", "SE", "SSE",
                     "S", "SSW", "SW", "WSW",
                     "W", "WNW", "NW", "NNW"),
             info = "wdir_to_cardinal()")

# rename_daily_columns

expect_equal(
  mesonet:::rename_daily_columns(c("TAIR_max", "TAIR_min", "TAIR_avg",
                                   "TAIR_maxo", "TAIR_mino",
                                   "RELH_max", "RELH_min", "RELH_avg",
                                   "RELH_maxo", "RELH_mino",
                                   "TDEW_max", "TDEW_min", "TDEW_avg",
                                   "TDEW_maxo", "TDEW_mino",
                                   "PRES_max", "PRES_min", "PRES_avg",
                                   "PRES_maxo", "PRES_mino",
                                   "WS2M_max", "WS2M_min", "WS2M_avg",
                                   "WS2M_maxo", "WS2M_mino",
                                   "WSPD_max", "WSPD_min", "WSPD_avg",
                                   "WSPD_maxo", "WSPD_mino")),
    c("TMAX", "TMIN", "TAVG", "TMAXO", "TMINO",
      "HMAX", "HMIN", "HAVG", "HMAXO", "HMINO",
      "DMAX", "DMIN", "DAVG", "DMAXO", "DMINO",
      "PMAX", "PMIN", "PAVG", "PMAXO", "PMINO",
      "2MAX", "2MIN", "2AVG", "2MAXO", "2MINO",
      "WSMX", "WSMN", "WSPD", "WSMXO", "WSMNO"),
  info = "rename_daily_columns()"
  )

# summarize_across()

subdaily_df <- data.frame(
    DATE = c(
      seq.POSIXt(from = as.POSIXct("1994-02-01 00:00", tz = "UTC"),
                 to = as.POSIXct("1994-02-03 23:55", tz = "UTC"),
                 by = as.difftime(5, units = "mins")),
      seq.POSIXt(from = as.POSIXct("1994-02-01 00:00", tz = "UTC"),
                 to = as.POSIXct("1994-02-03 23:55", tz = "UTC"),
                 by = as.difftime(5, units = "mins")))
  ) |>
  within({
    RELH = units::set_units(rep(31:33, each = 288), "percent")
    TAIR = units::set_units(rep(1:3, each = 288), "°C")
    WSPD = units::set_units(rep(1:3, each = 288), "m/s")
    WVEC = units::set_units(rep(1:3, each = 288), "m/s")
    WDIR = units::set_units(rep(201:203, each = 288), "degrees")
    WDSD = units::set_units(rep(11:13, each = 288), "degrees")
    WSSD = units::set_units(rep(1:3, each = 288), "m/s")
    WMAX = units::set_units(rep(1:3, each = 288), "m/s")
    RAIN = units::set_units(rep(1:3, each = 288), "mm")
    PRES = units::set_units(rep(101:103, each = 288), "kPa")
    SRAD = units::set_units(rep(201:203, each = 288), "W/m^2")
    TA9M = units::set_units(rep(1:3, each = 288), "°C")
    WS2M = units::set_units(rep(1:3, each = 288), "m/s")
    TS10 = units::set_units(5.5, "°C")
    TB10 = units::set_units(7.9, "°C")
    TS05 = units::set_units(6.1, "°C")
    TB05 = units::set_units(8.7, "°C")
    TS30 = units::set_units(5.7, "°C")
    STID = rep(c("ACME", "ALTU"), each = 288*3)
    STNM = 89L
    TS25 = units::set_units(NA_real_, "°C")
    TS60 = units::set_units(NA_real_, "°C")
    TR05 = units::set_units(NA_real_, "°C")
    TR25 = units::set_units(NA_real_, "°C")
    TR60 = units::set_units(NA_real_, "°C")
    TR75 = units::set_units(NA_real_, "°C")
    TS45 = units::set_units(NA_real_, "°C")
    VW05 = units::set_units(NA_real_, "cm^3/cm^3")
    VW25 = units::set_units(NA_real_, "cm^3/cm^3")
    VW45 = units::set_units(NA_real_, "cm^3/cm^3")
    VDEF = units::set_units(NA_real_, "kPa")
    TDEW = units::set_units(NA_real_, "°C")
  }) |>
  mesonet:::standardize_column_order()

# summarize_across() for overall average (i.e. no groups)

expected_avg <-
  subdaily_df |>
  with({
    data.frame(
      RELH_avg = mean(RELH, na.rm = TRUE),
      TAIR_avg = mean(TAIR, na.rm = TRUE)
    )
  })

actual_avg <- mesonet:::summarize_across(
  .data = subdaily_df,
  .cols = c("RELH", "TAIR"),
  .fns = list(avg = \(.x) mean(.x, na.rm = TRUE)))


expect_equal(actual_avg,
             expected_avg,
             info = "Test summarize_across() overall average")

# summarize_across() for average of each STID and DATE

expected_avg <-
  subdaily_df[,c("STID", "DATE")] |>
  within({
    DATE = as.Date(DATE, tz = "Etc/GMT+6")
  }) |>
  unique() |>
  within({
    TAIR_avg = units::set_units(NA_real_, "Celsius")
    RELH_avg = units::set_units(NA_real_, "percent")
  })

row.names(expected_avg) <- NULL

for(.i in 1:nrow(expected_avg)){
  subdaily_df_subset <-
    subdaily_df |>
    within({
      DATE = as.Date(DATE, tz = "Etc/GMT+6")
    }) |>
    with({
      subdaily_df[STID == expected_avg$STID[.i] &
                    DATE == expected_avg$DATE[.i], ]
    })
  expected_avg[.i, "RELH_avg"] <-
    subdaily_df |>
    with({
      mean(subdaily_df_subset$RELH, na.rm = TRUE)
    })
  expected_avg[.i, "TAIR_avg"] <-
    subdaily_df |>
    with({
      mean(subdaily_df_subset$TAIR, na.rm = TRUE)
    })
}

actual_avg <-
  subdaily_df |>
  within({
    DATE = as.Date(DATE, tz = "Etc/GMT+6")
  }) |>
  mesonet:::summarize_across(
  .data = _,
  .cols = c("RELH", "TAIR"),
  .fns = list(avg = \(.x) mean(.x, na.rm = TRUE)),
  .groups = c("STID", "DATE"))

expect_equal(actual_avg,
             expected_avg,
             info = "Test summarize_across() average for each STID and DATE")

# summarize_across() for average of each STID and DATE

expected_avg <-
  subdaily_df[,c("STID", "DATE")] |>
  within({
    DATE = as.Date(DATE, tz = "Etc/GMT+6")
  }) |>
  unique() |>
  within({
    TAIR_sd = units::set_units(NA_real_, "Celsius")
    TAIR_avg = units::set_units(NA_real_, "Celsius")
    RELH_sd = units::set_units(NA_real_, "percent")
    RELH_avg = units::set_units(NA_real_, "percent")
  })

row.names(expected_avg) <- NULL

for(.i in 1:nrow(expected_avg)){
  subdaily_df_subset <-
    subdaily_df |>
    within({
      DATE = as.Date(DATE, tz = "Etc/GMT+6")
    }) |>
    with({
      subdaily_df[STID == expected_avg$STID[.i] &
                    DATE == expected_avg$DATE[.i], ]
    })
  expected_avg[.i, "RELH_avg"] <-
    subdaily_df |>
    with({
      mean(subdaily_df_subset$RELH, na.rm = TRUE)
    })
  expected_avg[.i, "TAIR_avg"] <-
    subdaily_df |>
    with({
      mean(subdaily_df_subset$TAIR, na.rm = TRUE)
    })
  expected_avg[.i, "RELH_sd"] <-
    subdaily_df |>
    with({
      sd(subdaily_df_subset$RELH, na.rm = TRUE)
    })
  expected_avg[.i, "TAIR_sd"] <-
    subdaily_df |>
    with({
      sd(subdaily_df_subset$TAIR, na.rm = TRUE)
    })
}

actual_avg <-
  subdaily_df |>
  within({
    DATE = as.Date(DATE, tz = "Etc/GMT+6")
  }) |>
  mesonet:::summarize_across(
  .data = _,
  .cols = c("RELH", "TAIR"),
  .fns = list(avg = \(.x) mean(.x, na.rm = TRUE),
              sd = \(.x) sd(.x, na.rm = TRUE)),
  .groups = c("STID", "DATE"))

expect_equal(actual_avg,
             expected_avg,
             info = "Test summarize_across() average and st. dev for each STID and DATE")


###
# Test mnet_daily_summary()
###
c("TMAX", "TMIN", "TAVG", "TMAXO", "TMINO",
  "HMAX", "HMIN", "HAVG", "HMAXO", "HMINO",
  "DMAX", "DMIN", "DAVG", "DMAXO", "DMINO",
  "PMAX", "PMIN", "PAVG", "PMAXO", "PMINO",
  "2MAX", "2MIN", "2AVG", "2MAXO", "2MINO",
  "WSMX", "WSMN", "WSPD", "WSMXO", "WSMNO"),

expected_avg <-
  data.frame(
    STNM = 89L,
    STID = rep(c("ACME", "ALTU"), each = 4),
    DATE = rep(as.Date(c("1994-01-31", "1994-02-01", "1994-02-02", "1994-02-03")), 2),
    TMIN = units::set_units(rep(c(NA, 1, 2, NA), 2), "°C"),
    TAVG = units::set_units(rep(c(NA, 1.25, 2.25, NA), 2), "°C"),
    TMAX = units::set_units(rep(c(NA, 2, 3, NA), 2), "°C"),
    TMINO = rep(c(NA, 216, 216, NA), 2),
    TMAXO = rep(c(NA, 72, 72, NA), 2),
    HMIN = units::set_units(rep(c(NA, 31, 32, NA), 2), "percent"),
    HAVG = units::set_units(rep(c(NA, 31.25, 32.25, NA), 2), "percent"),
    HMAX = units::set_units(rep(c(NA, 32, 33, NA), 2), "percent"),
    HMINO = rep(c(NA, 216, 216, NA), 2),
    HMAXO = rep(c(NA, 72, 72, NA), 2),
    WSPD_min = units::set_units(rep(c(NA, 1, 2, NA), 2), "m/s"),
    WSPD_avg = units::set_units(rep(c(NA, 1.25, 2.25, NA), 2), "m/s"),
    WSPD_max = units::set_units(rep(c(NA, 2, 3, NA), 2), "m/s"),
    WDIR_min = units::set_units(rep(c(NA, 201, 202, NA), 2), "degrees"),
    WDIR_avg = units::set_units(rep(c(NA, 201.25, 202.25, NA), 2), "degrees"),
    WDIR_max = units::set_units(rep(c(NA, 202, 203, NA), 2), "degrees"),
    WDSD_min = units::set_units(rep(c(NA, 11, 12, NA), 2), "degrees"),
    WDSD_avg = units::set_units(rep(c(NA, 11.25, 12.25, NA), 2), "degrees"),
    WDSD_max = units::set_units(rep(c(NA, 12, 13, NA), 2), "degrees"),
    WSSD_min = units::set_units(rep(c(NA, 1, 2, NA), 2), "m/s"),
    WSSD_avg = units::set_units(rep(c(NA, 1.25, 2.25, NA), 2), "m/s"),
    WSSD_max = units::set_units(rep(c(NA, 2, 3, NA), 2), "m/s"),
    PRES_min = units::set_units(rep(c(NA, 101, 102, NA), 2), "kPa"),
    PRES_avg = units::set_units(rep(c(NA, 101.25, 102.25, NA), 2), "kPa"),
    PRES_max = units::set_units(rep(c(NA, 102, 103, NA), 2), "kPa"),
    WS2M_min = units::set_units(rep(c(NA, 1, 2, NA), 2), "m/s"),
    WS2M_avg = units::set_units(rep(c(NA, 1.25, 2.25, NA), 2), "m/s"),
    WS2M_max = units::set_units(rep(c(NA, 2, 3, NA), 2), "m/s"),
    DMIN = units::set_units(rep(NA_real_, 8), "°C"),
    DAVG = units::set_units(rep(NA_real_, 8), "°C"),
    DMAX = units::set_units(rep(NA_real_, 8), "°C"),
    DMINO = rep(c(NA, NA, NA, NA), 2),
    DMAXO = rep(c(NA, NA, NA, NA), 2),
    # WMAX = units::set_units(rep(1:3, 2), "m/s"),
    # RAIN = units::set_units(rep(1:3, 2), "mm"),
    # SRAD = units::set_units(rep(201:203, 2), "W/m^2"),
    # TA9M = units::set_units(rep(1:3, 2), "°C"),
    # TS10 = units::set_units(rep(5.5, 6), "°C"),
    # TB10 = units::set_units(rep(7.9, 6), "°C"),
    # TS05 = units::set_units(rep(6.1, 6), "°C"),
    # TB05 = units::set_units(rep(8.7, 6), "°C"),
    # TS30 = units::set_units(rep(5.7, 6), "°C"),
    # STNM = 89L,
    # TS25 = units::set_units(rep(NA_real_, 6), "°C"),
    # TS60 = units::set_units(rep(NA_real_, 6), "°C"),
    # TR05 = units::set_units(rep(NA_real_, 6), "°C"),
    # TR25 = units::set_units(rep(NA_real_, 6), "°C"),
    # TR60 = units::set_units(rep(NA_real_, 6), "°C"),
    # TR75 = units::set_units(rep(NA_real_, 6), "°C"),
    # TS45 = units::set_units(rep(NA_real_, 6), "°C"),
    # VW05 = units::set_units(rep(NA_real_, 6), "cm^3/cm^3"),
    # VW25 = units::set_units(rep(NA_real_, 6), "cm^3/cm^3"),
    # VW45 = units::set_units(rep(NA_real_, 6), "cm^3/cm^3"),
    # VDEF = units::set_units(rep(NA_real_, 6), "kPa"),
  )

actual_avg <-
  subdaily_df |>
  mesonet::mnet_daily_summary()

expect_equal(colnames(actual_avg),
             colnames(expected_avg),
             info = "Test mnet_daily_summary() - check column names")

expect_equal(actual_avg,
             expected_avg,
info = "Test mnet_daily_summary() - check data frame contents")
