library(tinytest)
# Test wdir_to_cardinal()

dir_deg <-
  c(     0,  22.5, 45.0,  67.5,
      90.0, 112.5, 135.0, 157.5,
     180.0, 202.5, 225.0, 247.5,
     270.0, 292.5, 315.0, 337.5) |>
  units::set_units("degrees")

expect_equal(mesonet:::wdir_to_cardinal(dir_deg),
             c("N", "NNE", "NE", "ENE",
               "E", "ESE", "SE", "SSE",
               "S", "SSW", "SW", "WSW",
               "W", "WNW", "NW", "NNW"),
             info = "wdir_to_cardinal()")

dir_deg <-
  c(348.75,  11.25,  33.75,  56.25,
     78.75, 101.25, 123.75, 146.25,
    168.75, 191.25, 213.75, 236.25,
    258.75, 281.25, 303.75, 326.25) |>
  units::set_units("degrees")

expect_equal(mesonet:::wdir_to_cardinal(dir_deg),
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
    TAIR = units::set_units(rep(1:3, each = 288), "Celsius")
    WSPD = units::set_units(rep(1:3, each = 288), "m/s")
    WVEC = units::set_units(rep(1:3, each = 288), "m/s")
    WDIR = units::set_units(rep(201:203, each = 288), "degrees")
    WDSD = units::set_units(rep(11:13, each = 288), "degrees")
    WSSD = units::set_units(rep(1:3, each = 288), "m/s")
    WMAX = units::set_units(rep(1:3, each = 288), "m/s")
    RAIN = units::set_units(rep(1:3, each = 288), "mm")
    PRES = units::set_units(rep(101:103, each = 288), "kPa")
    SRAD = units::set_units(rep(201:203, each = 288), "W/m^2")
    TA9M = units::set_units(rep(1:3, each = 288), "Celsius")
    WS2M = units::set_units(rep(1:3, each = 288), "m/s")
    TS10 = units::set_units(rep(c(5.5, NA, NA), 96), "Celsius")
    TB10 = units::set_units(rep(c(7.9, NA, NA), 96), "Celsius")
    TS05 = units::set_units(rep(c(6.1, NA, NA), 96), "Celsius")
    TB05 = units::set_units(rep(c(8.7, NA, NA), 96), "Celsius")
    TS30 = units::set_units(rep(c(5.7, NA, NA), 96), "Celsius")
    STID = rep(c("ACME", "ALTU"), each = 288*3)
    STNM = 89L
    TS25 = units::set_units(NA_real_, "Celsius")
    TS60 = units::set_units(NA_real_, "Celsius")
    TR05 = units::set_units(NA_real_, "Celsius")
    TR25 = units::set_units(NA_real_, "Celsius")
    TR60 = units::set_units(NA_real_, "Celsius")
    TR75 = units::set_units(NA_real_, "Celsius")
    TS45 = units::set_units(NA_real_, "Celsius")
    VW05 = units::set_units(NA_real_, "cm^3/cm^3")
    VW25 = units::set_units(NA_real_, "cm^3/cm^3")
    VW45 = units::set_units(NA_real_, "cm^3/cm^3")
    VDEF = units::set_units(NA_real_, "kPa")
    TDEW = units::set_units(NA_real_, "Celsius")
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
    DATE = as.POSIXct(as.Date(DATE, tz = "Etc/GMT+6"), tz = "Etc/GMT+6")
  }) |>
  unique() |>
  within({
    TAIR_min = units::set_units(NA_real_, "Celsius")
    TAIR_max = units::set_units(NA_real_, "Celsius")
    TAIR_sd = units::set_units(NA_real_, "Celsius")
    TAIR_avg = units::set_units(NA_real_, "Celsius")
    RELH_min = units::set_units(NA_real_, "percent")
    RELH_max = units::set_units(NA_real_, "percent")
    RELH_sd = units::set_units(NA_real_, "percent")
    RELH_avg = units::set_units(NA_real_, "percent")
  })

row.names(expected_avg) <- NULL

for(.i in 1:nrow(expected_avg)){
  subdaily_df_subset <-
    subdaily_df |>
    within({
      DATE = as.POSIXct(as.Date(DATE, tz = "Etc/GMT+6"), tz = "Etc/GMT+6")
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
  expected_avg[.i, "RELH_max"] <-
    subdaily_df |>
    with({
      units::keep_units(max, subdaily_df_subset$RELH, na.rm = TRUE)
    })
  expected_avg[.i, "TAIR_max"] <-
    subdaily_df |>
    with({
      units::keep_units(max, subdaily_df_subset$TAIR, na.rm = TRUE)
    })
  expected_avg[.i, "RELH_min"] <-
    subdaily_df |>
    with({
      units::keep_units(min, subdaily_df_subset$RELH, na.rm = TRUE)
    })
  expected_avg[.i, "TAIR_min"] <-
    subdaily_df |>
    with({
      units::keep_units(min, subdaily_df_subset$TAIR, na.rm = TRUE)
    })
}

actual_avg <-
  subdaily_df |>
  within({
    DATE = as.POSIXct(as.Date(DATE, tz = "Etc/GMT+6"), tz = "Etc/GMT+6")
  }) |>
  mesonet:::summarize_across(
  .data = _,
  .cols = c("RELH", "TAIR"),
  .fns = list(avg = \(.x) mean(.x, na.rm = TRUE),
              sd = \(.x) units::keep_units(sd, .x, na.rm = TRUE),
              max = \(.x) max( .x, na.rm = TRUE),
              min = \(.x) min(.x, na.rm = TRUE)),
  .groups = c("STID", "DATE"))

expect_equal(actual_avg,
             expected_avg,
             info = "Test summarize_across() average and st. dev for each STID and DATE")


# summarize_across() for overall average (i.e. no groups) with no TAIR column

expected_avg <-
  subdaily_df |>
  with({
    data.frame(
      RELH_avg = mean(RELH, na.rm = TRUE)
    )
  })

expect_warning(
  {actual_avg <-
    subdaily_df |>
    within({TAIR = NULL}) |>
    mesonet:::summarize_across(
      .data = _,
      .cols = c("RELH", "TAIR"),
      .fns = list(avg = \(.x) mean(.x, na.rm = TRUE)))},
  pattern = "^The following variables were missing from dataset and will not be summarized:")

expect_equal(actual_avg,
             expected_avg,
             info = "Test summarize_across() missing TAIR column")

############################################################
# Test mnet_summarize() for daily interval with QC variables
############################################################
if(tinytest::at_home()){
  expected_avg <-
    data.frame(
      STNM = 89L,
      STID = rep(c("ACME", "ALTU"), each = 4),
      DATE = rep(as.POSIXct(c("1994-01-31", "1994-02-01", "1994-02-02", "1994-02-03"), tz = "Etc/GMT+6"), 2),
      TMIN = units::set_units(rep(c(NA, 1, 2, NA), 2), "Celsius"),
      TAVG = units::set_units(rep(c(NA, 1.25, 2.25, NA), 2), "Celsius"),
      TMAX = units::set_units(rep(c(NA, 2, 3, NA), 2), "Celsius"),
      HMIN = units::set_units(rep(c(NA, 31, 32, NA), 2), "percent"),
      HAVG = units::set_units(rep(c(NA, 31.25, 32.25, NA), 2), "percent"),
      HMAX = units::set_units(rep(c(NA, 32, 33, NA), 2), "percent"),
      WSMN = units::set_units(rep(c(NA, 1, 2, NA), 2), "m/s"),
      WSPD = units::set_units(rep(c(NA, 1.25, 2.25, NA), 2), "m/s"),
      WSMX = units::set_units(rep(c(NA, 2, 3, NA), 2), "m/s"),
      PDIR = rep(c(NA, "SSW", "SSW", NA), 2),
      SDIR = rep(c(NA, "SSW", "SSW", NA), 2),
      PDFQ = units::set_units(rep(c(NA, 100, 100, NA), 2), "percent"),
      SDFQ = units::set_units(rep(c(NA, 100, 100, NA), 2), "percent"),
      PMIN = units::set_units(rep(c(NA, 101, 102, NA), 2), "kPa"),
      PAVG = units::set_units(rep(c(NA, 101.25, 102.25, NA), 2), "kPa"),
      PMAX = units::set_units(rep(c(NA, 102, 103, NA), 2), "kPa"),
      `2MIN` = units::set_units(rep(c(NA, 1, 2, NA), 2), "m/s"),
      `2AVG` = units::set_units(rep(c(NA, 1.25, 2.25, NA), 2), "m/s"),
      `2MAX` = units::set_units(rep(c(NA, 2, 3, NA), 2), "m/s"),
      DMIN = units::set_units(rep(NA_real_, 8), "Celsius"),
      DAVG = units::set_units(rep(NA_real_, 8), "Celsius"),
      DMAX = units::set_units(rep(NA_real_, 8), "Celsius"),
      WMAX = units::set_units(rep(c(NA, 2:3, NA), 2), "m/s"),
      RAIN = units::set_units(rep(c(NA, 216*1 + 72*2, 216*2 + 72*3 , NA), 2), "mm"),
      RNUM = rep(c(NA, 288, 288, NA), 2),
      RMAX = units::set_units(rep(c(NA, 2, 3, NA)*60/5, 2), "mm/hour"),
      ATOT = units::set_units(rep(c(NA, (201*18+202*6), (202*18+203*6), NA)*60*60*1e-6, 2), "MJ/d/m2"),
      AMAX = units::set_units(rep(c(NA, 202, 203, NA), 2), "W/m2"),
      `9AVG` = units::set_units(rep(c(NA, (18*1 + 6*2)/24, (18*2 + 6*3)/24, NA), 2), "Celsius"),
      SAVG = units::set_units(rep(c(NA, rep(5.5, 2), NA), 2), "Celsius"),
      SMAX = units::set_units(rep(c(NA, rep(5.5, 2), NA), 2), "Celsius"),
      SMIN = units::set_units(rep(c(NA, rep(5.5, 2), NA), 2), "Celsius"),
      BAVG = units::set_units(rep(c(NA, rep(7.9, 2), NA), 2), "Celsius"),
      BMAX = units::set_units(rep(c(NA, rep(7.9, 2), NA), 2), "Celsius"),
      BMIN = units::set_units(rep(c(NA, rep(7.9, 2), NA), 2), "Celsius"),
      S5AV = units::set_units(rep(c(NA, rep(6.1, 2), NA), 2), "Celsius"),
      S5MX = units::set_units(rep(c(NA, rep(6.1, 2), NA), 2), "Celsius"),
      S5MN = units::set_units(rep(c(NA, rep(6.1, 2), NA), 2), "Celsius"),
      B5AV = units::set_units(rep(c(NA, rep(8.7, 2), NA), 2), "Celsius"),
      B5MX = units::set_units(rep(c(NA, rep(8.7, 2), NA), 2), "Celsius"),
      B5MN = units::set_units(rep(c(NA, rep(8.7, 2), NA), 2), "Celsius"),
      S3AV = units::set_units(rep(c(NA, rep(5.7, 2), NA), 2), "Celsius"),
      S3MX = units::set_units(rep(c(NA, rep(5.7, 2), NA), 2), "Celsius"),
      S3MN = units::set_units(rep(c(NA, rep(5.7, 2), NA), 2), "Celsius"),
      S25AV = units::set_units(rep(NA_real_, 8), "Celsius"),
      S25MX = units::set_units(rep(NA_real_, 8), "Celsius"),
      S25MN = units::set_units(rep(NA_real_, 8), "Celsius"),
      S60AV = units::set_units(rep(NA_real_, 8), "Celsius"),
      S60MX = units::set_units(rep(NA_real_, 8), "Celsius"),
      S60MN = units::set_units(rep(NA_real_, 8), "Celsius"),
      TR05 = units::set_units(rep(NA_real_, 8), "Celsius"),
      TR25 = units::set_units(rep(NA_real_, 8), "Celsius"),
      TR60 = units::set_units(rep(NA_real_, 8), "Celsius"),
      TR75 = units::set_units(rep(NA_real_, 8), "Celsius"),
      TS45 = units::set_units(rep(NA_real_, 8), "Celsius"),
      VW05 = units::set_units(rep(NA_real_, 8), "cm^3/cm^3"),
      VW25 = units::set_units(rep(NA_real_, 8), "cm^3/cm^3"),
      VW45 = units::set_units(rep(NA_real_, 8), "cm^3/cm^3"),
      VDEF = units::set_units(rep(NA_real_, 8), "kPa"),
      TMINO = rep(c(NA, 216, 216, NA), 2),
      TMAXO = rep(c(NA, 72, 72, NA), 2),
      TBAD = rep(c(288*18/24, 0, 0, 288*6/24), 2),
      HMINO = rep(c(NA, 216, 216, NA), 2),
      HMAXO = rep(c(NA, 72, 72, NA), 2),
      HBAD = rep(c(288*18/24, 0, 0, 288*6/24), 2),
      `9BAD` = rep(c(288*18/24, 0, 0, 288*6/24), 2),
      WSMNO = rep(c(NA, 216, 216, NA), 2),
      WSMXO = rep(c(NA, 72, 72, NA), 2),
      WBAD = rep(c(288*18/24, 0, 0, 288*6/24), 2),
      WMAXO = rep(c(NA, 72, 72, NA), 2),
      IBAD = rep(c(288*18/24, 0, 0, 288*6/24), 2),
      RBAD = rep(c(288*18/24, 0, 0, 288*6/24), 2),
      PMINO = rep(c(NA, 216, 216, NA), 2),
      PMAXO = rep(c(NA, 72, 72, NA), 2),
      PBAD = rep(c(288*18/24, 0, 0, 288*6/24), 2),
      `2MINO` = rep(c(NA, 216, 216, NA), 2),
      `2MAXO` = rep(c(NA, 72, 72, NA), 2),
      `2BAD` = rep(c(288*18/24, 0, 0, 288*6/24), 2),
      AMAXO = rep(c(NA, 72, 72, NA), 2),
      ABAD = rep(c(288*18/24, 0, 0, 288*6/24), 2),
      DBAD = rep(288, 8),
      B5MNO = rep(c(NA, 96, 96, NA), 2),
      BMINO = rep(c(NA, 96, 96, NA), 2),
      S5MNO = rep(c(NA, 96, 96, NA), 2),
      SMINO = rep(c(NA, 96, 96, NA), 2),
      S25NO = rep(NA_real_, 8),
      S3MNO = rep(c(NA, 96, 96, NA), 2),
      S60NO = rep(NA_real_, 8),
      B5MXO = rep(c(NA, 96, 96, NA), 2),
      BMAXO = rep(c(NA, 96, 96, NA), 2),
      S5MXO = rep(c(NA, 96, 96, NA), 2),
      SMAXO = rep(c(NA, 96, 96, NA), 2),
      S25XO = rep(NA_real_, 8),
      S3MXO = rep(c(NA, 96, 96, NA), 2),
      S60XO = rep(NA_real_, 8),
      B5BD = rep(c(72, 0, 0, 24), 2),
      BBAD = rep(c(72, 0, 0, 24), 2),
      S5BD = rep(c(72, 0, 0, 24), 2),
      SBAD = rep(c(72, 0, 0, 24), 2),
      S25BD = rep(96, 8),
      S3BD = rep(c(72, 0, 0, 24), 2),
      S60BD = rep(96, 8),
      R05BD = rep(48, 8),
      R25BD = rep(48, 8),
      R60BD = rep(48, 8),
      R75BD = rep(48, 8),
      check.names = FALSE
    ) |>
    mesonet:::standardize_column_order()

  actual_avg <-
    subdaily_df |>
    mesonet::mnet_summarize(include_qc_variables = TRUE)

  expect_equal(colnames(actual_avg),
               colnames(expected_avg),
               info = "Test mnet_summarize() - daily interval with QC variables, check column names")

  if(ncol(actual_avg) == ncol(expected_avg) &
     all(colnames(actual_avg) == colnames(expected_avg))){
    expect_equal(actual_avg,
                 expected_avg,
                 info = "Test mnet_summarize() - daily interval with QC variables, check data frame contents")
  }
}

###############################################################
# Test mnet_summarize() for daily interval without QC variables
###############################################################

expected_avg <-
  data.frame(
    STNM = 89L,
    STID = rep(c("ACME", "ALTU"), each = 4),
    DATE = rep(as.POSIXct(c("1994-01-31", "1994-02-01", "1994-02-02", "1994-02-03"), tz = "Etc/GMT+6"), 2),
    TMIN = units::set_units(rep(c(NA, 1, 2, NA), 2), "Celsius"),
    TAVG = units::set_units(rep(c(NA, 1.25, 2.25, NA), 2), "Celsius"),
    TMAX = units::set_units(rep(c(NA, 2, 3, NA), 2), "Celsius"),
    HMIN = units::set_units(rep(c(NA, 31, 32, NA), 2), "percent"),
    HAVG = units::set_units(rep(c(NA, 31.25, 32.25, NA), 2), "percent"),
    HMAX = units::set_units(rep(c(NA, 32, 33, NA), 2), "percent"),
    WSMN = units::set_units(rep(c(NA, 1, 2, NA), 2), "m/s"),
    WSPD = units::set_units(rep(c(NA, 1.25, 2.25, NA), 2), "m/s"),
    WSMX = units::set_units(rep(c(NA, 2, 3, NA), 2), "m/s"),
    PDIR = rep(c(NA, "SSW", "SSW", NA), 2),
    SDIR = rep(c(NA, "SSW", "SSW", NA), 2),
    PMIN = units::set_units(rep(c(NA, 101, 102, NA), 2), "kPa"),
    PAVG = units::set_units(rep(c(NA, 101.25, 102.25, NA), 2), "kPa"),
    PMAX = units::set_units(rep(c(NA, 102, 103, NA), 2), "kPa"),
    `2MIN` = units::set_units(rep(c(NA, 1, 2, NA), 2), "m/s"),
    `2AVG` = units::set_units(rep(c(NA, 1.25, 2.25, NA), 2), "m/s"),
    `2MAX` = units::set_units(rep(c(NA, 2, 3, NA), 2), "m/s"),
    DMIN = units::set_units(rep(NA_real_, 8), "Celsius"),
    DAVG = units::set_units(rep(NA_real_, 8), "Celsius"),
    DMAX = units::set_units(rep(NA_real_, 8), "Celsius"),
    WMAX = units::set_units(rep(c(NA, 2:3, NA), 2), "m/s"),
    RAIN = units::set_units(rep(c(NA, 216*1 + 72*2, 216*2 + 72*3 , NA), 2), "mm"),
    RMAX = units::set_units(rep(c(NA, 2, 3, NA)*60/5, 2), "mm/hour"),
    ATOT = units::set_units(rep(c(NA, (201*18+202*6), (202*18+203*6), NA)*60*60*1e-6, 2), "MJ/d/m2"),
    AMAX = units::set_units(rep(c(NA, 202, 203, NA), 2), "W/m2"),
    `9AVG` = units::set_units(rep(c(NA, (18*1 + 6*2)/24, (18*2 + 6*3)/24, NA), 2), "Celsius"),
    SAVG = units::set_units(rep(c(NA, rep(5.5, 2), NA), 2), "Celsius"),
    SMAX = units::set_units(rep(c(NA, rep(5.5, 2), NA), 2), "Celsius"),
    SMIN = units::set_units(rep(c(NA, rep(5.5, 2), NA), 2), "Celsius"),
    BAVG = units::set_units(rep(c(NA, rep(7.9, 2), NA), 2), "Celsius"),
    BMAX = units::set_units(rep(c(NA, rep(7.9, 2), NA), 2), "Celsius"),
    BMIN = units::set_units(rep(c(NA, rep(7.9, 2), NA), 2), "Celsius"),
    S5AV = units::set_units(rep(c(NA, rep(6.1, 2), NA), 2), "Celsius"),
    S5MX = units::set_units(rep(c(NA, rep(6.1, 2), NA), 2), "Celsius"),
    S5MN = units::set_units(rep(c(NA, rep(6.1, 2), NA), 2), "Celsius"),
    B5AV = units::set_units(rep(c(NA, rep(8.7, 2), NA), 2), "Celsius"),
    B5MX = units::set_units(rep(c(NA, rep(8.7, 2), NA), 2), "Celsius"),
    B5MN = units::set_units(rep(c(NA, rep(8.7, 2), NA), 2), "Celsius"),
    S3AV = units::set_units(rep(c(NA, rep(5.7, 2), NA), 2), "Celsius"),
    S3MX = units::set_units(rep(c(NA, rep(5.7, 2), NA), 2), "Celsius"),
    S3MN = units::set_units(rep(c(NA, rep(5.7, 2), NA), 2), "Celsius"),
    S25AV = units::set_units(rep(NA_real_, 8), "Celsius"),
    S25MX = units::set_units(rep(NA_real_, 8), "Celsius"),
    S25MN = units::set_units(rep(NA_real_, 8), "Celsius"),
    S60AV = units::set_units(rep(NA_real_, 8), "Celsius"),
    S60MX = units::set_units(rep(NA_real_, 8), "Celsius"),
    S60MN = units::set_units(rep(NA_real_, 8), "Celsius"),
    TR05 = units::set_units(rep(NA_real_, 8), "Celsius"),
    TR25 = units::set_units(rep(NA_real_, 8), "Celsius"),
    TR60 = units::set_units(rep(NA_real_, 8), "Celsius"),
    TR75 = units::set_units(rep(NA_real_, 8), "Celsius"),
    TS45 = units::set_units(rep(NA_real_, 8), "Celsius"),
    VW05 = units::set_units(rep(NA_real_, 8), "cm^3/cm^3"),
    VW25 = units::set_units(rep(NA_real_, 8), "cm^3/cm^3"),
    VW45 = units::set_units(rep(NA_real_, 8), "cm^3/cm^3"),
    VDEF = units::set_units(rep(NA_real_, 8), "kPa"),
    check.names = FALSE
  ) |>
  mesonet:::standardize_column_order()

actual_avg <-
  subdaily_df |>
  mesonet::mnet_summarize(include_qc_variables = FALSE)

expect_equal(colnames(actual_avg),
             colnames(expected_avg),
             info = "Test mnet_summarize() - daily interval without QC variables, check column names")

if(ncol(actual_avg) == ncol(expected_avg) &
   all(colnames(actual_avg) == colnames(expected_avg))){
  expect_equal(actual_avg,
               expected_avg,
               info = "Test mnet_summarize() - daily interval without QC variables, check data frame contents")
}


######################################################################################
# Test mnet_summarize() for daily interval without QC variables - individual variables
######################################################################################

expected_avg <-
  data.frame(
    STNM = 89L,
    STID = rep(c("ACME", "ALTU"), each = 4),
    DATE = rep(as.POSIXct(c("1994-01-31", "1994-02-01", "1994-02-02", "1994-02-03"), tz = "Etc/GMT+6"), 2),
    TMIN = units::set_units(rep(c(NA, 1, 2, NA), 2), "Celsius"),
    TAVG = units::set_units(rep(c(NA, 1.25, 2.25, NA), 2), "Celsius"),
    TMAX = units::set_units(rep(c(NA, 2, 3, NA), 2), "Celsius"),
    HMIN = units::set_units(rep(c(NA, 31, 32, NA), 2), "percent"),
    HAVG = units::set_units(rep(c(NA, 31.25, 32.25, NA), 2), "percent"),
    HMAX = units::set_units(rep(c(NA, 32, 33, NA), 2), "percent"),
    PMIN = units::set_units(rep(c(NA, 101, 102, NA), 2), "kPa"),
    PAVG = units::set_units(rep(c(NA, 101.25, 102.25, NA), 2), "kPa"),
    PMAX = units::set_units(rep(c(NA, 102, 103, NA), 2), "kPa"),
    `2MIN` = units::set_units(rep(c(NA, 1, 2, NA), 2), "m/s"),
    `2AVG` = units::set_units(rep(c(NA, 1.25, 2.25, NA), 2), "m/s"),
    `2MAX` = units::set_units(rep(c(NA, 2, 3, NA), 2), "m/s"),
    RAIN = units::set_units(rep(c(NA, 216*1 + 72*2, 216*2 + 72*3 , NA), 2), "mm"),
    RMAX = units::set_units(rep(c(NA, 2, 3, NA)*60/5, 2), "mm/hour"),
    ATOT = units::set_units(rep(c(NA, (201*18+202*6), (202*18+203*6), NA)*60*60*1e-6, 2), "MJ/d/m2"),
    AMAX = units::set_units(rep(c(NA, 202, 203, NA), 2), "W/m2"),
    check.names = FALSE
  ) |>
  mesonet:::standardize_column_order()

variables <-
  list(
    TAIR = c("TAVG", "TMAX", "TMIN"),
    RELH = c("HAVG", "HMAX", "HMIN"),
    PRES = c("PAVG", "PMAX", "PMIN"),
    WS2M = c("2AVG", "2MAX", "2MIN"),
    RAIN = c("RAIN", "RMAX"),
    SRAD = c("ATOT", "AMAX")
  )

for(v in names(variables)){
  expect_warning({
    actual_avg <-
      subdaily_df |>
      subset(select = c("STID", "DATE", v)) |>
      mesonet::mnet_summarize(include_qc_variables = FALSE)
  })

  expect_equal(colnames(actual_avg),
               c("STID", "DATE", variables[[v]]),
               info = paste0("Test mnet_summarize() - daily interval, only ", v, " check column names"))

  if(ncol(actual_avg) == length(c("STID", "DATE", variables[[v]])) &
     all(colnames(actual_avg) == c("STID", "DATE", variables[[v]]))){
    expect_equal(actual_avg,
                 expected_avg[, c("STID", "DATE", variables[[v]])],
                 info = paste0("Test mnet_summarize() - daily interval, only ", v, " check data frame contents"))
  }

}

#############################################################
# Test mnet_summarize() for hourly interval with QC variables
#############################################################
if(tinytest::at_home()){
  expected_avg <-
    data.frame(
      STID = rep(c("ACME", "ALTU"), each = 3*24),
      DATE = rep(seq(as.POSIXct("1994-01-31 19:00", tz = "Etc/GMT+6"),
                     as.POSIXct("1994-02-03 18:00", tz = "Etc/GMT+6"),
                     by = as.difftime(1, units = "hours")), 2),
      TMIN = units::set_units(rep(c(1:3, 1:3), each = 24), "Celsius"),
      TAVG = units::set_units(rep(c(1:3, 1:3), each = 24), "Celsius"),
      TMAX = units::set_units(rep(c(1:3, 1:3), each = 24), "Celsius"),
      WSMN = units::set_units(rep(c(1:3, 1:3), each = 24), "m/s"),
      WSPD = units::set_units(rep(c(1:3, 1:3), each = 24), "m/s"),
      WSMX = units::set_units(rep(c(1:3, 1:3), each = 24), "m/s"),
      PDIR = rep("SSW", 2*3*24),
      SDIR = rep("SSW", 2*3*24),
      PDFQ = units::set_units(rep(100, 2*3*24), "percent"),
      SDFQ = units::set_units(rep(100, 2*3*24), "percent"),
      RAIN = units::set_units(rep(c(1:3*12, 1:3*12), each = 24), "mm"),
      RNUM = rep(12, 2*3*24),
      RMAX = units::set_units(rep(c(1:3*12, 1:3*12), each = 24), "mm/hour"),
      ATOT = units::set_units(units::set_units(rep(c(201:203, 201:203), each = 24), "W/m2"), "MJ/d/m2"),
      AMAX = units::set_units(rep(c(201:203, 201:203), each = 24), "W/m2"),
      SAVG = units::set_units(rep(5.5, 2*3*24), "Celsius"),
      SMAX = units::set_units(rep(5.5, 2*3*24), "Celsius"),
      SMIN = units::set_units(rep(5.5, 2*3*24), "Celsius"),
      TR05 = units::set_units(rep(NA_real_, 2*3*24), "Celsius"),
      TMINO = rep(12, 2*3*24),
      TMAXO = rep(12, 2*3*24),
      TBAD = rep(0, 2*3*24),
      WSMNO = rep(12, 2*3*24),
      WSMXO = rep(12, 2*3*24),
      WBAD = rep(0, 2*3*24),
      IBAD = rep(0, 2*3*24),
      RBAD = rep(0, 2*3*24),
      AMAXO = rep(12, 2*3*24),
      ABAD = rep(0, 2*3*24),
      SMINO = rep(4, 2*3*24),
      SMAXO = rep(4, 2*3*24),
      SBAD = rep(0, 2*3*24),
      R05BD = rep(2, 2*3*24),
      check.names = FALSE
    ) |>
    mesonet:::standardize_column_order()

  expect_warning({
    actual_avg <-
      subdaily_df |>
      subset(select = c("STID", "DATE", "TAIR", "WSPD", "WDIR", "RAIN", "SRAD",
                        "TS10", "TR05")) |>
      mesonet::mnet_summarize(interval = "1 hour",
                              include_qc_variables = TRUE)
  })

  expect_equal(colnames(actual_avg),
               colnames(expected_avg),
               info = "Test mnet_summarize() - hourly interval with QC variables, check column names")

  if(ncol(actual_avg) == ncol(expected_avg) &
     all(colnames(actual_avg) == colnames(expected_avg))){
    expect_equal(actual_avg,
                 expected_avg,
                 info = "Test mnet_summarize() - hourly interval with QC variables, check data frame contents")
  }
}

################################################################
# Test mnet_summarize() for hourly interval without QC variables
################################################################
expected_avg <-
  data.frame(
    STID = rep(c("ACME", "ALTU"), each = 3*24),
    DATE = rep(seq(as.POSIXct("1994-01-31 19:00", tz = "Etc/GMT+6"),
                   as.POSIXct("1994-02-03 18:00", tz = "Etc/GMT+6"),
                   by = as.difftime(1, units = "hours")), 2),
    TMIN = units::set_units(rep(c(1:3, 1:3), each = 24), "Celsius"),
    TAVG = units::set_units(rep(c(1:3, 1:3), each = 24), "Celsius"),
    TMAX = units::set_units(rep(c(1:3, 1:3), each = 24), "Celsius"),
    WSMN = units::set_units(rep(c(1:3, 1:3), each = 24), "m/s"),
    WSPD = units::set_units(rep(c(1:3, 1:3), each = 24), "m/s"),
    WSMX = units::set_units(rep(c(1:3, 1:3), each = 24), "m/s"),
    PDIR = rep("SSW", 2*3*24),
    SDIR = rep("SSW", 2*3*24),
    RAIN = units::set_units(rep(c(1:3*12, 1:3*12), each = 24), "mm"),
    RMAX = units::set_units(rep(c(1:3*12, 1:3*12), each = 24), "mm/hour"),
    ATOT = units::set_units(units::set_units(rep(c(201:203, 201:203), each = 24), "W/m2"), "MJ/d/m2"),
    AMAX = units::set_units(rep(c(201:203, 201:203), each = 24), "W/m2"),
    SAVG = units::set_units(rep(5.5, 2*3*24), "Celsius"),
    SMAX = units::set_units(rep(5.5, 2*3*24), "Celsius"),
    SMIN = units::set_units(rep(5.5, 2*3*24), "Celsius"),
    TR05 = units::set_units(rep(NA_real_, 2*3*24), "Celsius"),
    check.names = FALSE
  ) |>
  mesonet:::standardize_column_order()

expect_warning({
  actual_avg <-
    subdaily_df |>
    subset(select = c("STID", "DATE", "TAIR", "WSPD", "WDIR", "RAIN", "SRAD",
                      "TS10", "TR05")) |>
    mesonet::mnet_summarize(interval = "1 hour",
                            include_qc_variables = FALSE)
})

expect_equal(colnames(actual_avg),
             colnames(expected_avg),
             info = "Test mnet_summarize() - hourly interval without QC variables, check column names")

if(ncol(actual_avg) == ncol(expected_avg) &
   all(colnames(actual_avg) == colnames(expected_avg))){
  expect_equal(actual_avg,
               expected_avg,
               info = "Test mnet_summarize() - hourly interval without QC variables, check data frame contents")
}

#############################################################
# Test mnet_summarize() for 30 min interval with QC variables
#############################################################
if(tinytest::at_home()){
  expected_avg <-
    data.frame(
      STID = rep(c("ACME", "ALTU"), each = 3*24*2),
      DATE = rep(seq(as.POSIXct("1994-01-31 18:30", tz = "Etc/GMT+6"),
                     as.POSIXct("1994-02-03 18:00", tz = "Etc/GMT+6"),
                     by = as.difftime(30, units = "mins")), 2),
      TMIN = units::set_units(rep(c(1:3, 1:3), each = 24*2), "Celsius"),
      TAVG = units::set_units(rep(c(1:3, 1:3), each = 24*2), "Celsius"),
      TMAX = units::set_units(rep(c(1:3, 1:3), each = 24*2), "Celsius"),
      WSMN = units::set_units(rep(c(1:3, 1:3), each = 24*2), "m/s"),
      WSPD = units::set_units(rep(c(1:3, 1:3), each = 24*2), "m/s"),
      WSMX = units::set_units(rep(c(1:3, 1:3), each = 24*2), "m/s"),
      PDIR = rep("SSW", 2*3*24*2),
      SDIR = rep("SSW", 2*3*24*2),
      PDFQ = units::set_units(rep(100, 2*3*24*2), "percent"),
      SDFQ = units::set_units(rep(100, 2*3*24*2), "percent"),
      RAIN = units::set_units(rep(c(1:3*6, 1:3*6), each = 24*2), "mm"),
      RNUM = rep(6, 2*3*24*2),
      RMAX = units::set_units(rep(c(1:3*12, 1:3*12), each = 24*2), "mm/hour"),
      ATOT = units::set_units(units::set_units(rep(c(201:203, 201:203), each = 24*2), "W/m2"), "MJ/d/m2"),
      AMAX = units::set_units(rep(c(201:203, 201:203), each = 24*2), "W/m2"),
      SAVG = units::set_units(rep(5.5, 2*3*24*2), "Celsius"),
      SMAX = units::set_units(rep(5.5, 2*3*24*2), "Celsius"),
      SMIN = units::set_units(rep(5.5, 2*3*24*2), "Celsius"),
      TR05 = units::set_units(rep(NA_real_, 2*3*24*2), "Celsius"),
      TMINO = rep(6, 2*3*24*2),
      TMAXO = rep(6, 2*3*24*2),
      TBAD = rep(0, 2*3*24*2),
      WSMNO = rep(6, 2*3*24*2),
      WSMXO = rep(6, 2*3*24*2),
      WBAD = rep(0, 2*3*24*2),
      IBAD = rep(0, 2*3*24*2),
      RBAD = rep(0, 2*3*24*2),
      AMAXO = rep(6, 2*3*24*2),
      ABAD = rep(0, 2*3*24*2),
      SMINO = rep(2, 2*3*24*2),
      SMAXO = rep(2, 2*3*24*2),
      SBAD = rep(0, 2*3*24*2),
      R05BD = rep(1, 2*3*24*2),
      check.names = FALSE
    ) |>
    mesonet:::standardize_column_order()

  expect_warning({
    actual_avg <-
      subdaily_df |>
      subset(select = c("STID", "DATE", "TAIR", "WSPD", "WDIR", "RAIN", "SRAD",
                        "TS10", "TR05")) |>
      mesonet::mnet_summarize(interval = "30 min",
                              include_qc_variables = TRUE)
  })

  expect_equal(colnames(actual_avg),
               colnames(expected_avg),
               info = "Test mnet_summarize() - 30 min interval with QC variables, check column names")

  if(ncol(actual_avg) == ncol(expected_avg) &
     all(colnames(actual_avg) == colnames(expected_avg))){
    expect_equal(actual_avg,
                 expected_avg,
                 info = "Test mnet_summarize() - 30 min interval with QC variables, check data frame contents")
  }
}

################################################################
# Test mnet_summarize() for 30 min interval without QC variables
################################################################
if(tinytest::at_home()){
  expected_avg <-
    data.frame(
      STID = rep(c("ACME", "ALTU"), each = 3*24*2),
      DATE = rep(seq(as.POSIXct("1994-01-31 18:30", tz = "Etc/GMT+6"),
                     as.POSIXct("1994-02-03 18:00", tz = "Etc/GMT+6"),
                     by = as.difftime(30, units = "mins")), 2),
      TMIN = units::set_units(rep(c(1:3, 1:3), each = 24*2), "Celsius"),
      TAVG = units::set_units(rep(c(1:3, 1:3), each = 24*2), "Celsius"),
      TMAX = units::set_units(rep(c(1:3, 1:3), each = 24*2), "Celsius"),
      WSMN = units::set_units(rep(c(1:3, 1:3), each = 24*2), "m/s"),
      WSPD = units::set_units(rep(c(1:3, 1:3), each = 24*2), "m/s"),
      WSMX = units::set_units(rep(c(1:3, 1:3), each = 24*2), "m/s"),
      PDIR = rep("SSW", 2*3*24*2),
      SDIR = rep("SSW", 2*3*24*2),
      RAIN = units::set_units(rep(c(1:3*6, 1:3*6), each = 24*2), "mm"),
      RMAX = units::set_units(rep(c(1:3*12, 1:3*12), each = 24*2), "mm/hour"),
      ATOT = units::set_units(units::set_units(rep(c(201:203, 201:203), each = 24*2), "W/m2"), "MJ/d/m2"),
      AMAX = units::set_units(rep(c(201:203, 201:203), each = 24*2), "W/m2"),
      SAVG = units::set_units(rep(5.5, 2*3*24*2), "Celsius"),
      SMAX = units::set_units(rep(5.5, 2*3*24*2), "Celsius"),
      SMIN = units::set_units(rep(5.5, 2*3*24*2), "Celsius"),
      TR05 = units::set_units(rep(NA_real_, 2*3*24*2), "Celsius"),
      check.names = FALSE
    ) |>
    mesonet:::standardize_column_order()


  expect_warning({
    actual_avg <-
      subdaily_df |>
      subset(select = c("STID", "DATE", "TAIR", "WSPD", "WDIR", "RAIN", "SRAD",
                        "TS10", "TR05")) |>
      mesonet::mnet_summarize(interval = "30 min",
                              include_qc_variables = FALSE)
  })

  expect_equal(colnames(actual_avg),
               colnames(expected_avg),
               info = "Test mnet_summarize() - 30 min interval without QC variables, check column names")

  if(ncol(actual_avg) == ncol(expected_avg) &
     all(colnames(actual_avg) == colnames(expected_avg))){
    expect_equal(actual_avg,
                 expected_avg,
                 info = "Test mnet_summarize() - 30 min interval without QC variables, check data frame contents")
  }
}
