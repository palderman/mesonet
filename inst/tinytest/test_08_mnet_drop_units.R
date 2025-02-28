library(tinytest)

df_with_units <-
  data.frame(
    DATE = as.POSIXct(757382400, tz = "UTC")
  ) |>
  within({
    RELH = units::set_units(31, "percent")
    TAIR = units::set_units(NA_real_, "°C")
    WSPD = units::set_units(4.6, "m/s")
    WVEC = units::set_units(4.5, "m/s")
    WDIR = units::set_units(182, "degrees")
    WDSD = units::set_units(12.6, "degrees")
    WSSD = units::set_units(1.3, "m/s")
    WMAX = units::set_units(8.7, "m/s")
    RAIN = units::set_units(0, "mm")
    PRES = units::set_units(97.939, "kPa")
    SRAD = units::set_units(0, "W/m^2")
    TA9M = units::set_units(14.1, "°C")
    WS2M = units::set_units(4, "m/s")
    TS10 = units::set_units(5.5, "°C")
    TB10 = units::set_units(7.9, "°C")
    TS05 = units::set_units(6.1, "°C")
    TB05 = units::set_units(8.7, "°C")
    TS30 = units::set_units(5.7, "°C")
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
    STID = "ACME"
    STNM = 89L
  }) |>
  mesonet:::standardize_column_order()

df_without_units <- df_with_units

for(i in seq_along(df_without_units)){
  if("units" %in% class(df_without_units[[i]])){
    df_without_units[[i]] <- units::drop_units(df_without_units[[i]])
  }
}

expect_equal(
  mesonet::mnet_drop_units(df_with_units),
  df_without_units
)
