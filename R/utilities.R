
make_date <- function(date_chr){
  date_chr |>
    as.character() |>
    as.POSIXct(format = "%Y%m%d",
               tz = "UTC")
}

standardize_column_names <- function(df){
  cnames <- colnames(df)
  replacement <- c(
    "_avg$" = "AVG",
    "_max$" = "MAX",
    "_max_count$" = "MAXO",
    "_min$" = "MIN",
    "_maxo$" = "MAXO",
    "_mino$" = "MINO",
    "_sd$" = "DEV",
    "_bad$" = "BAD",
    "^(VW.*)AVG" = "\\1",
    "^RELH" = "H",
    "^TAIR" = "T",
    "^TDEW" = "D",
    "^SRAD_sum$" = "ATOT",
    "^SRAD" = "A",
    "^RAIN" = "R",
    "^PRES" = "P",
    "^TA9M" = "9",
    "^WS2M" = "2",
    "WDIR_([sp]dir)" = "\\U\\1",
    "WDIR_([sp]dfq)" = "\\U\\1",
    "^R_sum" = "RAIN",
    "^R_gt0" = "RNUM",
    "^WSPD" = "WS",
    "^WSM([AI])" = "WSM",
    "^WSAVG" = "WSPD",
    "WSDEV" = "WDEV",
    "WMAXMAX" = "WMAX",
    "VDEFAVG" = "VDEF",
    "^T([SB])([0-9])" = "\\1\\2",
    "10" = "",
    "30" = "3",
    "([SB])05" = "\\15",
    "([035]AV)G" = "\\1",
    "^([SB].+)M([AI])" = "\\1M",
    "^(TR.+)AV" = "\\1",
    "WSBAD" = "WBAD",
    "WDIRBAD" = "IBAD",
    "([SB])([0-9]+)BAD" = "\\1\\2BD",
    "TR([0-9]+)BAD" = "R\\1BD",
    "(S[0-9]{2})M([XN]O)" = "\\1\\2"
  )
  regex <- names(replacement)
  for(.i in seq_along(replacement)){
    cnames <- gsub(regex[.i], replacement[.i], cnames, perl = TRUE)
  }
  colnames(df) <- cnames
  df
}

standardize_column_order <- function(df){

  col_ord <-
    c("STNM", "STID", "DATE",
      "RELH", "HAVG", "HMAX", "HMIN",
      "DAVG", "DMAX", "DMIN",
      "TAIR", "TAVG", "TMAX", "TMIN",
      "WSPD", "WSMX", "WSMN", "WDEV",
      "WVEC", "WDIR", "WDSD", "WSSD", "PDIR", "PDFQ", "SDIR", "SDFQ",
      "WMAX",
      "RAIN", "RMAX",
      "PRES", "PAVG", "PMAX", "PMIN", "MSLP",
      "SRAD", "ATOT", "AMAX",
      "TA9M", "9AVG",
      "WS2M", "2AVG", "2MAX", "2MIN", "2DEV",
      "TS10", "SAVG", "SMAX", "SMIN",
      "TB10", "BAVG", "BMAX", "BMIN",
      "TS05", "S5AV", "S5MX", "S5MN",
      "TB05", "B5AV", "B5MX", "B5MN",
      "TS30", "S3AV", "S3MX", "S3MN",
      "TS25", "S25AV", "S25MX", "S25MN",
      "TS60", "S60AV", "S60MX", "S60MN",
      "TR05", "TR25", "TR60", "TR75",
      "VW05", "VW25", "VW45", "VDEF", "TDEW",
      "HMAXO", "HMINO", "HBAD",
      "DMAXO", "DMINO", "DBAD",
      "TMAXO", "TMINO", "TBAD", "9BAD",
      "WSMXO", "WSMNO", "WMAXO", "WBAD", "IBAD", "2BAD",
      "RNUM", "RBAD",
      "PMAXO", "PMINO", "PBAD",
      "AMAXO", "ABAD",
      "B5MXO", "B5MNO", "B5BD",
      "BMAXO", "BMINO", "BBAD",
      "S5MXO", "S5MNO", "S5BD",
      "SMAXO", "SMINO", "SBAD",
      "S25XO", "S25NO", "S25BD",
      "S3MXO", "S3MNO", "S3BD",
      "S60XO", "S60NO", "S60BD",
      "R05BD", "R25BD", "R60BD", "R75BD"
    )

  col_ord <- col_ord[col_ord %in% colnames(df)]

  df[, col_ord]
}

mts_unit <- function(x, .name){
  if(.name %in% c("TA9M", "TAIR", "TB05", "TB10",
                  "TR05", "TR25", "TR60", "TR75",
                  "TS05", "TS10", "TS25", "TS30", "TS45", "TS60")){
    return(units::set_units(x, "Celsius"))
  }else if(.name %in% c("VW05", "VW25", "VW45",
                        "wcr05", "wcs05", "wcr10", "wcs10", "wcr25", "wcs25",
                        "wcr60", "wcs60", "wcr75", "wcs75")){
    return(units::set_units(x, "cm^3/cm^3"))
  }else if(.name %in% c("STID", "name", "city", "cdir", "cnty", "cdiv", "clas",
                        "text05", "text10", "text25", "text60", "text75")){
    return(as.character(x))
  }else if(.name %in% c("a05", "a10", "a25", "a60", "a75")){
    return(units::set_units(x, "1/kPa"))
  }else if(.name %in% c("n05", "n10", "n25", "n60", "n75")){
    return(units::set_units(x, "1"))
  }else if(.name %in% c("bulk5", "bulk10", "bulk25", "bulk60", "bulk75")){
    return(units::set_units(x, "g/cm3"))
  }else if(.name %in% "elev"){
    return(units::set_units(x, "m"))
  }else if(.name %in% c("grav5", "sand5", "silt5", "clay5",
                        "grav10", "sand10", "silt10", "clay10",
                        "grav25", "sand25", "silt25", "clay25",
                        "grav60", "sand60", "silt60", "clay60",
                        "grav75", "sand75", "silt75", "clay75")){
    return(units::set_units(x, "percent"))
  }else if(.name %in% c("WMAX", "WS2M", "WSPD", "WSSD", "WVEC")){
    return(units::set_units(x, "m/s"))
  }else if(.name %in% c("WDIR", "WDSD", "nlat", "elon")){
    return(units::set_units(x, "degrees"))
  }else if(.name %in% "SRAD"){
    return(units::set_units(x, "W/m^2"))
  }else if(.name %in% "RELH"){
    return(units::set_units(x, "percent"))
  }else if(.name %in% "RAIN"){
    return(units::set_units(x, "mm"))
  }else if(.name %in% "TIME"){
    return(units::set_units(x, "minutes"))
  }else if(.name %in% "PRES"){
    return(units::set_units(x, "millibars"))
  }else{
    return(x)
  }
}

set_mts_units <- function(.data){
  for(.col in names(.data)){
    .data[[.col]] <- mts_unit(.data[[.col]], .col)
  }
  .data
}

set_missing <- function(.data){
  for(.col in names(.data)){
    if(!.col %in% c("STID", "STNM", "TIME")){
      .data[[.col]] <- ifelse(.data[[.col]] < -990, NA_real_, .data[[.col]])
    }
  }
  .data
}

lag_units <- function(.x, n = 1, default = NA_real_){
  units::keep_units(\(.y) c(rep(default, n), utils::head(.y, -n)),
                    x = .x)
}

#
# lag_diff <- function(.x){
#   units::keep_units(\(.y) diff(c(0, .y)),
#                     x = .x)
# }

sat_vap_pres <- function(Tair) {
  # Clausius-Clapeyron
  T_K <- units::set_units(Tair, "Kelvin") |>
    units::drop_units()
  # L is for liquid water
  L <- 2.5e6 # J/kg
  Rv <- 461 # J/K/kg
  To <- 273.15 # K
  eo <- 0.6113 # kPa
  es <- eo*exp(L/Rv*(1/To-1/T_K)) |>
    units::set_units("kPa")
  return(es)
}

T_sat <- function(es) {
  # Clausius-Clapeyron
  es <- units::drop_units(es)
  # L is for liquid water
  L <- 2.5e6 # J/kg
  Rv <- 461 # J/K/kg
  To <- 273.15 # K
  eo <- 0.6113 # kPa
  Tair <- 1/(1/To-log(es/eo)*Rv/L)
  T_K <- units::set_units(Tair, "Kelvin")
  T_C <- units::set_units(T_K, "Celsius")
  return(T_C)
}

calc_tdew <- function(Tair, RH){
  T_K <- units::set_units(Tair, "Kelvin") |>
    units::drop_units()
  RH_pct <- units::set_units(RH, "percent") |>
    units::drop_units()
  # Using inversion of Clausius-Clapeyron
  es <- sat_vap_pres(T_K) |>
    units::drop_units()
  ea <- RH_pct/100*es # kPa
  # L is for liquid water
  L <- 2.5e6 # J/kg
  Rv <- 461 # J/K/kg
  To <- 273.15 # K
  eo <- 0.6113 # kPa
  T_dew <-
    units::set_units(1/(1/To - log(ea/eo)*Rv/L), "Kelvin") |>
    units::set_units("Celsius")
  return(T_dew)
}
