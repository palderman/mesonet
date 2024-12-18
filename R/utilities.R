
make_date <- function(date_chr){
  date_chr |>
    as.character() |>
    as.POSIXct(format = "%Y%m%d",
               tz = "UTC")
}

standardize_column_order <- function(df){

  col_ord <-
    c("STNM", "STID", "DATE",
      "RELH", "TAIR", "WSPD", "WVEC", "WDIR", "WDSD", "WSSD", "WMAX",
      "RAIN", "PRES", "SRAD", "TA9M", "WS2M", "TS10", "TB10", "TS05",
      "TB05", "TS30", "TS25", "TS60", "TR05", "TR25",
      "TR60", "TR75", "TS45", "VW05", "VW25", "VW45", "VDEF", "TDEW")

  col_ord <- col_ord[col_ord %in% colnames(df)]

  df[, col_ord]
}

mts_unit <- function(x, .name){
  if(.name %in% c("TA9M", "TAIR", "TB05", "TB10",
                  "TR05", "TR25", "TR60", "TR75",
                  "TS05", "TS10", "TS25", "TS30", "TS45", "TS60")){
    return(units::set_units(x, "Celsius"))
  }else if(.name %in% c("VW05", "VW25", "VW45")){
    return(units::set_units(x, "cm^3/cm^3"))
  }else if(.name %in% c("WMAX", "WS2M", "WSPD", "WSSD", "WVEC")){
    return(units::set_units(x, "m/s"))
  }else if(.name %in% c("WDIR", "WDSD")){
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
  units::keep_units(\(.y) c(rep(default, n), head(.y, -n)),
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
