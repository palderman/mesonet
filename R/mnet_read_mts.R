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

#' Read an Oklahoma Mesonet time series file
#'
#' @export
#'
#' @md
#'
#' @param file_path file path to a single Oklahoma Mesonet time series (MTS)
#'  file
#'
#' @return A data frame with Oklahoma Mesonet data. See below for a list of
#'  variables including column ID, name, unit and description:
#'
#' |**ID** |**Name**                                    |**Unit**                |**Description**                                                                                                                                                                                           |
#' |:------|:-------------------------------------------|:-----------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
#' |PRES   |Station Atmospheric Pressure                |kPa                     |5-minute averaged atmospheric pressure.                                                                                                                                                                   |
#' |RAIN   |Precipitation                               |millimeters             |Liquid precipitation accumulation since 0000 UTC. Frozen precipitation cannot be recorded until it melts; therefore, precipitation from snow may not be recorded until several days after the snow event. |
#' |RELH   |Relative Humidity                           |percent                 |5-minute averaged relative humidity at 1.5m.                                                                                                                                                              |
#' |SRAD   |Solar Radiation                             |watts per square meter  |5-minute averaged downwelling global solar radiation.                                                                                                                                                     |
#' |STID   |Station ID                                  |                        |Station ID                                                                                                                                                                                                |
#' |STNM   |Station Number                              |                        |Station Number                                                                                                                                                                                            |
#' |TA9M   |Air Temperature at 9m                       |degrees Celsius         |5-minute averaged air temperature at 9m                                                                                                                                                                   |
#' |TAIR   |Air Temperature                             |degrees Celsius         |5-minute averaged air temperature at 1.5m.                                                                                                                                                                |
#' |TB05   |Temperature Under Bare Soil at 5cm          |degrees Celsius         |15-minute averaged temperature under bare soil at 5cm. This variable is only available prior to December 1, 2013.                                                                                         |
#' |TB10   |Temperature Under Bare Soil at 10cm         |degrees Celsius         |15-minute averaged temperature under bare soil at 10cm.                                                                                                                                                   |
#' |TIME   |Time                                        |minutes after base time |Minutes after base time (typically 0000 UTC)                                                                                                                                                              |
#' |TR05   |Soil Moisture Calibrated Delta-T at 5cm     |degrees Celsius         |30-minute calibrated change in temperature of soil over time after a heat pulse is introduced. Used to calculate soil water potential, fractional water index, or volumetric water.                       |
#' |TR25   |Soil Moisture Calibrated Delta-T at 25cm    |degrees Celsius         |30-minute calibrated change in temperature of soil over time after a heat pulse is introduced. Used to calculate soil water potential, fractional water index, or volumetric water.                                 |
#' |TR60   |Soil Moisture Calibrated Delta-T at 60cm    |degrees Celsius         |30-minute calibrated change in temperature of soil over time after a heat pulse is introduced. Used to calculate soil water potential, fractional water index, or volumetric water.                                 |
#' |TR75   |Soil Moisture Calibrated Delta-T at 75cm    |degrees Celsius         |30-minute calibrated change in temperature of soil over time after a heat pulse is introduced. Used to calculate soil water potential, fractional water index, or volumetric water.                                 |
#' |TS05   |Temperature Under Native Vegetation at 5cm  |degrees Celsius         |15-minute averaged temperature under native vegetation at 5cm.                                                                                                                                            |
#' |TS10   |Temperature Under Native Vegetation at 10cm |degrees Celsius         |15-minute averaged temperature under native vegetation at 10cm.                                                                                                                                           |
#' |TS25   |Temperature Under Native Vegetation at 25cm |degrees Celsius         |15-minute averaged temperature under native vegetation at 25cm. This variable is only available after and including December 1, 2013.                                                                     |
#' |TS30   |Temperature Under Native Vegetation at 30cm |degrees Celsius         |15-minute averaged temperature under native vegetation at 30cm. This variable is only available prior to December 1, 2013.                                                                                |
#' |TS45   |Temperature Under Native Vegetation at 45cm |degrees Celsius         |15-minute averaged temperature under native vegetation at 45cm.                                                                                                                                           |
#' |TS60   |Temperature Under Native Vegetation at 60cm |degrees Celsius         |15-minute averaged temperature under native vegetation at 60cm. This variable is only available after and including December 1, 2013.                                                                     |
#' |VW05   |Volumetric soil water Under Native Vegetation at 5cm  |cm^3^ cm^-3^  |5-minute averaged volumetric soil water under native vegetation at 5cm.                                                                                                                                   |
#' |VW25   |Volumetric soil water Under Native Vegetation at 25cm |cm^3^ cm^-3^  |5-minute averaged volumetric soil water under native vegetation at 25cm.                                                                                                                                  |
#' |VW45   |Volumetric soil water Under Native Vegetation at 45cm |cm^3^ cm^-3^  |5-minute averaged volumetric soil water under native vegetation at 45cm.                                                                                                                                  |
#' |WDIR   |Wind Direction                              |degrees                 |5-minute averaged wind direction at 10m.                                                                                                                                                                  |
#' |WDSD   |Wind Direction Standard Deviation           |degrees                 |Standard deviation of wind direction during the 5-minute interval.                                                                                                                                        |
#' |WMAX   |Maximum Wind Speed                          |meters per second       |Highest 3-second wind speed at 10m sample.                                                                                                                                                                |
#' |WS2M   |2m Wind Speed                               |meters per second       |5-minute averaged wind speed at 2m                                                                                                                                                                        |
#' |WSPD   |Wind Speed                                  |meters per second       |5-minute averaged wind speed at 10m.                                                                                                                                                                      |
#' |WSSD   |Wind Speed Standard Deviation               |meters per second       |Standard deviation of wind speed during the 5-minute interval.                                                                                                                                            |
#' |WVEC   |Wind Vector                                 |meters per second       |5-minute averaged wind velocity (speed and direction accounted for) at 10m.                                                                                                                               |
#'
mnet_read_mts <- function(file_name){

  expected_cols <- data.frame(
    RELH = NA_real_, TAIR = NA_real_, WSPD = NA_real_,
    WVEC = NA_real_, WDIR = NA_real_, WDSD = NA_real_,
    WSSD = NA_real_, WMAX = NA_real_, RAIN = NA_real_,
    PRES = NA_real_, SRAD = NA_real_, TA9M = NA_real_,
    WS2M = NA_real_, TS10 = NA_real_, TB10 = NA_real_,
    TS05 = NA_real_, TS25 = NA_real_, TS60 = NA_real_,
    TR05 = NA_real_, TR25 = NA_real_, TR60 = NA_real_,
    TR75 = NA_real_,
    TB05 = NA_real_, TS30 = NA_real_, TS45 = NA_real_,
    VW05 = NA_real_, VW25 = NA_real_, VW45 = NA_real_)  |>
    subset(!is.na(RELH))

  file_raw <- readLines(file_name)

  base_time <-
    file_raw |>
    getElement(2) |>
    gsub("(^ +[^ ]+ +)|( +00 +00 +00$)", "", x = _) |>
    gsub(" +", "-", x = _) |>
    paste0(' 00:00:00') |>
    as.POSIXct(tz='UTC')

  col_names <-
    file_raw |>
    getElement(3) |>
    strsplit(split = " +") |>
    unlist() |>
    grep("^$", x = _, value = TRUE, invert = TRUE)

  col_classes <-
    col_names |>
    length() |>
    rep("numeric", times = _) |>
    setNames(nm = col_names)

  col_classes[col_names == "STID"] <- "character"
  col_classes[col_names == "STNM"] <- "integer"

  mts_data <-
    file_raw |>
    tail(-3) |>
    read.table(text = _,
               col.names = col_names,
               colClasses = col_classes,
               header = FALSE) |>
    merge(expected_cols, all = TRUE) |>
    sort_by(~TIME) |>
    set_missing() |>
    set_mts_units() |>
    within({
      DATE = base_time + as.numeric(units::set_units(TIME, "seconds"))
      PRES = units::set_units(PRES, "kPa")
      TDEW = calc_tdew(TAIR, RELH)
      VDEF = (units::set_units(100, "percent") - RELH)*sat_vap_pres(TAIR)
    })

  return(mts_data)
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
