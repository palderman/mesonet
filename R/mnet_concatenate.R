#' Concatenate subdaily Oklahoma Mesonet records for multiple dates or stations.
#'
#' @export
#'
#' @md
#'
#' @inheritParams mnet_requisition_list
#'
#' @return A data frame with Oklahoma Mesonet data. See below for a list of
#'  variables including column ID, name, unit and description:
#'
#' |**ID** |**Name**                                    |**Unit**                |**Description**                                                                                                                                                                                                        |
#' |:------|:-------------------------------------------|:-----------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
#' |PRES   |Station Atmospheric Pressure                |kPa                     |5-minute averaged atmospheric pressure.                                                                                                                                                                                |
#' |RAIN   |Precipitation                               |millimeters             |Liquid precipitation accumulation since previous 5-min record. Frozen precipitation cannot be recorded until it melts; therefore, precipitation from snow may not be recorded until several days after the snow event. |
#' |RELH   |Relative Humidity                           |percent                 |5-minute averaged relative humidity at 1.5m.                                                                                                                                                                           |
#' |SRAD   |Solar Radiation                             |watts per square meter  |5-minute averaged downwelling global solar radiation.                                                                                                                                                                  |
#' |STID   |Station ID                                  |                        |Station ID                                                                                                                                                                                                             |
#' |STNM   |Station Number                              |                        |Station Number                                                                                                                                                                                                         |
#' |TA9M   |Air Temperature at 9m                       |degrees Celsius         |5-minute averaged air temperature at 9m                                                                                                                                                                                |
#' |TAIR   |Air Temperature                             |degrees Celsius         |5-minute averaged air temperature at 1.5m.                                                                                                                                                                             |
#' |TB05   |Temperature Under Bare Soil at 5cm          |degrees Celsius         |15-minute averaged temperature under bare soil at 5cm. This variable is only available prior to December 1, 2013.                                                                                                      |
#' |TB10   |Temperature Under Bare Soil at 10cm         |degrees Celsius         |15-minute averaged temperature under bare soil at 10cm.                                                                                                                                                                |
#' |TIME   |Time                                        |minutes after base time |Minutes after base time (typically 0000 UTC)                                                                                                                                                                           |
#' |TR05   |Soil Moisture Calibrated Delta-T at 5cm     |degrees Celsius         |30-minute calibrated change in temperature of soil over time after a heat pulse is introduced. Used to calculate soil water potential, fractional water index, or volumetric water.                                    |
#' |TR25   |Soil Moisture Calibrated Delta-T at 25cm    |degrees Celsius         |30-minute calibrated change in temperature of soil over time after a heat pulse is introduced. Used to calculate soil water potential, fractional water index, or volumetric water.                                    |
#' |TR60   |Soil Moisture Calibrated Delta-T at 60cm    |degrees Celsius         |30-minute calibrated change in temperature of soil over time after a heat pulse is introduced. Used to calculate soil water potential, fractional water index, or volumetric water.                                    |
#' |TR75   |Soil Moisture Calibrated Delta-T at 75cm    |degrees Celsius         |30-minute calibrated change in temperature of soil over time after a heat pulse is introduced. Used to calculate soil water potential, fractional water index, or volumetric water.                                    |
#' |TS05   |Temperature Under Native Vegetation at 5cm  |degrees Celsius         |15-minute averaged temperature under native vegetation at 5cm.                                                                                                                                                         |
#' |TS10   |Temperature Under Native Vegetation at 10cm |degrees Celsius         |15-minute averaged temperature under native vegetation at 10cm.                                                                                                                                                        |
#' |TS25   |Temperature Under Native Vegetation at 25cm |degrees Celsius         |15-minute averaged temperature under native vegetation at 25cm. This variable is only available after and including December 1, 2013.                                                                                  |
#' |TS30   |Temperature Under Native Vegetation at 30cm |degrees Celsius         |15-minute averaged temperature under native vegetation at 30cm. This variable is only available prior to December 1, 2013.                                                                                             |
#' |TS45   |Temperature Under Native Vegetation at 45cm |degrees Celsius         |15-minute averaged temperature under native vegetation at 45cm.                                                                                                                                                        |
#' |TS60   |Temperature Under Native Vegetation at 60cm |degrees Celsius         |15-minute averaged temperature under native vegetation at 60cm. This variable is only available after and including December 1, 2013.                                                                                  |
#' |VW05   |Volumetric soil water Under Native Vegetation at 5cm  |cm^3^ cm^-3^  |5-minute averaged volumetric soil water under native vegetation at 5cm.                                                                                                                                                |
#' |VW25   |Volumetric soil water Under Native Vegetation at 25cm |cm^3^ cm^-3^  |5-minute averaged volumetric soil water under native vegetation at 25cm.                                                                                                                                               |
#' |VW45   |Volumetric soil water Under Native Vegetation at 45cm |cm^3^ cm^-3^  |5-minute averaged volumetric soil water under native vegetation at 45cm.                                                                                                                                               |
#' |WDIR   |Wind Direction                              |degrees                 |5-minute averaged wind direction at 10m.                                                                                                                                                                               |
#' |WDSD   |Wind Direction Standard Deviation           |degrees                 |Standard deviation of wind direction during the 5-minute interval.                                                                                                                                                     |
#' |WMAX   |Maximum Wind Speed                          |meters per second       |Highest 3-second wind speed at 10m sample.                                                                                                                                                                             |
#' |WS2M   |2m Wind Speed                               |meters per second       |5-minute averaged wind speed at 2m                                                                                                                                                                                     |
#' |WSPD   |Wind Speed                                  |meters per second       |5-minute averaged wind speed at 10m.                                                                                                                                                                                   |
#' |WSSD   |Wind Speed Standard Deviation               |meters per second       |Standard deviation of wind speed during the 5-minute interval.                                                                                                                                                         |
#' |WVEC   |Wind Vector                                 |meters per second       |5-minute averaged wind velocity (speed and direction accounted for) at 10m.                                                                                                                                            |
#'
mnet_concatenate <- function(stid = NULL,
                             start_date = NULL,
                             end_date = NULL,
                             site_info = NULL,
                             file_cache = NULL){

  rds_df <-
    mnet_requisition_list(stid = stid,
                          start_date = start_date,
                          end_date = end_date,
                          site_info = site_info,
                          file_cache = file_cache) |>
    with({
      mts_to_rds_list(mts_path)
    }) |>
    lapply(readRDS) |>
    fast_rbind() |>
    sort_df(c("STID", "DATE"))

    row.names(rds_df) <- 1:nrow(rds_df)

    rds_df$RAIN <- calc_rain(rds_df)

  return(rds_df)

}

mts_to_rds_list <- function(mts_list){
  # Replace mts with rds in file path list
  rds_list <-
    mts_list |>
    gsub("((?<= |/|\\\\)mts(?= |/|\\\\))|(mts$)", "rds", x = _, perl = TRUE)

  return(rds_list)

}

calc_rain <- function(rds_df, method = 2){

  # Assumes that data are sorted by STID and DATE

  rain <- vector(mode = "numeric", length = nrow(rds_df))

  if(method == 1){
    Date <- as.Date(rds_df$DATE + as.difftime(-1, units = "secs"))
    for(stid in unique(rds_df$STID)){
      for(date in unique(Date)){
        rain[rds_df$STID == stid & Date == as.Date(date)] <-
          rds_df$RAIN[rds_df$STID == stid & Date == as.Date(date)] |>
          c(0, .x = _) |>
          diff()
      }
    }
  }else if(method == 2){
    for(stid in unique(rds_df$STID)){
      stid_index <- with(rds_df, STID == stid)
      r_index <- with(rds_df, order(DATE[stid_index]))
      rain[stid_index][r_index] <-
        with(rds_df,{
          rds_df$RAIN[stid_index][r_index] |>
            c(rep(NA_real_, 24*60/5 - 1),
              x = _,
              NA_real_) |>
            matrix(nrow = 24*60/5) |>
            apply(2, \(.x) diff(c(0,.x))) |>
            as.vector() |>
            tail(-(24*60/5 - 1)) |>
            head(-1)
        })
    }
    if("STID" %in% colnames(rds_df)){
    }else{
      r_index <- with(rds_df, order(DATE))
    }
  }
  if("units" %in% class(rds_df$RAIN)){
    units(rain) <- units(rds_df$RAIN)
  }
  rain
}



