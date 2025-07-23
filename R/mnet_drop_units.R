#' Drop units from all columns of Oklahoma Mesonet data frame
#'
#' @param df a data frame with units columns
#'
#' @return A data frame with identical data, but without units
#'
#' @examples
#'
#' mesonet_data <- data.frame(
#'     DATE = as.POSIXct(757382400, tz = "UTC")) |>
#'   within({
#'   RELH = units::set_units(31, "percent")
#'   TAIR = units::set_units(NA_real_, "Celsius")
#'   WSPD = units::set_units(4.6, "m/s")
#'   WVEC = units::set_units(4.5, "m/s")
#'   WDIR = units::set_units(182, "degrees")
#'   RAIN = units::set_units(0, "mm")
#'   PRES = units::set_units(97.939, "kPa")
#'   SRAD = units::set_units(0, "W/m^2")
#'   TA9M = units::set_units(14.1, "Celsius")
#'   WS2M = units::set_units(4, "m/s")
#'   STID = "ACME"
#'  })
#'
#' mnet_drop_units(mesonet_data)
#'
#' @export
#'
mnet_drop_units <- function(df){
  for(i in seq_along(df)){
   if("units" %in% class(df[[i]])){
      df[[i]] <- units::drop_units(df[[i]])
    }
  }
  return(df)
}
