# To handle "no visible binding for global variable" NOTEs during checking:
utils::globalVariables(c("datc", "datd"))

#' Calculate a requisition list for Oklahoma Mesonet MTS files
#'
#' Calculate a requisition list of Mesonet Time Series (MTS) files from the
#'   Oklahoma Mesonet for given stations and dates
#'
#' @export
#'
#' @param stid a character vector of four-digit station identifiers
#'  for the Mesonet stations from which to download data
#'
#' @param start_date the first date for which to download data specified in
#'  Central Standard Time (i.e. "America/Costa_Rica") zone as a Date or POSIXt
#'  object or a string with the format YYYY-MM-DD, where YYYY is the four-digit
#'  year, MM is the two-digit numeric month and DD is the two-digit day of month
#'
#' @param end_date the final date for which to download data specified in
#'  Central Standard Time (i.e. "America/Costa_Rica") zone as a Date or POSIXt
#'  object or a string with the format YYYY-MM-DD, where YYYY is the four-digit
#'  year, MM is the two-digit numeric month and DD is the two-digit day of month
#'
#' @param site_info a data frame with site and soil information for each
#'  Oklahoma Mesonet station as returned by \link[mesonet]{mnet_site_info}. If
#'  NULL, \link[mesonet]{mnet_site_info} will be used to download this
#'  information internally
#'
#' @param file_cache a character string providing a path to the local Mesonet
#'  file cache. If NULL, the function will search for the local file cache and
#'  if not found will prompt the user to create one.
#'
#' @param ask whether or not to ask about creating a local Mesonet file cache
#'
mnet_requisition_list <- function(stid = NULL,
                                  start_date = NULL,
                                  end_date = NULL,
                                  site_info = NULL,
                                  file_cache = NULL,
                                  ask = TRUE){

  mesonet_cache <- local_mesonet_cache(file_cache, ask = ask)

  if(is.null(stid)){
    stid <- mesonet_cache$stid
  }

  if(is.null(start_date)){
    start_date <- as.POSIXct("1994-01-01", tz = "UTC")
  }else if(length(start_date) != length(stid) & length(start_date) != 1){
    stop("`start_date` must be of length one or match the length of `stid`.")
  }else{
    start <-
      start_date |>
      as.POSIXct(tz = "America/Costa_Rica") |>
      trunc("days") |>
      as.POSIXct(tz = "UTC") |>
      trunc("days")
  }

  if(is.null(end_date)){
    end_date <-
      Sys.time() |>
      as.POSIXct(tz = "UTC") |>
      trunc("days")
  }else if(length(end_date) != length(stid) & length(end_date) != 1){
    stop("`end_date` must be of length one or match the length of `stid`.")
  }else{
    end <-
      end_date |>
      as.POSIXct(tz = "America/Costa_Rica") |>
      trunc("days") |>
      as.POSIXct(tz = "UTC") |>
      trunc("days") |>
      pmin(Sys.time()) |>
      trunc("days")
  }

  if(is.null(site_info)){
    site_info <- mnet_site_info() |>
      with({
        data.frame(stid = stid,
                   datc = datc,
                   datd = datd)
      })
  }

  mts_files <-
    data.frame(stid = stid,
               start = start,
               end = end) |>
    # Construct list of dates
    within({
      date_list = mapply(\(.start, .end) seq.POSIXt(.start, .end, by = "days"),
                         .start = start, .end = end,
                         SIMPLIFY = FALSE)
      stid = mapply(\(.stid, .dates) rep(.stid, times = length(.dates)),
                    .stid = stid, .dates = date_list,
                    SIMPLIFY = FALSE)
    }) |>
    # unnest stid and dates
    with({
      data.frame(stid = unlist(stid),
                 date = unlist(date_list))
    }) |>
    merge(site_info) |>
    subset(date >= datc & date <= datd,
           select = c("stid", "date")) |>
    within({
      date = as.POSIXct(date, tz = "UTC")
      date_path = format(date, "%Y/%m/%d")
      date_prefix = format(date, "%Y%m%d")
      mts_rel_path = paste0("mts/", date_path, "/",
                            date_prefix, tolower(stid), ".mts")
      mts_path = paste0(mesonet_cache, "/", mts_rel_path)
    })

  return(mts_files)
}
