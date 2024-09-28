#' Download Mesonet Time Series files
#'
#' Downloads Mesonet Time Series (MTS) files from the Oklahoma Mesonet for
#'  a given set of station identifiers and date range
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
#' @param delay a delay in milliseconds specifying the time interval between
#'  file downloads so as to not overwhelm the Oklahoma Mesonet data server
#'
#' @param root_url the root url from which to download MTS files (see
#'  \link[mesonet]{mnet_root_url})
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
#' @param silent whether to suppress output to terminal (TRUE) or allow output
#'  to terminal (FALSE)
#'
mnet_download_mts <- function(stid, start_date, end_date,
                              delay = 1000,
                              root_url = mnet_root_url(),
                              site_info = NULL,
                              file_cache = NULL,
                              ask = !silent,
                              silent = FALSE){

  if(length(start_date) != length(stid) & length(start_date) != 1){
    stop("`start_date` must be of length one or match the length of `stid`.")
  }else{
    start <-
      start_date |>
      as.POSIXct(tz = "America/Costa_Rica") |>
      trunc("days") |>
      as.POSIXct(tz = "UTC") |>
      trunc("days")
  }

  if(length(end_date) != length(stid) & length(end_date) != 1){
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

  mesonet_cache <- local_mesonet_cache(file_cache, ask = ask)

  file_urls <-
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
      date_url = format(date, "%Y/%m/%d")
      date_prefix = format(date, "%Y%m%d")
      url = paste0(root_url, "/mts/", date_url, "/",
                   date_prefix, tolower(stid), ".mts")
      local_path = paste0(mesonet_cache, "/mts/", date_url, "/",
                          date_prefix, tolower(stid), ".mts")
    })

  file_urls |>
    with({
      dirname(local_path)
    }) |>
    unique() |>
    lapply(dir.create, recursive = TRUE, showWarnings = FALSE)

  file_urls |>
    with({
      if(!silent){
        cat("Downloading files...\n")
        pb <- txtProgressBar()
      }
      for(i in seq_along(url)){
        if(!file.exists(local_path[i])){
          download.file(url[i], local_path[i], quiet = TRUE)
          Sys.sleep(delay/1000)
        }
        if(!silent) setTxtProgressBar(pb, i/length(url))
      }
      if(!silent) close(pb)
    })

  file_urls |>
    with({
      local_path[file.exists(local_path)]
    }) |>
    invisible()
}

local_mesonet_cache <- function(mesonet_cache_dir = NULL,
                             ask = TRUE){

  if(is.null(mesonet_cache_dir)){
    mesonet_cache_dir <-
      path.expand("~") |>
      file.path(".mesonet_cache")
  }

  if(dir.exists(mesonet_cache_dir)){
    return(mesonet_cache_dir)
  }else if(ask){
    msg <- c(
      "Do you want to create a local mesonet cache directory at: \n  ",
      mesonet_cache_dir
    ) |>
      paste0(collapse = "")
    create_cache_dir <- askYesNo(msg)
    if(!is.na(create_cache_dir) & create_cache_dir){
      dir.create(mesonet_cache_dir, recursive = TRUE, showWarnings = FALSE)
    }else{
      mesonet_cache_dir <- NULL
    }
  }else{
    mesonet_cache_dir <- NULL
  }

  if(is.null(mesonet_cache_dir)){
    mesonet_cache_dir <-
      tempdir() |>
      file.path(".mesonet_cache")
    dir.create(mesonet_cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  mesonet_cache_dir

}
