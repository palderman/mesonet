#' Download Mesonet Time Series files
#'
#' Downloads Mesonet Time Series (MTS) files from the Oklahoma Mesonet for
#'  a given set of station identifiers and date range
#'
#' @export
#'
#' @inheritParams mnet_requisition_list
#'
# @param delay a delay in milliseconds specifying the time interval between
#  file downloads so as to not overwhelm the Oklahoma Mesonet data server
#'
#' @param root_url the root url from which to download MTS files (see
#'  \link[mesonet]{mnet_root_url})
#'
#' @param silent whether to suppress output to terminal (TRUE) or allow output
#'  to terminal (FALSE)
#'
mnet_download_mts <- function(stid,
                              start_date = NULL,
                              end_date = NULL,
                              root_url = mnet_root_url(),
                              site_info = NULL,
                              file_cache = NULL,
                              ask = !silent,
                              silent = FALSE){

  delay <- 500

  file_urls <-
    mnet_requisition_list(stid = stid,
                          start_date = start_date,
                          end_date = end_date,
                          site_info = site_info,
                          file_cache = file_cache) |>
    within({
      url = paste0(root_url, "/", mts_rel_path)
    })

  file_urls |>
    with({
      dirname(mts_path)
    }) |>
    unique() |>
    lapply(dir.create, recursive = TRUE, showWarnings = FALSE)

  file_urls |>
    with({
      if(!silent){
        cat("Downloading files...\n")
        pb <- txtProgressBar()
      }
      for(.i in seq_along(url)){
        if(!file.exists(mts_path[.i])){
          utils::download.file(url[.i], mts_path[.i], quiet = TRUE)
          Sys.sleep(delay/1000)
        }
        if(!silent) setTxtProgressBar(pb, .i/length(url))
      }
      if(!silent) close(pb)
    })

  file_urls |>
    with({
      mts_path[file.exists(mts_path)]
    }) |>
    invisible()
}
