#' Retrieve Oklahoma Mesonet subdaily data
#'
#' Retrieves data from the Oklahoma Mesonet for
#'  a given set of station identifiers and date range
#'
#' @export
#'
#' @inheritParams mnet_download_mts
#'
#' @inherit mnet_concatenate
#'
mnet_retrieve <- function(stid,
                          start_date = NULL,
                          end_date = NULL,
                          delay = 1000,
                          root_url = mnet_root_url(),
                          site_info = NULL,
                          file_cache = NULL,
                          ask = !silent,
                          silent = FALSE){

}
