# To handle "no visible binding for global variable" NOTEs during checking:
utils::globalVariables(c("mts_path", "mts_rel_path"))

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
#' @examples
#'
#' \dontshow{
#'   mesonet_cache_dir <- mnet_test_cache(rds_files = TRUE)
#'   previous_options <- options(.mesonet_cache = mesonet_cache_dir)
#' }
#'
#' mnet_retrieve(stid = "ACME",
#'                  start_date = "1994-01-02",
#'                  end_date = "1994-01-03")
#'
#' \dontshow{
#'   unlink(mesonet_cache_dir, recursive = TRUE)
#'   options(previous_options)
#' }
#'
mnet_retrieve <- function(stid,
                          start_date = NULL,
                          end_date = NULL,
                          root_url = mnet_root_url(),
                          site_info = NULL,
                          file_cache = NULL,
                          ask = !silent,
                          silent = FALSE){

  mts_rds_df <-
    mnet_requisition_list(stid = stid,
                          start_date = start_date,
                          end_date = end_date,
                          site_info = site_info,
                          file_cache = file_cache,
                          ask = ask) |>
    within({
      rds_path = mts_to_rds_list(mts_path)
      rds_rel_path = mts_to_rds_list(mts_rel_path)
    })

  # Download any missing MTS files
  if(any(!file.exists(mts_rds_df$mts_path))){
    mnet_download_mts(stid = stid,
                      start_date = start_date,
                      end_date = end_date,
                      root_url = root_url,
                      site_info = site_info,
                      file_cache = file_cache,
                      ask = ask,
                      silent = silent)
  }

  # Convert MTS files to rds format
  mts_rds_df |>
    within({
      process_mts_to_rds = file.mtime(mts_path) > file.mtime(rds_path)
      process_mts_to_rds[is.na(process_mts_to_rds)] = TRUE
    }) |>
    with({
      mts_rds_df[process_mts_to_rds, ]
    }) |>
    within({
      mts_contents = lapply(mts_path, mnet_read_mts)
    }) |>
    with({
      mapply(\(.contents, .file_name){
        dir.create(dirname(.file_name),
                   recursive = TRUE,
                   showWarnings = FALSE)
        saveRDS(.contents, .file_name)
      },
      .contents = mts_contents,
      .file_name = rds_path)
    })

  # Concetenate daily observations
  combined_data <-
    mnet_concatenate(stid = stid,
                     start_date = start_date,
                     end_date = end_date,
                     site_info = site_info,
                     file_cache = file_cache)

  return(combined_data)
}
