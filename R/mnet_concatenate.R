#' Concatenate subdaily Oklahoma Mesonet records for multiple dates or stations.
#'
#' @export
#'
#' @inheritParams mnet_requisition_list
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
    do.call(rbind.data.frame, args = _) |>
    (\(.x) with(.x, .x[order(STID, TIME),]))()

  row.names(rds_df) <- 1:nrow(rds_df)

    DATE <- as.Date(rds_df$TIME - 1)

    for(stid in unique(rds_df$STID)){
      for(date in unique(DATE)){
        rds_df$RAIN[rds_df$STID == stid & DATE == as.Date(date)] <-
          rds_df$RAIN[rds_df$STID == stid & DATE == as.Date(date)] |>
            c(0, .x = _) |>
            diff()
      }
    }

  return(rds_df)

}

mts_to_rds_list <- function(mts_list){
  # Replace mts with rds in file path list
  rds_list <-
    mts_list |>
    gsub("((?<= |/|\\\\)mts(?= |/|\\\\))|(mts$)", "rds", x = _, perl = TRUE)

  return(rds_list)

}
