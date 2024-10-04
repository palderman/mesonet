#' Concatenate subdaily Oklahoma Mesonet records
#'
#' Concatenate subdaily Oklahoma Mesonet records for multiple dates or stations.
#' Rainfall
#'
#' @export
#'
#' @inheritParams mnet_requisition_list
#'
#' @param mc.cores number of cores to use for reading/processing data
#'
mnet_subdaily_c <- function(stid = NULL,
                            start_date = NULL,
                            end_date = NULL,
                            site_info = NULL,
                            file_cache = NULL,
                            mc.cores = 1){

  mclapply(opt$INPUTFILES,readRDS,mc.cores=20) %>%
    rbindlist(.,fill=TRUE) %>%
    arrange(.,STID,TIME) %>%
    mutate(tempTIME = floor_date(TIME - minutes(1),'day') %>%
             as.POSIXct()) %>%
    group_by(STID, tempTIME) %>%
    # Convert raw RAIN value (daily cumulative sum of rainfall) to 5-min RAIN value
    mutate(RAIN = c(0, RAINraw) %>% diff()) %>%
    ungroup() %>%
    select(-tempTIME,-RAINraw) %>%
    saveRDS(.,opt$OUTPUTFILE)

}

gen_local_rds_list <- function(mts_root){

  # Create regular expression for replacing mts with rds
  sep_regex <- .Platform$file.sep |>
    gsub(r"(\\)", r"(\\\)", x = _) |>
    paste0("((", ., ")mts(",., "))|(mts$)")

  # Replace mts with rds in file path list
  rds_list <- mts_list |>
    gsub(mts_regex, "\\2rds\\3", .)

  return(rds_list)

}
