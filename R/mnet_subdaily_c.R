#' Concatenate subdaily Oklahoma Mesonet records
#'
#' Concatenate subdaily Oklahoma Mesonet records for multiple dates or stations.
#' Rainfall
#'
#' @export
#'
#' @param file_cache a character string providing a path to the local Mesonet
#'  file cache. If NULL, the function will search for the local file cache and
#'  if not found will prompt the user to create one.
#'
mnet_subdaily_c <- function(file_cache, mc.cores = 1){
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
