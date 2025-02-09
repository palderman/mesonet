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
    create_cache_dir <- utils::askYesNo(msg)
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
