# To handle "no visible binding for global variable" NOTEs during checking:
utils::globalVariables("stid")

#' Download Mesonet site and soil information for all Mesonet sites
#'
#' Downloads a table of Mesonet site and soil information from the Oklahoma
#'  Mesonet website.
#'
#' @export
#'
#' @inheritParams mnet_requisition_list
#'
#' @param url the url to the page on the Oklahoma Mesonet website where site
#'   and soil information are stored
#'
#' @param clear_cache whether to clear cached copy of site and soil
#'  information and re-download
#'
#' @return a data frame containing site and soil information. See Details for
#'   definition of variable descriptions.
#'
#' @md
#'
#' @details
#'
#' Variable descriptions for Mesonet site and soil information.
#'
#' |**Variable** |**Description**                            |
#' |:--------|:--------------------------------------|
#' |stnm     |Station Number                         |
#' |stid     |Station Identifier                     |
#' |name     |Station Name                           |
#' |city     |Nearest Incorporated Town              |
#' |rang     |Range From Town To Station             |
#' |cdir     |Compass Direction From Town To Station |
#' |cnty     |County                                 |
#' |nlat     |North Latitude                         |
#' |elon     |East Longitude                         |
#' |elev     |Elevation In Meters                    |
#' |cdiv     |Oklahoma Climate Division              |
#' |clas     |Station Class                          |
#' |wcr05    |5 cm Residual Water Content (cm3/cm3)  |
#' |wcs05    |5 cm Saturated Water Content (cm3/cm3) |
#' |a05      |5 cm Alpha Constant (1/kPa)            |
#' |n05      |5 cm N Constant (dimensionless)        |
#' |bulk5    |5 cm Soil Bulk Density (g/cm3)         |
#' |grav5    |5 cm Soil Percentage Gravel            |
#' |sand5    |5 cm Soil Percentage Sand              |
#' |silt5    |5 cm Soil Percentage Silt              |
#' |clay5    |5 cm Soil Percentage Clay              |
#' |text5    |5 cm Soil Texture Class                |
#' |wcr25    |25 cm Residual Water Content (cm3/cm3) |
#' |wcs25    |25 cm Saturated Water Content (cm3/cm3)|
#' |a25      |25 cm Alpha Constant                   |
#' |n25      |25 cm N Constant                       |
#' |bulk25   |25 cm Soil Bulk Density                |
#' |grav25   |25 cm Soil Percentage Gravel           |
#' |sand25   |25 cm Soil Percentage Sand             |
#' |silt25   |25 cm Soil Percentage Silt             |
#' |clay25   |25 cm Soil Percentage Clay             |
#' |text25   |25 cm Soil Texture Class               |
#' |wcr60    |60 cm Residual Water Content (cm3/cm3) |
#' |wcs60    |60 cm Saturated Water Content (cm3/cm3)|
#' |a60      |60 cm Alpha Constant                   |
#' |n60      |60 cm N Constant                       |
#' |bulk60   |60 cm Soil Bulk Density                |
#' |grab60   |60 cm Soil Percentage Gravel           |
#' |sand60   |60 cm Soil Percentage Sand             |
#' |silt60   |60 cm Soil Percentage Silt             |
#' |clay60   |60 cm Soil Percentage Clay             |
#' |text60   |60 cm Soil Texture Class               |
#' |wcr75    |75 cm Residual Water Content (cm3/cm3) |
#' |wcs75    |75 cm Saturated Water Content (cm3/cm3)|
#' |a75      |75 cm Alpha Constant                   |
#' |n75      |75 cm N Constant                       |
#' |bulk75   |75 cm Soil Bulk Density                |
#' |grav75   |75 cm Soil Percentage Gravel           |
#' |sand75   |75 cm Soil Percentage Sand             |
#' |silt75   |75 cm Soil Percentage Silt             |
#' |clay75   |75 cm Soil Percentage Clay             |
#' |text75   |75 cm Soil Texture Class               |
#' |datc     |Date Commissioned                      |
#' |datd     |Date De-Commissioned                   |
#'
#' @examples
#'
#' \dontshow{
#'   mesonet_cache_dir <- mnet_test_cache(site_info = TRUE)
#'   previous_options <- options(.mesonet_cache = mesonet_cache_dir)
#' }
#'
#' mnet_site_info()
#'
#' \dontshow{
#'   unlink(mesonet_cache_dir, recursive = TRUE)
#'   options(previous_options)
#' }
#'
mnet_site_info <- function(url = "https://api.mesonet.org/index.php/export/station_location_soil_information",
                           file_cache = NULL,
                           clear_cache = FALSE){

  mesonet_cache <- local_mesonet_cache(file_cache, ask = FALSE)

  csv_file_name <- mesonet_cache |>
    file.path("station_location_soil_information.csv")

  rds_file_name <-
    csv_file_name |>
    gsub("csv$", "rds", x = _)

  if(clear_cache){
    for(.f in c(csv_file_name, rds_file_name)){
      if(file.exists(.f)) unlink(.f)
    }
  }

  if(file.exists(rds_file_name)){

    sta_info <- readRDS(rds_file_name)

  }else{

    if(!file.exists(csv_file_name)){
      utils::download.file(url, csv_file_name, quiet = TRUE)
    }

    sta_info <- utils::read.csv(csv_file_name, stringsAsFactors = FALSE)

    colnames(sta_info) <-
      sta_info |>
      colnames() |>
      tolower()

    for(v in colnames(sta_info)){
      if(is.numeric(sta_info[[v]])){
        sta_info[[v]][sta_info[[v]] == -999] <-
          methods::as(NA, class(sta_info[[v]]))
      }else if(is.character(sta_info[[v]])){
        sta_info[[v]][sta_info[[v]] == "-999"] <-
          methods::as(NA, class(sta_info[[v]]))
      }
    }

    sta_info <-
      sta_info |>
      set_mts_units() |>
      within({
        datc = make_date(datc)
        datc = ifelse(stid == "WEB3",
                      as.POSIXct("2021-06-16", tz = "UTC"),
                      datc)
        datc = as.POSIXct(datc, tz = "UTC")
        datd = make_date(datd)
      })

    saveRDS(sta_info, rds_file_name)

  }

  return(sta_info)

}
