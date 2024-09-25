#' Download Mesonet site and soil information for all Mesonet sites
#'
#' Downloads a table of Mesonet site and soil information from the Oklahoma
#'  Mesonet website.
#'
#' @export
#'
#' @param url the url to the page on the Oklahoma Mesonet website where site
#'   and soil information are stored
#'
#' @return a data frame containing site and soil information. See \link{Details} for
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
#' |wcr05    |5 cm Residual Water Content            |
#' |wcs05    |5 cm Saturated Water Content           |
#' |a05      |5 cm Alpha Constant                    |
#' |n05      |5 cm N Constant                        |
#' |bulk5    |5 cm Soil Bulk Density                 |
#' |grav5    |5 cm Soil Percentage Gravel            |
#' |sand5    |5 cm Soil Percentage Sand              |
#' |silt5    |5 cm Soil Percentage Silt              |
#' |clay5    |5 cm Soil Percentage Clay              |
#' |text5    |5 cm Soil Texture Class                |
#' |wcr25    |25 cm Residual Water Content           |
#' |wcs25    |25 cm Saturated Water Content          |
#' |a25      |25 cm Alpha Constant                   |
#' |n25      |25 cm N Constant                       |
#' |bulk25   |25 cm Soil Bulk Density                |
#' |grav25   |25 cm Soil Percentage Gravel           |
#' |sand25   |25 cm Soil Percentage Sand             |
#' |silt25   |25 cm Soil Percentage Silt             |
#' |clay25   |25 cm Soil Percentage Clay             |
#' |text25   |25 cm Soil Texture Class               |
#' |wcr60    |60 cm Residual Water Content           |
#' |wcs60    |60 cm Saturated Water Content          |
#' |a60      |60 cm Alpha Constant                   |
#' |n60      |60 cm N Constant                       |
#' |bulk60   |60 cm Soil Bulk Density                |
#' |grab60   |60 cm Soil Percentage Gravel           |
#' |sand60   |60 cm Soil Percentage Sand             |
#' |silt60   |60 cm Soil Percentage Silt             |
#' |clay60   |60 cm Soil Percentage Clay             |
#' |text60   |60 cm Soil Texture Class               |
#' |wcr75    |75 cm Residual Water Content           |
#' |wcs75    |75 cm Saturated Water Content          |
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
mnet_site_info <- function(url = "https://api.mesonet.org/index.php/export/station_location_soil_information"){

  file_name <- tempfile(fileext = "csv")

  download.file(url, file_name, quiet = TRUE)

  sta_info <- read.csv(file_name, stringsAsFactors = FALSE)

  colnames(sta_info) <-
    sta_info |>
    colnames() |>
    tolower()

  for(v in colnames(sta_info)){
    if(is.numeric(sta_info[[v]])){
      sta_info[[v]][sta_info[[v]] == -999] <- as(NA, class(sta_info[[v]]))
    }else if(is.character(sta_info[[v]])){
      sta_info[[v]][sta_info[[v]] == "-999"] <- as(NA, class(sta_info[[v]]))
    }
  }

  sta_info$datc <-
    sta_info$datc |>
    make_date()

  sta_info$datd <-
    sta_info$datd |>
    make_date()

  return(sta_info)

}

make_date <- function(date_chr){
  date_chr |>
    as.character() |>
    as.POSIXct(format = "%Y%m%d",
               tz = "UTC")
}
