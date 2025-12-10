#' Create example local mesonet file cache
#'
#' Create an example local mesonet file cache for running tests and examples
#'
#' @export
#'
#' @param file_cache an optional character string that provides a path to the
#'  directory to use for the local mesonet file cache. If missing, a new
#'  directory will be created within the temporary directory for the current
#'  session.
#'
#' @param site_info a logical value indicating whether to create an example
#'  version of the site_info.csv file in the local file cache
#'
#' @param mts_files a logical value indicating whether to create example
#'  versions of Mesonet Time Series (MTS) files in the local file cache
#'
#' @param rds_files a logical value indicating whether to create example
#'  versions of processed Mesonet data in the form of R data serialized (RDS)
#'  files in the local file cache
#'
#' @return Invisibly returns the full path to the test file cache
#'
#' @examples
#'
#' # Create test cache directory and write site info file
#' test_cache <- mnet_test_cache(site_info = TRUE)
#'
#' # Create test cache directory and write MTS and RDS files
#' test_cache <- mnet_test_cache(mts_files = TRUE,
#'                               rds_files = TRUE)
#'
#' # Remove test cache directory
#' unlink(test_cache, recursive = TRUE)
#'
mnet_test_cache <- function(file_cache,
                            site_info = FALSE,
                            mts_files = FALSE,
                            rds_files = FALSE){

  if(missing(file_cache)){
    file_cache <-
      tempdir() |>
      file.path(".mesonet_cache")
  }

  if(!dir.exists(file_cache)) dir.create(file_cache, recursive = TRUE)

  if(site_info){

    site_info_path <-
      file_cache |>
      file.path("site_info.csv")

    site_info_path |>
      dirname() |>
      dir.create(recursive = TRUE, showWarnings = FALSE)

    c("stnm,stid,name,city,rang,cdir,cnty,nlat,elon,elev,cdiv,clas,WCR05,WCS05,A05,N05,BULK5,GRAV5,SAND5,SILT5,CLAY5,TEXT5,WCR10,WCS10,A10,N10,BULK10,GRAV10,SAND10,SILT10,CLAY10,TEXT10,WCR25,WCS25,A25,N25,BULK25,GRAV25,SAND25,SILT25,CLAY25,TEXT25,WCR60,WCS60,A60,N60,BULK60,GRAV60,SAND60,SILT60,CLAY60,TEXT60,WCR75,WCS75,A75,N75,BULK75,GRAV75,SAND75,SILT75,CLAY75,TEXT75,datc,datd",
      "110,ACME,Acme,\"Rush Springs\",4,WNW,Grady,34.808330,-98.023250,397,Central,STANDARD,0.034,0.41,0.273,1.39,1.27,0.1,73,19.7,7.2,\"Sandy Loam\",0.037,0.41,0.253,1.38,1.29,0,69.9,20.8,9.4,,0.05,0.411,0.206,1.36,1.33,0.1,60.3,23.9,15.7,\"Sandy Loam\",0.068,0.361,0.192,1.34,1.6,0.1,52.5,25.9,21.5,\"Sandy Clay Loam\",0.07,0.347,0.178,1.32,1.68,0,53.2,24.1,22.8,\"Sandy Clay Loam\",19940101,20991231",
      "1,ADAX,Ada,Ada,2,NNE,Pontotoc,34.798510,-96.669090,295,\"South Central\",STANDARD,0.05,0.483,0.272,1.36,1.01,0,61.1,22,16.9,\"Sandy Loam\",0.055,0.445,0.216,1.35,1.21,0,58.6,22.9,18.6,,0.069,0.324,0.095,1.26,1.79,0,51.1,25.4,23.5,\"Sandy Clay Loam\",0.08,0.317,0.06,1.21,1.85,0,47.6,24.6,27.8,\"Sandy Clay Loam\",0.086,0.318,0.053,1.2,1.86,0,43.6,26.6,29.8,\"Clay Loam\",19940101,20991231",
      "2,ALTU,Altus,Altus,3,S,Jackson,34.587220,-99.338080,416,Southwest,STANDARD,0.089,0.384,0.078,1.29,1.57,0,23.8,40,36.2,\"Clay Loam\",0.092,0.387,0.076,1.28,1.57,0,22.3,41.3,36.4,,0.103,0.396,0.071,1.26,1.56,0,17.8,45.2,37,\"Silty Clay Loam\",0.118,0.373,0.073,1.17,1.69,0,14.3,40.9,44.8,\"Silty Clay\",0.118,0.346,0.059,1.16,1.73,0,15,40.9,44.1,\"Silty Clay\",19940101,20991231",
      "116,ALV2,Alva,Alva,7.2,SSW,Woods,36.708230,-98.709740,439,\"North Central\",STANDARD,0.086,0.365,0.037,1.34,1.66,0,33.3,31.4,35.3,\"Clay Loam\",0.089,0.372,0.044,1.31,1.64,0,30.3,31,38.7,,0.1,0.396,0.076,1.25,1.58,0,21.2,29.7,49.1,Clay,0.115,0.382,0.13,1.2,1.64,0,19.8,29.8,50.5,Clay,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,19981217,20991231",
      "3,ALVA,Alva,Alva,2,S,Woods,36.779700,-98.671700,450,\"North Central\",STANDARD,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,19940101,19981216",
      ""
    ) |>
    write(site_info_path)
  }

  if(mts_files | rds_files){
    # Construct reference dataset
    ref_df <- data.frame(
      DATE = seq.POSIXt(from = as.POSIXct("1994-01-01 00:00", tz = "UTC"),
                        to = as.POSIXct("1994-01-05 23:55", tz = "UTC"),
                        by = as.difftime(5, units = "mins"))
    ) |>
      within({
        RELH = units::set_units(31, "percent")
        TAIR = units::set_units(NA_real_, "Celsius")
        WSPD = units::set_units(4.6, "m/s")
        WVEC = units::set_units(4.5, "m/s")
        WDIR = 182
        WDSD = 12.6
        WSSD = units::set_units(1.3, "m/s")
        WMAX = units::set_units(8.7, "m/s")
        RAIN = units::set_units(c(288,1:287), "mm")
        PRES = units::set_units(97.939, "kPa")
        SRAD = units::set_units(0, "W/m^2")
        TA9M = units::set_units(14.1, "Celsius")
        WS2M = units::set_units(4, "m/s")
        TS10 = units::set_units(5.5, "Celsius")
        TB10 = units::set_units(7.9, "Celsius")
        TS05 = units::set_units(6.1, "Celsius")
        TB05 = units::set_units(8.7, "Celsius")
        TS30 = units::set_units(5.7, "Celsius")
        STID = "ACME"
        STNM = 89L
        TS25 = units::set_units(NA_real_, "Celsius")
        TS60 = units::set_units(NA_real_, "Celsius")
        TR05 = units::set_units(NA_real_, "Celsius")
        TR25 = units::set_units(NA_real_, "Celsius")
        TR60 = units::set_units(NA_real_, "Celsius")
        TR75 = units::set_units(NA_real_, "Celsius")
        TS45 = units::set_units(NA_real_, "Celsius")
        VW05 = units::set_units(NA_real_, "cm^3/cm^3")
        VW25 = units::set_units(NA_real_, "cm^3/cm^3")
        VW45 = units::set_units(NA_real_, "cm^3/cm^3")
        VDEF = units::set_units(NA_real_, "kPa")
        TDEW = units::set_units(NA_real_, "Celsius")
      })

    all_dates <-
      ref_df$DATE |>
      as.Date() |>
      unique()

    for(i in seq_along(all_dates)){

      mts_file_name <-
        all_dates[i] |>
        format("mts/%Y/%m/%d/%Y%m%dacme.mts") |>
        paste0(file_cache, "/", .x = _) |>
        gsub("/", .Platform$file.sep, x = _)

      if(mts_files){

        mts_file_name |>
          dirname() |>
          dir.create(recursive = TRUE, showWarnings = FALSE)

        ref_df |>
          with({
            ref_df[as.Date(DATE) == all_dates[i],]
          }) |>
          with({
            paste0(
              sprintf("%5s", STID),
              sprintf("%6i", STNM),
              sprintf("%6i", (as.numeric(DATE) %% (24*60*60))/60),
              sprintf("%7.0f", RELH),
              sprintf("%7.1f", TAIR),
              sprintf("%7.1f", WSPD),
              sprintf("%7.1f", WVEC),
              sprintf("%6.0f", WDIR),
              sprintf("%7.1f", WDSD),
              sprintf("%7.1f", WSSD),
              sprintf("%7.1f", WMAX),
              sprintf("%8.2f", RAIN),
              sprintf("%9.2f", PRES),
              sprintf("%6.0f", SRAD),
              sprintf("%7.1f", TA9M),
              sprintf("%7.1f", WS2M),
              sprintf("%7.1f", TS10),
              sprintf("%7.1f", TB10),
              sprintf("%7.1f", TS05),
              sprintf("%7.1f", TB05),
              sprintf("%7.1f", TS30)
            )
          }) |>
          gsub("  NA", "-999", x = _) |>
          c("  101 ! <Copyright statement goes here>",
            format(all_dates[i], "  18 %Y %m %d 00 00 00"),
            " STID  STNM  TIME   RELH   TAIR   WSPD   WVEC  WDIR   WDSD   WSSD   WMAX    RAIN     PRES  SRAD   TA9M   WS2M   TS10   TB10   TS05   TB05   TS30",
            x = _) |>
          write(mts_file_name)
      }

      if(rds_files){

        rds_file_name <-
          mts_file_name |>
          mts_to_rds_list()

        rds_file_name |>
          dirname() |>
          dir.create(recursive = TRUE, showWarnings = FALSE)

        ref_df |>
          with({
            ref_df[as.Date(DATE) == all_dates[i],]
          }) |>
          saveRDS(rds_file_name)
      }
    }
  }

  invisible(file_cache)
}
