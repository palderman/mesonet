#' Calculate soil volumteric water content for Oklahoma Mesonet data
#'
#' Calculate soil volumetric water content from delta-T soil temperature change data
#'  from the Oklahoma Mesonet using \link[mesonet]{mnet_calc_mp} to calculate
#'  matric potential and \link[mesonet]{mnet_van_genuchten} to calculate the
#'  corresponding volumetric water content using parameters provided by
#'  \link[mesonet]{mnet_site_info}.
#'
#' @export
#'
#' @inheritParams mnet_download_mts
#'
#' @param data a data frame that contains a column of Mesonet station
#'  identifier codes (i.e. STID) and columns for delta-T temperature change
#'  data (i.e. TR05, TR25, TR60, TR75)
#'
#' @return a data frame containing new columns with volumetric water content for
#'  each column of delta-T temperature change data.
#'
mnet_calc_vwc <- function(data, site_info = NULL){

  stopifnot(any(c("TR05", "TR25", "TR60", "TR75") %in% colnames(data) |
                  c("MP05", "MP25", "MP60", "MP75") %in% colnames(data)))

  stopifnot(any(c("STID", "stid") %in% colnames(data)))

  colnames(data) <- colnames(data) |> toupper()

  if(any((! c("MP05", "MP25", "MP60", "MP75") %in% colnames(data)) &
         c("TR05", "TR25", "TR60", "TR75") %in% colnames(data))){
    data <- mnet_calc_mp(data)
  }

  mp_cols <-
    colnames(data) |>
    grep("^MP", x = _, value = TRUE)

  vwc_cols <- gsub("^MP", "VWC", x = mp_cols)

  if(is.null(site_info)) site_info <- mnet_site_info()

  vg_prms <-
    site_info |>
    colnames() |>
    grep("(stid)|(^wc)|(^a[0-9])|(^n[0-9])", x = _, value = TRUE) |>
    (\(.x) site_info[, .x])() |>
    merge(x = data[, "STID", drop = FALSE],
          y = _,
          by.x = "STID", by.y = "stid",
          all.x = TRUE)

  for(.col in seq_along(vwc_cols)){
    wcr_col <- gsub("^MP", "wcr", mp_cols[[.col]])
    wcs_col <- gsub("^MP", "wcs", mp_cols[[.col]])
    a_col <- gsub("^MP", "a", mp_cols[[.col]])
    n_col <- gsub("^MP", "n", mp_cols[[.col]])
    data[[vwc_cols[.col]]] <- mnet_van_genuchten(MP = data[[mp_cols[.col]]],
                                                 WCr = vg_prms[[wcr_col]],
                                                 WCs = vg_prms[[wcs_col]],
                                                 a = vg_prms[[a_col]],
                                                 n = vg_prms[[n_col]])
    data[[mp_cols[.col]]] <- NULL
  }

  data

}
