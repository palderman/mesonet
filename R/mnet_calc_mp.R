#' Calculate soil matric potential for Oklahoma Mesonet data
#'
#' Calculate soil matric potential from delta-T soil temperature change data
#'  from the Oklahoma Mesonet using the equation from Zhang et al (2019) <doi:10.2136/sssaj2018.12.0481>: MP = 2083/(1+ exp(-3.35*(Tref-3.17)))
#'  where Tref is the measured delta-T data
#'
#' @export
#'
#' @param data a data frame that contains columns for delta-T temperature change
#'  data (i.e. TR05, TR25, TR60, TR75)
#'
#' @return a data frame containing new columns with matric potential (kPa) for each
#'  column of delta-T temperature change data.
#'
#' @examples
#'
#' mesonet_data <- data.frame(TR05 = 3.17, TR25 = 2.17, TR60 = 2.0, TR75 = 1.0)
#'
#' mnet_calc_mp(mesonet_data)
#'
mnet_calc_mp <- function(data){

  stopifnot(any(c("TR05", "TR25", "TR60", "TR75") %in% colnames(data)))

  tr_cols <-
    colnames(data) |>
    grep("^TR", x = _, value = TRUE)

  mp_cols <- gsub("^TR", "MP", x = tr_cols)

  for(.col in seq_along(tr_cols)){
    if("units" %in% class(data[[tr_cols[.col]]])){
      deltaT <-
        data[[tr_cols[.col]]] |>
        units::set_units("Celsius") |>
        units::drop_units()
      data[[mp_cols[.col]]] <-
        units::set_units(-2083/(1+ exp(-3.35*(deltaT-3.17))), "kPa")
    }else{
      data[[mp_cols[.col]]] <-
        -2083/(1+ exp(-3.35*(data[[tr_cols[.col]]]-3.17)))
    }
  }

  data

}
