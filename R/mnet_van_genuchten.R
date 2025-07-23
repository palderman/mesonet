#' Calculate volumetric soil water content with van Genuchten curve
#'
#' Calculate volumetric soil water content using the van Genuchten curve
#'
#' @export
#'
#' @param MP a vector of matric potential at which to calculate volumetric soil water content
#'
#' @param WCr residual water content for dry soil
#'
#' @param WCs saturated water content for wet soil
#'
#' @param a the alpha parameter for the van Genuchten equation related to the
#'  inverse of the air entry suction
#'
#' @param n the n parameter for the van Genuchten equation related to the
#'  pore-size distribution
#'
#' @return a vector of volumetric soil water content values
#'
#' @examples
#'
#' mnet_van_genuchten(-100, 0.034, 0.41, 0.273, 1.39)
#'
mnet_van_genuchten <- function (MP, WCr, WCs, a, n){

  return_units <- FALSE

  if("units" %in% class(MP)){
    MP <-
      units::set_units(MP, "kPa") |>
      units::drop_units()
    return_units <- TRUE
  }

  if("units" %in% class(WCr)){
    WCr <-
      units::set_units(WCr, "cm3/cm3") |>
      units::drop_units()
    return_units <- TRUE
  }

  if("units" %in% class(WCs)){
    WCs <-
      units::set_units(WCs, "cm3/cm3") |>
      units::drop_units()
    return_units <- TRUE
  }

  if("units" %in% class(a)){
    a <-
      units::set_units(a, "1/kPa") |>
      units::drop_units()
    return_units <- TRUE
  }

  if("units" %in% class(n)){
    n <-
      units::set_units(n, "1") |>
      units::drop_units()
    return_units <- TRUE
  }

  stopifnot(a > 0)
  stopifnot(n > 1)
  stopifnot(WCr <= 1)
  stopifnot(WCr >= 0)
  stopifnot(WCs <= 1)
  stopifnot(WCs >= 0)
  stopifnot(WCr < WCs)

  vg <- WCr + (WCs - WCr)/(1 + (-a * MP)^n)^(1 - 1/n)

  if(return_units){
    return(units::set_units(vg, "cm3/cm3"))
  }else{
    return(vg)
  }
}
