#' Root url for Oklahoma Mesonet Time Series files
#'
#' @export
#'
#' @param data_source the data source for which to provide the url: "mesonet"
#'  for the Oklahoma Mesonet, "fcars" for the USDA ARS station network in the
#'  Fort Cobb watershed, or "ars" for the USDA ARS station network in the
#'  Little Washita watershed
#'
#' @return a character string with the root URL for the desired Mesonet data
#'  source
#'
#' @examples
#'
#' mnet_root_url()
#'
#' mnet_root_url("fcars")
#'
mnet_root_url <- function(data_source = "mesonet"){

  stopifnot(data_source %in% c("mesonet", "fcars", "ars"))

  "https://data.mesonet.org/data/public" |>
    paste0("/", data_source)
}
