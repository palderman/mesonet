#' Drop units from all columns of Oklahoma Mesonet data frame
#'
#' @param df a data frame with units columns
#'
#' @return A data frame with identical data, but without units
#'
#' @export
#'
mnet_drop_units <- function(df){
  for(i in seq_along(df)){
    if("units" %in% class(df[[i]])){
      df[[i]] <- units::drop_units(df[[i]])
    }
  }
  return(df)
}
