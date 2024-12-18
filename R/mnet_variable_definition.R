#' Find Mesonet variable definition
#'
#' @description
#'
#' Provide definition of a requested set of Mesonet variables including (by
#'  default) the standard variable identifier (ID), name, unit, and description.
#'
#' @param id a string or regular expression to use for retrieving definitions
#'   of Mesonet variables
#'
#' @param columns a character vector of columns to include in the
#'  output. Possible values include any combination of "ID", "Variable Name",
#'   "Unit" or "Description"
#'
#' @export
#'
mnet_variable_definition <- function(id,
                                     columns = c("ID", "Variable Name", "Unit",
                                                 "Description")){
  # variable_definitions is a package-internal data structure
  variable_definitions |>
    with({
      variable_definitions[grep(id, ID), columns]
    })
}
