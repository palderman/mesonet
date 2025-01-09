daily_def_df <-
  "https://mesonet.org/about/data-descriptions/daily-summaries" |>
  rvest::read_html() |>
  rvest::html_table() |>
  do.call(rbind.data.frame, args = _)

daily_def_df <-
  daily_def_df[, c("ID", "Variable Name", "Unit", "Description")]

mts_def_df <-
  "https://mesonet.org/about/data-descriptions/mdf-mts-files" |>
    rvest::read_html() |>
    rvest::html_table() |>
    getElement(1)

colnames(mts_def_df) <-
  colnames(mts_def_df) |>
  gsub("^Name$", "Variable Name", x = _)

mts_def_df <-
  mts_def_df[, c("ID", "Variable Name", "Unit", "Description")]

variable_definitions <-
  rbind(mts_def_df, daily_def_df) |>
  as.data.frame() |>
  within({
    Unit = gsub("Fahrenheit", "Celsius", Unit)
    Unit = gsub("(millibar[s]{0,1})|(inches of mercury)", "kilopascal", Unit)
    Unit = gsub("inches", "millimeters", Unit)
    Unit = gsub("miles per hour", "meters per second", Unit)
    Unit = gsub("watts", "Watts", Unit)
    ID = ifelse(ID == "S3MNO" & grepl("25cm", `Variable Name`),
                "S25NO",
                ID)
  })

# variable_definitions <-
#   variable_definitions |>
#   with({
#     variable_definitions[!duplicated(ID),]
#   }) |>
#   as.data.frame()

usethis::use_data(variable_definitions, internal = TRUE, overwrite = TRUE)
