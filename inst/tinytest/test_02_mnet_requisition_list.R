library(tinytest)

test_cache <-
  tempdir() |>
    file.path(".mesonet_cache")

test_cache |>
  dir.create(recursive = TRUE, showWarnings = FALSE)

mesonet::mnet_test_cache(test_cache, site_info = TRUE)

# one stid multiple dates
actual_req_list <-
  mesonet::mnet_requisition_list(
    stid = "ACME",
    start_date = "1994-01-01",
    end_date = "1994-01-05",
    file_cache = test_cache)

expect_equal(actual_req_list$mts_rel_path,
             paste0("mts/1994/01/0", 1:5,"/1994010", 1:5, "acme.mts"))


# multiple stid single dates
sites <- c("ACME", "ADAX", "ALTU") |>
  sort()

actual_req_list <-
  mesonet::mnet_requisition_list(
    stid = sites,
    start_date = "1994-01-01",
    end_date = "1994-01-01",
    file_cache = test_cache)

expect_equal(actual_req_list$mts_rel_path,
             paste0("mts/1994/01/01/19940101", tolower(sites),".mts"))


# multiple stid multiple dates
sites <- c("ACME", "ADAX", "ALTU") |>
  sort()

start_dates <- c("1994-01-01", "1994-01-03", "1994-01-05")
end_dates <- c("1994-01-02", "1994-01-04", "1994-01-06")

actual_req_list <-
  mesonet::mnet_requisition_list(
    stid = sites,
    start_date = start_dates,
    end_date = end_dates,
    file_cache = test_cache)

expected <-
  paste0("mts/1994/01/0", 1:6, "/1994010", 1:6, rep(tolower(sites), each = 2),".mts")

expect_equal(actual_req_list$mts_rel_path,
             expected)

expect_error(
  mesonet::mnet_requisition_list(
    stid = sites,
    start_date = start_dates[1:2],
    end_date = end_dates[1:2],
    file_cache = test_cache)
)

expect_error(
  mesonet::mnet_requisition_list(
    stid = sites[1:2],
    start_date = start_dates,
    end_date = end_dates,
    file_cache = test_cache)
)

unlink(test_cache, recursive = TRUE)
