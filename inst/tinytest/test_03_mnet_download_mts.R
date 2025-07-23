library(tinytest)

test_scenario <-
  data.frame(
    stid = c("ALTU", "STIL", "LAHO"),
    start = c("1994-02-01", "1994-02-02", "1994-02-03"),
    end = c("1994-02-03", "1994-02-04", "1994-02-05"))

test_site_info <-
  data.frame(
    stid = c("ALTU", "STIL", "LAHO"),
    datc = as.POSIXct("1994-01-01", tz = "UTC"),
    datd = as.POSIXct("2099-12-31", tz = "UTC")
  )

# Create remote cache for testing

test_remote_cache <-
  tempdir() |>
  file.path(".test_remote_cache")

test_remote_cache |>
  dir.create(recursive = TRUE,
             showWarnings = FALSE)

# Write test files to test_remote_cache

test_files <-
  test_scenario |>
  within({
    stid = lapply(stid, \(.x) rep(.x, 3))
    dates = mapply(\(.start, .end){
      seq(as.POSIXct(.start, tz = "UTC"),
          as.POSIXct(.end, tz = "UTC"),
          by = "days")
    }, .start = start, .end = end, SIMPLIFY = FALSE)
  }) |>
  with({
    data.frame(
      row.names = NULL,
      stid = unlist(stid),
      dates = unlist(dates) |> as.POSIXct(tz = "UTC")
    )
  }) |>
  within({
    year_dir = format(dates, "%Y")
    month_dir = format(dates, "%m")
    day_dir = format(dates, "%d")
    file_name = format(dates, "%Y%m%d") |>
      paste0(stid, ".mts") |>
      tolower()
    rel_path = file.path("mts",
                         year_dir, month_dir, day_dir,
                         file_name)
    file_path = file.path(test_remote_cache, rel_path)
  })

test_files |>
  with({
    mapply(\(.path, .rel_path){
      dirname(.path) |>
        dir.create(recursive = TRUE,
                   showWarnings = FALSE)
      write(.rel_path, .path)
    }, .path = file_path, .rel_path = rel_path)
  })

# Set up test_root_url

test_root_url <-
  test_remote_cache |>
  gsub("\\\\", "/", x = _) |>
  gsub("^/*", "///", x = _) |>
  paste0("file:", .x = _)

test_local_cache <-
  tempdir() |>
  file.path(".mesonet_cache")

dir.create(test_local_cache,
           showWarnings = FALSE,
           recursive = TRUE)

test_scenario |>
  with({
    mesonet::mnet_download_mts(stid = stid,
                               start_date = start,
                               end_date = end,
                               root_url = test_root_url,
                               file_cache = test_local_cache,
                               site_info = test_site_info,
                               silent = TRUE)
  })

file_list <-
  list.files(test_local_cache,
             recursive = TRUE)

expect_true(
  all(file.exists(file.path(test_local_cache, test_files$rel_path)))
  )

file_contents <-
  test_local_cache |>
  file.path(test_files$rel_path) |>
  lapply(readLines) |>
  unlist()

expect_equal(file_contents, test_files$rel_path)

unlink(file.path(test_local_cache, test_files$rel_path))

actual_delay <-
system.time({
  test_scenario |>
    with({
      mesonet::mnet_download_mts(stid = stid[1],
                                 start_date = start[1],
                                 end_date = start[1],
                                 root_url = test_root_url,
                                 file_cache = test_local_cache,
                                 site_info = test_site_info,
                                 silent = TRUE)
    })
})["elapsed"]

expect_true(actual_delay > 0.5)

unlink(test_remote_cache, recursive = TRUE)

unlink(test_local_cache, recursive = TRUE)

