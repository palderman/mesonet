library(tinytest)

# Construct reference dataset

data.frame(DATE = as.POSIXct("1994-01-01", tz = "UTC") + (0:4)*24*60*60)

# Write reference dataset to temporary .mesonet_cache

# Convert reference dataset to expected concatenated
expected_subdaily <- data.frame()

# Use mnet_subdaily_c() to read reference data from .mesonet_cache
actual_subdaily <- NULL


expect_equal(actual_subdaily,
             expected_subdaily)
