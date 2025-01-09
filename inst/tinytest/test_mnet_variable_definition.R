library(tinytest)

expected <-
  data.frame(ID = c("RAIN", "RAIN"),
             `Variable Name` = c("Precipitation", "Rain"),
             Unit = c("millimeters", "millimeters"),
             Description = c("Liquid precipitation accumulation since 0000 UTC. Frozen precipitation cannot be recorded until it melts; therefore, precipitation from snow may not be recorded until several days after the snow event.",
                             "Liquid precipitation measured each day.  Frozen precipitation cannot be recorded until it melts; therefore, precipitation from snow may not be recorded until several days after the snow event."),
             check.names = FALSE)

actual <- mesonet::mnet_variable_definition("RAIN")

row.names(actual) <- 1:nrow(actual)

expect_equal(actual, expected)

expected <-
  data.frame(ID = c("2AVG", "2MAX", "2MIN", "2DEV", "2BAD"),
             `Variable Name` = c("Average Wind Speed at 2m",
                                 "Maximum 2m Wind Speed",
                                 "Minimum 2m Wind Speed",
                                 "Standard Deviation of Wind Speed at 2m",
                                 "Number of Errant 2m Wind Speed Observations"),
             Unit = c("meters per second",
                      "meters per second",
                      "meters per second",
                      "meters per second",
                      "number of 5-minute observations"),
             Description = c("Average of all 5-minute 2m wind speed observations each day.",
                             "Highest 5-minute averaged 2m wind speed measurement each day.",
                             "Lowest 5-minute averaged 2m wind speed measurement each day.",
                             "Standard deviation of the wind speed at 2m during a 5-minute observation period.",
                             "Number of errant 5-minute 2m wind speed observations each day."),
             check.names = FALSE)

actual <- mesonet::mnet_variable_definition("^2")

row.names(actual) <- 1:nrow(actual)

expect_equal(actual, expected)

expected <-
  data.frame(
    ID = c("WMAX", "TMAX", "HMAX", "DMAX", "RMAX",
           "PMAX", "WMAX", "2MAX", "AMAX", "BMAX", "SMAX"),
    `Variable Name` = c("Maximum Wind Speed",
                        "Maximum Daily Air Temperature",
                        "Maximum Humidity",
                        "Maximum Dewpoint Temperature",
                        "Maximum 5-minute Rainfall Rate",
                        "Maximum Station Pressure",
                        "Maximum Wind Gust",
                        "Maximum 2m Wind Speed",
                        "Maximum Solar Radiation",
                        "Maximum Temperature Bare Soil at 10cm",
                        "Maximum Temperature Under Native Vegetation at 10cm"),
    Unit = c("meters per second",
             "degrees Celsius",
             "percent",
             "degrees Celsius",
             "millimeters per hour",
             "kilopascal",
             "meters per second",
             "meters per second",
             "Watts per square meter",
             "degrees Celsius",
             "degrees Celsius"),
    Description = c("Highest 3-second wind speed at 10m sample.",
                    "Highest 5-minute averaged temperature observation reported each day.",
                    "Highest 5-minute averaged humidity observation reported each day.",
                    "Highest 5-minute averaged dewpoint temperature each day. Dewpoint temperature is derived from 1.5m air temperature and the corresponding humidity value.",
                    "Highest 5-minute averaged rainfall rate each day.",
                    "Highest 5-minute averaged station air pressure observation each day.",
                    "Highest 3-second wind speed measurement each day.",
                    "Highest 5-minute averaged 2m wind speed measurement each day.",
                    "Highest 5-minute averaged solar radiation measurement each day.",
                    "Highest 15-minute averaged soil temperature observation each day.",
                    "Highest 15-minute averaged soil temperature observation each day."),
    check.names = FALSE)

actual <- mesonet::mnet_variable_definition("MAX$")

row.names(actual) <- 1:nrow(actual)

expect_equal(actual, expected)
