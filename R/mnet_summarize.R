#' Produce a daily summary of Oklahoma Mesonet subdaily data
#'
#' @param sub_daily a data frame with subdaily measurements from the Oklahoma Mesonet
#' such as that produced by [mnet_read_mts()].
#'
#' @param tz a length-one character vector specifying which time zone to use for daily summary. Use
#'  [base::OlsonNames()] to obtain a listing of valid available time zones.
#'
#' @param interval the interval over which to summarize data. May be defined as
#'  a difftime object (see [base::difftime()]) or a character value compatible
#'  with [units::as_units()]. Tested intervals include "1 day" (the default),
#'  "30 min", and "1 hour", although other intervals may work (e.g. "3 hours").
#'
#' @param include_qc_variables a length-one logical vector specifying whether or
#'  or not to include quality control variables (e.g. number of errant
#'  observations) in the output
#'
#' @md
#'
#' @return A data frame with daily summaries of Oklahoma Mesonet data. See
#'  [mnet_variable_definition()] or the table below for the ID, variable name,
#'  unit and description for each column.
#'
#' |**ID** |**Variable Name**                                                         |**Unit**                         |**Description**                                                                                                                                                                                                          |
#' |:------|:-------------------------------------------------------------------------|:--------------------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
#' |2AVG   |Average Wind Speed at 2m                                                  |meters per second                |Average of all 5-minute 2m wind speed observations each day.                                                                                                                                                             |
#' |2BAD   |Number of Errant 2m Wind Speed Observations                               |number of 5-minute observations  |Number of errant 5-minute 2m wind speed observations each day.                                                                                                                                                           |
#' |2DEV   |Standard Deviation of Wind Speed at 2m                                    |meters per second                |Standard deviation of the wind speed at 2m during a 5-minute observation period.                                                                                                                                         |
#' |2MAX   |Maximum 2m Wind Speed                                                     |meters per second                |Highest 5-minute averaged 2m wind speed measurement each day.                                                                                                                                                            |
#' |2MIN   |Minimum 2m Wind Speed                                                     |meters per second                |Lowest 5-minute averaged 2m wind speed measurement each day.                                                                                                                                                             |
#' |9AVG   |Average Air Temperature at 9m                                             |degrees Celsius                  |Average of all 5-minute averaged 9-m air temperature observations each day.                                                                                                                                              |
#' |9BAD   |Number of Errant 9m Air Temperature Observations                          |number of 5-minute observations  |Number of errant 5-minute 9m air temperature observations each day.                                                                                                                                                      |
#' |ABAD   |Number of Errant Solar Radiation Observations                             |number of 5-minute observations  |Number of errant 5-minute solar radiation observations each day.                                                                                                                                                         |
#' |AMAX   |Maximum Solar Radiation                                                   |Watts per square meter           |Highest 5-minute averaged solar radiation measurement each day.                                                                                                                                                          |
#' |AMAXO  |Maximum Solar Radiation Observation Number                                |5-minute observation number      |Daily observation number that measured highest 5-minute averaged solar radiation value each day                                                                                                                          |
#' |ATOT   |Total Solar Radiation                                                     |mega Joules per square meter     |Daily accumulation of solar radiation each day.                                                                                                                                                                          |
#' |B5AV   |Average Temperature Under Bare Soil at 5cm                                |degrees Celsius                  |Average of all 15-minute averaged soil temperature observations each day.                                                                                                                                                |
#' |B5BD   |Number of Errant Temperature Under Bare Soil at 5cm Observations          |number of 15-minute observations |Number of errant 15-minute temperature under bare soil at 5cm observations each day. This variable is only available prior to December 1, 2013.                                                                          |
#' |B5MN   |Minimum Temperature Under Bare Soil at 5cm                                |degrees Celsius                  |Lowest 15-minute averaged soil temperature observation each day. This variable is only available prior to December 1, 2013.                                                                                              |
#' |B5MNO  |Minimum Temperature Under Bare Soil at 5cm Observation Number             |15-minute observation number     |Daily 15-minute observation number that measured minimum temperature under bare soil at 5cm each day. This variable is only available prior to December 1, 2013.                                                         |
#' |B5MX   |Maximum Temperature Bare Soil at 5cm                                      |degrees Celsius                  |Highest 15-minute averaged soil temperature observation each day. This variable is only available prior to December 1, 2013.                                                                                             |
#' |B5MXO  |Maximum Temperature Under Bare Soil at 5cm Observation Number             |15-minute observation number     |Daily 15-minute observation number that measured maximum temperature under bare soil at 5cm each day. This variable is only available prior to December 1, 2013.                                                         |
#' |BAVG   |Average Temperature Under Bare Soil at 10cm                               |degrees Celsius                  |Average of all 15-minute averaged soil temperature observations each day. This variable is only available prior to December 1, 2013.                                                                                     |
#' |BBAD   |Number of Errant Temperature Under Bare Soil at 10cm Observations         |number of 15-minute observations |Number of errant 15-minute temperature under bare soil at 10cm observations each day.                                                                                                                                    |
#' |BMAX   |Maximum Temperature Bare Soil at 10cm                                     |degrees Celsius                  |Highest 15-minute averaged soil temperature observation each day.                                                                                                                                                        |
#' |BMAXO  |Maximum Temperature Under Bare Soil at 10cm Observation Number            |15-minute observation number     |Daily 15-minute observation number that measured maximum temperature under bare soil at 10cm each day.                                                                                                                   |
#' |BMIN   |Minimum Temperature Under Native Vegetation at 10cm                       |degrees Celsius                  |Lowest 15-minute averaged soil temperature observation each day.                                                                                                                                                         |
#' |BMINO  |Minimum Temperature Under Bare Soil at 10cm Observation Number            |15-minute observation number     |Daily 15-minute observation number that measured minimum temperature under bare soil at 10cm each day.                                                                                                                   |
#' |CDEG   |Cooling Degree Days                                                       |degrees Celsius                  |Number of degrees Celsius    that the average daily air temperature is greater than 65 degrees Celsius   .                                                                                                               |
#' |DATE   |Date of summary in Central Standard Time                                  |                                 |                                                                                                                                                                                                                         |
#' |DAVG   |Average Dewpoint Temperature                                              |degrees Celsius                  |Average of all 5-minute averaged dewpoint temperatures each day. Dewpoint temperature is derived from 1.5m air temperature and the corresponding humidity value.                                                         |
#' |DBAD   |Number of Errant Dewpoint Temperature Observations                        |number of 5-minute observations  |Number of errant 5-minute dewpoint temperature observations each day.                                                                                                                                                    |
#' |DMAX   |Maximum Dewpoint Temperature                                              |degrees Celsius                  |Highest 5-minute averaged dewpoint temperature each day. Dewpoint temperature is derived from 1.5m air temperature and the corresponding humidity value.                                                                 |
#' |DMAXO  |Maximum Daily Dewpoint Temperature Observation Number                     |5-minute observation number      |Daily observation number that measured highest 5-minute averaged dewpoint temperature each day.                                                                                                                          |
#' |DMIN   |Minimum Dewpoint Temperature                                              |degrees Celsius                  |Lowest 5-minute averaged dewpoint temperature each day. Dewpoint temperature is derived from 1.5m air temperature and the corresponding humidity value.                                                                  |
#' |DMINO  |Minimum Daily Dewpoint Temperature Observation Number                     |5-minute observation number      |Daily observation number that measured lowest 5-minute averaged dewpoint temperature each day.                                                                                                                           |
#' |HAVG   |Average Humidity                                                          |percent                          |Average of all 5-minute averaged humidity observations each day.                                                                                                                                                         |
#' |HBAD   |Number of Errant Humidity Observations                                    |number of 5-minute observations  |Number of errant 5-minute humidity observations each day.                                                                                                                                                                |
#' |HDEG   |Heating Degree Days                                                       |degrees Celsius                  |Number of degrees Celsius that the average daily air temperature is less than 18.3 degrees Celsius.                                                                                                                  |
#' |HMAX   |Maximum Humidity                                                          |percent                          |Highest 5-minute averaged humidity observation reported each day.                                                                                                                                                        |
#' |HMAXO  |Maximum Daily Humidity Observation Number                                 |5-minute observation number      |Daily observation number that measured highest 5-minute averaged humidity each day.                                                                                                                                      |
#' |HMIN   |Minimum Humidity                                                          |percent                          |Lowest 5-minute averaged humidity observation reported each day.                                                                                                                                                         |
#' |HMINO  |Minimum Daily Humidity Observation Number                                 |5-minute observation number      |Daily observation number that measured lowest 5-minute averaged humidity each day.                                                                                                                                       |
#' |HTBAD  |Number of Errant Heat Index Observations                                  |number of 5-minute observations  |Number of errant 5-minute heat index observations each day.                                                                                                                                                              |
#' |HTMX   |Maximum Heat Index Temperature                                            |degrees Celsius                  |Largest 5-minute averaged heat index observation each day. Derived using 5-minute averaged air temperature and corresponding 5-minute averaged humidity observation.                                                     |
#' |HTMXO  |Maximum Daily Heat Index Observation Number                               |5-minute observation number      |Daily observation number that measured highest 5-minute averaged heat index each day.                                                                                                                                    |
#' |IBAD   |Number of Errant Wind Direction Observations                              |number of 5-minute observations  |Number of errant 5-minute 10m wind direction observations each day.                                                                                                                                                      |
#' |MSLP   |Mean Sea Level Pressure                                                   |inches of mercury                |Average of all 5-minute averaged station air pressure observations adjusted for station elevation each day.                                                                                                              |
#' |PAVG   |Average Station Pressure                                                  |inches of mercury                |Average of all 5-minute averaged station air pressure observations each day.                                                                                                                                             |
#' |PBAD   |Number of Errant Station Pressure Observations                            |number of 5-minute observations  |Number of errant 5-minute station pressure observations each day.                                                                                                                                                        |
#' |PDFQ   |Primary Wind Direction Frequency                                          |percentage                       |Frequency of observations in the given wind direction category.                                                                                                                                                          |
#' |PDIR   |Primary Wind Direction                                                    |16-point cardinal direction      |Most common wind direction for the day based on 16-point compass heading (i.e., 0 is N; 1 is NNE; 15 is NNW).                                                                                                            |
#' |PMAX   |Maximum Station Pressure                                                  |inches of mercury                |Highest 5-minute averaged station air pressure observation each day.                                                                                                                                                     |
#' |PMAXO  |Maximum Daily Station Pressure Observation Number                         |5-minute observation number      |Daily observation number that measured highest 5-minute averaged station pressure each day.                                                                                                                              |
#' |PMIN   |Minimum Station Pressure                                                  |inches of mercury                |Lowest 5-minute averaged station air pressure observation each day.                                                                                                                                                      |
#' |PMINO  |Minimum Daily Station Pressure Observation Number                         |5-minute observation number      |Daily observation number that measured lowest 5-minute averaged station pressure each day.                                                                                                                               |
#' |R05BD  |Number of Errant Soil Moisture Calibrated Delta-T at 5cm Observations     |number of 30-minute observations |Number of errant 30-minute calibrated delta-t at 5cm observations.                                                                                                                                                       |
#' |R25BD  |Number of Errant Soil Moisture Calibrated Delta-T at 25cm Observations    |number of 30-minute observations |Number of errant 30-minute calibrated delta-t at 25cm observations.                                                                                                                                                      |
#' |R60BD  |Number of Errant Soil Moisture Calibrated Delta-T at 60cm Observations    |number of 30-minute observations |Number of errant 30-minute calibrated delta-t at 60cm observations.                                                                                                                                                      |
#' |R75BD  |Number of Errant Soil Moisture Calibrated Delta-T at 75cm Observations    |number of 30-minute observations |*Decommissioned March 2012. Number of errant 30-minute calibrated delta-t at 75cm observations.                                                                                                                           |
#' |RAIN   |Rain                                                                      |inches                           |Liquid precipitation measured each day.  Frozen precipitation cannot be recorded until it melts; therefore, precipitation from snow may not be recorded until several days after the snow event.                         |
#' |RBAD   |Number of Errant 5-minute Rain Observations                               |number of 5-minute observations  |Number of errant 5-minute rain observations each day.                                                                                                                                                                    |
#' |RMAX   |Maximum 5-minute Rainfall Rate                                            |inches per hour                  |Highest 5-minute averaged rainfall rate each day.                                                                                                                                                                        |
#' |RNUM   |Number of 5-minute Rainy Periods                                          |number of 5-minute observations  |Number of 5-minute observations in which precipitation increased. Frozen precipitation cannot be recorded until it melts; therefore precipitation from snow may not be recorded until several days after the snow event. |
#' |S25AV  |Average Temperature Under Native Vegetation at 25cm                       |degrees Celsius                  |Average of all 15-minute averaged soil temperature observations each day. This variable is only available after and including December 1, 2013.                                                                          |
#' |S25BD  |Number of Errant Temperature Under Native Vegetation at 25cm Observations |number of 15-minute observations |Number of errant 15-minute temperature under native vegetation at 25cm observations each day. This variable is only available after and including December 1, 2013.                                                      |
#' |S25MN  |Minimum Temperature Under Native Vegetation at 25cm                       |degrees Celsius                  |Lowest 15-minute averaged soil temperature observation each day. This variable is only available after and including December 1, 2013.                                                                                   |
#' |S25MX  |Maximum Temperature Under Native Vegetation at 25cm                       |degrees Celsius                  |Highest 15-minute averaged soil temperature observation each day. This variable is only available after and including December 1, 2013.                                                                                  |
#' |S25NO  |Minimum Temperature Under Native Vegetation at 25cm Observation Number    |15-minute observation number     |Daily 15-minute observation number that measured minimum temperature under native vegetation at 25cm each day. This variable is only available after and including December 1, 2013.                                     |
#' |S25XO  |Maximum Temperature Under Native Vegetation at 25cm Observation Number    |15-minute observation number     |Daily 15-minute observation number that measured maximum temperature under native vegetation at 25cm each day. This variable is only available after and including December 1, 2013.                                     |
#' |S3AV   |Average Temperature Under Native Vegetation at 30cm                       |degrees Celsius                  |Average of all 15-minute averaged soil temperature observations each day. This variable is only available prior to December 1, 2013.                                                                                     |
#' |S3BD   |Number of Errant Temperature Under Native Vegetation at 30cm Observations |number of 15-minute observations |Number of errant 15-minute temperature under native vegetation at 30cm observations each day. This variable is only available prior to December 1, 2013.                                                                 |
#' |S3MN   |Minimum Temperature Under Native Vegetation at 30cm                       |degrees Celsius                  |Lowest 15-minute averaged soil temperature observation each day. This variable is only available prior to December 1, 2013.                                                                                              |
#' |S3MNO  |Minimum Temperature Under Native Vegetation at 30cm Observation Number    |15-minute observation number     |Daily 15-minute observation number that measured minimum temperature under native vegetation at 30cm each day. This variable is only available prior to December 1, 2013.                                                |
#' |S3MX   |Maximum Temperature Under Native Vegetation at 30cm                       |degrees Celsius                  |Highest 15-minute averaged soil temperature observation each day. This variable is only available prior to December 1, 2013.                                                                                             |
#' |S3MXO  |Maximum Temperature Under Native Vegetation at 30cm Observation Number    |15-minute observation number     |Daily 15-minute observation number that measured maximum temperature under native vegetation at 30cm each day. This variable is only available prior to December 1, 2013.                                                |
#' |S5AV   |Average Temperature Under Native Vegetation at 5cm                        |degrees Celsius                  |Average of all 15-minute averaged soil temperature observations each day.                                                                                                                                                |
#' |S5BD   |Number of Errant Temperature Under Native Vegetation at 5cm Observations  |number of 15-minute observations |Number of errant 15-minute temperature under native vegetation at 5cm observations each day.                                                                                                                             |
#' |S5MN   |Minimum Temperature Under Native Vegetation at 5cm                        |degrees Celsius                  |Lowest 15-minute averaged soil temperature observation each day.                                                                                                                                                         |
#' |S5MNO  |Minimum Temperature Under Native Vegetation at 5cm Observation Number     |15-minute observation number     |Daily 15-minute observation number that measured minimum temperature under native vegetation at 5cm each day.                                                                                                            |
#' |S5MX   |Maximum Temperature Under Native Vegetation at 5cm                        |degrees Celsius                  |Highest 15-minute averaged soil temperature observation each day.                                                                                                                                                        |
#' |S5MXO  |Maximum Temperature Under Native Vegetation at 5cm Observation Number     |15-minute observation number     |Daily 15-minute observation number that measured maximum temperature under native vegetation at 5cm each day.                                                                                                            |
#' |S60AV  |Average Temperature Under Native Vegetation at 60cm                       |degrees Celsius                  |Average of all 15-minute averaged soil temperature observations each day. This variable is only available after and including December 1, 2013.                                                                          |
#' |S60BD  |Number of Errant Temperature Under Native Vegetation at 60cm Observations |number of 15-minute observations |Number of errant 15-minute temperature under native vegetation at 60cm observations each day. This variable is only available after and including December 1, 2013.                                                      |
#' |S60MN  |Minimum Temperature Under Native Vegetation at 60cm                       |degrees Celsius                  |Lowest 15-minute averaged soil temperature observation each day. This variable is only available after and including December 1, 2013.                                                                                   |
#' |S60MX  |Maximum Temperature Under Native Vegetation at 60cm                       |degrees Celsius                  |Highest 15-minute averaged soil temperature observation each day. This variable is only available after and including December 1, 2013.                                                                                  |
#' |S60NO  |Minimum Temperature Under Native Vegetation at 60cm Observation Number    |15-minute observation number     |Daily 15-minute observation number that measured minimum temperature under native vegetation at 60cm each day. This variable is only available after and including December 1, 2013.                                     |
#' |S60XO  |Maximum Temperature Under Native Vegetation at 60cm Observation Number    |15-minute observation number     |Daily 15-minute observation number that measured maximum temperature under native vegetation at 60cm each day. This variable is only available after and including December 1, 2013.                                     |
#' |SAVG   |Average Temperature Under Native Vegetation at 10cm                       |degrees Celsius                  |Average of all 15-minute averaged soil temperature observations each day.                                                                                                                                                |
#' |SBAD   |Number of Errant Temperature Under Native Vegetation at 10cm Observations |number of 15-minute observations |Number of errant 15-minute temperature under native vegetation at 10cm observations each day.                                                                                                                            |
#' |SDFQ   |Secondary Wind Direction Frequency                                        |percentage                       |Frequency of observations in the given wind direction category.                                                                                                                                                          |
#' |SDIR   |Secondary Wind Direction                                                  |16-point cardinal direction      |Second most common wind direction for the day based on 16-point compass heading.                                                                                                                                         |
#' |SMAX   |Maximum Temperature Under Native Vegetation at 10cm                       |degrees Celsius                  |Highest 15-minute averaged soil temperature observation each day.                                                                                                                                                        |
#' |SMAXO  |Maximum Temperature Under Native Vegetation at 10cm Observation Number    |15-minute observation number     |Daily 15-minute observation number that measured maximum temperature under native vegetation at 10cm each day.                                                                                                           |
#' |SMIN   |Minimum Temperature Under Native Vegetation at 10cm                       |degrees Celsius                  |Lowest 15-minute averaged soil temperature observation each day.                                                                                                                                                         |
#' |SMINO  |Minimum Temperature Under Native Vegetation at 10cm Observation Number    |15-minute observation number     |Daily 15-minute observation number that measured minimum temperature under native vegetation at 10cm each day.                                                                                                           |
#' |STID   |Station ID                                                                |                                 |Station ID                                                                                                                                                                                                               |
#' |TAVG   |Average Air Temperature                                                   |degrees Celsius                  |Average of all 5-minute averaged temperature observations each day.                                                                                                                                                      |
#' |TBAD   |Number of Errant 1.5m Air Temperature Observations                        |number of 5-minute observations  |Number of errant 5-minute averaged 1.5m air temperature observations each day.                                                                                                                                           |
#' |TMAX   |Maximum Daily Air Temperature                                             |degrees Celsius                  |Highest 5-minute averaged temperature observation reported each day.                                                                                                                                                     |
#' |TMAXO  |Maximum Daily Air Temperature Observation Number                          |5-minute observation number      |Daily observation number that measured highest 5-minute averaged 1.5 m air temperature each day.                                                                                                                         |
#' |TMIN   |Minimum Daily Air Temperature                                             |degrees Celsius                  |Lowest 5-minute averaged temperature observation reported each day.                                                                                                                                                      |
#' |TMINO  |Minimum Daily Air Temperature Observation Number                          |5-minute observation number      |Daily observation number that measured lowest 5-minute averaged 1.5 m air temperature each day.                                                                                                                          |
#' |TR05   |Soil Moisture Calibrated Delta-T at 5cm                                   |degrees Celsius                  |Calibrated change in temperature of soil over time after a heat pulse is introduced. Used to calculate soil water potential, fractional water index, or volumetric water.                                                |
#' |TR25   |Soil Moisture Calibrated Delta-T at 25cm                                  |degrees Celsius                  |Calibrated change in temperature of soil over time after a heat pulse is introduced. Used to calculate soil water potential, fractional water index, or volumetric water.                                                |
#' |TR60   |Soil Moisture Calibrated Delta-T at 60cm                                  |degrees Celsius                  |Calibrated change in temperature of soil over time after a heat pulse is introduced. Used to calculate soil water potential, fractional water index, or volumetric water.                                                |
#' |TR75   |Soil Moisture Calibrated Delta-T at 75cm                                  |degrees Celsius                  |*Decommissioned March 2012. Calibrated change in temperature of soil over time after a heat pulse is introduced. Used to calculate soil water potential, fractional water index, or volumetric water.                     |
#' |VDEF   |Average Daily Vapor Deficit                                               |millibars                        |Average of all 5-minute averaged vapor deficit estimates each day.                                                                                                                                                       |
#' |WBAD   |Number of Errant Wind Speed Observations                                  |number of 5-minute observations  |Number of errant 5-minute 10m wind speed observations each day.                                                                                                                                                          |
#' |WCBAD  |Number of Errant Wind Chill Observations                                  |number of 5-minute observations  |Number of errant 5-minute wind chill observations each day.                                                                                                                                                              |
#' |WCMN   |Minimum Wind Chill Index Temperature                                      |degrees Celsius                  |Lowest 5-minute averaged wind chill observation each day. Derived using 5-minute averaged air temperature and corresponding 5-minute averaged 10-m wind speed observation.                                               |
#' |WCMNO  |Minimum Daily Wind Chill Observation Number                               |5-minute observation number      |Daily observation number that measured lowest 5-minute averaged wind chill each day.                                                                                                                                     |
#' |WDEV   |Standard Deviation of Wind Speed at 10m                                   |meters per second                |Standard deviation of the wind speed at 10m during a 5-minute observation period.                                                                                                                                        |
#' |WMAX   |Maximum Wind Gust                                                         |meters per second                |Highest 3-second wind speed measurement each day.                                                                                                                                                                        |
#' |WMAXO  |Maximum Wind Gust Observation Number                                      |5-minute observation number      |Daily 5-minute observation number that measured maximum wind gust each day.                                                                                                                                              |
#' |WSMN   |Minimum Wind Speed                                                        |meters per second                |Lowest 5-minute averaged 10m wind speed measurement each day.                                                                                                                                                            |
#' |WSMNO  |Minimum Wind Speed Observation Number                                     |5-minute observation number      |Daily 5-minute observation number that measured minimum wind speed at 10m each day.                                                                                                                                      |
#' |WSMX   |Maximum Wind Speed                                                        |meters per second                |Highest 5-minute averaged 10m wind speed measurement each day.                                                                                                                                                           |
#' |WSMXO  |Maximum Wind Speed Observation Number                                     |5-minute observation number      |Daily 5-minute observation number that measured maximum wind speed at 10m each day.                                                                                                                                      |
#' |WSPD   |Average Wind Speed                                                        |meters per second                |Average of all 5-minute wind speed observations each day.                                                                                                                                                                |
#'
#' @export
#'
mnet_summarize <- function(sub_daily,
                           tz = "Etc/GMT+6",
                           interval = "1 day",
                           include_qc_variables = FALSE){

  stopifnot(any(c("character", "difftime") %in% class(interval)))

  if(is.character(interval)){
    interval <-
      units::as_units(interval) |>
      units::set_units("sec") |>
      units::drop_units() |>
      as.difftime(units = "secs")
  }

  int_adj <-
    {units::as_units(interval)/units::set_units(1, "day")} |>
    units::drop_units()

  num <- c(
    obs = floor(288*int_adj),
    srad = floor(288*int_adj),
    stemp = floor(96*int_adj),
    smoist = floor(48*int_adj)
  )

  thrsh <- floor(num*.9)

  if(interval == as.difftime(1, units = "days")){
    thrsh["srad"] <- floor(num["srad"]*0.95)
  }

  threshold_temperature <-
    units::set_units(65, "Fahrenheit") |>
    units::set_units("Celsius")

  sub_daily <-
    sub_daily |>
    within({
      DATE = round_date(DATE, interval, tz)
    })

  daily <- list(
    # TAIR to TDEW
    summarize_across(
      .data = sub_daily,
      .cols = c("TAIR", "RELH", "WSPD", "PRES", "WS2M", "TDEW"),
      .fns = list(
        # 260 is at least 90% of the 288 5-minute observations per day
        min = \(.x) qc_summary(.x, min, thrsh["obs"]),
        avg = \(.x) qc_summary(.x, mean, thrsh["obs"]),
        max = \(.x) qc_summary(.x, max, thrsh["obs"])),
      .groups = c("STNM", "STID", "DATE")),

    # WDIR
    summarize_across(
      .data = sub_daily,
      .cols = c("WDIR"),
      .fns = list(
        # 260 is at least 90% of the 288 5-minute observations per day
        pdir = \(.x) qc_summary(.x, calc_pdir, thrsh["obs"]),
        sdir = \(.x) qc_summary(.x, calc_sdir, thrsh["obs"])),
      .groups = c("STNM", "STID", "DATE")),

    # RAIN
    summarize_across(
      .data = sub_daily,
      .cols = c("RAIN"),
      .fns = list(
        # 260 is at least 90% of the 288 5-minute observations per day
        sum = \(.x) qc_summary(.x, sum, thrsh["obs"]),
        max = \(.x) qc_summary(.x, max, thrsh["obs"])),
      .groups = c("STNM", "STID", "DATE")),

    # SRAD sum
    summarize_across(
      .data = sub_daily,
      .cols = c("SRAD"),
      .fns = list(
        # 260 is at least 90% of the 288 5-minute observations per day
        sum = \(.x) qc_summary(.x, srad_sum, thrsh["srad"], interval = interval)),
      .groups = c("STNM", "STID", "DATE")),

    # WMAX and SRAD max and max_count
    summarize_across(
      .data = sub_daily,
      .cols = c("WMAX", "SRAD"),
      .fns = list(
        # 260 is at least 90% of the 288 5-minute observations per day
        max = \(.x) qc_summary(.x, max, thrsh["obs"])),
      .groups = c("STNM", "STID", "DATE")),

    # TA9M and VDEF avg
    summarize_across(
      .data = sub_daily,
      .cols = c("TA9M", "VDEF"),
      .fns = list(
        # 260 is at least 90% of the 288 5-minute observations per day
        avg = \(.x) qc_summary(.x, mean, thrsh["obs"])),
      .groups = c("STNM", "STID", "DATE")),

    # Soil temperature
    summarize_across(
      .data = sub_daily,
      .cols = c("TS05", "TS10", "TS25", "TS30", "TS60", "TB05", "TB10"),
      .fns = list(# 87 is at least 90% of the 96 15-minute observations per day
        min = \(.x) qc_summary(.x, min, thrsh["stemp"]),
        avg = \(.x) qc_summary(.x, mean, thrsh["stemp"]),
        max = \(.x) qc_summary(.x, max, thrsh["stemp"])),
      .groups = c("STNM", "STID", "DATE")),

    # Soil moisture
    summarize_across(
      .data = sub_daily,
      .cols = c("TR05", "TR25", "TR60", "TR75", "VW05", "VW25", "VW45"),
      .fns = list(# 44 is at least 90% of the 48 30-minute observations per day
        avg = \(.x) qc_summary(.x, mean, thrsh["smoist"])),
      .groups = c("STNM", "STID", "DATE"))
  )
  if(include_qc_variables){
    daily_qc <- list(

      # TAIR to TDEW
      summarize_across(
        .data = sub_daily,
        .cols = c("TAIR", "RELH", "WSPD", "PRES"),
        .fns = list(
          # 260 is at least 90% of the 288 5-minute observations per day
          maxo = \(.x) qc_summary(.x, max_count, thrsh["obs"]),
          mino = \(.x) qc_summary(.x, min_count, thrsh["obs"])),
        .groups = c("STNM", "STID", "DATE")),

      # WMAX and SRAD max and max_count
      summarize_across(
        .data = sub_daily,
        .cols = c("WMAX", "SRAD"),
        .fns = list(
          # 260 is at least 90% of the 288 5-minute observations per day
          max_count = \(.x) qc_summary(.x, max_count, thrsh["obs"])),
        .groups = c("STNM", "STID", "DATE")),

      # PDFQ and SDFQ
      summarize_across(
        .data = sub_daily,
        .cols = c("WDIR"),
        .fns = list(
          # 260 is at least 90% of the 288 5-minute observations per day
          pdfq = \(.x) qc_summary(.x, calc_pdir_freq, thrsh["obs"]),
          sdfq = \(.x) qc_summary(.x, calc_sdir_freq, thrsh["obs"])),
        .groups = c("STNM", "STID", "DATE")),

      # RAIN
      summarize_across(
        .data = sub_daily,
        .cols = c("RAIN"),
        .fns = list(
          # 260 is at least 90% of the 288 5-minute observations per day
          gt0 = \(.x) qc_summary(.x, gt0_count, thrsh["obs"])),
        .groups = c("STNM", "STID", "DATE")),

      # Number of bad/missing observations for TAIR through SRAD
      summarize_across(
        .data = sub_daily,
        .cols = c("TAIR", "RELH", "WSPD", "WDIR", "PRES", "WS2M",
                  "TDEW", "TA9M", "RAIN", "SRAD"),
        .fns = list(bad = \(.x) num["obs"] - sum(!is.na(.x))),
        .groups = c("STNM", "STID", "DATE")),

      # Number of bad/missing observations for soil temperature
      summarize_across(
        .data = sub_daily,
        .cols = c("TS05", "TS10", "TS25", "TS30", "TS60", "TB05", "TB10"),
        .fns = list(# Soil temperature are 15-min observations so 96 would be
          #  no missing data, 87 would be > 90% complete data:
          bad = \(.x) num["stemp"] - sum(!is.na(.x)),
          maxo = \(.x) qc_summary(.x, max_count, thrsh["stemp"]),
          mino = \(.x) qc_summary(.x, min_count, thrsh["stemp"])),
        .groups = c("STNM", "STID", "DATE")),

      # Number of bad/missing observations for soil moisture
      summarize_across(
        .data = sub_daily,
        .cols = c("TR05", "TR25", "TR60", "TR75"),
        .fns = list(# Soil moisture are 30-min observations so 48 would be
          #  no missing data
          bad = \(.x) num["smoist"] - sum(!is.na(.x))),
        .groups = c("STNM", "STID", "DATE"))
    )

    daily <- c(daily, daily_qc)

  }

  daily <- daily[!sapply(daily, is.null)]

  # Drop STNM, STID, and DATE to avoid column name duplication
  if(length(daily) > 1){
    for(i in 2:length(daily)){
      keep_cols <- grep("(^STNM$)|(^STID$)|(^DATE$)",
                        colnames(daily[[i]]),
                        invert = TRUE)
      daily[[i]] <- daily[[i]][, keep_cols, drop = FALSE]
    }
  }

  daily <-
    daily |>
    do.call(cbind.data.frame, args = _) |>
    within({
      CDEG = max(TAIR_avg - threshold_temperature,
                 units::set_units(0, "Celsius"))
      HDEG = max(threshold_temperature - TAIR_avg,
                 units::set_units(0, "Celsius"))
      # Convert mm per 5 min to mm per h
      RAIN_max = units::set_units(units::drop_units(RAIN_max)*60/5, "mm/h")
    }) |>
    standardize_column_names() |>
    standardize_column_order()

  return(daily)

}

summarize_across <- function(.data, .cols, .fns, .groups = NULL){

  if(any(!.cols %in% colnames(.data))){
    .cols[!.cols %in% colnames(.data)] |>
      paste0(collapse = ", ") |>
      paste0("The following variables were missing from dataset and will not be summarized:\n  ", .x = _) |>
      warning()
  }

  .cols <- .cols[.cols %in% colnames(.data)]

  if(any(!.groups %in% colnames(.data))){
    .groups[!.groups %in% colnames(.data)] |>
      paste0(collapse = ", ") |>
      paste0("The following grouping variables were missing from dataset and will not be used:\n  ", .x = _) |>
      warning()
  }

  .groups <- .groups[.groups %in% colnames(.data)]

  if(length(.cols) == 0) return(NULL)

  data_out <-
    .data[, .groups] |>
    unique()

  if(nrow(data_out) == 0){
    data_out <-
      data.frame(.x = NA_real_) |>
      within({.x = NULL})
  }

  row.names(data_out) <- NULL

  if(!is.list(.fns)) .fns <- list(fun = .fns)

  if(is.null(names(.fns))){
    names(.fns) <- paste0("fn", seq_along(.fns))
  }

  for(.row in 1:nrow(data_out)){
    if(is.null(.groups)){
      .data_sub <- .data
    }else{
      # add logic for subsetting data based on group columns
      .data_sub <- filter_by_groups(.data, data_out[.row, .groups])
    }
    for(.col in .cols){
      for(.fn in names(.fns)){
        .cname <- paste0(.col, "_", .fn)
        if(!.cname %in% names(data_out)){
          data_out[[.cname]] <- .fns[[.fn]](.data_sub[[.col]])
        }else{
          data_out[[.cname]][.row] <- .fns[[.fn]](.data_sub[[.col]])
        }
      }
    }
  }

  return(data_out)
}

filter_by_groups <- function(df, filter_df){
  ind_list <- vector(mode = "list",
                     length = length(filter_df))
  names(ind_list) <- names(filter_df)
  for(.i in names(filter_df)){
    ind_list[[.i]] <- df[[.i]] == filter_df[[.i]]
  }
  df[Reduce(`&`, ind_list), ]
}

qc_summary <- function(x, FUN, min_n_obs = 260, ...){

  n_obs <- sum(!is.na(x))

  if(n_obs < min_n_obs){
    value <- suppressWarnings(FUN(x, na.rm=TRUE, ...))
    value[] <- NA
  }else{
    value <- FUN(x, na.rm=TRUE, ...)
  }

  return(value)
}

max_count <- function(x, na.rm = FALSE){
  if(any(!is.na(x))){
    sum(x[!is.na(x)] == max(x, na.rm = na.rm))
  }else{
    NA_real_
  }
}

min_count <- function(x, na.rm = FALSE){
  if(any(!is.na(x))){
    sum(x[!is.na(x)] == min(x, na.rm = na.rm))
  }else{
    NA_real_
  }
}

gt0_count <- function(x, na.rm = FALSE){
  if(any(!is.na(x))){
    if("units" %in% class(x)){
      sum(units::drop_units(x) > 0, na.rm = na.rm)
    }else{
      sum(x > 0, na.rm = na.rm)
    }
  }else{
    NA_real_
  }
}

srad_sum <- function(x, na.rm = FALSE, interval = as.difftime(1, units = "days")){
  if(any(!is.na(x))){
    mean(units::set_units(x, "megajoule/m2/d"), na.rm = na.rm)*units::as_units(interval)
  }else{
    units::set_units(NA_real_, "megajoule/m2/d")*units::as_units(interval)
  }
}

units_sd <- function(x, na.rm = FALSE){
  units::keep_units(sd, x, na.rm = na.rm)
}

rename_daily_columns <- function(col_names){

  replacements <- c("TAIR_" = "T",
                    "RELH_" = "H",
                    "TDEW_" = "D",
                    "PRES_" = "P",
                    "WS2M_" = "2",
                    "TA9M_" = "9",
                    "WSPD_m.(.)" = "WSM\\1",
                    "WSPD_avg" = "WSPD",
                    "WDIR_bad" = "IBAD",
                    "RAIN_" = "R",
                    "SRAD_" = "A",
                    "TB05_avg" = "B5AV",
                    "TB05_(.).(.)" = "B5\\1\\2",
                    "TB10_" = "B",
                    "TS05_avg" = "S5AV",
                    "TS05_(.).(.)$" = "S5\\1\\2",
                    "T(S3)0_avg" = "\\1AV",
                    "T(S3)0_(.).(.)$" = "\\1\\2\\3",
                    "T(R|S)([0-9]{2})_bad" = "\\1\\2BD",
                    "T(R|S)([0-9]{2})_avg" = "\\1\\2AV",
                    "T(R|S)([0-9]{2})_(.).(.)$" = "\\1\\2\\3\\4",
                    "T(R|S)([0-9]{2})_..(.)o$" = "\\1\\2\\3O")

  for(i in seq_along(replacements)){
    col_names <- gsub(names(replacements)[i], replacements[i], col_names)
  }

  col_names |>
    toupper()
}

wdir_to_cardinal <- function(wdir){

  cdir <- c("N", "NNE", "NE", "ENE",
            "E", "ESE", "SE", "SSE",
            "S", "SSW", "SW", "WSW",
            "W", "WNW", "NW", "NNW")

  increment <- 360/length(cdir)

  wdir_tmp <- units::drop_units(wdir) + increment/2
  wdir_tmp[wdir_tmp >= 360] <- wdir_tmp[wdir_tmp >= 360] - 360

  cdir[floor(wdir_tmp / increment + 1)]

}

calc_pdir <- function(wdir, na.rm = TRUE){
  if(na.rm) wdir <- wdir[!is.na(wdir)]
  wdir_to_cardinal(wdir) |>
    table() |>
    sort(decreasing = TRUE) |>
    names() |>
    head(1)
}

calc_pdir_freq <- function(wdir, na.rm = TRUE){

  if(na.rm) wdir <- wdir[!is.na(wdir)]

  wdir_tab <-
    wdir_to_cardinal(wdir) |>
    table() |>
    sort(decreasing = TRUE)

  units::set_units(wdir_tab[1]/sum(wdir_tab)*100, "percent")
}

calc_sdir_freq <- function(wdir, na.rm = TRUE){

  if(na.rm) wdir <- wdir[!is.na(wdir)]

  wdir_tab <-
    wdir_to_cardinal(wdir) |>
    table() |>
    sort(decreasing = TRUE)

  if(length(wdir_tab) > 1){
    return(units::set_units(wdir_tab[2]/sum(wdir_tab)*100, "percent"))
  }else{
    return(units::set_units(wdir_tab[1]/sum(wdir_tab)*100, "percent"))
  }
}

calc_sdir <- function(wdir, na.rm = TRUE){
  if(na.rm) wdir <- wdir[!is.na(wdir)]
  wdir_to_cardinal(wdir) |>
    table() |>
    sort(decreasing = TRUE) |>
    names() |>
    head(2) |>
    tail(1)
}

round_date <- function(time, interval, tz = ""){

  if(tz == "") tz <- attr(time, "tzone")

  if(interval == as.difftime(1, units = "days")){
    t_offset <- c(as.POSIXct("1970-01-01", tz = attr(time, "tzone")),
                  as.POSIXct("1970-01-01", tz = tz)) |>
      diff()
    time_out <- as.POSIXct(as.Date(time, tz = tz), tz = tz) + t_offset
  }else{
    int_dbl <- as.double(interval, units = "secs")
    time_out <- as.POSIXct((as.numeric(time) %/% int_dbl + 1)*int_dbl, tz = tz)
  }

  return(time_out)
}
