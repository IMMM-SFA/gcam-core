# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LB104.load_curve_enduse_HDDCDD_USA
#'
#' Use HDDCDD to calculate buildings heating and cooling demand profiles.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L104.HistoricalDD_S_Segment_gcamusa}, \code{L104.DD_S_Segment_all_gcamusa},
#' The corresponding file in the original data system was \code{LA104.load_curves_enduse_HDDCDD.R} (gcamusa dispatch level1).
#' @details Use HDDCDD to calculate buildings heating and cooling demand profiles.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select top_n
#' @importFrom tidyr gather spread
#' @author YO May 2020
module_gcamusa_LB104.load_curve_enduse_HDDCDD_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             "L102.load_segments_gcamusa",
             "L102.date_load_curve_mapping_S_gcamusa",
             OPTIONAL_FILE = "gcam-usa/dispatch/DD_prima45_prima85"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L104.HistoricalDD_S_Segment_gcamusa",
             "L104.DD_S_Segment_all_gcamusa"))
  } else if(command == driver.MAKE) {

    date_time <- date <- year <- month <- day <- hour <- state_name <-
      state <- value <- rcp <- monthName <- n <-
      wt <- cdd_super_peak <- is_super_peak_final <- segment <- HDD <-
      CDD <- grid_region <- day_night <-
      hours <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    L102.load_segments <- get_data(all_data, "L102.load_segments_gcamusa")
    L102.date_load_curve_mapping_S <- get_data(all_data, "L102.date_load_curve_mapping_S_gcamusa")
    DD_prima45_prima85_raw <- get_data(all_data, "gcam-usa/dispatch/DD_prima45_prima85")

    # If the raw PRIMA datasets are available, go through the full computations below
    # If not, use the pre-saved summary file (i.e., the output of this chunk!)
    if(is.null(DD_prima45_prima85_raw)) {
      # PRIMA datasets are not available, so used prebuilt outputs
      L104.HistoricalDD_S_Segment_gcamusa <- prebuilt_data("L104.HistoricalDD_S_Segment_gcamusa")
      L104.DD_S_Segment_all_gcamusa <- prebuilt_data("L104.DD_S_Segment_all_gcamusa")
    } else {

      DD_prima45_prima85_raw %>%
        mutate(date_time = as.POSIXct(date_time, tz="GMT", format="%Y-%m-%d_%H:%M:%S"),
               date = as.POSIXct(format(date_time, tz="EST",format="%Y-%m-%d_%H:%M:%S",usetz=FALSE),tz="EST",format="%Y-%m-%d_%H:%M:%S"),
               year = as.integer(format(date, "%Y")),
               month = as.integer(format(date, "%m")),
               day = as.integer(format(date, "%d")),
               hour = as.integer(format(date, "%H"))) %>%
        rename(state_name = state) %>%
        left_join(select(states_subregions, state, state_name), by = "state_name") %>%
        select(state, date, value, rcp, year, month, day, hour) %>%
        filter(!is.na(state))%>%
        mutate(value = if_else(abs(value) > gcamusa.DEGREE_HOUR_CUTOFF, value, 0.0)) %>%
        tibble::as_tibble() ->
        DD_EST

      # Adjusting DD to make sure we have a complete dataset for the relevant years
      # Remove leap year data for February 29th.
      # Remove non-model years
      DD_EST %>%
        filter(!(month==2 & day==29),
               year %in% c(2000,2005,2010, MODEL_FUTURE_YEARS)) ->
        DD_EST_noLeap_modelYears

      # After converting timezones from GMT to EST we lose the last few hours of Dec 30 (hours 19 to 23)
      # Copy these hours from previous day Dec 29th 2100
      DD_EST_noLeap_modelYears %>%
        filter(year==2100,month==12,day==29, hour %in% c(19:23))%>%
        mutate(day=30)->
        DD_EST_noLeap_modelYears_30Dec2100_MissingYears

      bind_rows(DD_EST_noLeap_modelYears,DD_EST_noLeap_modelYears_30Dec2100_MissingYears)->
        DD_EST_noLeap_modelYears_30Dec2100_Complete

      # PRIMA dataset also did not have Dec 31st for 2100
      # Copy Dec 31st from Dec 30th
      DD_EST_noLeap_modelYears_30Dec2100_Complete %>%
        filter(year==2100,month==12,day==30)%>%
        mutate(day=31)->
        DD_EST_noLeap_modelYears_31Dec2100

      # Combine to create processed DD dataframe
      bind_rows(DD_EST_noLeap_modelYears_30Dec2100_Complete,DD_EST_noLeap_modelYears_31Dec2100)->
        DD

      # Get day and hour for mapping
      L102.date_load_curve_mapping_S %>%
        mutate(date = as.POSIXct(date, tz="EST", format="%Y-%m-%d %H:%M:%S"),
               date = as.POSIXct(format(date, tz="EST",format="%Y-%m-%d_%H:%M:%S",usetz=FALSE), tz="EST",format="%Y-%m-%d_%H:%M:%S"),
               hour = as.integer(format(date, "%H")),
               monthName = month,
               month = as.integer(format(date, "%m")),
               day = as.integer(format(date, "%d"))) %>%
        select(-date,-monthName) %>%
        tibble::as_tibble() ->
        L102.date_load_curve_mapping_S_full

      monthAssign = tibble::tibble(month=c(1:12), monthName=month.abb)

      DD %>%
        left_join_error_no_match(L102.date_load_curve_mapping_S_full, by=c("state", "month", "day", "hour")) %>%
        left_join_error_no_match(monthAssign, by=c("month")) ->
        DD_fixMissing

      DD_fixMissing %>%
        ungroup() %>%
        group_by(state,rcp,year) %>%
        top_n(n=-gcamusa.ELEC_SUPERPEAK_HRS,wt=value) %>%
        mutate(cdd_super_peak=TRUE)%>%
        ungroup()-> DD_top10

      DD_fixMissing %>%
        mutate(cdd_super_peak=FALSE) %>%
        bind_rows(DD_top10) -> DD_cdd_superpeak

      DD_cdd_superpeak %>%
        mutate(is_super_peak_final = if_else((year %in% HISTORICAL_YEARS), is_super_peak, cdd_super_peak)) -> DD_superpeak

      DD_superpeak %>%
        mutate(segment = if_else(is_super_peak_final, gcamusa.ELEC_SEGMENT_SUPERPEAK, paste(monthName, day_night, sep=gcamusa.SEGMENT_DELIM)),
               HDD = if_else(value > 0.0, value, 0.0),
               CDD = if_else(value < 0.0, -value, 0.0)) %>%
        group_by(state, segment, rcp, year) %>%
        summarize(HDD = sum(HDD),
                  CDD = sum(CDD)) %>%
        ungroup() ->
        L104.DD_S_Segment_noHIAK

      # TODO: what to do with these?
      # http://www.rssweather.com/climate/Hawaii/Honolulu/
      L104.HI <- tibble(state="HI", grid_region="Hawaii grid", month=gcamusa.MONTH_ORDER, high=c(80.4, 80.7, 81.7, 83.1, 84.9, 86.9, 87.8, 88.9, 88.9, 87.2, 84.3, 81.7), low=c(65.7, 65.4, 66.9, 68.2, 69.6, 72.1, 73.8, 74.7, 74.2, 73.2, 71.1, 67.8))
      # http://www.rssweather.com/climate/Alaska/Anchorage/
      L104.AK <- tibble(state="AK", grid_region="Alaska grid", month=gcamusa.MONTH_ORDER, high=c(22.2, 25.8, 33.6, 43.9, 54.9, 62.3, 66.3, 63.3, 55.0, 40.0, 27.7, 23.7), low=c(9.3, 11.7, 18.2, 28.7, 38.9, 47.0, 51.5, 49.4, 41.4, 28.3, 15.9, 11.4))
      bind_rows(L104.HI, L104.AK) %>%
        gather(day_night, value, high:low) %>%
        mutate(day_night = if_else(day_night == "high", "day", "night"),
               segment = paste(month, day_night, sep="_"),
               value = 66.0 - value,
               HDD = if_else(value > 0.0, value, 0.0),
               CDD = if_else(value < 0.0, -value, 0.0)) %>%
        select(state, grid_region, segment, HDD, CDD) ->
        L104.HI_AK

      # Repeat for all years and rcps in L104.DD_S_Segment
      L104.HI_AK_years <- cbind(L104.HI_AK, year = rep(unique(DD_superpeak$year),
                                                       each = nrow(L104.HI_AK)))

      L104.HI_AK_full <- cbind(L104.HI_AK_years, rcp = rep(unique(DD_superpeak$rcp),
                                                           each = nrow(L104.HI_AK_years))) %>%
        mutate(rcp = as.character(rcp))

      L104.HI_AK_full %>%
        group_by(state, grid_region, year, rcp) %>%
        summarize(HDD=max(HDD)*1.1,
                  CDD=max(CDD)*1.1) %>%
        mutate(segment = gcamusa.ELEC_SEGMENT_SUPERPEAK,
               HDD = if_else(state == "HI", 0, HDD),
               CDD = if_else(state == "AK", 0, CDD)) %>%
        bind_rows(L104.HI_AK_full) %>%
        left_join(L102.load_segments, by=c("grid_region","segment")) %>%
        mutate(HDD = HDD * hours,
               CDD = CDD * hours) %>%
        ungroup() %>%
        select(names(L104.DD_S_Segment_noHIAK)) ->
        L104.HI_AK_hddcdd

      L104.DD_S_Segment_noHIAK %>%
        bind_rows(L104.HI_AK_hddcdd) ->
        L104.DD_S_Segment_all

      L104.DD_S_Segment_all %>%
        filter (rcp == "rcp8.5",
                year == MODEL_FINAL_BASE_YEAR) %>%
        select (-rcp,-year) ->
        L104.HistoricalDD_S_Segment

    # Produce outputs
      L104.HistoricalDD_S_Segment %>%
      add_title("HDD/CDD by state and load segment for base year") %>%
      add_units("degree-day") %>%
      add_comments("HDD/CDD by state and load segment for base year") %>%
      add_legacy_name("L104.HistoricalDD_S_Segment") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L102.load_segments_gcamusa",
                     "L102.date_load_curve_mapping_S_gcamusa",
                     "gcam-usa/dispatch/DD_prima45_prima85") ->
        L104.HistoricalDD_S_Segment_gcamusa

      L104.DD_S_Segment_all %>%
        add_title("HDD/CDD by state, load segment, rcp and year") %>%
        add_units("degree-day") %>%
        add_comments("HDD/CDD by state, load segment, rcp and year") %>%
        add_legacy_name("L104.DD_S_Segment_all") %>%
        same_precursors_as("L104.HistoricalDD_S_Segment_gcamusa")->
        L104.DD_S_Segment_all_gcamusa



    verify_identical_prebuilt(L104.HistoricalDD_S_Segment_gcamusa,
                              L104.DD_S_Segment_all_gcamusa)
    }

    return_data(L104.HistoricalDD_S_Segment_gcamusa, L104.DD_S_Segment_all_gcamusa)
  } else {
    stop("Unknown command")
  }
}
