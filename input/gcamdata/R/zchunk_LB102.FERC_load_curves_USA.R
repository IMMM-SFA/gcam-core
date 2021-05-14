# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LB102.FERC_load_curves_USA
#'
#' Process FERC hours data into load segments by grid regions for dispatch model.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L102.date_load_curve_mapping_S_gcamusa}, \code{L102.load_segments_gcamusa},
#' \code{L102.invest_segments_gcamusa}. The corresponding file in the
#' original data system was \code{LA102.FERC_load_curves.R} (gcamusa dispatch level1).
#' @details Compute load curve related parameters for investment and disparch segments
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select nth recode
#' @importFrom tidyr gather spread
#' @author PLP YO May 2020
module_gcamusa_LB102.FERC_load_curves_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/states_coordinates",
             FILE = "gcam-usa/dispatch/Respondent_IDs_fix_mismatch",
             FILE = "gcam-usa/dispatch/eia_operators_nerc_region_mapping",
             OPTIONAL_FILE = "gcam-usa/dispatch/FERC_hourly_gen"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L102.date_load_curve_mapping_S_gcamusa",
             "L102.load_segments_gcamusa",
             "L102.invest_segments_gcamusa"))
  } else if(command == driver.MAKE) {

    report_yr <- hour <- generation <- date <- NERC.Region <- rel_gen <- state <-
      state_name <- lat <- lon <- region_coord <- day_night <-
      sunrise <- sunset <- month <- area <- grid_region <-
      is_super_peak <- hours <- generation.fraction <-
      relative.generation <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    states_coordinates <- get_data(all_data, "gcam-usa/states_coordinates")
    ferc_resp_eia_code <- get_data(all_data, "gcam-usa/dispatch/Respondent_IDs_fix_mismatch")
    eia_operators_nerc_region_mapping <- get_data(all_data, "gcam-usa/dispatch/eia_operators_nerc_region_mapping")
    FERC_hourly_gen_raw <- get_data(all_data, "gcam-usa/dispatch/FERC_hourly_gen")


    # -----------------------------------------------------------------------------
    # Perform computations

    # If the large raw FERC datasets are available, go through the full computations below
    # If not, use the pre-built outputs
    if(is.null(FERC_hourly_gen_raw)) {
      # FERC hourly data are not available, so used saved outputs
      L102.date_load_curve_mapping_S_gcamusa <- prebuilt_data("L102.date_load_curve_mapping_S_gcamusa")
      L102.load_segments_gcamusa <- prebuilt_data("L102.load_segments_gcamusa")
      L102.invest_segments_gcamusa <- prebuilt_data("L102.invest_segments_gcamusa")
    } else {

      # We will generate load curve shapes from just the final calibration year
      # for simplicity
      FERC_hourly_gen_raw %>%
        filter(report_yr == MODEL_FINAL_BASE_YEAR) %>%
        gather(hour, generation, matches("^hour..$")) %>%
        filter(generation > 0) %>%
        mutate(hour = as.integer(gsub('hour', '', hour))) %>%
        # TODO: day light savings?
        filter(hour != 25) %>%
        # convert date and hour into a single date-time column (note this process is relatively slow)
        mutate(date = as.POSIXct(plan_date, format="%m/%d/%y %H:%M", tz="EST") + ((hour-1) * 60 * 60)) %>%
        select(respondent_id, date, generation) ->
        FERC_hourly_gen

      # map respondents / operators to NERC regions and aggregate
      FERC_hourly_gen %>%
        left_join_error_no_match(ferc_resp_eia_code, by = "respondent_id") %>%
        # TODO: this should be ljerno but there seem to be conflicting Operator.ID -> NERC.Region
        # mappings in eia_operators_nerc_region_mapping; look into this
        left_join(eia_operators_nerc_region_mapping, by = c("eia_code" = "Operator.ID")) %>%
        filter(generation > 0) %>%
        group_by(NERC.Region, date) %>%
        summarize(generation=sum(generation)) %>%
        ungroup() ->
        NERC_hourly_gen

      if(MODEL_FINAL_BASE_YEAR == 2010) {
        # Data error is way off base and throws things off, manualy reset it to something resasonable
        NERC_hourly_gen[NERC_hourly_gen$NERC.Region == "ASCC" & NERC_hourly_gen$date == as.POSIXct("2010-04-01 00:00:00", tz="EST"), "generation"] <- 270.0
        # NOTE: the following hours were missing:
        # ASCC 2010-03-14 00:00:00   Mar     night         NA      NA
        # ASCC 2010-03-14 01:00:00   Mar     night         NA      NA
        # ASCC 2010-11-07 00:00:00   Nov     night         NA      NA
        # Manually resetting values by checking generation in the surrounding hours
        NERC_hourly_gen %<>%
          bind_rows(tibble(NERC.Region = "ASCC",
                           date = as.POSIXct(c("2010-03-14 00:00:00", "2010-03-14 01:00:00", "2010-11-07 00:00:00"), tz="EST"),
                           generation = c(300.0, 290.0, 280.0)))
      }

      # Since our Grid regions do not perfectly align with NERC regions we will now switch to
      # "relative geneartion" where the date with the highest generation is given 1 and all
      # other date/hours are relative to that.
      NERC_hourly_gen %>%
        group_by(NERC.Region) %>%
        mutate(rel_gen = percent_rank(generation)) %>%
        ungroup() ->
        NERC_hourly_rel_gen

      # We want to partition hours into day/night.  To do that we will calculate sunrise and
      # sunset trigonmically.
      # To do that we will need to explicitly use some lat/long
      # to use, so we have read in the "center" of each state more or less
      states_subregions %>%
        select(state, state_name, NERC.Region) %>%
        left_join_error_no_match(states_coordinates, by=c("state_name")) %>%
        select(-state_name) ->
        region_coord

      # we also need this by the grid region so we need to aggregate the
      # state lat/long
      # TODO: is taking the mean the "right" thing to do do aggregate them?
      region_coord %>%
        group_by(NERC.Region) %>%
        summarize(lat = mean(lat), lon = mean(lon)) %>%
        ungroup() %>%
        mutate(state = NERC.Region) %>%
        bind_rows(region_coord) ->
        region_coord

      # https://en.wikipedia.org/wiki/Sunrise_equation
      calc_day_night <- function(d) {
        d %>%
          mutate(n = as.integer(difftime(date, as.POSIXct(paste0(MODEL_FINAL_BASE_YEAR, "-01-01 00:00:00"), tz="EST"), units="days"))) ->
          d
        J_star <- d$n - d$lon / 360.0
        M <- (357.5291 + 0.98560028 * J_star) %% 360.0
        C <- 1.9148 * sin(M * pi/180) + 0.0200 * sin(2*M * pi/180) + 0.0003 * sin(3*M * pi/180)
        lamda <- (M + C + 180 + 102.9372) %% 360.0
        J_transit <- 0.5 + J_star + 0.0053 * sin(M * pi/180) - 0.0069 * sin(2*lamda * pi/180)
        sin_delta <- sin(lamda * pi/180) * sin(23.44 * pi/180)
        omega <- acos((sin(-0.83 * pi/180) - sin(d$lat * pi/180) * sin_delta) / (cos(d$lat *pi/180) * sqrt(1 - sin_delta^2))) * 180/pi

        J_transit <- d$n + 0.5
        J_set <- J_transit + omega / 360.0
        J_rise <- J_transit - omega / 360.0

        d %>%
          mutate(sunrise = as.POSIXct(paste0(MODEL_FINAL_BASE_YEAR, "-01-01 00:00:00"), tz="EST") + J_rise * 24 * 60 * 60) %>%
          mutate(sunset = as.POSIXct(paste0(MODEL_FINAL_BASE_YEAR, "-01-01 00:00:00"), tz="EST") + J_set * 24 * 60 * 60)
      }

      # create mappings from hours into day/night and month
      region_coord %>%
        expand(., ., tibble(date = as.POSIXct(paste0(MODEL_FINAL_BASE_YEAR, "-01-01 00:00:00"), tz="EST") + ((seq(1:8760) - 1) * 60 * 60))) %>%
        calc_day_night() %>%
        mutate(day_night = if_else(sunrise <= date & date < sunset, "day", "night")) %>%
        mutate(month = format(date, format="%b")) %>%
        select(state, NERC.Region, date, month, day_night) ->
        date_load_curve_mapping

      # splice out the states for output which will be needed in other chunks such as to
      # calculate segment specific capacity factors for renewables
      date_load_curve_mapping %>%
        filter(state != NERC.Region) ->
        L102.date_load_curve_mapping_S

      # reclassify the top N (assumption in gcamusa.ELEC_SUPERPEAK_HRS) hours of
      # generation as in the "super peak" segment instead of of the month + day/night
      # that they were in
      date_load_curve_mapping %>%
        filter(state == NERC.Region) %>%
        select(-state) %>%
        left_join_error_no_match(NERC_hourly_rel_gen, by = c("NERC.Region", "date")) %>%
        group_by(NERC.Region) %>%
        arrange(desc(rel_gen)) %>%
        mutate(is_super_peak = rel_gen >= nth(rel_gen, gcamusa.ELEC_SUPERPEAK_HRS)) ->
        find_super_peak

      # aggregate hourly data into montly day/night keeping the number of hours in the
      # segment, the average load of the segment, and fraction of total generation
      find_super_peak %>%
        mutate(segment = if_else(is_super_peak, gcamusa.ELEC_SEGMENT_SUPERPEAK, paste(month, day_night, sep=gcamusa.SEGMENT_DELIM))) %>%
        group_by(NERC.Region, segment) %>%
        summarize(hours = n(), generation = sum(generation)) %>%
        # Note still group_by NERC.Region at this point
        mutate(generation.fraction = generation / sum(generation)) %>%
        mutate(generation = generation / hours) %>%
        mutate(relative.generation = generation / max(generation)) %>%
        select(-generation) %>%
        ungroup() ->
        NERC_segments

      # NERC regions are coarser than our grid regions so we will copy the normalized
      # load curve to our grid rid regions that share a NERC region.
      states_subregions %>%
        select(grid_region, NERC.Region) %>%
        distinct() %>%
        # using left_join here as we are expanding rows so as to copy
        # the load curves to grid regions which share a NERC region
        left_join(NERC_segments, by=c("NERC.Region")) %>%
        select(-NERC.Region) ->
        L102.load_segments

      # We need to go back and update the state level segment matching to include
      # which hours got flagged as superpeak in that state's corresponding NERC region
      L102.date_load_curve_mapping_S %<>%
        left_join_error_no_match(select(find_super_peak, NERC.Region, date, is_super_peak), by=c("NERC.Region", "date")) %>%
        select(-NERC.Region)

      # Now calculate the shape of the investment sector curves
      # We have chosen some breaks as assumptions to bin hours based off of percentile load
      # in that hour.  We will then summarize the hourly data to count: the number of hours
      # in each bin, total area in terms of generation, and the fraction of total generation
      # which is in each bin.
      find_super_peak %>% mutate(invest_segment = cut(rel_gen, breaks=gcamusa.ELEC_INV_SHAPE)) %>%
        group_by(NERC.Region, invest_segment) %>%
        summarize(hours=n(), generation=mean(generation)) %>%
        # Note still grouped by NERC.Region
        arrange(desc(invest_segment)) %>%
        mutate(hours=cumsum(hours)) %>%
        mutate(area=generation * hours) %>%
        mutate(generation.fraction = area / sum(area)) %>%
        ungroup() ->
        L102.invest_segments_NERC

      # The bins names are generated automatically by dplyr using the cut off ranges,
      # recode them back to the investment sector names
      elec_inv_names_recode <- gcamusa.ELEC_INV_NAMES
      L102.invest_segments_NERC %>%
        pull(invest_segment) %>%
        unique() ->
        names(elec_inv_names_recode)
      L102.invest_segments_NERC %>%
        mutate(invest_segment = recode(invest_segment, !!! elec_inv_names_recode)) ->
        L102.invest_segments_NERC

      # Again NERC regions are coarser than our grid regions so we will copy the
      # investment curve to our grid rid regions that share a NERC region.
      states_subregions %>%
        select(grid_region, NERC.Region) %>%
        distinct() %>%
        # using left_join here as we are expanding rows so as to copy
        # the investment curves to grid regions which share a NERC region
        left_join(L102.invest_segments_NERC, by=c("NERC.Region")) %>%
        select(-NERC.Region) ->
        L102.invest_segments


      # -----------------------------------------------------------------------------
      # Produce outputs

      L102.date_load_curve_mapping_S %>%
        add_title("A state by state mapping from date+time to load segment") %>%
        add_units("NA") %>%
        add_comments("A state by state mapping from date+time to load segment") %>%
        add_legacy_name("L102.date_load_curve_mapping_S") %>%
        add_precursors("gcam-usa/states_subregions",
                       "gcam-usa/states_coordinates") ->
        L102.date_load_curve_mapping_S_gcamusa

      L102.load_segments %>%
        add_title("Defines the relative shape of each load segment by grid_region") %>%
        add_units("hours / % / rank") %>%
        add_comments("Defines the relative shape of each load segment by grid_region") %>%
        add_legacy_name("L102.load_segments") %>%
        add_precursors("gcam-usa/states_subregions",
                       "gcam-usa/states_coordinates",
                       "gcam-usa/dispatch/Respondent_IDs_fix_mismatch",
                       "gcam-usa/dispatch/eia_operators_nerc_region_mapping",
                       "gcam-usa/dispatch/FERC_hourly_gen") ->
        L102.load_segments_gcamusa

      L102.invest_segments %>%
        add_title("Defines the relative shape of each investment segment by grid_region") %>%
        add_units("hours / MW / MWh / %") %>%
        add_comments("Defines the relative shape of each investment segment by grid_region") %>%
        add_legacy_name("L102.invest_segments") %>%
        same_precursors_as("L102.load_segments")->
        L102.invest_segments_gcamusa

      verify_identical_prebuilt(L102.date_load_curve_mapping_S_gcamusa,
                                L102.load_segments_gcamusa,
                                L102.invest_segments_gcamusa)
    }

    return_data(L102.date_load_curve_mapping_S_gcamusa, L102.load_segments_gcamusa, L102.invest_segments_gcamusa)
  } else {
    stop("Unknown command")
  }
}
