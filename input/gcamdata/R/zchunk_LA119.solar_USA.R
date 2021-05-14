# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LA119.solar
#'
#' Compute scalars by state to vary capacity factors by state.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:
#' \code{L119.CapacityFactor_CSP_state_gcamusa}, \code{L119.CapacityFactor_CSP_state_segment_gcamusa},
#' \code{L119.CapacityFactor_PV_state_gcamusa}, \code{L119.CapacityFactor_PV_state_segment_gcamusa},
#' The corresponding file in the original data system was \code{LA119.Solar.R} (gcam-usa level1).
#' @details This chunk computes capacity factors for central station PV and CSP technologies by state.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter if_else group_by mutate select summarise summarise_at vars
#' @importFrom tidyr gather spread
#' @author GI, FF, AS Apr 2017 / YO Jun 2020
module_gcamusa_LA119.solar <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/season_month_mapping",
             FILE = "gcam-usa/NREL_us_re_capacity_factors",
             "L102.date_load_curve_mapping_S_gcamusa",
             FILE = "gcam-usa/reeds_regions_states",
             FILE = "gcam-usa/dispatch/ReEDS_segment_hours",
             FILE = "gcam-usa/dispatch/UPV_CFt",
             FILE = "gcam-usa/dispatch/CSP_NoStor_CFts"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L119.CapacityFactor_PV_state_gcamusa",
             "L119.CapacityFactor_CSP_state_gcamusa",
             "L119.CapacityFactor_PV_state_segment_gcamusa",
             "L119.CapacityFactor_CSP_state_segment_gcamusa"))
  } else if(command == driver.MAKE) {

    fuel <- value <- State <- . <- value.x <- value.y <- sector <- scaler <-
        state <- state_name <- NULL     # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    season_month_mapping <- get_data(all_data, "gcam-usa/season_month_mapping")
    NREL_us_re_capacity_factors <- get_data(all_data, "gcam-usa/NREL_us_re_capacity_factors")
    L102.date_load_curve_mapping_S <- get_data(all_data, "L102.date_load_curve_mapping_S_gcamusa")


    ReEDS_region_mapping_raw <- get_data(all_data, "gcam-usa/reeds_regions_states")
    ReEDS_segment_hours <- get_data(all_data, "gcam-usa/dispatch/ReEDS_segment_hours")
    PV_cf_raw <- get_data(all_data, "gcam-usa/dispatch/UPV_CFt")
    CSP_cf_raw <- get_data(all_data, "gcam-usa/dispatch/CSP_NoStor_CFts")

    # ===================================================
    # Create scalers to scale capacity factors read in the assumptions file in the energy folder.
    # These scalers will then be used to create capacity factors by state.
    # The idea is to vary capacity factors for solar technologies by state depending on the varying solar irradiance by state.

    # Converting NREL_us_re_capacity_factors to long-form and removing read-in value for the average
    NREL_us_re_capacity_factors %>%
      gather(fuel, value, -State) %>%
      filter(State != "Average") ->
      NREL_us_re_capacity_factors_longform

    # Calculate average capacity factor by fuel (not including the 0 capacity factors)
    NREL_us_re_capacity_factors_longform %>%
      group_by(fuel) %>%
      summarise_at(vars(value), list(~ mean(.[. != 0]))) -> # Average does not include 0 capacity factors
      Capacityfactor_average

    # Creating scalers by state by dividing capacity factor by the average
    # Using state name abbreviations instead of full names
    NREL_us_re_capacity_factors_longform %>%
      left_join_error_no_match(Capacityfactor_average, by = "fuel") %>%
      mutate(scaler = value.x / value.y, sector = "electricity generation") %>%
      select(State, sector, fuel, scaler) %>%
      left_join_error_no_match(
        select(states_subregions, state, state_name),
        by = c("State" = "state_name")) %>% # Need to rename to match with base dataframe
      select(state, sector, fuel, scaler, -State) -> # Removing full state name
      Capacityfactors_scaled

    # Creating solar PV table by using Urban_Utility_scale_PV fuel values
    Capacityfactors_scaled %>%
      filter(fuel == "Urban_Utility_scale_PV") %>%
      mutate(fuel = "solar PV") ->
      L119.CapFacScaler_PV_state

    # Creating solar CSP table by using CSP fuel values
    Capacityfactors_scaled %>%
      filter(fuel == "CSP") %>%
      mutate(fuel = "solar CSP") %>%
      # Null CSP capacity factor implies that CSP is not suitable in the state.
      # Set capacity factor to small number (0.001) to prevent divide by 0 error in GCAM.
      mutate(scaler = if_else(scaler > 0, scaler, 0.001)) ->
      L119.CapFacScaler_CSP_state

    # Updated capacity factor from REEDS data and process by state and segment for PV
    PV_cf_raw %>%
      gather("hour", "capacity_factor", dplyr::matches("[0-9]+")) %>%
      mutate(hour = as.integer(hour)) %>%
      mutate(date = as.POSIXct(paste0(MODEL_FINAL_BASE_YEAR, "-01-01 00:00:00"), tz="EST") + ((hour - 1) * 60 * 60)) %>%
      rename(PV_class = `PV class`) ->
      PV_cf

    ReEDS_region_mapping_raw %>%
      filter(Country == "USA") %>%
      select(BA, State) %>%
      rename(state = State) ->
      ReEDS_region_mapping

    L102.date_load_curve_mapping_S$date <- as.POSIXct(L102.date_load_curve_mapping_S$date, tz="EST")

    PV_cf %>%
      left_join_error_no_match(ReEDS_region_mapping %>% distinct(), by=c("BA")) %>%
      left_join_error_no_match(L102.date_load_curve_mapping_S, by=c("state", "date")) ->
      PV_cf

    # For each state, find the PV class that has the highest annual average capacity factor
    # We'll use this class to calculate segment capacity factors, to be consistent with our
    # approach in module_gcamusa_L2238.PV_reeds_USA and the annual capacity factors
    # we use elsewhere in the model.  Note that with our new capacity factor -based resource curves,
    # these segment capacity factors will be reduced over time as more capacity is deployed using
    # lower quality resources.
    PV_cf %>%
      group_by(state, PV_class) %>%
      summarize(capacity.factor = mean(capacity_factor)) %>%
      ungroup() %>%
      group_by(state) %>%
      filter(capacity.factor == max(capacity.factor)) ->
      PV_cf_state_class_annual

    PV_cf %>%
      mutate(segment = if_else(is_super_peak, gcamusa.ELEC_SEGMENT_SUPERPEAK,
                               paste(month, day_night, sep=gcamusa.SEGMENT_DELIM))) %>%
      semi_join(PV_cf_state_class_annual, by = c("state", "PV_class")) %>%
      group_by(state, segment) %>%
      summarize(capacity.factor = mean(capacity_factor)) %>%
      ungroup() ->
      L119.CapacityFactor_PV_state_segment

    PV_cf %>%
      semi_join(PV_cf_state_class_annual, by = c("state", "PV_class")) %>%
      group_by(state) %>%
      summarize(capacity.factor = mean(capacity_factor)) %>%
      ungroup() ->
      L119.CapacityFactor_PV_state

    # fill missing states (AK, HI, DC) from the ReEDS dataset by using national average
    # by segement adjusted by the old dataset's scaler
    L119.CapacityFactor_PV_state_segment %>%
      group_by(segment) %>%
      summarize(capacity.factor = mean(capacity.factor)) %>%
      expand(., ., L119.CapFacScaler_PV_state %>%
               filter(!(state %in% unique(L119.CapacityFactor_PV_state_segment$state)))) %>%
      mutate(capacity.factor = capacity.factor * scaler) %>%
      select(state, segment, capacity.factor) %>%
      bind_rows(L119.CapacityFactor_PV_state_segment, .) ->
      L119.CapacityFactor_PV_state_segment

    L119.CapFacScaler_PV_state %>%
      filter(!(state %in% unique(L119.CapacityFactor_PV_state$state))) %>%
      mutate(capacity.factor = mean(L119.CapacityFactor_PV_state$capacity.factor) * scaler) %>%
      select(state, capacity.factor) %>%
      bind_rows(L119.CapacityFactor_PV_state, .) ->
      L119.CapacityFactor_PV_state

    # Updated capacity factor from REEDS data and process by state and segment for CSP

    ReEDS_segment_hours %>%
      # TODO: create a mapping file?
      mutate(day_night = case_when(
        `time of day` == "10PM-6AM" ~ "night",
        `time of day` == "6AM-1PM" ~ "day",
        `time of day` == "1PM-5PM" ~ "day",
        `time of day` == "5PM-10PM" ~ "night",
        TRUE ~ gcamusa.ELEC_SEGMENT_SUPERPEAK)) %>%
      left_join(season_month_mapping, ., by=c("season")) %>%
      mutate(segment = if_else(day_night == gcamusa.ELEC_SEGMENT_SUPERPEAK, gcamusa.ELEC_SEGMENT_SUPERPEAK, paste(month, day_night, sep=gcamusa.SEGMENT_DELIM))) %>%
      select(timeslice, segment, hours) %>%
      distinct() ->
      ReEDS_timeslice_mapping

    CSP_cf_raw %>%
      left_join(ReEDS_timeslice_mapping, by = c("timeslice")) ->
      CSP_cf

    # For each state, find the CSP class that has the highest annual average capacity factor
    # We'll use this class to calculate segment capacity factors, to be consistent with our
    # approach in module_gcamusa_L2239.CSP_reeds_USA and the annual capacity factors
    # we use elsewhere in the model.  Note that with our new capacity factor -based resource curves,
    # these segment capacity factors will be reduced over time as more capacity is deployed using
    # lower quality resources.
    CSP_cf %>%
      group_by(class) %>%
      summarize(CF = mean(CF)) %>%
      ungroup() %>%
      filter(CF == max(CF)) ->
      CSP_cf_class_annual

    CSP_cf %>%
      semi_join(CSP_cf_class_annual, by = "class") %>%
      mutate(CF = CF * hours) %>%
      group_by() %>%
      summarize(CF = sum(CF), hours = sum(hours)) %>%
      ungroup() %>%
      mutate(CF = CF / hours) %>%
      expand(., ., L119.CapFacScaler_CSP_state) %>%
      mutate(capacity.factor = CF * scaler) %>%
      select(state, capacity.factor) ->
      L119.CapacityFactor_CSP_state

    CSP_cf %>%
      semi_join(CSP_cf_class_annual, by = "class") %>%
      group_by(segment) %>%
      summarize(capacity.factor = mean(CF)) %>%
      expand(., ., L119.CapFacScaler_CSP_state) %>%
      mutate(capacity.factor = capacity.factor * scaler,
             # application of scaler may lead to capacity factor > 1
             # reset these capacity factors to 1
             capacity.factor = if_else(capacity.factor > 1, 1, capacity.factor)) %>%
      select(state, segment, capacity.factor) ->
      L119.CapacityFactor_CSP_state_segment

    # Add new modification for dispatch model
    L119.CapacityFactor_PV_state %>%
      mutate(sector = "electricity generation") %>%
      mutate(fuel = "PV") %>%
      add_title("Capacity factor for solar PV by state") %>%
      add_units("Unitless") %>%
      add_comments("Processed data from ReEDS hourly cf data") %>%
      add_legacy_name("L119.CapacityFactor_PV_state (dispatch branch)") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/NREL_us_re_capacity_factors",
                     "L102.date_load_curve_mapping_S_gcamusa",
                     "gcam-usa/reeds_regions_states",
                     "gcam-usa/dispatch/UPV_CFt") ->
      L119.CapacityFactor_PV_state_gcamusa

    L119.CapacityFactor_PV_state_segment %>%
      mutate(sector = "electricity generation") %>%
      mutate(fuel = "PV") %>%
      add_title("Capacity factor for solar PV by state and segment") %>%
      add_units("Unitless") %>%
      add_comments("Processed data from ReEDS hourly cf data") %>%
      add_legacy_name("L119.CapacityFactor_PV_state_segment (dispatch branch)") %>%
      same_precursors_as("L119.CapacityFactor_PV_state_gcamusa") ->
      L119.CapacityFactor_PV_state_segment_gcamusa

    L119.CapacityFactor_CSP_state %>%
      mutate(sector = "electricity generation") %>%
      mutate(fuel = "CSP") %>%
      # add attributes for output...
      add_title("Capacity factor for solar CSP by state") %>%
      add_units("Unitless") %>%
      add_comments("Processed data from ReEDS hourly cf data") %>%
      add_legacy_name("L119.CapacityFactor_CSP_state (dispatch branch)") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/NREL_us_re_capacity_factors",
                     "L102.date_load_curve_mapping_S_gcamusa",
                     "gcam-usa/dispatch/ReEDS_segment_hours",
                     "gcam-usa/season_month_mapping",
                     "gcam-usa/dispatch/CSP_NoStor_CFts") ->
      L119.CapacityFactor_CSP_state_gcamusa

    L119.CapacityFactor_CSP_state_segment %>%
      mutate(sector = "electricity generation") %>%
      mutate(fuel = "CSP") %>%
      add_title("Capacity factor for solar CSP by state and segment", overwrite = T) %>%
      add_units("Unitless") %>%
      add_comments("Processed data from ReEDS hourly cf data") %>%
      add_legacy_name("L119.CapacityFactor_CSP_state_segment (dispatch branch)") %>%
      same_precursors_as("L119.CapacityFactor_CSP_state_gcamusa") ->
      L119.CapacityFactor_CSP_state_segment_gcamusa


    return_data(L119.CapacityFactor_CSP_state_gcamusa, L119.CapacityFactor_CSP_state_segment_gcamusa,
                L119.CapacityFactor_PV_state_gcamusa, L119.CapacityFactor_PV_state_segment_gcamusa)
  } else {
    stop("Unknown command")
  }
}
