# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LA114.Wind
#'
#' Compute capacity factors for wind by US state.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L114.CapacityFactor_wind_state_gcamusa}, \code{L114.CapacityFactor_wind_state_segment_gcamusa}.
#' The corresponding file in the original data system was \code{LA114.Wind.R} (gcam-usa level1).
#' @details Computes capacity factors for wind by US state.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select pull
#' @importFrom tidyr gather spread complete
#' @author ST September 2017 / YO June 2020
module_gcamusa_LA114.wind <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( FILE = "gcam-usa/us_state_wind",
              FILE = "energy/A23.globaltech_capital",
              FILE = "energy/A23.globaltech_OMfixed",
              FILE = "energy/A23.globaltech_OMvar",
              FILE = "gcam-usa/reeds_regions_states",
              "L102.date_load_curve_mapping_S_gcamusa",
              OPTIONAL_FILE = "gcam-usa/dispatch/wind_CFt"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L114.CapacityFactor_wind_state_gcamusa",
             "L114.CapacityFactor_wind_state_segment_gcamusa"))
  } else if(command == driver.MAKE) {

    technology <- year <- state <- sector <- capacity_factor <- fuel <- value <- base_cost <-
      region <- hour <- date <- Region <- State <- segment <- month <- day_night <- NULL  # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs

    us_state_wind <- get_data(all_data, "gcam-usa/us_state_wind")
    A23.globaltech_capital <- get_data(all_data, "energy/A23.globaltech_capital")
    A23.globaltech_OMfixed <- get_data(all_data, "energy/A23.globaltech_OMfixed")
    A23.globaltech_OMvar <- get_data(all_data, "energy/A23.globaltech_OMvar")
    L102.date_load_curve_mapping_S <- get_data(all_data, "L102.date_load_curve_mapping_S_gcamusa")
    wind_cf_raw <- get_data(all_data, "gcam-usa/dispatch/wind_CFt")
    ReEDS_region_mapping_raw <- get_data(all_data, "gcam-usa/reeds_regions_states")
    # ===================================================

    if(is.null(wind_cf_raw)) {
      # Proprietary FERC hourly data are not available, so used saved outputs
      L114.CapacityFactor_wind_state_gcamusa <- prebuilt_data("L114.CapacityFactor_wind_state_gcamusa")
      L114.CapacityFactor_wind_state_segment_gcamusa <- prebuilt_data("L114.CapacityFactor_wind_state_segment_gcamusa")
    } else {

      # Interpolating the cost tables as necessary to get the costs in the assumed wind base cost year
      A23.globaltech_capital %<>%
        nest() %>%
        mutate(data = lapply(data, fill_exp_decay_extrapolate, energy.WIND.BASE.COST.YEAR)) %>%
        unnest()

      A23.globaltech_OMfixed %<>%
        nest() %>%
        mutate(data = lapply(data, fill_exp_decay_extrapolate, energy.WIND.BASE.COST.YEAR)) %>%
        unnest()

      A23.globaltech_OMvar %<>%
        nest() %>%
        mutate(data = lapply(data, fill_exp_decay_extrapolate, energy.WIND.BASE.COST.YEAR)) %>%
        unnest()

      #Extract the costs. These are in 1975$

      L114.CapCost <- A23.globaltech_capital$value[A23.globaltech_capital$year == energy.WIND.BASE.COST.YEAR &
                                                     A23.globaltech_capital$technology == "wind"]

      L114.FixedChargeRate <- A23.globaltech_capital$fixed.charge.rate[A23.globaltech_capital$year == energy.WIND.BASE.COST.YEAR &
                                                                         A23.globaltech_capital$technology == "wind"]

      L114.OMFixedCost <- A23.globaltech_OMfixed$value[A23.globaltech_OMfixed$year == energy.WIND.BASE.COST.YEAR &
                                                         A23.globaltech_OMfixed$technology == "wind"]

      L114.OMVarCost <- A23.globaltech_OMvar$value[A23.globaltech_OMvar$year == energy.WIND.BASE.COST.YEAR &
                                                     A23.globaltech_OMvar$technology == "wind"]

      us_state_wind %<>%
        mutate(base_cost_75USDGJ = base_cost * gdp_deflator(1975, 2007) / CONV_KWH_GJ) %>%
        mutate(capacity_factor = ( L114.CapCost * L114.FixedChargeRate + L114.OMFixedCost ) /
                 ( CONV_KWH_GJ * CONV_YEAR_HOURS ) / ( base_cost_75USDGJ - ( L114.OMVarCost / ( 1000 * CONV_KWH_GJ ) ) ))

      L114.CapacityFactor_wind_state <- tibble::tibble(
        state = us_state_wind$region,
        sector = "electricity generation",
        fuel = "wind",
        capacity.factor = us_state_wind$capacity_factor)

      # process Reeds wind capacity factor data
      wind_cf_raw %>%
        gather("hour", "capacity_factor", dplyr::matches("[0-9]+")) %>%
        mutate(hour = as.integer(hour)) %>%
        mutate(date = as.POSIXct(paste0(MODEL_FINAL_BASE_YEAR, "-01-01 00:00:00"), tz="EST") + ((hour - 1) * 60 * 60)) ->
        wind_cf

      ReEDS_region_mapping_raw %>%
        filter(Country == "USA") %>%
        select(Region, State) %>%
        rename(state = State) ->
        ReEDS_region_mapping

      L102.date_load_curve_mapping_S$date <- as.POSIXct(L102.date_load_curve_mapping_S$date, tz="EST")

      wind_cf %>%
        left_join_error_no_match(ReEDS_region_mapping, by=c(`Wind Resource Region` = "Region")) %>%
        left_join_error_no_match(L102.date_load_curve_mapping_S, by=c("state", "date")) ->
        wind_cf

      wind_cf %>%
        mutate(segment = if_else(is_super_peak, gcamusa.ELEC_SEGMENT_SUPERPEAK, paste(month, day_night, sep=gcamusa.SEGMENT_DELIM))) %>%
        group_by(state, segment) %>%
        summarize(capacity.factor = mean(capacity_factor)) %>%
        ungroup() ->
        L114.CapacityFactor_wind_state_segment

      wind_cf %>%
        group_by(state) %>%
        summarize(capacity.factor = mean(capacity_factor)) %>%
        ungroup() ->
        L114.CapacityFactor_wind_state_avg

      # The ReEDS dataset doesn't have data for AK or HI so we will fall back to old assumptions for now
      L114.CapacityFactor_wind_state %>%
        filter(state %in% c("AK", "HI", "DC")) %>%
        expand(., ., unique(L114.CapacityFactor_wind_state_segment[, "segment"])) %>%
        select(state, segment, capacity.factor) %>%
        bind_rows(L114.CapacityFactor_wind_state_segment, .) ->
        L114.CapacityFactor_wind_state_segment

      L114.CapacityFactor_wind_state %>%
        filter(state %in% c("AK", "HI", "DC")) %>%
        select(state, capacity.factor) %>%
        bind_rows(L114.CapacityFactor_wind_state_avg, .) ->
        L114.CapacityFactor_wind_state_avg

      # Produce outputs
      L114.CapacityFactor_wind_state_avg %>%
        mutate(sector = "electricity generation") %>%
        mutate(fuel = "wind") %>%
        # add attributes for output...
        add_title("avaerage annual capacity factor by state for wind") %>%
        add_units("%") %>%
        add_comments("avaerage annual capacity factor by state for wind") %>%
        add_legacy_name("L114.CapacityFactor_wind_state (dispatch branch)") %>%
        add_precursors("gcam-usa/us_state_wind",
                       "energy/A23.globaltech_capital",
                       "energy/A23.globaltech_OMfixed",
                       "energy/A23.globaltech_OMvar",
                       "gcam-usa/reeds_regions_states",
                       "L102.date_load_curve_mapping_S_gcamusa",
                       "gcam-usa/dispatch/wind_CFt") ->
        L114.CapacityFactor_wind_state_gcamusa

      L114.CapacityFactor_wind_state_segment %>%
        mutate(sector = "electricity generation") %>%
        mutate(fuel = "wind") %>%
        # add attributes for output...
        add_title("capacity factor by state and load segment for wind", overwrite = T) %>%
        add_units("Unitless") %>%
        add_comments("capacity factor by state and load segment for wind") %>%
        add_legacy_name("L114.CapacityFactor_wind_state_segment (dispatch branch)") %>%
        same_precursors_as("L114.CapacityFactor_wind_state_gcamusa") ->
        L114.CapacityFactor_wind_state_segment_gcamusa

      verify_identical_prebuilt(L114.CapacityFactor_wind_state_gcamusa,
                                L114.CapacityFactor_wind_state_segment_gcamusa)
    }

    return_data(L114.CapacityFactor_wind_state_gcamusa, L114.CapacityFactor_wind_state_segment_gcamusa)
  } else {
    stop("Unknown command")
  }
}

