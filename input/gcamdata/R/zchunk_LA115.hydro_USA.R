# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LA115.hydro
#'
#' Compute hydropower capacity factors by state and month.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:
#' \code{L115.CapacityFactor_hydro_state_gcamusa}, \code{L115.CapacityFactor_hydro_state_segment_gcamusa}.
#' @details This chunk computes capacity factors for hydropower by state and month.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter if_else group_by mutate select summarise summarise_at vars
#' @importFrom tidyr gather spread
#' @author MTB 2021/05
module_gcamusa_LA115.hydro <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             "L102.load_segments_gcamusa",
             "L105.elec_capacity_state_vintage_gcamusa",
             FILE = "gcam-usa/dispatch/EIA_hydro_monthly.csv"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L115.CapacityFactor_hydro_state_gcamusa",
             "L115.CapacityFactor_hydro_state_segment_gcamusa"))
  } else if(command == driver.MAKE) {

    fuel <- value <- State <- . <- value.x <- value.y <- sector <- scaler <-
        state <- state_name <- NULL     # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    L102.load_segments_gcamusa <- get_data(all_data, "L102.load_segments_gcamusa")
    L105.elec_capacity_state_vintage_gcamusa <- get_data(all_data, "L105.elec_capacity_state_vintage_gcamusa")
    EIA_hydro_monthly <- get_data(all_data, "gcam-usa/dispatch/EIA_hydro_monthly.csv")

    # ===================================================
    # Perform calculations

    # Calculating monthly capacity factor for hydropower in each state
    # Start by cleaning / formatting EIA monthly hydropower generation data
    EIA_hydro_monthly %>%
      select(-units, -source_key) %>%
      gather(month, value, -description) %>%
      # there are a few NM (not meaningful) entries in the data set,
      # which were converted to blanks and parsed as NAs
      # remove these NA values (we don't want them to influence our average capacity factor)
      filter(!is.na(value)) %>%
      separate(description, c("drop", "state_name"), sep = " : ") %>%
      separate(month, c("month", "year"), sep = "_") %>%
      # filter for years in model historical years
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(states_subregions %>%
                                 select(state_name, state),
                               by = "state_name") %>%
      group_by(state, month) %>%
      summarise(generation = mean(value) * CONV_GWH_EJ) %>%
      ungroup() -> L115.EIA_hydro_monthly_avg

    # Summarize hydropower capacity
    # Note that we are assuming that hydropower capacity is constant over the
    # historical period (2001-2015), which should largely be true in the USA
    # TODO:  can use vintage info to assign capacity by year
    L105.elec_capacity_state_vintage_gcamusa %>%
      filter(elec_tech == "hydro") %>%
      group_by(state, elec_tech) %>%
      summarise(capacity = sum(capacity)) %>%
      ungroup() -> L115.hydro_capacity

    # Summarize hours per month (based on 2015)
    L102.load_segments_gcamusa %>%
      # filter out superpeak for now since it's not a month
      filter(segment != gcamusa.ELEC_SEGMENT_SUPERPEAK) %>%
      separate(segment, c("month", "drop"), sep = "_", remove = F) -> L102.load_segments_gcamusa

    L102.load_segments_gcamusa %>%
      # first sum hours per month
      group_by(grid_region, month) %>%
      summarise(hours = sum(hours)) %>%
      ungroup() %>%
      # now average across grid region.  variation across grid regions is limited.
      # hours/month can differ between grid regions because  superpeak is still separate,
      # but it's only 10 hours of 8760 in the year (0.2%), so this won't affect our calcuations much
      group_by(month) %>%
      summarise(hours = mean(hours)) %>%
      ungroup() -> L115.load_segments_hours

    # Join up capacity and generation, compute capacity factor
    L115.hydro_capacity %>%
      # join will duplicate rows by months of the year
      # LJENM will throw an error, so left_join() is used
      left_join(L115.EIA_hydro_monthly_avg, by = "state") %>%
      left_join_error_no_match(L115.load_segments_hours, by = "month") %>%
      # generation is in EJ
      # capacity is in EJ/year
      # calculate capacity factor
      mutate(capacity.factor = generation / (capacity * hours / CONV_YEAR_HOURS)) ->
      L115.CapacityFactor_hydro_state_month

    # Annual average hydro capacity factor by state
    # Note that these don't align with the annual capacity factors derived from EIA 860 data,
    # so we'll have to scale segment capacity factors in future chunks
    L115.CapacityFactor_hydro_state_month %>%
      group_by(state) %>%
      summarise(capacity.factor = sum(capacity.factor * hours) / sum(hours)) %>%
      ungroup() -> L115.CapacityFactor_hydro_state_gcamusa

    L115.CapacityFactor_hydro_state_month %>%
      # map from months back to segments
      # (assuming same capacity factor for day / night within a month)
      # join will duplicate rows because there are two segments per month (day and night)
      # LJENM will throw an error, so left_join() is used
      left_join(L102.load_segments_gcamusa %>%
                  distinct(month, segment),
                by = "month")  %>%
      select(state, segment, capacity.factor) ->
      L115.CapacityFactor_hydro_state_segment

    # Lastly, we need an assumption for superpeak.  Assume superpeak gets the maximum monthly capacity factor
    L115.CapacityFactor_hydro_state_segment %>%
      group_by(state) %>%
      filter(capacity.factor == max(capacity.factor)) %>%
      distinct(state, capacity.factor) %>%
      mutate(segment = gcamusa.ELEC_SEGMENT_SUPERPEAK) ->
      L115.CapacityFactor_hydro_state_segment_superpeak

    L115.CapacityFactor_hydro_state_segment %>%
      bind_rows(L115.CapacityFactor_hydro_state_segment_superpeak) ->
      L115.CapacityFactor_hydro_state_segment_gcamusa


    # ===================================================
    # Produce outputs

    L115.CapacityFactor_hydro_state_segment_gcamusa %>%
      mutate(sector = "electricity generation") %>%
      mutate(fuel = "hydro") %>%
      add_title("Capacity factor for hydropower by state and month") %>%
      add_units("Unitless") %>%
      add_comments("Processed data from ReEDS hourly cf data") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L102.load_segments_gcamusa",
                     "L105.elec_capacity_state_vintage_gcamusa",
                     "gcam-usa/dispatch/EIA_hydro_monthly.csv") ->
      L115.CapacityFactor_hydro_state_segment_gcamusa

    L115.CapacityFactor_hydro_state_gcamusa %>%
      mutate(sector = "electricity generation") %>%
      mutate(fuel = "hydro") %>%
      add_title("Capacity factor for hydropower by state") %>%
      add_units("Unitless") %>%
      add_comments("Processed data from ReEDS hourly cf data") %>%
      same_precursors_as("L115.CapacityFactor_hydro_state_segment_gcamusa") ->
      L115.CapacityFactor_hydro_state_gcamusa

    return_data(L115.CapacityFactor_hydro_state_segment_gcamusa,
                L115.CapacityFactor_hydro_state_gcamusa)
  } else {
    stop("Unknown command")
  }
}
