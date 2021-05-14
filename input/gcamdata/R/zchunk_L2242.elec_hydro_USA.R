# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2242.elec_hydro_USA
#'
#' Update hydropower capacity technology info to match historical (2019) generation for 2020 and all future years.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2242.CapacityTech_hydro_future}, \code{L2242.TechLifetime_hydro},
#' \code{L2242.CapacityTechSegmentCapFac_hydro}.
#' The corresponding file in the original data system was \code{L2242.elec_hydro_USA.R} (gcam-usa level2).
#' @details Update state-level hydro-electricity fixed outputs
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter lag mutate select semi_join
#' @importFrom tidyr complete nesting
#' @author MTB September 2018
module_gcamusa_L2242.elec_hydro_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = 'gcam-usa/states_subregions',
             FILE = 'gcam-usa/EIA_elec_gen_hydro',
             "L223.CapacityTech",
             "L223.Production_Dispatch",
             "L223.TechCapFac_Dispatch",
             "L223.TechLifetime_Dispatch"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2242.CapacityTech_hydro_future",
             "L2242.TechLifetime_hydro",
             "L2242.TechCapFac_hydro"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, 'gcam-usa/states_subregions')
    EIA_elec_gen_hydro <- get_data(all_data, 'gcam-usa/EIA_elec_gen_hydro')
    L223.CapacityTech <- get_data(all_data, 'L223.CapacityTech', strip_attributes = TRUE)
    L223.Production_Dispatch <- get_data(all_data, 'L223.Production_Dispatch', strip_attributes = TRUE)
    L223.TechCapFac_Dispatch <- get_data(all_data, 'L223.TechCapFac_Dispatch', strip_attributes = TRUE)
    L223.TechLifetime_Dispatch <- get_data(all_data, 'L223.TechLifetime_Dispatch', strip_attributes = TRUE)

    # Silence package checks
    subsector <- year <- fixedOutput <- state <- EIA <- EIA_ratio <- fixedOutput_2015 <-
      AEO <- AEO_2015_ratio <- region <- supplysector <- stub.technology <-
      share.weight.year <- subs.share.weight <- tech.share.weight <-
      technology <- subsector_1 <- to.technology <- NULL

    # ===================================================
    # Data Processing

    # Isolate GCAM hydro generation in final model base year
    L223.Production_Dispatch %>%
      filter(subsector == "hydro",
             year == max(MODEL_BASE_YEARS),
             calOutputValue != 0) -> L2242.hydro_prod_baseyear

    # Calculate relative change in hydro generation from final model base year
    # to most recent historical year in EIA data, by state
    EIA_elec_gen_hydro %>%
      rename(state_name = state) %>%
      gather_years() %>%
      filter(year == max(MODEL_BASE_YEARS) | year == max(year),
             !is.na(value)) %>%
      left_join_error_no_match(states_subregions %>%
                                 select(state, state_name),
                               by = "state_name") %>%
      group_by(state) %>%
      mutate(ratio = value[year == max(year)] / value[year == max(MODEL_BASE_YEARS)]) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(state, ratio) -> L2242.hydro_EIA_ratio_hist

    # Apply EIA hydro generation ratios to GCAM final model base year values
    # to get future hydro generation by state
    L2242.hydro_prod_baseyear %>%
      select(region, GCAM = calOutputValue) %>%
      left_join_error_no_match(L2242.hydro_EIA_ratio_hist, by = c("region" = "state")) %>%
      mutate(value = GCAM * ratio,
             year = min(MODEL_FUTURE_YEARS)) %>%
      select(region, generation = value) -> L2242.hydro_prod_future

    # In order to update future hydro prodcution, we need to update capacity in 2020,
    # since we currently can't change capacity factor year to year for a given vintage
    # Start by setting up a 2020 capacity.  We'll assume that 2020 capacity is the same
    # as 2015, but we'll give this new "vintage" a different capacity factor.
    L223.CapacityTech %>%
      filter(subsector == "hydro",
             year == max(MODEL_BASE_YEARS)) %>%
      mutate(year = min(MODEL_FUTURE_YEARS)) -> L2242.CapacityTech_hydro_future

    # Update dispatch tech vintage lifetimes.  The 2015 vintage will have a 5-year lifetime
    # since we're replacing it with a new vintage in 2020.  The 2020 vintage will use the
    # same 100-year lifetime that the default 2015 vintage used so that it is available in
    # all future model periods.
    L223.TechLifetime_Dispatch %>%
      filter(subsector == "hydro") %>%
      semi_join(L2242.CapacityTech_hydro_future, by = "region") %>%
      mutate(lifetime = 5) -> L2242.TechLifetime_hydro_hist

    L223.TechLifetime_Dispatch %>%
      filter(subsector == "hydro") %>%
      semi_join(L2242.CapacityTech_hydro_future, by = "region") %>%
      mutate(year = min(MODEL_FUTURE_YEARS)) -> L2242.TechLifetime_hydro_fut

    L2242.TechLifetime_hydro_hist %>%
      bind_rows(L2242.TechLifetime_hydro_fut) -> L2242.TechLifetime_hydro

    # Finally, update capacity factors to get correct hydropower generation in future years
    L2242.CapacityTech_hydro_future %>%
      select(region, capacity) %>%
      left_join_error_no_match(L2242.hydro_prod_future, by = "region") %>%
      mutate(capacity.factor = generation / capacity) %>%
      select(region, capacity.factor) -> L2242.CapacityTech_hydro_future_capfac

    L223.TechCapFac_Dispatch %>%
      select(-capacity.factor) %>%
      filter(subsector == "hydro",
             year == max(MODEL_BASE_YEARS)) %>%
      mutate(year = min(MODEL_FUTURE_YEARS)) %>%
      left_join_error_no_match(L2242.CapacityTech_hydro_future_capfac, ., by = "region") -> L2242.TechCapFac_hydro


    # ===================================================
    # Produce outputs

    L2242.CapacityTech_hydro_future %>%
      add_title("Dispatch technology hydropower capacity by state") %>%
      add_units("capacity: EJ / year (if operated at 100% capacity factor)") %>%
      add_comments("Set hydro capacity by state for future model years") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/EIA_elec_gen_hydro",
                     "L223.CapacityTech",
                     "L223.Production_Dispatch") ->
      L2242.CapacityTech_hydro_future

    L2242.TechLifetime_hydro %>%
      add_title("Dispatch technology lifetime for hydropower") %>%
      add_units("years") %>%
      add_comments("The 2015 vintage will have a 5-year lifetime since we're replacing it with a new vintage in 2020") %>%
      add_comments("The 2020 vintage will have a 100-year lifetime so that it is available in all future model periods") %>%
      same_precursors_as("L2242.CapacityTech_hydro_future") %>%
      add_precursors("L223.TechLifetime_Dispatch") ->
      L2242.TechLifetime_hydro

    L2242.TechCapFac_hydro %>%
      add_title("Dispatch technology hydropower capacity factor by state") %>%
      add_units("capacity factor (hours of operation / hours in year (8760)") %>%
      add_comments("Capacity factor calculated to match EIA hydropower generation data from most recent historical year (2019)") %>%
      same_precursors_as("L2242.CapacityTech_hydro_future") %>%
      add_precursors("L223.TechCapFac_Dispatch") ->
      L2242.TechCapFac_hydro

    return_data(L2242.CapacityTech_hydro_future,
                L2242.TechLifetime_hydro,
                L2242.TechCapFac_hydro)

  } else {
    stop("Unknown command")
  }
}
