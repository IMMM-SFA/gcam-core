# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LB123.Electricity
#'
#' Calculate electricity fuel consumption, electricity generation, and inputs and outputs of net ownuse
#' (the electricity used by production/transformation facilities) by state.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L123.in_EJ_state_elec_F_tech}, \code{L123.out_EJ_state_elec_F}, \code{L123.in_EJ_state_ownuse_elec},
#' \code{L123.out_EJ_state_ownuse_elec}, \code{L123.out_EJ_state_elec_F_tech}, \code{L123.capacity_EJ_state_elec_F_tech},
#' \code{L123.capacity_factor_EJ_state_elec_F_tech}
#' The corresponding file in the original data system was \code{LB123.Electricity.R} (gcam-usa level1).
#' @details By state, calculates electricity fuel consumption, electricity generation, and inputs and outputs of net ownuse.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by left_join mutate select summarise transmute
#' @importFrom tidyr gather spread replace_na
#' @author RLH August 2017 YO Jun 2020

module_gcamusa_LB123.Electricity <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L123.in_EJ_R_elec_F_Yh",
             "L123.out_EJ_R_elec_F_Yh",
             FILE = "gcam-usa/EIA_elect_td_ownuse",
             "L105.elec_fuelconsumption_state_vintage_gcamusa",
             "L105.elec_generation_state_vintage_gcamusa",
             "L105.elec_capacity_state_vintage_gcamusa",
             "L126.in_EJ_R_elecownuse_F_Yh",
             "L126.out_EJ_R_elecownuse_F_Yh",
             "L132.out_EJ_state_indchp_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L123.in_EJ_state_elec_F",
             "L123.in_EJ_state_elec_F_tech",
             "L123.out_EJ_state_elec_F",
             "L123.in_EJ_state_ownuse_elec",
             "L123.out_EJ_state_ownuse_elec",
             "L123.out_EJ_state_elec_F_tech",
             "L123.capacity_EJ_state_elec_F_tech",
             "L123.capacity_factor_EJ_state_elec_F_tech"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    State <- state <- state_name <- GCAM_region_ID <- year <- value <- sector <-
      fuel <- CSP_GWh <- value.x <- value.y <- net_EJ_USA <- DirectUse_MWh <- NULL

    # Load required inputs
    L123.in_EJ_R_elec_F_Yh <- get_data(all_data, "L123.in_EJ_R_elec_F_Yh") %>%
      filter(GCAM_region_ID == gcam.USA_CODE)
    L123.out_EJ_R_elec_F_Yh <- get_data(all_data, "L123.out_EJ_R_elec_F_Yh") %>%
      filter(GCAM_region_ID == gcam.USA_CODE)
    EIA_elect_td_ownuse <- get_data(all_data, "gcam-usa/EIA_elect_td_ownuse")
    L126.in_EJ_R_elecownuse_F_Yh <- get_data(all_data, "L126.in_EJ_R_elecownuse_F_Yh") %>%
      filter(GCAM_region_ID == gcam.USA_CODE)
    L126.out_EJ_R_elecownuse_F_Yh <- get_data(all_data, "L126.out_EJ_R_elecownuse_F_Yh") %>%
      filter(GCAM_region_ID == gcam.USA_CODE)
    L132.out_EJ_state_indchp_F <- get_data(all_data, "L132.out_EJ_state_indchp_F")

    elec_fuelconsumption_state_vintage <- get_data(all_data, "L105.elec_fuelconsumption_state_vintage_gcamusa" )
    elec_generation_state_vintage <- get_data(all_data, "L105.elec_generation_state_vintage_gcamusa" )
    elec_capacity_state_vintage <- get_data(all_data, "L105.elec_capacity_state_vintage_gcamusa" )

    # ===================================================
    # ELECTRICITY - INPUT & OUTPUT
    # Using the approach in dispatch branch to calcluate percentage shares of electricity generation and capacity shares
    # Need to confirm data sources of elec_XXXX_vintages above

    # Original note: drop vintage for now (by plp)
    elec_fuelconsumption_state_vintage %>%
      group_by(state, gcam_fuel, elec_tech) %>%
      summarize(en_in = sum(en_in)) %>%
      ungroup() ->
      elec_fuelconsumption_state_vintage

    elec_generation_state_vintage %>%
      group_by(state, gcam_fuel, elec_tech) %>%
      summarize(en_out = sum(en_out)) %>%
      ungroup() ->
      elec_generation_state_vintage

    elec_capacity_state_vintage %>%
      group_by(state, gcam_fuel, elec_tech) %>%
      summarize(capacity = sum(capacity)) %>%
      ungroup() ->
      elec_capacity_state_vintage

    # derive state shares for energy input and output
    elec_fuelconsumption_state_vintage %>%
      group_by(gcam_fuel) %>%
      mutate(value = en_in / sum(en_in)) %>%
      ungroup() %>%
      rename(fuel = gcam_fuel) ->
      L123.pct_in_state_elec_F

    elec_generation_state_vintage %>%
      group_by(gcam_fuel) %>%
      mutate(value = en_out / sum(en_out)) %>%
      ungroup() %>%
      rename(fuel = gcam_fuel) ->
      L123.pct_out_state_elec_F

    # TODO: just using fixed share for all historical years

    L123.pct_in_state_elec_F %>%
      repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) %>%
      select(-en_in) ->
      L123.pct_in_state_elec_F

    L123.pct_out_state_elec_F %>%
      repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) %>%
      select(-en_out) ->
      L123.pct_out_state_elec_F

    L123.pct_in_state_elec_F %>%
      filter(fuel %in% L123.in_EJ_R_elec_F_Yh$fuel) %>%
      left_join_error_no_match(L123.in_EJ_R_elec_F_Yh, by = c("fuel", "year")) %>%
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel, elec_tech, year, value) ->
      L123.in_EJ_state_elec_F_tech

    L123.in_EJ_state_elec_F_tech %>%
      group_by(state, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() ->
      L123.in_EJ_state_elec_F

    L123.pct_out_state_elec_F %>%
      left_join_error_no_match(L123.out_EJ_R_elec_F_Yh, by = c("fuel", "year")) %>%
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel, elec_tech, year, value) ->
      L123.out_EJ_state_elec_F_tech

    L123.out_EJ_state_elec_F_tech %>%
      group_by(state, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() ->
      L123.out_EJ_state_elec_F

    # ELECTRICITY - CAPACITY
    # rewrite from dispatch branch LB123.Electricity.R
    # TODO: giving 2010 capacity to all historical years (capacity factors will low < 2010) (plp)

    elec_capacity_state_vintage %>%
      repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) ->
      L123.capacity_EJ_state_elec_F_tech

    # Note: here the capacity unit is EJ
    # TODO: check why some capacity factor is greater than 1
    L123.out_EJ_state_elec_F_tech %>%
      left_join_error_no_match(elec_capacity_state_vintage %>% rename(fuel = gcam_fuel),
                               by = c("state", "fuel", "elec_tech")) %>%
      mutate(capacity_factor = value / capacity) %>%
      select(-capacity) ->
      L123.capacity_factor_EJ_state_elec_F_tech

    # ELECTRICITY - OWNUSE
    # NOTE: Electricity net own use energy is apportioned to states on the basis of EIA's direct use by state
    # First calculate the national own use quantity
    # Keep the original code as it is for this portion
    L123.net_EJ_USA_ownuse <- L126.in_EJ_R_elecownuse_F_Yh %>%
      left_join_error_no_match(L126.out_EJ_R_elecownuse_F_Yh, by = c("sector", "fuel", "year")) %>%
      # Net value = input value - output value
      mutate(net_EJ_USA = value.x - value.y) %>%
      select(sector, year, net_EJ_USA)

    # Then build table with each state's share of the national ownuse. Note that this is assumed invariant over time.
    L123.net_pct_state_USA_ownuse_elec <- tidyr::crossing(state = gcamusa.STATES,
                                                 sector = "electricity ownuse",
                                                 fuel = "electricity",
                                                 year = HISTORICAL_YEARS) %>%
      # Add in ownuse by state
      left_join_error_no_match(EIA_elect_td_ownuse %>%
                                 select(State, DirectUse_MWh), by = c("state" = "State")) %>%
      group_by(sector, fuel, year) %>%
      # Compute state share of total
      mutate(value = DirectUse_MWh / sum(DirectUse_MWh)) %>%
      ungroup()

    # Net own use = national total multiplied by each state's share
    L123.net_EJ_state_ownuse_elec <- L123.net_pct_state_USA_ownuse_elec %>%
      left_join_error_no_match(L123.net_EJ_USA_ownuse, by = c("sector", "year")) %>%
      # Multiply state share by USA total
      mutate(value = value * net_EJ_USA)

    # The input of the electricity_net_ownuse sector is equal to sum of all generation (industrial CHP + electric sector)
    L123.in_EJ_state_ownuse_elec <- bind_rows(L123.out_EJ_state_elec_F, L132.out_EJ_state_indchp_F) %>%
      group_by(state, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(sector = "electricity ownuse",
             fuel = "electricity") %>%
      select(state, sector, fuel, year, value)

    # Output of electricity_net_ownuse sector is equal to input minus ownuse "net" energy
    L123.out_EJ_state_ownuse_elec <- L123.in_EJ_state_ownuse_elec %>%
      left_join_error_no_match(L123.net_EJ_state_ownuse_elec, by = c("state", "sector", "fuel", "year")) %>%
      # Input value - net value
      mutate(value = value.x - value.y) %>%
      select(state, sector, fuel, year, value)
    # ===================================================

    # Produce outputs
    L123.in_EJ_state_elec_F %>%
      add_title("Electricity sector energy consumption by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("State fuel shares created from elec_fuelconsumption_state_vintage multiplied by USA totals from L123.in_EJ_R_elec_F_Yh") %>%
      add_legacy_name("L123.in_EJ_state_elec_F") %>%
      add_precursors("L105.elec_fuelconsumption_state_vintage_gcamusa", "L123.in_EJ_R_elec_F_Yh") ->
      L123.in_EJ_state_elec_F

    L123.in_EJ_state_elec_F_tech %>%
      add_title("Electricity sector energy consumption by state and fuel and technology") %>%
      add_units("EJ") %>%
      add_comments("State fuel shares created from elec_fuelconsumption_state_vintage multiplied by USA totals from L123.in_EJ_R_elec_F_Yh") %>%
      add_legacy_name("L123.in_EJ_state_elec_F_tech") %>%
      add_precursors("L105.elec_fuelconsumption_state_vintage_gcamusa", "L123.in_EJ_R_elec_F_Yh") ->
      L123.in_EJ_state_elec_F_tech

    L123.out_EJ_state_elec_F %>%
      add_title("Electricity generation by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("State fuel shares created from elec_generation_state_vintage multiplied by USA totals from L123.out_EJ_R_elec_F_Yh") %>%
      add_legacy_name("L123.out_EJ_state_elec_F") %>%
      add_precursors("L105.elec_generation_state_vintage_gcamusa", "L123.out_EJ_R_elec_F_Yh") ->
      L123.out_EJ_state_elec_F

    L123.out_EJ_state_elec_F_tech %>%
      add_title("Electricity generation by state, fuel and technology") %>%
      add_units("EJ") %>%
      add_comments("State fuel shares created from elec_generation_state_vintage multiplied by USA totals from L123.out_EJ_R_elec_F_Yh") %>%
      add_legacy_name("L123.out_EJ_state_elec_F_tech") %>%
      add_precursors("L105.elec_generation_state_vintage_gcamusa", "L123.out_EJ_R_elec_F_Yh") ->
      L123.out_EJ_state_elec_F_tech

    L123.in_EJ_state_ownuse_elec %>%
      add_title("Input to electricity net ownuse by state") %>%
      add_units("EJ") %>%
      add_comments("Sum of all generation from L123.out_EJ_state_elec_F and L132.out_EJ_state_indchp_F") %>%
      add_legacy_name("L123.in_EJ_state_ownuse_elec") %>%
      add_precursors("L123.out_EJ_R_elec_F_Yh", "L132.out_EJ_state_indchp_F") ->
      L123.in_EJ_state_ownuse_elec

    L123.out_EJ_state_ownuse_elec %>%
      add_title("Output of electricity net ownuse by state") %>%
      add_units("EJ") %>%
      add_comments("Input values from L123.in_EJ_state_ownuse_elec subtracted by net values") %>%
      add_comments("Net values created with states shares from EIA_elect_td_ownuse and USA total net from L126 files") %>%
      add_legacy_name("L123.out_EJ_state_ownuse_elec") %>%
      add_precursors("L123.out_EJ_R_elec_F_Yh", "L132.out_EJ_state_indchp_F",
                     "L126.in_EJ_R_elecownuse_F_Yh", "L126.out_EJ_R_elecownuse_F_Yh", "gcam-usa/EIA_elect_td_ownuse")  ->
      L123.out_EJ_state_ownuse_elec

    L123.capacity_EJ_state_elec_F_tech %>%
      add_title("Electricity generation capacity by state and fuel and tech") %>%
      add_units("EJ") %>%
      add_comments("giving 2015 capacity to all historical years") %>%
      add_legacy_name("L123.capacity_EJ_state_elec_F_tech") %>%
      add_precursors("L105.elec_capacity_state_vintage_gcamusa")  ->
      L123.capacity_EJ_state_elec_F_tech

    L123.capacity_factor_EJ_state_elec_F_tech %>%
      add_title("Electricity generation capacity factor by state and fuel and tech") %>%
      add_units("NA") %>%
      add_comments("Need to check why some capacity factor is greater than 1") %>%
      add_legacy_name("L123.capacity_factor_EJ_state_elec_F_tech") %>%
      add_precursors("L105.elec_capacity_state_vintage_gcamusa", "L105.elec_generation_state_vintage_gcamusa",
                     "L123.out_EJ_R_elec_F_Yh")  ->
      L123.capacity_factor_EJ_state_elec_F_tech


    return_data(L123.in_EJ_state_elec_F_tech, L123.out_EJ_state_elec_F, L123.in_EJ_state_ownuse_elec, L123.out_EJ_state_ownuse_elec,
                L123.out_EJ_state_elec_F_tech, L123.capacity_EJ_state_elec_F_tech, L123.capacity_factor_EJ_state_elec_F_tech,
                L123.in_EJ_state_elec_F)
  } else {
    stop("Unknown command")
  }
}
