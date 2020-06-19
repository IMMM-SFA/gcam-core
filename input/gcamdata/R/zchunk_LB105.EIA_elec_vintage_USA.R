# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LB105.EIA_elec_vintage_USA
#'
#' Process 2015 EIA Form 923 and Form 860
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L105.elec_capacity_state_vintage_gcamusa}, \code{L105.elec_generation_state_vintage_gcamusa},
#' \code{L105.elec_generation_gridR_vintage_gcamusa}, \code{L105.elec_fuelconsumption_state_vintage_gcamusa}.
#' There is no corresponding file in the original data system. This was originally a preprocessing code
#' @details Process 2015 EIA Form 923 and Form 860
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select inner_join
#' @importFrom tidyr gather spread
#' @author YO Jun 2020
module_gcamusa_LB105.EIA_elec_vintage_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/EIA_923_generator_gen_fuel_2015",
             FILE = "gcam-usa/EIA_860_generators_existing_2015",
             FILE = "gcam-usa/prime_mover_map",
             FILE = "gcam-usa/calibrated_techs_dispatch_usa"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L105.elec_capacity_state_vintage_gcamusa",
             "L105.elec_generation_state_vintage_gcamusa",
             "L105.elec_generation_gridR_vintage_gcamusa",
             "L105.elec_fuelconsumption_state_vintage_gcamusa"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    en_in <- Total.Fuel.Consumption.MMBtus <- en_in_elec <- Elec.Fuel.Consumption.MMBtus <-
      en_out <- Net.Generation.MWh <- Reported.Fuel.Type.Code <- Plant.ID <- Combined.Heat.Power.Plant <-
      Plant.State <- NERC.Region <- gcam_fuel <- elec_tech <- eff <- eff_elec <-
      vint_cap_weighted <- Operating.Year <- Nameplate.MW <- Plant.Code <- Sector.Name <-
      vintage <- NAMEPLATE <- state <- capacity <- NULL # silence package check.

    # Load required inputs
    eia_923_data_raw <- get_data(all_data, "gcam-usa/EIA_923_generator_gen_fuel_2015")
    eia_860_data_raw <- get_data(all_data, "gcam-usa/EIA_860_generators_existing_2015")
    prime_mover_map <- get_data(all_data, "gcam-usa/prime_mover_map")
    calibrated_techs_dispatch_usa <- get_data(all_data, "gcam-usa/calibrated_techs_dispatch_usa")
    # -----------------------------------------------------------------------------
    # Perform computations

    # process EIA 923 Form to get efficiency data, but seems not used anyway for now
    eia_923_data_raw %>%
      mutate(en_in = Total.Fuel.Consumption.MMBtus * CONV_BTU_KJ * 1e-9) %>%
      mutate(en_in_elec = Elec.Fuel.Consumption.MMBtus * CONV_BTU_KJ * 1e-9) %>%
      mutate(en_out = Net.Generation.MWh * CONV_MWH_EJ) %>%
      filter(!is.na(Reported.Fuel.Type.Code)) %>%
      left_join_error_no_match(prime_mover_map, by = c("Reported.Fuel.Type.Code", "Reported.Prime.Mover")) %>%
      group_by(Plant.ID, Combined.Heat.Power.Plant, Plant.State, NERC.Region, gcam_fuel, elec_tech) %>%
      summarize(en_in = sum(en_in), en_in_elec = sum(en_in_elec), en_out = sum(en_out)) %>%
      ungroup() %>%
      mutate(eff = en_out / en_in) %>%
      mutate(eff_elec = en_out / en_in_elec) ->
      eia_923_data

    # process EIA 860 Form to get capacity by vintage
    eia_860_data_raw %>%
      left_join_error_no_match(prime_mover_map,
                               by = c("Prime.Mover" = "Reported.Prime.Mover",
                                      "Energy.Source.1" = "Reported.Fuel.Type.Code")) %>%
      # Note vintages are by generator.. so we need to tag the vintage somehow by
      # plant, using capacity weighted average for now
      mutate(vint_cap_weighted = Operating.Year * Nameplate.MW) %>%
      group_by(Plant.Code, Sector.Name, gcam_fuel, elec_tech) %>%
      summarize(Nameplate.MW=sum(Nameplate.MW),
                vint_cap_weighted = sum(vint_cap_weighted)) %>%
      mutate(vintage=as.integer(vint_cap_weighted / Nameplate.MW)) %>%
      select(-vint_cap_weighted) %>%
      mutate(NAMEPLATE=Nameplate.MW * CONV_MWH_EJ * CONV_YEAR_HOURS) %>%
      ungroup() ->
      eia_860_data

    # combine Form 923 and 860
    eia_923_data %>%
      ungroup() %>%
      # left missing 2566 plant IDs, right missing 180 plant IDs, but both missing IDs did not add up to
      # too much generation/capacity
      inner_join(eia_860_data, by=c("Plant.ID"="Plant.Code", "gcam_fuel", "elec_tech")) ->
      eia_elec_data_full

    # keep non-CHP only
    eia_elec_data_full %>%
      # here need to keep the only two CHP plants in DC, one using NG the other using biomass; Both are very small
      # otherwise there will be no capacity in DC and creating a lot of mapping errors in subsequent chunks
      mutate(Combined.Heat.Power.Plant = ifelse(Plant.State == "DC", "N", Combined.Heat.Power.Plant)) %>%
      filter(Combined.Heat.Power.Plant == 'N' , eff_elec >= 0, eff_elec <= 1.0) %>%
      mutate(elec_tech = if_else(gcam_fuel == "biomass", "biomass_conv", elec_tech)) %>%
      mutate(elec_tech = sub('_ice$', '_turbine', elec_tech)) %>%
      filter(elec_tech %in% unique(calibrated_techs_dispatch_usa$elec_tech)) %>%
      rename(state = Plant.State) ->
      eia_elec_data

    # capacity by state by vintage
    eia_elec_data %>%
      group_by(state, gcam_fuel, elec_tech, vintage) %>%
      summarize(capacity = sum(NAMEPLATE)) %>%
      ungroup() ->
      elec_capacity_state_vintage

    # generation by state by vintage
    eia_elec_data %>%
      group_by(state, gcam_fuel, elec_tech, vintage) %>%
      summarize(en_out = sum(en_out)) %>%
      ungroup() ->
      elec_generation_state_vintage

    # generation by grid by vintage
    eia_elec_data %>%
      rename(grid_region = NERC.Region) %>%
      group_by(grid_region, gcam_fuel, elec_tech, vintage) %>%
      summarize(en_out = sum(en_out)) %>%
      ungroup() ->
      elec_generation_gridR_vintage

    # fuel consumption by state by vintage
    eia_elec_data %>%
      group_by(state, gcam_fuel, elec_tech, vintage) %>%
      summarize(en_in = sum(en_in)) %>%
      ungroup() ->
      elec_fuelconsumption_state_vintage

    # Produce outputs
    elec_capacity_state_vintage %>%
      add_title("capacity by state by vintage up to 2015") %>%
      add_units("EJ") %>%
      add_comments("Processed from EIA From 860 and 923 (Year 2015)") %>%
      add_legacy_name("elec_capacity_state_vintage") %>%
      add_precursors("gcam-usa/EIA_923_generator_gen_fuel_2015",
                     "gcam-usa/EIA_860_generators_existing_2015",
                     "gcam-usa/prime_mover_map",
                     "gcam-usa/calibrated_techs_dispatch_usa") ->
      L105.elec_capacity_state_vintage_gcamusa

    elec_generation_state_vintage %>%
      add_title("generation by state by vintage up to 2015") %>%
      add_units("EJ") %>%
      add_comments("Processed from EIA From 860 and 923 (Year 2015)") %>%
      add_legacy_name("elec_generation_state_vintage") %>%
      same_precursors_as("L105.elec_capacity_state_vintage_gcamusa") ->
      L105.elec_generation_state_vintage_gcamusa

    elec_generation_gridR_vintage %>%
      add_title("generation by grid by vintage up to 2015") %>%
      add_units("EJ") %>%
      add_comments("Processed from EIA From 860 and 923 (Year 2015)") %>%
      add_legacy_name("elec_generation_gridR_vintage") %>%
      same_precursors_as("L105.elec_capacity_state_vintage_gcamusa") ->
      L105.elec_generation_gridR_vintage_gcamusa

    elec_fuelconsumption_state_vintage %>%
      add_title("fuel consumption by state by vintage up to 2015") %>%
      add_units("EJ") %>%
      add_comments("Processed from EIA From 860 and 923 (Year 2015)") %>%
      add_legacy_name("elec_fuelconsumption_state_vintage") %>%
      same_precursors_as("L105.elec_capacity_state_vintage_gcamusa") ->
      L105.elec_fuelconsumption_state_vintage_gcamusa

    return_data(L105.elec_capacity_state_vintage_gcamusa,
                L105.elec_generation_state_vintage_gcamusa,
                L105.elec_generation_gridR_vintage_gcamusa,
                L105.elec_fuelconsumption_state_vintage_gcamusa)
  } else {
    stop("Unknown command")
  }
}
