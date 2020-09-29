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
#' \code{L105.elec_fuelconsumption_state_vintage_gcamusa}.
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
             FILE = "gcam-usa/calibrated_techs_dispatch_usa",
             FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/UCS_tech_names",
             FILE = "gcam-usa/UCS_water_types",
             FILE = "gcam-usa/A23.elecS_tech_mapping_cool",
             FILE = "gcam-usa/UCS_Database",
             FILE = "gcam-usa/usa_seawater_states_basins"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L105.elec_capacity_state_vintage_gcamusa",
             "L105.elec_generation_state_vintage_gcamusa",
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

    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    UCS_tech_names <- get_data(all_data, "gcam-usa/UCS_tech_names")
    UCS_water_types <- get_data(all_data, "gcam-usa/UCS_water_types")
    A23.elecS_tech_mapping_cool <- get_data(all_data, "gcam-usa/A23.elecS_tech_mapping_cool")
    UCS_Database <- get_data(all_data, "gcam-usa/UCS_Database")
    usa_seawater_states_basins <- get_data(all_data, "gcam-usa/usa_seawater_states_basins")


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


    # Map in cooling system specifications to get capacity / generation by gen / cooling tech combination

    # Define states and basins that have access to seawater where seawater cooling will be allowed
    seawater_states_basins <- unique(usa_seawater_states_basins$seawater_region)

    # Inital processing of UCS database
    UCS_Database %>%
      rename(Plant.Code = `Plant Code`,
             state = State,
             cap_MW = `Nameplate Capacity (MW)`,
             out_MWh = `Estimated Generation (MWh)`,
             Technology.Code = `Technology Code`,
             reported_water_source = `Reported Water Source (Type)`) %>%
      # filter out entries with no technology detail
      filter(!is.na(`Generation Technology`)) %>%
      # reset non-coastal state seawater use to surface water
      # currently applies only to Michigan and Wisconsin
      mutate(reported_water_source = if_else(!(state %in% seawater_states_basins) &
                                               reported_water_source == gcamusa.UCS_WATER_TYPE_OCEAN,
                                             gcamusa.UCS_WATER_TYPE_SURFACE,
                                             reported_water_source)) %>%
      # introduce tech and water type names
      left_join_error_no_match(UCS_tech_names %>%
                                 select(Technology.Code, fuel, cooling_system),
                               by = "Technology.Code") %>%
      left_join_error_no_match(UCS_water_types %>%
                                 rename(reported_water_source = `Reported Water Source (Type)`),
                               by = "reported_water_source") %>%
      # UCS database includes Plant Code & Plant-Generator Code
      # There are often multiple Plant-Generator entries per Plant Code, while EIA data has one entry per Plant Code
      # Filter for largest generator (by capacity) within a plant, to avoid duplicating entries in EIA data
      group_by(Plant.Code, state, fuel, cooling_system, water_type) %>%
      summarise(cap_MW = sum(cap_MW)) %>%
      ungroup() %>%
      group_by(Plant.Code, state) %>%
      filter(cap_MW == max(cap_MW)) %>%
      distinct(Plant.Code, state, cooling_system, water_type) ->
      UCS_db_adj

    # UCS_db_adj %>%
    #   group_by(Plant.Code, state) %>%
    #   filter(row_number() > 1) -> TEST


    # Join EIA capacity / generation data w/ UCS cooling data
    eia_elec_data %>%
      # LJENM will produce lots of NAs because UCS data set is incomplete; we'll deal with NA values below
      # even after filtering for largest generator within a given plant, some duplicate entries remain in UCS_db_adj
      # use left_join_keep_first_only() to avoid duplicating entries in EIA capacity / generation data
      left_join_keep_first_only(UCS_db_adj, by = c("Plant.ID" = "Plant.Code", "state")) %>%
      # some plants in the UCS data have cooling systems which GCAM doesn't model
      # (e.g. recirculating for PV, none for coal_conv) - reset these to reasonable values
      mutate(cooling_system = if_else(elec_tech %in% gcamusa.ELEC_TECHS_NO_COOLING, gcamusa.ELEC_COOLING_SYSTEM_NONE, cooling_system),
             cooling_system = if_else(!(elec_tech %in% gcamusa.ELEC_TECHS_NO_COOLING) & cooling_system == gcamusa.ELEC_COOLING_SYSTEM_NONE,
                                      gcamusa.ELEC_COOLING_SYSTEM_DEFAULT, cooling_system),
             # set water type to "none" for techs with no cooling system
             water_type = if_else(cooling_system == gcamusa.ELEC_COOLING_SYSTEM_NONE, gcamusa.ELEC_COOLING_SYSTEM_NONE, water_type),
             # hydro and PV have some water wd / consumption even though they don't have cooling systems; set water type to fresh
             water_type = if_else(elec_tech %in% gcamusa.ELEC_TECHS_NO_COOLING_FRESH, gcamusa.WATER_TYPE_FRESH, water_type),
             # if water type is seawater, set cooling system to seawater
             cooling_system = if_else(water_type == gcamusa.WATER_TYPE_SEAWATER, gcamusa.WATER_TYPE_SEAWATER, cooling_system)) ->
      eia_elec_data_water

    # split out data with cooling system specified vs. not (NAs)
    eia_elec_data_water %>%
      filter(!is.na(cooling_system)) -> eia_elec_data_water_cool

    # NOTE:  ~2,500 plants have no cooling tech info, but this only
    # amounts to ~14% of capacity and ~11% of generation nationally
    eia_elec_data_water %>%
      filter(is.na(cooling_system)) -> eia_elec_data_water_missing

    # calculate national cooling system shares, distinguishing between coastal and non-coastal states,
    # to fill in missing cooling tech data
    # calcluate shares on capacity and generation basis
    eia_elec_data_water_cool %>%
      mutate(seawater = if_else(state %in% seawater_states_basins, "coastal", "non-coastal")) %>%
      group_by(seawater, gcam_fuel, elec_tech, cooling_system, water_type) %>%
      summarise(Nameplate.MW = sum(Nameplate.MW),
                en_out = sum(en_out)) %>%
      ungroup() %>%
      group_by(seawater, gcam_fuel, elec_tech) %>%
      mutate(cap_share = Nameplate.MW / sum(Nameplate.MW),
             gen_share = en_out / sum(en_out)) %>%
      ungroup() -> eia_elec_data_water_cool_shares

    # map shares
    eia_elec_data_water_missing %>%
      mutate(seawater = if_else(state %in% seawater_states_basins, "coastal", "non-coastal")) %>%
      # remove cooling_system and water_type columns, which have NAs
      select(-cooling_system, -water_type) %>%
      # join is intended to duplicate rows by number of potential cooling techs
      # LJENM will throw error, so LJ is used
      left_join(eia_elec_data_water_cool_shares %>%
                  select(-Nameplate.MW, -en_out),
                by = c("seawater", "gcam_fuel", "elec_tech")) %>%
      # TODO:  more robust
      # one NA entry - IN RL_CC (no other non-coastal techs in this category)
      # assign recirculating freshwater for now
      replace_na(list(cooling_system = "recirculating",
                      water_type = "fresh",
                      gen_share = 1,
                      cap_share = 1)) %>%
      # share out energy variables by energy share, and capacity variables by capacity share
      # the underlying assumption is that cooling systems have identical efficiencies for a given gen tech
      mutate(en_in = en_in * gen_share,
             en_in_elec = en_in_elec * gen_share,
             en_out = en_out * gen_share,
             Nameplate.MW = Nameplate.MW * cap_share,
             NAMEPLATE = NAMEPLATE * cap_share) %>%
      # remove seawater, cap share, and gen share info, which we no longer need
      select(-seawater, -cap_share, -gen_share) -> eia_elec_data_water_inferred

    eia_elec_data_water_cool %>%
      bind_rows(eia_elec_data_water_inferred) -> eia_elec_data_water_full

    # # check for any goofy tech / cooling system combinations
    # eia_elec_data_water_full %>%
    #   group_by(gcam_fuel, elec_tech, cooling_system, water_type) %>%
    #   summarize(en_in_elec = sum(en_in_elec),
    #             en_out = sum(en_out),
    #             NAMEPLATE = sum(NAMEPLATE)) -> TEST

    # # Make sure that we're not inflating capacity or generation by mapping in cooling system info
    # eia_elec_data %>%
    #   summarize(en_in_elec = sum(en_in_elec),
    #             en_out = sum(en_out),
    #             NAMEPLATE = sum(NAMEPLATE)) -> TEST1
    #
    # eia_elec_data_water_full %>%
    #   summarize(en_in_elec = sum(en_in_elec),
    #             en_out = sum(en_out),
    #             NAMEPLATE = sum(NAMEPLATE)) -> TEST2


    # Aggregate capacity and generation by tech / cooling system
    # capacity by state by vintage
    eia_elec_data_water_full %>%
      group_by(state, gcam_fuel, elec_tech, vintage, cooling_system, water_type) %>%
      summarize(capacity = sum(NAMEPLATE)) %>%
      ungroup() ->
      elec_capacity_state_vintage

    # generation by state by vintage
    eia_elec_data_water_full %>%
      group_by(state, gcam_fuel, elec_tech, vintage, cooling_system, water_type) %>%
      summarize(en_out = sum(en_out)) %>%
      ungroup() ->
      elec_generation_state_vintage

    # fuel consumption by state by vintage
    eia_elec_data_water_full %>%
      group_by(state, gcam_fuel, elec_tech, vintage, cooling_system, water_type) %>%
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
                     "gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/states_subregions",
                     "gcam-usa/UCS_tech_names",
                     "gcam-usa/UCS_water_types",
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     "gcam-usa/UCS_Database",
                     "gcam-usa/usa_seawater_states_basins") ->
      L105.elec_capacity_state_vintage_gcamusa

    elec_generation_state_vintage %>%
      add_title("generation by state by vintage up to 2015") %>%
      add_units("EJ") %>%
      add_comments("Processed from EIA From 860 and 923 (Year 2015)") %>%
      add_legacy_name("elec_generation_state_vintage") %>%
      same_precursors_as("L105.elec_capacity_state_vintage_gcamusa") ->
      L105.elec_generation_state_vintage_gcamusa

    elec_fuelconsumption_state_vintage %>%
      add_title("fuel consumption by state by vintage up to 2015") %>%
      add_units("EJ") %>%
      add_comments("Processed from EIA From 860 and 923 (Year 2015)") %>%
      add_legacy_name("elec_fuelconsumption_state_vintage") %>%
      same_precursors_as("L105.elec_capacity_state_vintage_gcamusa") ->
      L105.elec_fuelconsumption_state_vintage_gcamusa

    return_data(L105.elec_capacity_state_vintage_gcamusa,
                L105.elec_generation_state_vintage_gcamusa,
                L105.elec_fuelconsumption_state_vintage_gcamusa)
  } else {
    stop("Unknown command")
  }
}
