#' module_gcamusa_L2241.coal_retire_USA
#'
#' Generates GCAM-USA model input for removing coal capacity retired between 2011 and 2015, 2016 and 2020, and vintaging capacity which continues to operate beyond 2020.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2241.TechProd_elec_coalret_dispatch_gcamusa}, \code{L2241.CapacityTech_elec_coalret_dispatch_gcamusa},
#' \code{L2241.TechEff_elec_coalret_dispatch_gcamusa}, \code{L2241.TechSCurve_elec_coalret_dispatch_gcamusa},
#' \code{L2241.TechShrwt_elec_coalret_dispatch_gcamusa}, \code{L2241.TechOMvar_elec_coalret_dispatch_gcamusa},
#' \code{L2241.TechProd_coal_vintage_dispatch_gcamusa}, \code{L2241.CapacityTech_coal_vintage_dispatch_gcamusa},
#' \code{L2241.TechEff_coal_vintage_dispatch_gcamusa}, \code{L2241.TechOMvar_coal_vintage_dispatch_gcamusa},
#' \code{L2241.TechShrwt_coal_vintage_dispatch_gcamusa}, \code{L2241.TechSCurve_coal_vintage_dispatch_gcamusa},
#' \code{L2241.TechCapFac_coalret_vintage_dispatch_gcamusa}, \code{L2241.CapacityTechAvail_coalret_vintage_dispatch_gcamusa}.
#' The corresponding file in the original data system was \code{L2241.coal_slow_fast_retire_USA.R} (gcam-usa level2).
#' @details This chunk creates add-on files to take the fraction of reduction in coal electricity generation between 2010 and 2015 for each state and
#' forces that generation to retire in 2015. It also tempers retirement assumptions for the remaining coal fleet to allow
#' most 2015 generation to continue through mid-century.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RC Aug 2018 / YO Jul 2020
module_gcamusa_L2241.coal_retire_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/A23.elec_tech_mapping_coal_retire_dispatch",
             FILE = "gcam-usa/A23.elec_tech_coal_retire_SCurve_dispatch",
             FILE = "gcam-usa/EIA_coal_generation_2018",
             FILE = "gcam-usa/dispatch/AEO2019Plantfile",
             FILE = "gcam-usa/dispatch/ECP_mapping",
             FILE = "gcam-usa/EIA_923_generator_gen_fuel_2018",
             FILE = "gcam-usa/dispatch/coal_vintage_bins",
             FILE = "gcam-usa/A23.dispatch_capacitytech_min_cap_fac",
             "L123.out_EJ_state_elec_F_tech",
             "L123.in_EJ_state_elec_F_tech",
             "L223.CapacityTech",
             "L223.TechEff_Cal",
             "L223.TechOMvar_Dispatch",
             "L223.TechCapFac_Dispatch"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2241.TechProd_elec_coalret_dispatch_gcamusa",
             "L2241.CapacityTech_elec_coalret_dispatch_gcamusa",
             "L2241.TechEff_elec_coalret_dispatch_gcamusa",
             "L2241.TechSCurve_elec_coalret_dispatch_gcamusa",
             "L2241.TechShrwt_elec_coalret_dispatch_gcamusa",
             "L2241.TechOMvar_elec_coalret_dispatch_gcamusa",
             "L2241.TechProd_coal_vintage_dispatch_gcamusa",
             "L2241.CapacityTech_coal_vintage_dispatch_gcamusa",
             "L2241.TechEff_coal_vintage_dispatch_gcamusa",
             "L2241.TechOMvar_coal_vintage_dispatch_gcamusa",
             "L2241.CapacityTechMinCapFac_coal_vintage_dispatch_gcamusa",
             "L2241.TechProfitShutdown_coal_vintage_dispatch_gcamusa",
             "L2241.TechShrwt_coal_vintage_dispatch_gcamusa",
             "L2241.TechSCurve_coal_vintage_dispatch_gcamusa",
             "L2241.TechCapFac_coalret_vintage_dispatch_gcamusa",
             "L2241.CapacityTechAvail_coalret_vintage_dispatch_gcamusa"))

  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # silence package check notes
    region <- year <- source.key <- fuel <- state <- units <- value <- state_name  <-
      retire_frac <- elec_tech <- calOutput_OLD <- technology <- tech.share.weight <- calOutputValue <-
      share.weight.year <- subs.share.weight <- capacity <- capacity_OLD <- current.tech <-
      share.weight <- input.OM.var <- OM.var <- T_PID <- T_UID <- EFDcd <- ECPcd <- WC_NP <-
      TRFURB <- W_SYR <- W_RYR <- W_FOM <- W_VOM <- WHRATE <- Description <- efficiency <-
      OMF <- OMV <- Plant.ID <- Unit.ID <- Operating.Year <- Refurb.Year <- Planned.Retirement.Year <-
      median_size <- size <- vintage.bin <- generation <- Retirement.Year <- cap.lifetime <- State <-
      generation.share.vintage <- capacity.share.vintage <- subsector <- technology.new <-
      steepness <- half.life <- Plant.Id <- Generator.Id <- generation_2018 <- Net.Generation.Year.To.Date <-
      efficiency_weighted <- OMV_weighted <- min.capacity.factor <- NULL

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    A23.elec_tech_mapping_coal_retire_dispatch <- get_data(all_data, "gcam-usa/A23.elec_tech_mapping_coal_retire_dispatch")
    A23.elec_tech_coal_retire_SCurve_dispatch <- get_data(all_data, "gcam-usa/A23.elec_tech_coal_retire_SCurve_dispatch")
    EIA_coal_generation_2018 <- get_data(all_data, "gcam-usa/EIA_coal_generation_2018")
    REEDS_Plantfile <- get_data(all_data, "gcam-usa/dispatch/AEO2019Plantfile")
    ECP_mapping <- get_data(all_data, "gcam-usa/dispatch/ECP_mapping")
    eia_923_data_2018 <- get_data(all_data, "gcam-usa/EIA_923_generator_gen_fuel_2018")
    vintage_bins_mapping <- get_data(all_data, "gcam-usa/dispatch/coal_vintage_bins")
    A23.dispatch_capacitytech_min_cap_fac <- get_data(all_data, "gcam-usa/A23.dispatch_capacitytech_min_cap_fac")

    L123.out_EJ_state_elec_F_tech <- get_data(all_data, "L123.out_EJ_state_elec_F_tech")
    L123.in_EJ_state_elec_F_tech <- get_data(all_data, "L123.in_EJ_state_elec_F_tech")
    L223.CapacityTech <- get_data(all_data, "L223.CapacityTech")
    L223.TechEff_Cal <- get_data(all_data, "L223.TechEff_Cal")
    L223.TechOMvar_Dispatch <- get_data(all_data, "L223.TechOMvar_Dispatch")
    L223.TechCapFac_Dispatch <- get_data(all_data, "L223.TechCapFac_Dispatch")


    # -----------------------------------------------------------------------------
    # Perform computations

    # 1. capacity technology that will be retired in 2020

    # Prepare a table for capacity technologies with all states and base years
    A23.elec_tech_mapping_coal_retire_dispatch %>%
      mutate(year = MODEL_FINAL_BASE_YEAR) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) ->
      L2241.elec_USA_coalret_base

    EIA_coal_gen_years <- EIA_coal_generation_2018 %>%
      gather_years()
    EIA_coal_gen_years <- unique(EIA_coal_gen_years$year)

    # Calculate fraction of historical (2015) vintage retired by 2018 (as a proxy of 2020)
    EIA_coal_generation_2018 %>%
      select(-source.key) %>%
      gather_years() %>%
      replace_na(list(value = 0)) %>%
      # The EIA coal generation data set (EIA_coal_generation_2018) begins in 2001
      # This is fine for the data's intended purposes, but causes test_timeshift to fail when max(MODEL_BASE_YEARS) < 2001
      # Backfill historical values with 2001 values to avoid test_timeshift failure
      complete(nesting(fuel, state, units), year = c(HISTORICAL_YEARS, EIA_coal_gen_years))  %>%
      group_by(fuel, state, units) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      rename(state_name = state) %>%
      left_join_error_no_match(states_subregions %>%
                                 select(state, state_name),
                               by = "state_name") %>%
      filter(year %in% c(MODEL_FINAL_BASE_YEAR, max(year))) %>%
      select(state, year, units, value) %>%
      group_by(state) %>%
      mutate(retire_frac = 1 - (value / value[year==MODEL_FINAL_BASE_YEAR])) %>%
      filter(year == max(year)) %>%
      replace_na(list(retire_frac = 0)) %>%
      # 4 states (AR, HI, SD, WA) have retirement fractions < 0 (more coal in 2018 than base year) (all are small values besides AR)
      # for these states, default all fractions < 0 to 0; this will (slightly) under-estimate coal generation in 2018,
      # which is acceptable because 2018 will over-estimate 2020 generation
      mutate(retire_frac = if_else(retire_frac < 0, 0, retire_frac)) %>%
      select(region = state, retire_frac) -> fraction_coal_gen_retire

    #Create two technologies: coal_base_conv pul and coal_base_conv pul_retire_2020
    # L2241.TechProd_elec_coalret_dispatch_gcamusa:  Calibration outputs for conventional coal electricity plants by U.S. state

    # 2015 total conv coal generaiton.
    L123.out_EJ_state_elec_F_tech %>%
      filter(elec_tech == "coal_conv" & year == MODEL_FINAL_BASE_YEAR) %>%
      rename(region = state) %>%
      select(region, value) ->
      L123.total_coal_gen

    L2241.elec_USA_coalret_base %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      filter(region %in% L123.total_coal_gen$region) %>%
      left_join_error_no_match(L123.total_coal_gen, by = "region") %>%
      # To check: Based on EIA 923, ME has no coal (conv pul) in 2015, but just a CHP plant
      # but in fraction_coal_gen_retire based on the online EIA form, it has utility coal generation in 2015 and 2018
      # this is the only inconsistent state, but the generation in ME is minor
      left_join_error_no_match(fraction_coal_gen_retire, by = "region") %>%
      rename(calOutput_OLD = value) %>%
      mutate(calOutputValue = if_else(!grepl("_retire2020", technology),
                                      calOutput_OLD * (1 - retire_frac),
                                      calOutput_OLD * retire_frac),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      mutate(share.weight.year = year) %>%
      mutate(subs.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) ->
      L2241.TechProd_elec_coalret_dispatch_gcamusa

    # L2241.CapacityTech_elec_coalret_dispatch_gcamusa: existing capacity for conventional coal electricity plants by U.S. state
    L223.CapacityTech %>%
      filter(capacity.technology == "coal (conv pul)" & year == MODEL_FINAL_BASE_YEAR) %>%
      select(region, capacity) ->
      L223.total_coal_capacity

    L2241.elec_USA_coalret_base %>%
      filter(region %in% L123.total_coal_gen$region,
             year == MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L223.total_coal_capacity, by = "region") %>%
      # here we still use fraction_coal_gen_retire to approximate capacity retirement
      # this would assume these plant has the same capacity factor in 2015 and 2018
      # otherwise we will need another data flow (probably EIA 860 for both 2015 and 2018 to track which capacity is retired)
      left_join_error_no_match(fraction_coal_gen_retire, by = "region") %>%
      rename(capacity_OLD = capacity) %>%
      mutate(capacity = if_else(!grepl("_retire2020", technology),
                                capacity_OLD * (1 - retire_frac),
                                capacity_OLD * retire_frac)) %>%
      rename(dispatch.sector = supplysector, capacity.technology = technology) %>%
      select(LEVEL2_DATA_NAMES[["CapacityTech"]]) ->
      L2241.CapacityTech_elec_coalret_dispatch_gcamusa

    # Create a table to read in efficiencies for the new technologies in calibration years
    # L2241.TechEff_elec_coalret_dispatch_gcamusa: Efficiencies of U.S. conventional coal electricity plants in calibration years
    L2241.elec_USA_coalret_base %>%
      filter(region %in% L123.total_coal_gen$region,
             year == MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L223.TechEff_Cal,
                               by = c("region", "supplysector", "subsector", "current.tech" = "technology", "year")) %>%
      arrange(region, technology, year) %>%
      select(LEVEL2_DATA_NAMES[["TechEff"]]) ->
      L2241.TechEff_elec_coalret_dispatch_gcamusa

    # Create a table to read in s-curve retirement parameters for the new technologies
    # L2241.TechSCurve_elec_coalret_dispatch_gcamusa:  s-curve shutdown decider for historic U.S. conventional coal electricity plants
    # Note that this updates s-curve retirement parameters for both the existing technologies and the new "retire_2020" technologies
    L2241.TechProd_elec_coalret_dispatch_gcamusa %>%
      select(region, supplysector, subsector, technology, year) %>%
      left_join_error_no_match(A23.elec_tech_coal_retire_SCurve_dispatch,
                               by = c("technology")) ->
      L2241.TechSCurve_elec_coalret_dispatch_gcamusa

    # Prepare a table for capacity technologies with all states and final base year
    A23.elec_tech_mapping_coal_retire_dispatch %>%
      # this is only needed for the new "retire_2020" technologies
      filter(grepl("_retire2020", technology)) %>%
      mutate(year = MODEL_FINAL_BASE_YEAR) %>%
      repeat_add_columns(tibble(region = gcamusa.STATES)) ->
      L2241.elec_USA_coalret

    # Share-weights
    # L2241.TechShrwt_elec_coalret_dispatch_gcamusa: Shareweights for historic U.S. conventional coal electricity plants (2015 only)
    L2241.elec_USA_coalret %>%
      filter(region %in% L123.total_coal_gen$region) %>%
      mutate(share.weight = gcamusa.DEFAULT_SHAREWEIGHT) %>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]]) ->
      L2241.TechShrwt_elec_coalret_dispatch_gcamusa

    # OM varible costs
    # L2241.TechOMvar_elec_coalret_dispatch_gcamusa: OM var costs of historic U.S. conventional coal electricity plants
    L2241.elec_USA_coalret %>%
      filter(region %in% L123.total_coal_gen$region) %>%
      left_join_error_no_match(L223.TechOMvar_Dispatch,
                               by = c("region", "supplysector", "subsector", "current.tech" = "technology", "year")) %>%
      select(LEVEL2_DATA_NAMES[["TechOMvar"]]) ->
      L2241.TechOMvar_elec_coalret_dispatch_gcamusa


    # ===================================================
    # 2. Vintage coal plants which operate beyond 2015

    # process REEDS plant files to obtain coal generation by vintages
    # Variable codes
    # EFDcd - plant type
    # ECPcd - fuel category
    # T_PID - plant ID
    # T_UID - unit ID
    # TRFURB - Original Start Year
    # W_SYR - Start Year
    # W_RYR - retirement year
    # WC_NP - nameplate capacity
    # W_FOM - fixed O&M ($1987)
    # W_VOM - variable O&M ($1987)
    # WCOMB_V - Variable Cost, Nox Comb Control  ($1987)
    # W_DSIV - Variable Cost, DSI ($1987)
    # W_FFV - Variable Cost, Fabric Filter ($1987)
    # WSCR_V - Variable Cost, SCR ($1987)
    # WSNCR_V - Variable Cost, SNCR ($1987)
    # WHRATE - heat rate

    REEDS_Plantfile %>%
      select(T_PID, T_UID, WSTATE, EFDcd, ECPcd, WC_NP, TRFURB, W_SYR, W_RYR, W_FOM,
             W_VOM, WCOMB_V, W_DSIV, W_FFV, WSCR_V, WSNCR_V, WHRATE) %>%
      # filter for plants not yet retired in 2020
      filter(W_RYR > 2020) %>%
      left_join_error_no_match(ECP_mapping %>%
                                 select(-Description),
                               by = c("ECPcd" = "ECPT")) %>%
      filter(Fuel == "Coal",
             # filter out IGCC plants
             !(ECPcd %in% c("IG", "IS")),
             # filter out CCS plants
             EFDcd != "CAS") %>%
      # appears to be some duplicate identifiers, so summarise
      group_by(T_PID, T_UID, WSTATE, EFDcd, ECPcd, TRFURB, W_SYR, W_RYR, W_FOM,
               W_VOM, WCOMB_V, W_DSIV, W_FFV, WSCR_V, WSNCR_V, WHRATE) %>%
      summarise(WC_NP = sum(WC_NP)) %>%
      ungroup() %>%
      mutate(efficiency = CONV_KWH_BTU / WHRATE,
             OMF = W_FOM * gdp_deflator(1975, 1987),
             OMV = (W_VOM + WCOMB_V + W_DSIV + W_FFV + WSCR_V + WSNCR_V) * gdp_deflator(1975, 1987),
             year = TRFURB - min(TRFURB)) %>%
      select(Plant.ID = T_PID,
             Unit.ID = T_UID,
             region = WSTATE,
             Operating.Year = TRFURB,
             Refurb.Year = W_SYR,
             Planned.Retirement.Year = W_RYR,
             year,
             capacity = WC_NP,
             efficiency, OMF, OMV) %>%
      left_join_error_no_match(vintage_bins_mapping, by = "Operating.Year") %>%
      group_by(region, vintage) %>%
      mutate(median_size = median(capacity),
             size = if_else(capacity >= median_size, "big", "small")) %>%
      ungroup() %>%
      mutate(vintage.bin = paste0(vintage, "_", size))->
      REEDS_coal

    # obtain state-shares of generation and capacity in 2018 ()
    REEDS_coal %>%
      # Obtain unit/generator-level generation in 2015 from generator-level generation data in Form 923.
      # 13 units are missing geneation data in eia_923_data_2018.  LJENM throws error; use left_join for now
      # most missing units or pre1970s and 1970s
      # these missing units missed both state and generation
      select(Plant.ID, Unit.ID, Operating.Year, Planned.Retirement.Year, capacity, vintage.bin) %>%
      left_join(eia_923_data_2018 %>%
                  rename(generation = Net.Generation.Year.To.Date),
                by = c("Unit.ID" = "Generator.Id", "Plant.ID" = "Plant.Id")) %>%
      mutate(generation = as.numeric(generation)) %>%
      na.omit() %>%
      # a couple of plants in MO, kY, and MI have negative generation values - reset to zero
      mutate(generation = if_else(generation < 0.0, 0.0, generation)) ->
      L2241.coal_units_gen_2018

    # The Planned.Retirement.Year variable reflects planned retirements.
    # When no Planned.Retirement.Year is available, we assume a maximum lifetime of 80 years
    L2241.coal_units_gen_2018 %>%
      # Categorize coal units by vintage bins. We stick with 5-year bins for now
      mutate(Planned.Retirement.Year = as.numeric(Planned.Retirement.Year)) %>%
      # For units without a Planned.Retirement.Year, we assume a maximum lifetime of 80 years
      mutate(Retirement.Year = if_else(is.na(Planned.Retirement.Year) | Planned.Retirement.Year == 9999,
                                       Operating.Year + gcamusa.AVG_COAL_PLANT_LIFETIME,
                                       Planned.Retirement.Year)) ->
      L2241.coal_units_ret_2018

    # Create a table of generation and capacity-weighted lifetime from 2015 and by state and vintage
    L2241.coal_units_ret_2018 %>%
      # Calculate lifetime from 2018, which is model base year
      mutate(cap.lifetime = capacity * (Retirement.Year - MODEL_FINAL_BASE_YEAR)) %>%
      group_by(State, vintage.bin) %>%
      summarise(lifetime = round(sum(cap.lifetime) / sum(capacity), 0), generation = sum(generation), capacity = sum(capacity)) %>%
      mutate(generation = generation * CONV_MWH_GJ * CONV_GJ_EJ,
             # For units with 0 or negative expected lifetime, we will set lifetime to 1 to avoid calibration issues
             lifetime = replace(lifetime, lifetime < 0, 1)) %>%
      ungroup() %>%
      # Keep share of each vintage bin of the total generation in each state ready to be applied to
      # calibrated value in 2015
      group_by(State) %>%
      mutate(generation.share.vintage = generation / sum(generation)) %>%
      mutate(capacity.share.vintage = capacity / sum(capacity)) %>%
      ungroup() %>%
      select(-capacity) ->
      L2241.coal_vintage_2018

    # Apply vintage share by state to calibrated capacity for slow retire component and create table to be read in
    L2241.coal_vintage_2018 %>%
      rename(region = State) %>%
      # LJENM is intended to duplicate rows so production can be allocated across vintages; use left_join to avoid error
      left_join(L2241.CapacityTech_elec_coalret_dispatch_gcamusa %>%
                  filter(subsector == "coal",
                         year == MODEL_FINAL_BASE_YEAR,
                         !grepl("retire", capacity.technology)),
                by = "region") %>%
      filter(!is.na(capacity)) %>%
      mutate(capacity = capacity * capacity.share.vintage,
             capacity.technology = paste(capacity.technology, vintage.bin, sep = "_")) %>%
      select(LEVEL2_DATA_NAMES[["CapacityTech"]]) ->
      L2241.CapacityTech_coal_vintage_dispatch_gcamusa

    # Read in zero capacity for final base year for existing coal conv pul technology
    # becuase this portion has been separate by vintage bins in the final base year
    L2241.CapacityTech_elec_coalret_dispatch_gcamusa %>%
      filter(capacity.technology == "coal (conv pul)") %>%
      mutate(capacity = 0) %>%
      select(LEVEL2_DATA_NAMES[["CapacityTech"]]) %>%
      bind_rows(L2241.CapacityTech_coal_vintage_dispatch_gcamusa) %>%
      arrange(region, capacity.technology, year) ->
      L2241.CapacityTech_coal_vintage_dispatch_gcamusa

    # Apply vintage share by state to calibrated values for slow retire component and create table to be read in
    L2241.coal_vintage_2018 %>%
      rename(region = State) %>%
      # LJENM is intended to duplicate rows so production can be allocated across vintages; use left_join to avoid error
      left_join(L2241.TechProd_elec_coalret_dispatch_gcamusa %>%
                  filter(subsector == "coal",
                         year == MODEL_FINAL_BASE_YEAR,
                         !grepl("retire", technology)),
                by = "region") %>%
      filter(!is.na(calOutputValue), calOutputValue != 0) %>%
      mutate(calOutputValue = calOutputValue * generation.share.vintage,
             # Create new technologies. Naming the variable as technology.new so that we can use technology as reference later
             technology = paste(technology, vintage.bin, sep = "_"),
             year = MODEL_FINAL_BASE_YEAR, share.weight.year = MODEL_FINAL_BASE_YEAR,
             subs.share.weight = gcamusa.DEFAULT_SHAREWEIGHT,
             tech.share.weight = gcamusa.DEFAULT_SHAREWEIGHT) %>%
      # Select variables. For now, include lifetime and vintage.bin as well. We'll remove it later
      select(LEVEL2_DATA_NAMES[["Production"]], lifetime, vintage.bin) ->
      L2241.TechProd_coal_vintage_dispatch_gcamusa

    # Create a table to read in S-curve parameters for vintage bin techs by state
    L2241.TechProd_coal_vintage_dispatch_gcamusa %>%
      select(region, supplysector, subsector, technology, year, lifetime) %>%
      mutate(steepness = gcamusa.COAL_RETIRE_STEEPNESS,
             half.life = round(lifetime * (gcamusa.AVG_COAL_PLANT_HALFLIFE / gcamusa.AVG_COAL_PLANT_LIFETIME), 0)) ->
      L2241.TechSCurve_coal_vintage_dispatch_gcamusa

    # generating weighted average efficiency and OM-var by bin
    eia_923_data_2018 %>%
      filter(Generator.Id != "") %>%
      select(Plant.Id, Generator.Id, generation_2018 = Net.Generation.Year.To.Date) %>%
      mutate(Plant.Id = as.numeric(Plant.Id),
             Generator.Id = as.character(Generator.Id)) %>%
      # to keep plants that only what REEDS_coal has
      semi_join(REEDS_coal, by = c("Plant.Id" = "Plant.ID", "Generator.Id" = "Unit.ID")) ->
      eia_923_gen

    REEDS_coal %>%
      mutate(Plant.ID = as.numeric(Plant.ID),
             Generator.ID = as.character(Unit.ID)) %>%
      select(-Unit.ID) %>%
      # here use left_join becuase 13 units (mostly small plants that REEDS_coal has but not in eia_923)
      left_join(eia_923_gen, by = c("Plant.ID" = "Plant.Id", "Generator.ID" = "Generator.Id")) %>%
      filter(!is.na(generation_2018)) %>%
      group_by(region, vintage.bin) %>%
      summarise(efficiency_weighted = sum(efficiency * generation_2018) / sum(generation_2018),
                OMV_weighted = sum(OMV * generation_2018) / sum(generation_2018)) %>%
      ungroup() %>%
      mutate(technology = paste0("coal (conv pul)_", vintage.bin)) %>%
      select(-vintage.bin) ->
      REEDS_coal_Eff_OMvar

    # Create a basic strucure with common variables
    L2241.TechProd_coal_vintage_dispatch_gcamusa %>%
      select(region, supplysector, subsector, technology, year) %>%
      unique() %>%
      filter(year == MODEL_FINAL_BASE_YEAR) ->
      L2241.TechProd_coal_vintage_dispatch_dataframe

    # Create efficiency for coal vintage capacity technologies
    L2241.TechProd_coal_vintage_dispatch_dataframe %>%
      left_join_error_no_match(REEDS_coal_Eff_OMvar %>% select(region, technology, efficiency_weighted),
                               by = c("region", "technology")) %>%
      rename(efficiency = efficiency_weighted) %>%
      # TODO: somehow update these as constants
      mutate(minicam.energy.input = "regional coal",
             market.name = "USA") %>%
      select(LEVEL2_DATA_NAMES[["TechEff"]]) ->
      L2241.TechEff_coal_vintage_dispatch_gcamusa

    # Variable OM costs
    L2241.TechProd_coal_vintage_dispatch_dataframe %>%
      left_join_error_no_match(REEDS_coal_Eff_OMvar %>% select(region, technology, OMV_weighted),
                               by = c("region", "technology")) %>%
      rename(OM.var = OMV_weighted) %>%
      mutate(input.OM.var = "OM-var") %>%
      select(LEVEL2_DATA_NAMES[['TechOMvar']]) ->
      L2241.TechOMvar_coal_vintage_dispatch_gcamusa

    # Create teable to read in min capacity factor
    bind_rows(L2241.TechOMvar_coal_vintage_dispatch_gcamusa,
              # not really needed but to avoid some errors messages in the model add them
              L2241.TechOMvar_elec_coalret_dispatch_gcamusa) %>%
      mutate(technology.match = gsub("_.*$", "", technology)) %>%
      select(-input.OM.var, -OM.var) %>%
      left_join_error_no_match(select(A23.dispatch_capacitytech_min_cap_fac, technology, min.capacity.factor),
                               by = c("technology.match" = "technology")) %>%
      select(-technology.match) %>%
      rename(dispatch.sector = supplysector,
             capacity.technology = technology) ->
      L2241.CapacityTechMinCapFac_coal_vintage_dispatch_gcamusa

    # Create table to read in capacity investment discount params
    L2241.CapacityTechMinCapFac_coal_vintage_dispatch_gcamusa %>%
      select(-min.capacity.factor) %>%
      mutate(median.shutdown.point = gcamusa.ELEC_CAP_INV_MEDIAN,
             profit.shutdown.steepness = gcamusa.ELEC_CAP_INV_STEEPNESS) %>%
      rename(supplysector = dispatch.sector,
             technology = capacity.technology) ->
      L2241.TechProfitShutdown_coal_vintage_dispatch_gcamusa

    # Create table to read in shareweights in future years
    L2241.TechProd_coal_vintage_dispatch_dataframe %>%
      mutate(share.weight = 0) %>%
      select(LEVEL2_DATA_NAMES[['TechShrwt']]) ->
      L2241.TechShrwt_coal_vintage_dispatch_gcamusa

    # Clean up coal vintage production table
    L2241.TechProd_coal_vintage_dispatch_gcamusa %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) %>%
      mutate(share.weight.year = year) %>%
      # Read in zero caloutputvalue for other base years
      replace_na(list(calOutputValue = 0, subs.share.weight = 1, tech.share.weight = 0)) ->
      L2241.TechProd_coal_vintage_dispatch_gcamusa

    # Read in zero calOutputValue for final base year for existing coal conv pul technology
    L2241.TechProd_elec_coalret_dispatch_gcamusa %>%
      filter(technology == "coal (conv pul)") %>%
      mutate(calOutputValue = 0, tech.share.weight = 0) %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) %>%
      bind_rows(L2241.TechProd_coal_vintage_dispatch_gcamusa) %>%
      arrange(region, technology, year) ->
      L2241.TechProd_coal_vintage_dispatch_gcamusa

    # Table with technology capacity factors
    L2241.CapacityTech_elec_coalret_dispatch_gcamusa %>%
      select(-capacity) %>%
      # remove default coal (conv pul) technology - CF assumptions already exist for this tech
      anti_join(A23.elec_tech_mapping_coal_retire_dispatch, by = c("capacity.technology" = "current.tech")) %>%
      rename(supplysector = dispatch.sector,
             technology = capacity.technology) %>%
      bind_rows(L2241.TechProd_coal_vintage_dispatch_dataframe) %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L223.TechCapFac_Dispatch %>%
                                 semi_join(A23.elec_tech_coal_retire_SCurve_dispatch, by = "technology") %>%
                                 select(-technology),
                               by = c("region", "supplysector", "subsector", "year" )) ->
      L2241.TechCapFac_coalret_vintage_dispatch_gcamusa

    # Table specifying that these techs are only available in 2015
    L2241.TechCapFac_coalret_vintage_dispatch_gcamusa %>%
      select(-year, -capacity.factor) %>%
      mutate(initial.available.year = MODEL_FINAL_BASE_YEAR,
             final.available.year = MODEL_FINAL_BASE_YEAR) -> L2241.CapacityTechAvail_coalret_vintage_dispatch_gcamusa

    # scale efficiency to match both energy input and ouput from IEA in 2015
    # here need to do both coal (conv pul)_retire2020 and coal vintage bins

    # total production
    L2241.TechProd_elec_coalret_dispatch_gcamusa %>%
      filter(technology == "coal (conv pul)_retire2020") %>%
      bind_rows(L2241.TechProd_coal_vintage_dispatch_gcamusa %>%
                  filter(technology != "coal (conv pul)")) -> L2241.total_production

    # current efficiency assumption
    L2241.TechEff_elec_coalret_dispatch_gcamusa %>%
      filter(technology == "coal (conv pul)_retire2020") %>%
      bind_rows(L2241.TechEff_coal_vintage_dispatch_gcamusa %>%
                  filter(technology != "coal (conv pul)")) -> L2241.efficiency_unadjusted

    # calculate the corresponding enengy input for coal based on unadjusted efficiency
    L2241.total_production %>%
      left_join_error_no_match(L2241.efficiency_unadjusted,
                               by = c("region", "supplysector", "subsector", "technology", "year")) %>%
      mutate(calInputValue = calOutputValue / efficiency) %>%
      group_by(region) %>%
      summarise(calInputValue = sum(calInputValue)) %>%
      ungroup() -> L2241.input_unadjusted

    # obtain the input energy from IEA balance
    L123.in_EJ_state_elec_F_tech %>%
      filter(year == 2015 & elec_tech == "coal_conv") %>%
      group_by(state) %>%
      summarise(calibratedInputValue = sum(value)) %>%
      ungroup() %>%
      rename(region = state) -> L2241.input_calibrated

    # derive efficiency adjustment factor at state level
    L2241.input_calibrated %>%
      left_join_error_no_match(L2241.input_unadjusted, by = "region") %>%
      mutate(eff_adj = calInputValue / calibratedInputValue) %>%
      select(region, eff_adj) ->
      L2241.eff_adj

    # update current efficiency table
    L2241.TechEff_elec_coalret_dispatch_gcamusa %>%
      left_join_error_no_match(L2241.eff_adj, by = "region") %>%
      mutate(efficiency = efficiency * eff_adj) %>%
      select(-eff_adj) -> L2241.TechEff_elec_coalret_dispatch_gcamusa

    L2241.TechEff_coal_vintage_dispatch_gcamusa %>%
      left_join_error_no_match(L2241.eff_adj, by = "region") %>%
      mutate(efficiency = efficiency * eff_adj) %>%
      select(-eff_adj) -> L2241.TechEff_coal_vintage_dispatch_gcamusa

    # ===================================================
    # Produce outputs

    L2241.TechProd_elec_coalret_dispatch_gcamusa %>%
      add_title("Calibration outputs for capacity technology coal (conv pul) by state in final base year") %>%
      add_units("EJ") %>%
      add_comments("Conventional coal electricity outputs are allocated to fast retire and slow retire technologies") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire_dispatch",
                     "gcam-usa/states_subregions",
                     "gcam-usa/EIA_coal_generation_2018",
                     "L123.out_EJ_state_elec_F_tech") ->
      L2241.TechProd_elec_coalret_dispatch_gcamusa

    L2241.CapacityTech_elec_coalret_dispatch_gcamusa %>%
      add_title("existing capacity for capacity technology coal (conv pul) by state in final base year") %>%
      add_units("EJ") %>%
      add_comments("Conventional coal electricity capacity are allocated to fast retire and slow retire technologies") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire_dispatch",
                     "gcam-usa/states_subregions",
                     "gcam-usa/EIA_coal_generation_2018",
                     "L223.CapacityTech") ->
      L2241.CapacityTech_elec_coalret_dispatch_gcamusa

    L2241.TechEff_elec_coalret_dispatch_gcamusa %>%
      add_title("efficiency for capacity technology coal (conv pul) by state in final base year") %>%
      add_units("Unitless") %>%
      add_comments("same efficiency are applied to fast retire and slow retire technologies") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire_dispatch",
                     "L123.out_EJ_state_elec_F_tech",
                     "L123.in_EJ_state_elec_F_tech",
                     "L223.TechEff_Cal") ->
      L2241.TechEff_elec_coalret_dispatch_gcamusa

    L2241.TechSCurve_elec_coalret_dispatch_gcamusa %>%
      add_title("S-curve shutdown decider for for capacity technology coal (conv pul) by state in final base year") %>%
      add_units("Unitless") %>%
      add_comments("Separate fast retire and slow retire technologies") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire_dispatch",
                     "gcam-usa/A23.elec_tech_coal_retire_SCurve_dispatch",
                     "gcam-usa/states_subregions",
                     "gcam-usa/EIA_coal_generation_2018",
                     "L123.out_EJ_state_elec_F_tech") ->
      L2241.TechSCurve_elec_coalret_dispatch_gcamusa

    L2241.TechShrwt_elec_coalret_dispatch_gcamusa %>%
      add_title("Shareweights for for capacity technology coal (conv pul) by state in final base year") %>%
      add_units("Unitless") %>%
      add_comments("Separate fast retire and slow retire technologies") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire_dispatch",
                     "L123.out_EJ_state_elec_F_tech") ->
      L2241.TechShrwt_elec_coalret_dispatch_gcamusa

    L2241.TechOMvar_elec_coalret_dispatch_gcamusa %>%
      add_title("Variable OM costs for for capacity technology coal (conv pul) by state in final base year") %>%
      add_units("1975$/MWh") %>%
      add_comments("Set the same variable OM cost values for fast retire and slow retire technologies") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire_dispatch",
                     "L123.out_EJ_state_elec_F_tech",
                     "L223.TechOMvar_Dispatch") ->
      L2241.TechOMvar_elec_coalret_dispatch_gcamusa

    L2241.CapacityTechMinCapFac_coal_vintage_dispatch_gcamusa %>%
      add_title("Capacity technology minimum capacity factor") %>%
      add_units("Unitless") %>%
      add_comments("Technologies will not be allowed to dispatch when it's capacity factor") %>%
      add_comments("would fall below this minimum value.") %>%
      add_precursors("gcam-usa/A23.dispatch_capacitytech_min_cap_fac",
                     "gcam-usa/A23.elec_tech_mapping_coal_retire_dispatch",
                     "L123.out_EJ_state_elec_F_tech") ->
      L2241.CapacityTechMinCapFac_coal_vintage_dispatch_gcamusa

    L2241.TechProfitShutdown_coal_vintage_dispatch_gcamusa %>%
      add_title("Dispatch technology capacity investment discount params") %>%
      add_units("NA") %>%
      add_comments("Profit shutdown param that are used to discount existing") %>%
      add_comments("capacity in investment decisions.") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire_dispatch",
                     "L123.out_EJ_state_elec_F_tech") ->
      L2241.TechProfitShutdown_coal_vintage_dispatch_gcamusa

    L2241.TechProd_coal_vintage_dispatch_gcamusa %>%
      add_title("Calibration outputs for slow_retire conventional coal electricity plants by detailed vintage and state") %>%
      add_units("EJ") %>%
      add_comments("Generation shares by vintage are calculated based on REEDS 2019 data and EIA Form 923 matched by plant and unit ID") %>%
      add_comments("Generation shares by vintage are then applied to slow_retire stub-technology 2015 generation in each state") %>%
      add_comments("Generation in other base years are set to zero to each vintage stub-technology") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire_dispatch",
                     "gcam-usa/states_subregions",
                     "gcam-usa/EIA_coal_generation_2018",
                     "L123.out_EJ_state_elec_F_tech",
                     "gcam-usa/EIA_923_generator_gen_fuel_2018",
                     "gcam-usa/dispatch/coal_vintage_bins",
                     "gcam-usa/dispatch/ECP_mapping",
                     "gcam-usa/dispatch/AEO2019Plantfile") ->
      L2241.TechProd_coal_vintage_dispatch_gcamusa

    L2241.CapacityTech_coal_vintage_dispatch_gcamusa %>%
      add_title("Existing capacity for slow_retire conventional coal electricity plants by detailed vintage and state") %>%
      add_units("EJ") %>%
      add_comments("Capacity shares by vintage are calculated based on REEDS 2019 data") %>%
      add_comments("Capacity shares by vintage are then applied to slow_retire stub-technology 2015 fleet in each state") %>%
      add_comments("Capacity of coal (conv pul) is set to zero in final base year") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire_dispatch",
                     "gcam-usa/states_subregions",
                     "gcam-usa/EIA_coal_generation_2018",
                     "L223.CapacityTech",
                     "gcam-usa/EIA_923_generator_gen_fuel_2018",
                     "gcam-usa/dispatch/coal_vintage_bins",
                     "gcam-usa/dispatch/ECP_mapping",
                     "gcam-usa/dispatch/AEO2019Plantfile") ->
      L2241.CapacityTech_coal_vintage_dispatch_gcamusa

    L2241.TechEff_coal_vintage_dispatch_gcamusa %>%
      add_title("Efficiencies for slow_retire conventional coal electricity plants by detailed vintage and state") %>%
      add_units("Unitless") %>%
      add_comments("Efficiency by vintage are calculated based on REEDS 2019 data") %>%
      add_comments("Efficiency are weighted by generation (EIA Form 923)") %>%
      add_precursors("gcam-usa/A23.elec_tech_mapping_coal_retire_dispatch",
                     "gcam-usa/states_subregions",
                     "gcam-usa/EIA_coal_generation_2018",
                     "L123.out_EJ_state_elec_F_tech",
                     "L123.in_EJ_state_elec_F_tech",
                     "gcam-usa/EIA_923_generator_gen_fuel_2018",
                     "gcam-usa/dispatch/coal_vintage_bins",
                     "gcam-usa/dispatch/ECP_mapping",
                     "gcam-usa/dispatch/AEO2019Plantfile") ->
      L2241.TechEff_coal_vintage_dispatch_gcamusa

    L2241.TechShrwt_coal_vintage_dispatch_gcamusa %>%
      add_title("Shareweights slow_retire conventional coal electricity plants by detailed vintage and state") %>%
      add_units("Unitless") %>%
      add_comments("set as zero for future years") %>%
      same_precursors_as("L2241.TechProd_coal_vintage_dispatch_gcamusa") ->
      L2241.TechShrwt_coal_vintage_dispatch_gcamusa

    L2241.TechOMvar_coal_vintage_dispatch_gcamusa %>%
      add_title("OM var for slow_retire conventional coal electricity plants by detailed vintage and state") %>%
      add_units("1975$/MWh") %>%
      add_comments("OM var by vintage are calculated based on REEDS 2019 data") %>%
      add_comments("OM var are weighted by generation (EIA Form 923)") %>%
      same_precursors_as("L2241.TechEff_coal_vintage_dispatch_gcamusa") ->
      L2241.TechOMvar_coal_vintage_dispatch_gcamusa

    L2241.TechSCurve_coal_vintage_dispatch_gcamusa %>%
      add_title("Lifetime and retirement parameters for slow_retire conventional coal electricity plants by detailed vintage and state") %>%
      add_units("years") %>%
      add_comments("Average lifetime for each vintage group is weighted by capacity, based on EIA unit-level 2015 data") %>%
      add_comments("Only for vintage groups with greater than 20 years of lifetime remaining") %>%
      same_precursors_as("L2241.TechProd_coal_vintage_dispatch_gcamusa") ->
      L2241.TechSCurve_coal_vintage_dispatch_gcamusa

    L2241.TechCapFac_coalret_vintage_dispatch_gcamusa %>%
      add_title("existing capacity for capacity technology coal (conv pul) by state in final base year") %>%
      add_units("EJ") %>%
      add_comments("Conventional coal electricity capacity are allocated to fast retire and slow retire technologies") %>%
      same_precursors_as("L2241.CapacityTech_elec_coalret_dispatch_gcamusa") %>%
      same_precursors_as("L2241.TechProd_coal_vintage_dispatch_gcamusa") %>%
      add_precursors("gcam-usa/A23.elec_tech_coal_retire_SCurve_dispatch",
                     "L223.TechCapFac_Dispatch") ->
      L2241.TechCapFac_coalret_vintage_dispatch_gcamusa

    L2241.CapacityTechAvail_coalret_vintage_dispatch_gcamusa %>%
      add_title("existing capacity for capacity technology coal (conv pul) by state in final base year") %>%
      add_units("EJ") %>%
      add_comments("Conventional coal electricity capacity are allocated to fast retire and slow retire technologies") %>%
      same_precursors_as("L2241.CapacityTech_elec_coalret_dispatch_gcamusa") %>%
      same_precursors_as("L2241.TechProd_coal_vintage_dispatch_gcamusa") %>%
      add_precursors("L2241.TechCapFac_coalret_vintage_dispatch_gcamusa") ->
      L2241.CapacityTechAvail_coalret_vintage_dispatch_gcamusa

    return_data(L2241.TechProd_elec_coalret_dispatch_gcamusa,
                L2241.CapacityTech_elec_coalret_dispatch_gcamusa,
                L2241.TechEff_elec_coalret_dispatch_gcamusa,
                L2241.TechSCurve_elec_coalret_dispatch_gcamusa,
                L2241.TechShrwt_elec_coalret_dispatch_gcamusa,
                L2241.TechOMvar_elec_coalret_dispatch_gcamusa,
                # vintage of existing techs
                L2241.TechProd_coal_vintage_dispatch_gcamusa,
                L2241.CapacityTech_coal_vintage_dispatch_gcamusa,
                L2241.TechEff_coal_vintage_dispatch_gcamusa,
                L2241.TechOMvar_coal_vintage_dispatch_gcamusa,
                L2241.CapacityTechMinCapFac_coal_vintage_dispatch_gcamusa,
                L2241.TechProfitShutdown_coal_vintage_dispatch_gcamusa,
                L2241.TechShrwt_coal_vintage_dispatch_gcamusa,
                L2241.TechSCurve_coal_vintage_dispatch_gcamusa,
                L2241.TechCapFac_coalret_vintage_dispatch_gcamusa,
                L2241.CapacityTechAvail_coalret_vintage_dispatch_gcamusa)

  } else {
    stop("Unknown command")
  }
}
