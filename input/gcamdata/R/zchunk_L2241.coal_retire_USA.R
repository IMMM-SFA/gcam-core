#' module_gcamusa_L2241.coal_retire_USA
#'
#' Generates GCAM-USA model input for removing coal capacity retired between 2011 and 2015, 2016 and 2020, and vintaging capacity which continues to operate beyond 2020.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2241.Production_coalret_vintage_dispatch_gcamusa}, \code{L2241.CapacityTech_coalret_vintage_dispatch_gcamusa},
#' \code{L2241.TechEff_coalret_vintage_dispatch_gcamusa}, \code{L2241.TechSCurve_coalret_vintage_dispatch_gcamusa},
#' \code{L2241.TechShrwt_coalret_vintage_dispatch_gcamusa}, \code{L2241.TechOMfixed_coalret_vintage_dispatch_gcamusa}, \code{L2241.TechOMvar_coalret_vintage_dispatch_gcamusa},
#' \code{L2241.TechCapFac_coalret_vintage_dispatch_gcamusa}, \code{L2241.CapacityTechAvail_coalret_vintage_dispatch_gcamusa},
#' \code{L2241.CapacityTechMinCapFac_coalret_vintage_dispatch_gcamusa}, \code{L2241.TechProfitShutdown_coalret_vintage_dispatch_gcamusa},
#' and \code{L2241.TechCoef_cool_coalret_vintage_dispatch_gcamusa}.
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
    return(c(FILE = "gcam-usa/calibrated_techs_dispatch_usa",
             FILE = "gcam-usa/A23.elec_tech_mapping_cool",
             FILE = "gcam-usa/A23.dispatch_capacitytech_min_cap_fac",
             FILE = "gcam-usa/A23.elec_tech_coal_retire_SCurve_dispatch",
             FILE = "gcam-usa/prime_mover_map",
             FILE = "gcam-usa/EIA_860_generators_existing_2018",
             FILE = "gcam-usa/EIA_860_generators_retired_2018",
             FILE = "gcam-usa/dispatch/AEO2019Plantfile",
             FILE = "gcam-usa/dispatch/ECP_mapping",
             FILE = "gcam-usa/dispatch/coal_vintage_bins",
             "L105.eia_elec_data_water",
             "L123.in_EJ_state_elec_F_tech",
             "L223.CapacityTech",
             "L223.Production_Dispatch",
             "L223.TechEff_Cal",
             "L223.TechOMfixed_Dispatch",
             "L223.TechOMvar_Dispatch",
             "L223.TechCapFac_Dispatch",
             "L223.TechCoef_Dispatch_cool"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2241.Production_coalret_vintage_dispatch_gcamusa",
             "L2241.CapacityTech_coalret_vintage_dispatch_gcamusa",
             "L2241.TechEff_coalret_vintage_dispatch_gcamusa",
             "L2241.TechSCurve_coalret_vintage_dispatch_gcamusa",
             "L2241.TechShrwt_coalret_vintage_dispatch_gcamusa",
             "L2241.TechOMfixed_coalret_vintage_dispatch_gcamusa",
             "L2241.TechOMvar_coalret_vintage_dispatch_gcamusa",
             "L2241.CapacityTechMinCapFac_coalret_vintage_dispatch_gcamusa",
             "L2241.TechProfitShutdown_coalret_vintage_dispatch_gcamusa",
             "L2241.TechCapFac_coalret_vintage_dispatch_gcamusa",
             "L2241.CapacityTechAvail_coalret_vintage_dispatch_gcamusa",
             "L2241.TechCoef_cool_coalret_vintage_dispatch_gcamusa"))

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
    calibrated_techs_dispatch_usa <- get_data(all_data, "gcam-usa/calibrated_techs_dispatch_usa")
    A23.elec_tech_mapping_cool <- get_data(all_data, "gcam-usa/A23.elec_tech_mapping_cool")
    A23.dispatch_capacitytech_min_cap_fac <- get_data(all_data, "gcam-usa/A23.dispatch_capacitytech_min_cap_fac")
    A23.elec_tech_coal_retire_SCurve_dispatch <- get_data(all_data, "gcam-usa/A23.elec_tech_coal_retire_SCurve_dispatch")

    prime_mover_map <- get_data(all_data, "gcam-usa/prime_mover_map")
    EIA_860_generators_existing_2018 <- get_data(all_data, "gcam-usa/EIA_860_generators_existing_2018")
    EIA_860_generators_retired_2018 <- get_data(all_data, "gcam-usa/EIA_860_generators_retired_2018")

    REEDS_Plantfile <- get_data(all_data, "gcam-usa/dispatch/AEO2019Plantfile")
    ECP_mapping <- get_data(all_data, "gcam-usa/dispatch/ECP_mapping")
    vintage_bins_mapping <- get_data(all_data, "gcam-usa/dispatch/coal_vintage_bins")

    L105.eia_elec_data_water <- get_data(all_data, "L105.eia_elec_data_water")
    L123.in_EJ_state_elec_F_tech <- get_data(all_data, "L123.in_EJ_state_elec_F_tech")
    L223.CapacityTech <- get_data(all_data, "L223.CapacityTech")
    L223.Production_Dispatch <- get_data(all_data, "L223.Production_Dispatch")
    L223.TechEff_Cal <- get_data(all_data, "L223.TechEff_Cal")
    L223.TechOMfixed_Dispatch <- get_data(all_data, "L223.TechOMfixed_Dispatch")
    L223.TechOMvar_Dispatch <- get_data(all_data, "L223.TechOMvar_Dispatch")
    L223.TechCapFac_Dispatch <- get_data(all_data, "L223.TechCapFac_Dispatch")
    L223.TechCoef_Dispatch_cool <- get_data(all_data, "L223.TechCoef_Dispatch_cool")


    # -----------------------------------------------------------------------------
    # Perform computations

    # 1. capacity technology that will be retired in 2020

    # Process EIA 860 Form to get capacity by vintage
    L105.eia_elec_data_water %>%
      filter(gcam_fuel == "coal",
             # remove three small industrial plants
             Sector.Name != "Industrial Non-CHP") %>%
      # if cooling system / water type are missing, set to recirculating / fresh water
      replace_na(list(cooling_system = gcamusa.ELEC_COOLING_SYSTEM_DEFAULT,
                      water_type = gcamusa.WATER_TYPE_FRESH)) %>%
      select(-Combined.Heat.Power.Plant, -NERC.Region, -Sector.Name, -eff, -eff_elec, -en_in_elec) -> EIA_elec_data_coal

    EIA_860_generators_existing_2018 %>%
      bind_rows(EIA_860_generators_retired_2018) %>%
      mutate(unit.capacity.MW = if_else(is.na(Nameplate.Capacity..MW.), Summer.Capacity..MW., Nameplate.Capacity..MW.)) %>%
      left_join_error_no_match(prime_mover_map,
                               by = c("Prime.Mover" = "Reported.Prime.Mover",
                                      "Energy.Source.1" = "Reported.Fuel.Type.Code")) %>%
      filter(gcam_fuel == "coal",
             # filter for plants retiring between 2016-2020
             (Retirement.Year > max(MODEL_BASE_YEARS) & Retirement.Year <= min(MODEL_FUTURE_YEARS) |
                Planned.Retirement.Year > max(MODEL_BASE_YEARS) & Planned.Retirement.Year <= min(MODEL_FUTURE_YEARS))) %>%
      # Note vintages are by generator.. so we need to tag the vintage somehow by
      # plant, using capacity weighted average for now
      group_by(State, Plant.Code) %>%
      # sum capacity and convert to same units as other files
      summarize(capacity_retired = sum(unit.capacity.MW) * CONV_MWH_EJ * CONV_YEAR_HOURS) %>%
      select(State, Plant.Code, capacity_retired) ->
      EIA_coal_retire_capacity

    EIA_elec_data_coal %>%
      select(state, Plant.ID, gcam_fuel, elec_tech, vintage, cooling_system, water_type, capacity = NAMEPLATE, en_in, en_out) %>%
      filter(gcam_fuel == "coal") %>%
      # not all coal plants are retired and thus not all coal plants are included in EIA_coal_retire_capacity table
      # join will produce NAs (resolved below); LJENM will error, so left_join() is used
      left_join(EIA_coal_retire_capacity,
                by = c("state" = "State",
                       "Plant.ID" = "Plant.Code")) %>%
      replace_na(list(capacity_retired = 0)) %>%
      mutate(capacity_remaining = capacity - capacity_retired,
             fraction_retired = capacity_retired / capacity,
             fraction_remaining = 1 - fraction_retired) %>%
      left_join_error_no_match(calibrated_techs_dispatch_usa %>%
                                 select(fuel, elec_tech, supplysector, subsector, technology),
                               by = c("gcam_fuel" = "fuel", "elec_tech")) %>%
      left_join_error_no_match(A23.elec_tech_mapping_cool %>%
                                 mutate(cooling_system = if_else(water_type == gcamusa.WATER_TYPE_SEAWATER,
                                                                 gcamusa.WATER_TYPE_SEAWATER,
                                                                 cooling_system)),
                               by = c("technology", "cooling_system", "water_type")) -> L2241.EIA_coal_plant

    # L2241.EIA_coal_plant represents retirement at the plant level - some plants have multiple
    # generation units, which may retire at different times
    # Here, aggregate to retirement by state & technology (including cooling tech detail), and
    # track share of capacity, fuel consumption, and electricity generation retired
    L2241.EIA_coal_plant %>%
      group_by(state, supplysector, subsector, to.technology) %>%
      summarise(cap_ret_share = sum(capacity_retired) / sum(capacity),
                en_in_ret_share = sum(en_in * fraction_retired) / sum(en_in),
                en_out_ret_share = sum(en_out * fraction_retired) / sum(en_out),
                efficiency = sum(en_out) / sum(en_in)) %>%
      ungroup() -> L2241.EIA_coal

    # Files for new coal_retire2020 technology
    L223.CapacityTech %>%
      filter(year == max(MODEL_BASE_YEARS),
             subsector == "coal") %>%
      rename(original_capacity = capacity) %>%
      left_join_error_no_match(L2241.EIA_coal,
                               by = c("region" = "state",
                                      "dispatch.sector" = "supplysector",
                                      "subsector",
                                      "capacity.technology" = "to.technology")) %>%
      rename(cap.tech = capacity.technology) %>%
      mutate(cap.tech.cool = paste0(cap.tech, " (retire 2020)")) ->
      L2241.CapacityTech

    # still active
    L2241.CapacityTech %>%
      filter(cap_ret_share < 1) %>%
      # rename(capacity = capacity_remaining,
      #        en_in = en_in_remaining,
      #        en_out = en_out_remaining) %>%
      select(region, dispatch.sector, subsector, cap.tech, cap.tech.cool, year,
             original_capacity, cap_ret_share, en_in_ret_share, en_out_ret_share, efficiency) ->
      L2241.CapacityTech_remaining

    # retired
    L2241.CapacityTech %>%
      filter(cap_ret_share > 0) %>%
      # rename(capacity = capacity_retired,
      #        en_in = en_in_retired,
      #        en_out = en_out_retired) %>%
      select(region, dispatch.sector, subsector, cap.tech, cap.tech.cool, year,
             original_capacity, cap_ret_share, en_in_ret_share, en_out_ret_share, efficiency)  ->
      L2241.CapacityTech_retired


    # Files for new coal "retire 2020" technology
    # Capacity
    L2241.CapacityTech_retired %>%
      # ensure that, for techs whose capacity is considered 100% retired,
      # 100% of original capacity assigned to new "retire" technology
      mutate(capacity = original_capacity * cap_ret_share) %>%
      select(region, dispatch.sector, subsector, capacity.technology = cap.tech.cool, year, capacity) ->
      L2241.CapacityTech_elec_coalret_dispatch_gcamusa

    # Historical production
    L2241.CapacityTech_retired %>%
      left_join_error_no_match(L223.Production_Dispatch,
                               by = c("region", "dispatch.sector" = "supplysector", "subsector",
                                      "cap.tech" = "technology", "year")) %>%
      # ensure that, for techs whose capacity is considered 100% retired,
      # 100% of original capacity assigned to new "retire" technology
      mutate(calOutputValue = calOutputValue * en_out_ret_share) %>%
      select(region, dispatch.sector, subsector, capacity.technology = cap.tech.cool, year,
             calOutputValue, share.weight.year, subs.share.weight, tech.share.weight) ->
      L2241.TechProd_elec_coalret_dispatch_gcamusa

    # Efficiency
    L2241.CapacityTech_retired %>%
      left_join_error_no_match(L223.TechEff_Cal %>%
                                 select(-efficiency),
                               by = c("region", "dispatch.sector" = "supplysector", "subsector",
                                      "cap.tech" = "technology", "year")) %>%
      select(region, dispatch.sector, subsector,  capacity.technology = cap.tech.cool, year,
             minicam.energy.input, efficiency, market.name) ->
      L2241.TechEff_elec_coalret_dispatch_gcamusa

    # Fixed O&M costs
    L2241.CapacityTech_retired %>%
      left_join_error_no_match(L223.TechOMfixed_Dispatch,
                               by = c("region", "dispatch.sector" = "supplysector", "subsector",
                                      "cap.tech" = "technology", "year")) %>%
      select(region, dispatch.sector, subsector,  capacity.technology = cap.tech.cool, year, input.OM.fixed, OM.fixed) ->
      L2241.TechOMfixed_elec_coalret_dispatch_gcamusa

    # Variable O&M costs
    L2241.CapacityTech_retired %>%
      left_join_error_no_match(L223.TechOMvar_Dispatch,
                               by = c("region", "dispatch.sector" = "supplysector", "subsector",
                                      "cap.tech" = "technology", "year")) %>%
      select(region, dispatch.sector, subsector,  capacity.technology = cap.tech.cool, year, input.OM.var, OM.var) ->
      L2241.TechOMvar_elec_coalret_dispatch_gcamusa

    # Lifetime / natural retirement
    L2241.CapacityTech_retired %>%
      select(region, dispatch.sector, subsector,  capacity.technology = cap.tech.cool, year) %>%
      mutate(lifetime = 5,
             steepness = 0.1,
             half.life = 2.5) ->
      # left_join_error_no_match(A23.elec_tech_coal_retire_SCurve_dispatch,
      #                          by = c("technology")) ->
      L2241.TechSCurve_elec_coalret_dispatch_gcamusa

    # Share-weights
    L2241.CapacityTech_retired %>%
      mutate(share.weight = gcamusa.DEFAULT_SHAREWEIGHT) %>%
      select(region, dispatch.sector, subsector, capacity.technology = cap.tech.cool, year, share.weight) ->
      L2241.TechShrwt_elec_coalret_dispatch_gcamusa

    # Economic shutdown function
    L2241.CapacityTech_retired %>%
      mutate(median.shutdown.point = gcamusa.ELEC_CAP_INV_MEDIAN,
             profit.shutdown.steepness = gcamusa.ELEC_CAP_INV_STEEPNESS) %>%
      # changing variable names because we'll use the stubtech L2 data name and node equivalence in the batch file
      select(region, supplysector = dispatch.sector, subsector, stub.technology = cap.tech.cool,
             year, median.shutdown.point, profit.shutdown.steepness) ->
      L2241.TechProfitShutdown_coalret_dispatch_gcamusa


    # ===================================================
    # 2. Vintage coal plants which operate beyond 2015

    # Process REEDS plant files to obtain coal generation by vintages
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
    # WCOMB_F - Fixed Cost, Nox Comb Control  ($1987)
    # W_DSIF - Fixed Cost, DSI ($1987)
    # W_FFF - Fixed Cost, Fabric Filter ($1987)
    # WSCR_F - Fixed Cost, SCR ($1987)
    # WSNCR_F - Fixed Cost, SNCR ($1987)
    # W_CAPAD - Annual Investment in Capital Addtions (1987$/MW)
    # W_VOM - variable O&M ($1987)
    # WCOMB_V - Variable Cost, Nox Comb Control  ($1987)
    # W_DSIV - Variable Cost, DSI ($1987)
    # W_FFV - Variable Cost, Fabric Filter ($1987)
    # WSCR_V - Variable Cost, SCR ($1987)
    # WSNCR_V - Variable Cost, SNCR ($1987)
    # WHRATE - heat rate

    REEDS_Plantfile %>%
      select(T_PID, T_UID, WSTATE, EFDcd, ECPcd, WC_NP, TRFURB, W_SYR, W_RYR, WHRATE,
             W_FOM, WCOMB_F, W_DSIF, W_FFF, WSCR_F, WSNCR_F, W_CAPAD,
             W_VOM, WCOMB_V, W_DSIV, W_FFV, WSCR_V, WSNCR_V) %>%
      # filter for plants not yet retired in 2020
      filter(W_RYR > min(MODEL_FUTURE_YEARS)) %>%
      left_join_error_no_match(ECP_mapping %>%
                                 select(-Description),
                               by = c("ECPcd" = "ECPT")) %>%
      filter(Fuel == "Coal",
             # # filter out IGCC plants
             # !(ECPcd %in% c("IG", "IS")),
             # filter out CCS plants
             EFDcd != "CAS") %>%
      # appears to be some duplicate identifiers, so summarise
      group_by(T_PID, T_UID, WSTATE, EFDcd, ECPcd, TRFURB, W_SYR, W_RYR, WHRATE,
               W_FOM, WCOMB_F, W_DSIF, W_FFF, WSCR_F, WSNCR_F, W_CAPAD,
               W_VOM, WCOMB_V, W_DSIV, W_FFV, WSCR_V, WSNCR_V) %>%
      summarise(WC_NP = sum(WC_NP)) %>%
      ungroup() %>%
      mutate(efficiency = CONV_KWH_BTU / WHRATE,
             # have to sum across a number of components to get complete fixed / variable O&M costs
             OMF = (W_FOM + WCOMB_F + W_DSIF + W_FFF + WSCR_F + WSNCR_F + W_CAPAD) * gdp_deflator(1975, 1987),
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
             efficiency, OMF, OMV) -> REEDS_coal

    # Obtain state-shares of generation and capacity in 2018
    REEDS_coal %>%
      select(Plant.ID, Unit.ID, region, Operating.Year, Planned.Retirement.Year, capacity, OMF, OMV) %>%
      # units with no retirement dates are set to 9999
      # reset to max lifetime assumed for coal plants in GCAM-USA
      group_by(Plant.ID, Unit.ID) %>%
      mutate(Max.Retirement.Year = Operating.Year + gcamusa.AVG_COAL_PLANT_LIFETIME,
             Retirement.Year = min(Planned.Retirement.Year, Max.Retirement.Year)) %>%
      ungroup() %>%
      # need to aggregate to plant-level (rather than unit-level)
      group_by(Plant.ID, region) %>%
      summarise(Retirement.Year = sum(Retirement.Year * capacity) / sum(capacity),
                OMF = sum(OMF * capacity) / sum(capacity),
                OMV = sum(OMV * capacity) / sum(capacity),
                capacity = sum(capacity)) %>%
      mutate(Retirement.Year = as.integer(Retirement.Year)) %>%
      # ReEDS data set has some plants not present in EIA form data (as processed by GCAM)
      # remove these since we need to maintain consistency with other parts of the data system
      semi_join(EIA_elec_data_coal, by = c("Plant.ID", "region" = "state")) %>%
      left_join_error_no_match(EIA_elec_data_coal, by = c("Plant.ID", "region" = "state")) %>%
      # assume that difference between ReEDS capacity and EIA_elec_data_coal capacity is what was retired
      mutate(fraction_remaining = min((capacity / Nameplate.MW), 1)) %>%
      rename(Operating.Year = vintage) %>%
      left_join_error_no_match(vintage_bins_mapping %>%
                                 rename(vintage.bin = vintage),
                               by = "Operating.Year") %>%
      left_join_error_no_match(calibrated_techs_dispatch_usa %>%
                                 select(fuel, elec_tech, supplysector, subsector, technology),
                               by = c("gcam_fuel" = "fuel", "elec_tech")) %>%
      left_join_error_no_match(A23.elec_tech_mapping_cool %>%
                                 mutate(cooling_system = if_else(water_type == gcamusa.WATER_TYPE_SEAWATER,
                                                                 gcamusa.WATER_TYPE_SEAWATER,
                                                                 cooling_system)),
                               by = c("technology", "cooling_system", "water_type")) %>%
      # sum relevant data up to state / tech / vintage bin level
      group_by(region, supplysector, subsector, technology, to.technology, vintage.bin) %>%
      summarise(Retirement.Year = sum(Retirement.Year * NAMEPLATE) / sum(NAMEPLATE),
                OMF = sum(OMF * NAMEPLATE) / sum(NAMEPLATE),
                OMV = sum(OMV * NAMEPLATE) / sum(NAMEPLATE),
                en_in = sum(en_in * fraction_remaining),
                en_out = sum(en_out * fraction_remaining),
                capacity = sum(NAMEPLATE)) %>%
      mutate(Retirement.Year = as.integer(Retirement.Year),
             efficiency = en_out / en_in) %>%
      ungroup() %>%
      # calculate shares of fuel consumption / electricity generation by vintage bin for each state / tech
      group_by(region, supplysector, subsector, technology, to.technology) %>%
      mutate(capacity_share = capacity / sum(capacity),
             en_in_share = en_in / sum(en_in),
             en_out_share = en_out / sum(en_out)) %>%
      ungroup() %>%
      select(-technology, -capacity, -en_in, -en_out) -> L2241.ReEDS_coal_remaining

    L2241.CapacityTech_remaining  %>%
      select(-cap.tech.cool, -efficiency) %>%
      # join is intended to duplicate rows (by vintage bins)
      # LJENM throws error, so left_join is used
      # a few NAs are also generated and addressed below
      left_join(L2241.ReEDS_coal_remaining %>%
                  rename(dispatch.sector = supplysector,
                         cap.tech = to.technology),
                by = c("region", "dispatch.sector", "subsector", "cap.tech")) %>%
      # if no matching entries are found in ReEDS data set, simply set share variables to 1
      # these entries with no vintage info will be handled separately
      replace_na(list(capacity_share = 1, en_in_share = 1, en_out_share = 1)) %>%
      mutate(capacity_share = (1 - cap_ret_share) * capacity_share,
             en_in_share = (1 - en_in_ret_share) * en_in_share,
             en_out_share = (1 - en_out_ret_share) * en_out_share) %>%
      select(-cap_ret_share, -en_in_ret_share, -en_out_ret_share) -> L2241.CapacityTech_remaining_vintage

    L2241.CapacityTech_remaining_vintage %>%
      filter(is.na(Retirement.Year)) -> L2241.CapacityTech_remaining_novintage

    L223.CapacityTech %>%
      semi_join(L2241.CapacityTech_remaining_novintage,
                by = c("region", "subsector", "capacity.technology" = "cap.tech", "year")) %>%
      left_join_error_no_match(L2241.CapacityTech_remaining_novintage %>%
                                 select(region, subsector, technology, cap.tech, year, capacity_share),
                               by = c("region", "subsector", "capacity.technology" = "cap.tech", "year")) %>%
      mutate(capacity = capacity * capacity_share) %>%
      select(-capacity_share) -> L2241.CapacityTech_coal_novintage

    L223.Production_Dispatch %>%
      semi_join(L2241.CapacityTech_remaining_novintage,
                by = c("region", "subsector", "technology" = "cap.tech", "year")) %>%
      left_join_error_no_match(L2241.CapacityTech_remaining_novintage %>%
                                 select(region, subsector, technology, cap.tech, year, en_out_share),
                               by = c("region", "subsector", "technology" = "cap.tech", "year")) %>%
      mutate(calOutputValue = calOutputValue * en_out_share) %>%
      select(-en_out_share) -> L2241.TechProd_coal_novintage

    L223.TechEff_Cal %>%
      semi_join(L2241.CapacityTech_remaining_novintage,
                by = c("region", "subsector", "technology" = "cap.tech", "year")) %>%
      rename(dispatch.sector = supplysector,
             capacity.technology = technology) -> L2241.TechEff_Cal_coal_novintage


    # New files for coal vintage technologies
    L2241.CapacityTech_remaining_vintage %>%
      filter(!is.na(Retirement.Year)) -> L2241.CapacityTech_remaining_vintage

    # Capacity
    L2241.CapacityTech_remaining_vintage %>%
      mutate(capacity.technology = paste0(cap.tech, " (", vintage.bin, ")"),
             capacity = original_capacity * capacity_share) %>%
      select(region, dispatch.sector, subsector, capacity.technology, year, capacity) ->
      L2241.CapacityTech_coal_vintage_dispatch_gcamusa

    # Historical production
    L2241.CapacityTech_remaining_vintage %>%
      left_join_error_no_match(L223.Production_Dispatch,
                               by = c("region", "dispatch.sector" = "supplysector", "subsector",
                                      "cap.tech" = "technology", "year")) %>%
      mutate(capacity.technology = paste0(cap.tech, " (", vintage.bin, ")"),
             calOutputValue = calOutputValue * en_out_share,
             year = max(MODEL_BASE_YEARS),
             share.weight.year = year,
             subs.share.weight = gcamusa.DEFAULT_SHAREWEIGHT,
             tech.share.weight = gcamusa.DEFAULT_SHAREWEIGHT) %>%
      select(region, dispatch.sector, subsector, capacity.technology, year,
             calOutputValue, share.weight.year, subs.share.weight, tech.share.weight) ->
      L2241.TechProd_coal_vintage_dispatch_gcamusa

    # Efficiency
    L2241.CapacityTech_remaining_vintage %>%
      left_join_error_no_match(L223.TechEff_Cal %>%
                                 select(-efficiency),
                               by = c("region", "dispatch.sector" = "supplysector", "subsector",
                                      "cap.tech" = "technology", "year")) %>%
      mutate(capacity.technology = paste0(cap.tech, " (", vintage.bin, ")")) %>%
      select(region, dispatch.sector, subsector, capacity.technology, year,
             minicam.energy.input, efficiency, market.name) ->
      L2241.TechEff_coal_vintage_dispatch_gcamusa

    # Fixed O&M costs
    L2241.CapacityTech_remaining_vintage %>%
      left_join_error_no_match(L223.TechOMfixed_Dispatch %>%
                                 select(-OM.fixed),
                               by = c("region", "dispatch.sector" = "supplysector", "subsector",
                                      "cap.tech" = "technology", "year")) %>%
      mutate(capacity.technology = paste0(cap.tech, " (", vintage.bin, ")")) %>%
      select(region, dispatch.sector, subsector, capacity.technology, year, input.OM.fixed, OM.fixed = OMF) ->
      L2241.TechOMfixed_coal_vintage_dispatch_gcamusa

    # Variable O&M costs
    L2241.CapacityTech_remaining_vintage %>%
      left_join_error_no_match(L223.TechOMvar_Dispatch %>%
                                 select(-OM.var),
                               by = c("region", "dispatch.sector" = "supplysector", "subsector",
                                      "cap.tech" = "technology", "year")) %>%
      mutate(capacity.technology = paste0(cap.tech, " (", vintage.bin, ")")) %>%
      select(region, dispatch.sector, subsector, capacity.technology, year, input.OM.var, OM.var = OMV) ->
      L2241.TechOMvar_coal_vintage_dispatch_gcamusa

    # Share-weights
    L2241.CapacityTech_remaining_vintage %>%
      mutate(capacity.technology = paste0(cap.tech, " (", vintage.bin, ")"),
             # dispatch technologies always get shareweight = 1
             share.weight = gcamusa.DEFAULT_SHAREWEIGHT) %>%
      select(region, dispatch.sector, subsector, capacity.technology, year, share.weight) ->
      L2241.TechShrwt_coal_vintage_dispatch_gcamusa

    # Economic shutdown function
    L2241.CapacityTech_remaining_vintage %>%
      mutate(capacity.technology = paste0(cap.tech, " (", vintage.bin, ")"),
             median.shutdown.point = gcamusa.ELEC_CAP_INV_MEDIAN,
             profit.shutdown.steepness = gcamusa.ELEC_CAP_INV_STEEPNESS) %>%
      # changing variable names because we'll use the stubtech L2 data name and node equivalence in the batch file
      select(region, supplysector = dispatch.sector, subsector, stub.technology = capacity.technology,
             year, median.shutdown.point, profit.shutdown.steepness) ->
      L2241.TechProfitShutdown_coal_vintage_dispatch_gcamusa

    # Lifetime / natural retirement
    L2241.CapacityTech_remaining_vintage %>%
      mutate(capacity.technology = paste0(cap.tech, " (", vintage.bin, ")"),
             lifetime = Retirement.Year - max(MODEL_BASE_YEARS)) %>%
      select(region, dispatch.sector, subsector,  capacity.technology, year, lifetime) %>%
      mutate(steepness = gcamusa.COAL_RETIRE_STEEPNESS,
             half.life = round(lifetime * (gcamusa.AVG_COAL_PLANT_HALFLIFE / gcamusa.AVG_COAL_PLANT_LIFETIME), 0)) ->
      L2241.TechSCurve_coal_vintage_dispatch_gcamusa

    # Tables with same info for both retire & vintage technologies
    L2241.CapacityTech_retired %>%
      select(region, dispatch.sector, subsector, cap.tech, capacity.technology = cap.tech.cool, year) %>%
      bind_rows(L2241.CapacityTech_remaining_vintage %>%
                  mutate(capacity.technology = paste0(cap.tech, " (", vintage.bin, ")")) %>%
                  select(region, dispatch.sector, subsector, cap.tech, capacity.technology, year)) -> L2241.CapacityTech_ret_vint

    # Water inputs
    L2241.CapacityTech_ret_vint %>%
      # join will duplicate rows because we're creating coefficients for both water withdrawals and consumption
      # LJENM errors, so left_join() is used
      left_join(L223.TechCoef_Dispatch_cool,
                by = c("region", "subsector", "cap.tech" = "technology", "year")) %>%
      select(region, supplysector, subsector, technology = capacity.technology,
             year, minicam.energy.input, coefficient, market.name) ->
      L2241.TechCoef_cool_coalret_vintage_dispatch_gcamusa

    # Minimum capacity factor
    # Note that including the retire technologies here is not strictly necessary
    # because they don't operate in future years, but including them will avoid
    # warning messages in the terminal / log file.
    L2241.CapacityTech_ret_vint %>%
      select(-cap.tech) %>%
      left_join_error_no_match(A23.dispatch_capacitytech_min_cap_fac %>%
                                 distinct(subsector, min.capacity.factor),
                               by = c("subsector")) ->
      L2241.CapacityTechMinCapFac_coalret_vintage_dispatch_gcamusa

    # Final available year - table specifying that these techs are only available in 2015
    L2241.CapacityTech_ret_vint %>%
      select(-cap.tech) %>%
      mutate(initial.available.year = MODEL_FINAL_BASE_YEAR,
             final.available.year = MODEL_FINAL_BASE_YEAR) %>%
      # changing variable names because we'll use the tech L2 data name and node equivalence in the batch file
      rename(supplysector = dispatch.sector,
             technology = capacity.technology) ->
      L2241.CapacityTechAvail_coalret_vintage_dispatch_gcamusa

    # Capacity factor
    L2241.CapacityTech_ret_vint %>%
      left_join_error_no_match(L223.TechCapFac_Dispatch,
                               by = c("region", "dispatch.sector" = "supplysector",
                                      "subsector", "cap.tech" = "technology", "year")) %>%
      select(-cap.tech) %>%
      # changing variable names because we'll use the tech L2 data name and node equivalence in the batch file
      rename(supplysector = dispatch.sector,
             technology = capacity.technology) ->
      L2241.TechCapFac_coalret_vintage_dispatch_gcamusa


    # Zero out capacity & production for all defuault (non-retire / vintage) technologies except the few that aren't vintaged
    # Capacity
    L223.CapacityTech %>%
      semi_join(L2241.CapacityTech_ret_vint,
                by = c("region", "dispatch.sector", "subsector",
                       "capacity.technology" = "cap.tech", "year")) %>%
      mutate(capacity = 0) -> L2241.CapacityTech_coal_dispatch_gcamusa

    # Production
    L223.Production_Dispatch %>%
      semi_join(L2241.CapacityTech_ret_vint,
                by = c("region", "supplysector" = "dispatch.sector", "subsector",
                       "technology" = "cap.tech", "year")) %>%
      mutate(calOutputValue = 0,
             tech.share.weight = 0) -> L2241.Production_coal_dispatch_gcamusa


    # Bind relevant tables together
    # Note that in many cases we will change the variable names because we'll use the
    # technology L2 data name and node equivalence in the batch file
    # Capacity
    L2241.CapacityTech_coal_dispatch_gcamusa %>% # zeroes out capacity for techs being replaced
      bind_rows(L2241.CapacityTech_elec_coalret_dispatch_gcamusa, # capacity for "retire 2020" techs
                L2241.CapacityTech_coal_vintage_dispatch_gcamusa, # capacity for vintage techs
                L2241.CapacityTech_coal_novintage) -> # capacity for few remaining techs with no vintage info
      L2241.CapacityTech_coalret_vintage_dispatch_gcamusa

    # Historical production
    L2241.Production_coal_dispatch_gcamusa %>% # zeroes out historical production for techs being replaced
      bind_rows(L2241.TechProd_elec_coalret_dispatch_gcamusa %>% # historical production for "retire 2020" techs
                  rename(supplysector = dispatch.sector,
                         technology = capacity.technology),
                L2241.TechProd_coal_vintage_dispatch_gcamusa %>% # historical production for vintage techs
                  rename(supplysector = dispatch.sector,
                         technology = capacity.technology),
                L2241.TechProd_coal_novintage) -> # historical production for few remaining techs with no vintage info
      L2241.Production_coalret_vintage_dispatch_gcamusa

    # Efficiency
    L2241.TechEff_elec_coalret_dispatch_gcamusa %>% # historical efficiency of "retire 2020" techs
      bind_rows(L2241.TechEff_coal_vintage_dispatch_gcamusa, # historical efficiency of vintage techs
                L2241.TechEff_Cal_coal_novintage) %>% # historical efficiency of few remaining techs with no vintage info
      rename(supplysector = dispatch.sector,
             technology = capacity.technology) ->
      L2241.TechEff_coalret_vintage_dispatch_gcamusa

    # Fixed O&M costs
    L2241.TechOMfixed_elec_coalret_dispatch_gcamusa %>%
      bind_rows(L2241.TechOMfixed_coal_vintage_dispatch_gcamusa) %>%
      rename(supplysector = dispatch.sector,
             technology = capacity.technology) ->
      L2241.TechOMfixed_coalret_vintage_dispatch_gcamusa

    # Variable O&M costs
    L2241.TechOMvar_elec_coalret_dispatch_gcamusa %>%
      bind_rows(L2241.TechOMvar_coal_vintage_dispatch_gcamusa) %>%
      rename(supplysector = dispatch.sector,
             technology = capacity.technology) ->
      L2241.TechOMvar_coalret_vintage_dispatch_gcamusa

    # Lifetime / natural retirement
    L2241.TechSCurve_elec_coalret_dispatch_gcamusa %>%
      bind_rows(L2241.TechSCurve_coal_vintage_dispatch_gcamusa) %>%
      rename(supplysector = dispatch.sector,
             technology = capacity.technology) ->
      L2241.TechSCurve_coalret_vintage_dispatch_gcamusa

    # Share-weights
    L2241.TechShrwt_elec_coalret_dispatch_gcamusa %>%
      bind_rows(L2241.TechShrwt_coal_vintage_dispatch_gcamusa) %>%
      rename(supplysector = dispatch.sector,
             technology = capacity.technology) ->
      L2241.TechShrwt_coalret_vintage_dispatch_gcamusa

    # Economic shutdown function
    L2241.TechProfitShutdown_coalret_dispatch_gcamusa %>%
      bind_rows(L2241.TechProfitShutdown_coal_vintage_dispatch_gcamusa) ->
      L2241.TechProfitShutdown_coalret_vintage_dispatch_gcamusa


    # Finally, scale efficiency to match both energy input and ouput from IEA in 2015
    # We need to do this because we take efficiencies from EIA & ReEDS data sets
    # But these are systematically lower than those calculated frome the IEA data
    # (likely because they include netownuse, which is modeled separately in GCAM)
    # First, calculate the implied enengy input for coal based on calibrated production and unadjusted efficiency
    L2241.Production_coalret_vintage_dispatch_gcamusa %>%
      # we zeroed out production for a bunch of technologies we're eliminating
      # filter them out here
      filter(calOutputValue > 0) %>%
      left_join_error_no_match(L2241.TechEff_coalret_vintage_dispatch_gcamusa,
                               by = c("region", "supplysector", "subsector", "technology", "year")) %>%
      mutate(calInputValue = calOutputValue / efficiency) %>%
      group_by(region) %>%
      summarise(calInputValue = sum(calInputValue)) %>%
      ungroup() -> L2241.input_unadjusted

    # Next, read in input energy from IEA balance
    L123.in_EJ_state_elec_F_tech %>%
      filter(year == 2015 & elec_tech == "coal_conv") %>%
      group_by(state) %>%
      summarise(calibratedInputValue = sum(value)) %>%
      ungroup() %>%
      rename(region = state) -> L2241.input_calibrated

    # Calculate an efficiency scalar at the state level
    L2241.input_calibrated %>%
      left_join_error_no_match(L2241.input_unadjusted, by = "region") %>%
      mutate(eff_adj = calInputValue / calibratedInputValue) %>%
      select(region, eff_adj) ->
      L2241.eff_adj

    # Update the efficiency table
    L2241.TechEff_coalret_vintage_dispatch_gcamusa %>%
      left_join_error_no_match(L2241.eff_adj, by = "region") %>%
      mutate(efficiency = efficiency * eff_adj) %>%
      select(-eff_adj) -> L2241.TechEff_coalret_vintage_dispatch_gcamusa


    # ===================================================
    # Produce outputs

    L2241.CapacityTech_coalret_vintage_dispatch_gcamusa %>%
      add_title("existing capacity for capacity technology coal (conv pul) by state in final base year") %>%
      add_units("EJ") %>%
      add_comments("Conventional coal electricity capacity are allocated to fast retire and slow retire technologies") %>%
      # TODO:  add_comments("Capacity shares by vintage are calculated based on REEDS 2019 data") %>%
      # add_comments("Capacity shares by vintage are then applied to slow_retire stub-technology 2015 fleet in each state") %>%
      # add_comments("Capacity of coal (conv pul) is set to zero in final base year") %>%
      add_precursors("L223.CapacityTech",
                     "gcam-usa/dispatch/coal_vintage_bins",
                     "gcam-usa/dispatch/ECP_mapping",
                     "gcam-usa/dispatch/AEO2019Plantfile") ->
      L2241.CapacityTech_coalret_vintage_dispatch_gcamusa

    L2241.Production_coalret_vintage_dispatch_gcamusa %>%
      add_title("Calibration outputs for capacity technology coal (conv pul) by state in final base year") %>%
      add_units("EJ") %>%
      add_comments("Conventional coal electricity outputs are allocated to fast retire and slow retire technologies") %>%
      # TODO: add_comments("Generation shares by vintage are calculated based on REEDS 2019 data and EIA Form 923 matched by plant and unit ID") %>%
      # add_comments("Generation shares by vintage are then applied to slow_retire stub-technology 2015 generation in each state") %>%
      # add_comments("Generation in other base years are set to zero to each vintage stub-technology") %>%
      add_precursors("gcam-usa/prime_mover_map",
                     "gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/EIA_860_generators_existing_2018",
                     "gcam-usa/EIA_860_generators_retired_2018",
                     "gcam-usa/A23.elec_tech_mapping_cool",
                     "gcam-usa/dispatch/coal_vintage_bins",
                     "gcam-usa/dispatch/ECP_mapping",
                     "gcam-usa/dispatch/AEO2019Plantfile",
                     "L105.eia_elec_data_water",
                     "L223.Production_Dispatch") ->
      L2241.Production_coalret_vintage_dispatch_gcamusa

    L2241.TechEff_coalret_vintage_dispatch_gcamusa %>%
      add_title("efficiency for capacity technology coal (conv pul) by state in final base year") %>%
      add_units("Unitless") %>%
      add_comments("same efficiency are applied to fast retire and slow retire technologies") %>%
      # TODO:  add_comments("Efficiency by vintage are calculated based on REEDS 2019 data") %>%
      # add_comments("Efficiency are weighted by generation (EIA Form 923)") %>%
      add_precursors("L123.in_EJ_state_elec_F_tech",
                     "L223.TechEff_Cal",
                     "gcam-usa/dispatch/coal_vintage_bins",
                     "gcam-usa/dispatch/ECP_mapping",
                     "gcam-usa/dispatch/AEO2019Plantfile") ->
      L2241.TechEff_coalret_vintage_dispatch_gcamusa

    L2241.TechSCurve_coalret_vintage_dispatch_gcamusa %>%
      add_title("S-curve shutdown decider for for capacity technology coal (conv pul) by state in final base year") %>%
      add_units("Unitless") %>%
      add_comments("Separate fast retire and slow retire technologies") %>%
      # TODO:  add_comments("Average lifetime for each vintage group is weighted by capacity, based on EIA unit-level 2015 data") %>%
      # add_comments("Only for vintage groups with greater than 20 years of lifetime remaining") %>%
      add_precursors("gcam-usa/A23.elec_tech_coal_retire_SCurve_dispatch") ->
      L2241.TechSCurve_coalret_vintage_dispatch_gcamusa

    L2241.TechShrwt_coalret_vintage_dispatch_gcamusa %>%
      add_title("Shareweights for for capacity technology coal (conv pul) by state in final base year") %>%
      add_units("Unitless") %>%
      add_comments("Separate fast retire and slow retire technologies") %>%
      same_precursors_as("L2241.Production_coalret_vintage_dispatch_gcamusa") ->
      L2241.TechShrwt_coalret_vintage_dispatch_gcamusa

    L2241.TechOMfixed_coalret_vintage_dispatch_gcamusa %>%
      add_title("Fixed OM costs for for capacity technology coal (conv pul) by state in final base year") %>%
      add_units("1975$/kW/yr") %>%
      add_comments("Set the same fixed OM cost values for fast retire and slow retire technologies") %>%
      # TODO:  add_comments("OM var by vintage are calculated based on REEDS 2019 data") %>%
      # add_comments("OM var are weighted by generation (EIA Form 923)") %>%
      same_precursors_as("L2241.TechEff_coal_vintage_dispatch_gcamusa") %>%
      add_precursors("L223.TechOMfixed_Dispatch") ->
      L2241.TechOMfixed_coalret_vintage_dispatch_gcamusa

    L2241.TechOMvar_coalret_vintage_dispatch_gcamusa %>%
      add_title("Variable OM costs for for capacity technology coal (conv pul) by state in final base year") %>%
      add_units("1975$/MWh") %>%
      add_comments("Set the same variable OM cost values for fast retire and slow retire technologies") %>%
      # TODO:  add_comments("OM var by vintage are calculated based on REEDS 2019 data") %>%
      # add_comments("OM var are weighted by generation (EIA Form 923)") %>%
      same_precursors_as("L2241.TechEff_coal_vintage_dispatch_gcamusa") %>%
      add_precursors("L223.TechOMvar_Dispatch") ->
      L2241.TechOMvar_coalret_vintage_dispatch_gcamusa

    L2241.CapacityTechMinCapFac_coalret_vintage_dispatch_gcamusa %>%
      add_title("Capacity technology minimum capacity factor") %>%
      add_units("Unitless") %>%
      add_comments("Technologies will not be allowed to dispatch when it's capacity factor") %>%
      add_comments("would fall below this minimum value.") %>%
      add_precursors("gcam-usa/A23.dispatch_capacitytech_min_cap_fac") ->
      L2241.CapacityTechMinCapFac_coalret_vintage_dispatch_gcamusa

    L2241.TechProfitShutdown_coalret_vintage_dispatch_gcamusa %>%
      add_title("Dispatch technology capacity investment discount params") %>%
      add_units("NA") %>%
      add_comments("Profit shutdown param that are used to discount existing") %>%
      add_comments("capacity in investment decisions.") %>%
      same_precursors_as("L2241.TechProd_coal_vintage_dispatch_gcamusa") ->
      L2241.TechProfitShutdown_coalret_vintage_dispatch_gcamusa

    L2241.TechCapFac_coalret_vintage_dispatch_gcamusa %>%
      add_title("existing capacity for capacity technology coal (conv pul) by state in final base year") %>%
      add_units("EJ") %>%
      add_comments("Conventional coal electricity capacity are allocated to fast retire and slow retire technologies") %>%
      same_precursors_as("L2241.CapacityTech_coalret_vintage_dispatch_gcamusa") %>%
      same_precursors_as("L2241.TechProd_coal_vintage_dispatch_gcamusa") %>%
      add_precursors("gcam-usa/A23.elec_tech_coal_retire_SCurve_dispatch",
                     "L223.TechCapFac_Dispatch") ->
      L2241.TechCapFac_coalret_vintage_dispatch_gcamusa

    L2241.CapacityTechAvail_coalret_vintage_dispatch_gcamusa %>%
      add_title("Existing capacity for capacity technology coal (conv pul) by state in final base year") %>%
      add_units("EJ") %>%
      add_comments("Conventional coal electricity capacity are allocated to fast retire and slow retire technologies") %>%
      same_precursors_as("L2241.CapacityTech_coalret_vintage_dispatch_gcamusa") %>%
      same_precursors_as("L2241.TechProd_coal_vintage_dispatch_gcamusa") %>%
      add_precursors("L2241.TechCapFac_coalret_vintage_dispatch_gcamusa") ->
      L2241.CapacityTechAvail_coalret_vintage_dispatch_gcamusa

    L2241.TechCoef_cool_coalret_vintage_dispatch_gcamusa %>%
      add_title("Historical conventional coal dispatch technology water withdrawal and consumption coefficients by state") %>%
      add_units("Unitless") %>%
      add_comments("Set technology water withdrawal and consumption coefficients by state") %>%
      add_precursors("L223.TechCoef_Dispatch_cool") ->
      L2241.TechCoef_cool_coalret_vintage_dispatch_gcamusa


    return_data(L2241.Production_coalret_vintage_dispatch_gcamusa,
                L2241.CapacityTech_coalret_vintage_dispatch_gcamusa,
                L2241.TechEff_coalret_vintage_dispatch_gcamusa,
                L2241.TechSCurve_coalret_vintage_dispatch_gcamusa,
                L2241.TechShrwt_coalret_vintage_dispatch_gcamusa,
                L2241.TechOMfixed_coalret_vintage_dispatch_gcamusa,
                L2241.TechOMvar_coalret_vintage_dispatch_gcamusa,
                L2241.CapacityTechMinCapFac_coalret_vintage_dispatch_gcamusa,
                L2241.TechProfitShutdown_coalret_vintage_dispatch_gcamusa,
                L2241.TechCapFac_coalret_vintage_dispatch_gcamusa,
                L2241.CapacityTechAvail_coalret_vintage_dispatch_gcamusa,
                L2241.TechCoef_cool_coalret_vintage_dispatch_gcamusa)

  } else {
    stop("Unknown command")
  }
}
