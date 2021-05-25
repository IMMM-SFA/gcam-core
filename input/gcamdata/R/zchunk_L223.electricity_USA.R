# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L223.electricity_USA
#'
#' Generates GCAM-USA model inputs for electrcity sector by grid regions and states.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L223.Sector_Investment}, \code{L223.SubsectorLogit_Investment_Fuel},
#' \code{L223.SubsectorShrwt_Investment_Fuel}, \code{L223.SubsectorInterpTo_Investment_Fuel},
#' \code{L223.SubsectorLogit_Investment}, \code{L223.SubsectorShrwt_Investment},
#' \code{L223.StubTech_Investment}, \code{L223.GlobalTechEff_Investment}, \code{L223.StubTechMarket_Investment},
#' \code{L223.GlobalTechOMfixed_Investment},\code{L223.GlobalTechOMvar_Investment}, \code{L223.GlobalTechCapital_Investment},
#' \code{L223.StubTechInterp_Investment_USA}, \code{L223.StubTechShrwt_Investment_USA},
#' \code{L223.GlobalTechShrwt_Investment}, \code{L223.GlobalTechCapFac_Investment}, \code{L223.TechCapFac_Investment},
#' \code{L223.GlobalTechCapture_Investment}, \code{L223.GlobalTechCost_Investment}, \code{L223.GlobalTechCost_CapacityCreditCalulator},
#' \code{L223.Sector_Investment_StateShare}, \code{L223.Subsector_Investment_StateShare}, \code{L223.CapacityTech},
#' \code{L223.SubsectorShrwtFllt_Investment_StateShare}, \code{L223.TechCoef_Investment_StateShare},
#' \code{L223.TechShrwt_Investment_StateShare}, \code{L223.TechShrwt_Dispatch}, \code{L223.TechEff_Dispatch}, \code{L223.CapacityTechInputPMult_geo},
#' \code{L223.DispatchSector}, \code{L223.Sector_Dispatch}, \code{L223.SubsectorLogit_Dispatch}, \code{L223.SubsectorShrwtFllt_Dispatch},
#' \code{L223.CapacityTech_FutureTechs}, \code{L223.TechOMvar_Dispatch}, \code{L223.TechLifetime_Dispatch}, \code{L223.TechSCurve_Dispatch},
#' \code{L223.TechCapFac_Dispatch}, \code{L223.TechCarbonCapture_Dispatch}, \code{L223.Production_Dispatch}, \code{L223.TechEff_Cal},
#' \code{L223.PrimaryRenewKeyword_Dispatch_USA},  \code{L223.AvgFossilEffKeyword_Dispatch_USA}, \code{L223.TechTrialMarket_Dispatch},
#' \code{L223.TechTrialMarket_Investment}, \code{L223.Sector_Dispatch_Grid}, \code{L223.DispatchSectorDispatchSegments},
#' \code{L223.InterestRate_FERC}, \code{L223.Pop_FERC}, \code{L223.BaseGDP_FERC}, \code{L223.LaborForceFillout_FERC},
#' \code{L223.StubTechCost_offshore_wind_Investment}, \code{L223.GlobalTechCoef_Investment_cool}, \code{L223.StubTechCoef_Investment_cool},
#' \code{L223.TechCoef_Dispatch_cool}, \code{L223.TechPmult_dispatch_wind_reeds_USA}, \code{L223.GlobalTechCapital_Investment_cool}.
#' The corresponding file in the original data system was \code{L223.electricity_USA.R} (gcam-usa level2 - dispatch branch).
#' @details This chunk generates input files to create an annualized electricity generation sector for each state
#' and creates the demand for the state-level electricity sectors in the grid regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select mutate_at funs
#' @importFrom tidyr gather spread nest unnest expand
#' @author YO Feb 2020
module_gcamusa_L223.electricity_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/NREL_us_re_technical_potential",
             FILE = "energy/A23.globaltech_eff",
             FILE = "gcam-usa/A10.renewable_resource_delete",
             FILE = "energy/A23.globaltech_retirement",
             FILE = "energy/A23.globaltech_shrwt",
             FILE = "energy/A23.globaltech_co2capture",
             FILE = "energy/A23.globaltech_keyword",
             FILE = "gcam-usa/A23.elec_tech_mapping_cool",
             FILE = "gcam-usa/A23.elec_tech_mapping_cool_shares_fut",
             FILE = "gcam-usa/usa_seawater_states_basins",
             FILE = "water/A23.CoolingSystemCosts",
             "L113.globaltech_OMfixed_ATB",
             "L113.globaltech_OMvar_ATB",
             "L113.globaltech_capital_ATB",
             "L114.CapacityFactor_wind_state_segment_gcamusa",
             "L114.CapacityFactor_wind_offshore_state_segment_gcamusa",
             "L115.CapacityFactor_hydro_state_gcamusa",
             "L115.CapacityFactor_hydro_state_segment_gcamusa",
             "L119.CapacityFactor_PV_state_gcamusa",
             "L119.CapacityFactor_CSP_state_gcamusa",
             "L119.CapacityFactor_PV_state_segment_gcamusa",
             "L119.CapacityFactor_CSP_state_segment_gcamusa",
             "L123.in_EJ_state_elec_F_tech",
             "L123.out_EJ_state_elec_F_tech",
             "L123.capacity_EJ_state_elec_F_tech",
             "L102.load_segments_gcamusa",
             "L102.invest_segments_gcamusa",
             FILE = "gcam-usa/A23.dispatch_sector",
             FILE = "gcam-usa/A23.dispatch_sector_state_share",
             FILE = "gcam-usa/A23.dispatch_subsector_interp",
             FILE = "gcam-usa/A23.dispatch_subsector_shrwt",
             FILE = "gcam-usa/A23.dispatch_subsector_shrwt_state_adj",
             FILE = "gcam-usa/A23.dispatch_subsector_shrwt_interpto_state_adj",
             FILE = "gcam-usa/A23.dispatch_subsector_logit",
             FILE = "gcam-usa/A23.dispatch_globaltech_eff_additional",
             FILE = "gcam-usa/A23.dispatch_globaltech_OMfixed_additional",
             FILE = "gcam-usa/A23.dispatch_globaltech_OMvar_additional",
             FILE = "gcam-usa/A23.dispatch_globaltech_capital_additional",
             FILE = "gcam-usa/A23.dispatch_globaltech_retirement_additional",
             FILE = "gcam-usa/A23.dispatch_globaltech_shrwt_additional",
             FILE = "gcam-usa/A23.globaltech_keyword_additional",
             FILE = "gcam-usa/A23.dispatch_additional_mapping",
             FILE = "gcam-usa/A23.dispatch_capacitytech_min_cap_fac",
             FILE = "gcam-usa/calibrated_techs_dispatch_usa",
             FILE = "gcam-usa/dispatch/capacity_credit_calculator",
             FILE = "gcam-usa/dispatch/TechTrialMarket_mapping",
             "L223.PrimaryRenewKeyword_elec",
             "L2233.GlobalTechCoef_elec_cool",
             "L2233.GlobalIntTechCoef_elec_cool",
             "L210.GrdRenewRsrcCurves_USA",
             "L120.GridCost_offshore_wind_USA",
             "L2237.StubTechCost_wind_reeds_USA",
             "L2238.StubTechCost_PV_reeds_USA",
             "L2239.StubTechCost_CSP_reeds_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L223.Sector_Investment",
             "L223.SubsectorLogit_Investment_Fuel",
             "L223.SubsectorShrwt_Investment_Fuel",
             "L223.SubsectorInterpTo_Investment_Fuel",
             "L223.SubsectorLogit_Investment",
             "L223.SubsectorShrwt_Investment",
             "L223.StubTech_Investment",
             "L233.GlobalInvestTech_Investment",
             "L223.GlobalTechEff_Investment",
             "L223.StubTechMarket_Investment",
             "L223.GlobalTechCoef_Investment_cool",
             "L223.StubTechCoef_Investment_cool",
             "L223.GlobalTechOMfixed_Investment",
             "L223.GlobalTechOMvar_Investment",
             "L223.GlobalTechCapital_Investment",
             "L223.GlobalTechCapital_Investment_cool",
             "L223.GlobalTechShrwt_Investment",
             "L223.GlobalIntTechEff_Investment",
             "L223.GlobalIntInvTechMaxCapFac_Investment",
             "L223.StubTechInterp_Investment_USA",
             "L223.StubTechShrwt_Investment_USA",
             "L223.GlobalTechCapFac_Investment",
             "L223.TechCapFac_Investment",
             "L223.GlobalTechCapture_Investment",
             "L223.GlobalTechCost_Investment",
             "L223.GlobalTechCost_CapacityCreditCalulator",
             "L223.Sector_Investment_StateShare",
             "L223.Subsector_Investment_StateShare",
             "L223.SubsectorShrwtFllt_Investment_StateShare",
             "L223.TechCoef_Investment_StateShare",
             "L223.TechPmult_Investment_StateShare",
             "L223.TechShrwt_Investment_StateShare",
             "L223.DispatchSector",
             "L223.Sector_Dispatch",
             "L223.SubsectorLogit_Dispatch",
             "L223.SubsectorShrwtFllt_Dispatch",
             "L223.CapacityTech_FutureTechs",
             "L223.CapacityTechSegmentCapFac",
             "L223.CapacityTechMinCapFac",
             "L223.CapacityTech",
             "L223.TechShrwt_Dispatch",
             "L223.TechEff_Dispatch",
             "L223.CapacityTechInputPMult_geo",
             "L223.StubTechEffFlag_Dispatch",
             "L223.TechCoef_Dispatch_cool",
             "L223.TechOMfixed_Dispatch",
             "L223.TechOMvar_Dispatch",
             "L223.TechLifetime_Dispatch",
             "L223.TechProfitShutdown_Dispatch",
             "L223.TechSCurve_Dispatch",
             "L223.TechCapFac_Dispatch",
             "L223.TechCarbonCapture_Dispatch",
             "L223.Production_Dispatch",
             "L223.TechEff_Cal",
             "L223.PrimaryRenewKeyword_Dispatch_USA",
             "L223.AvgFossilEffKeyword_Dispatch_USA",
             "L223.TechTrialMarket_Dispatch",
             "L223.TechTrialMarket_Investment",
             "L223.Sector_Dispatch_Grid",
             "L223.DispatchSectorDispatchSegments",
             "L223.InterestRate_FERC",
             "L223.Pop_FERC",
             "L223.BaseGDP_FERC",
             "L223.StubTechCost_offshore_wind_Investment",
             "L223.LaborForceFillout_FERC"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    grid_region <- Geothermal_Hydrothermal_GWh <- state <- geo_state_noresource <-
      region <- supplysector <- subsector <- technology <- year <- value <-
      sector <- calOutputValue <- fuel <- elec <- share <- avg.share <- pref <-
      share.weight.mult <- share.weight <- market.name <- sector.name <- subsector.name <-
      minicam.energy.input <- calibration <- secondary.output <- stub.technology <- tech <-
      capacity.factor <- scaler <- capacity.factor.capital <- . <- CFmax <- grid.cost <- NULL  # silence package check notes

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions", strip_attributes = TRUE)
    NREL_us_re_technical_potential <- get_data(all_data, "gcam-usa/NREL_us_re_technical_potential")
    A10.renewable_resource_delete <- get_data(all_data, "gcam-usa/A10.renewable_resource_delete")
    A23.globaltech_eff <- get_data(all_data, "energy/A23.globaltech_eff")
    A23.globaltech_retirement <- get_data(all_data, "energy/A23.globaltech_retirement", strip_attributes = TRUE)
    A23.globaltech_shrwt <- get_data(all_data, "energy/A23.globaltech_shrwt")
    A23.globaltech_co2capture <- get_data(all_data, "energy/A23.globaltech_co2capture")
    A23.globaltech_keyword <- get_data(all_data, "energy/A23.globaltech_keyword")

    A23.elec_tech_mapping_cool <- get_data(all_data, "gcam-usa/A23.elec_tech_mapping_cool")
    A23.elec_tech_mapping_cool_shares_fut <- get_data(all_data, "gcam-usa/A23.elec_tech_mapping_cool_shares_fut")
    usa_seawater_states_basins <- get_data(all_data, "gcam-usa/usa_seawater_states_basins")
    A23.CoolingSystemCosts <- get_data(all_data, "water/A23.CoolingSystemCosts", strip_attributes = TRUE)

    L113.globaltech_OMfixed_ATB <- get_data(all_data, "L113.globaltech_OMfixed_ATB", strip_attributes = TRUE)
    L113.globaltech_OMvar_ATB <- get_data(all_data, "L113.globaltech_OMvar_ATB", strip_attributes = TRUE)
    L113.globaltech_capital_ATB <- get_data(all_data, "L113.globaltech_capital_ATB", strip_attributes = TRUE)
    L114.CapacityFactor_wind_state_segment <- get_data(all_data, "L114.CapacityFactor_wind_state_segment_gcamusa", strip_attributes = TRUE)
    L114.CapacityFactor_wind_offshore_state_segment <- get_data(all_data, "L114.CapacityFactor_wind_offshore_state_segment_gcamusa", strip_attributes = TRUE)
    L115.CapacityFactor_hydro_state_gcamusa <- get_data(all_data, "L115.CapacityFactor_hydro_state_gcamusa", strip_attributes = TRUE)
    L115.CapacityFactor_hydro_state_segment_gcamusa <- get_data(all_data, "L115.CapacityFactor_hydro_state_segment_gcamusa", strip_attributes = TRUE)
    L119.CapacityFactor_PV_state <- get_data(all_data, "L119.CapacityFactor_PV_state_gcamusa", strip_attributes = TRUE)
    L119.CapacityFactor_PV_state_segment <- get_data(all_data, "L119.CapacityFactor_PV_state_segment_gcamusa", strip_attributes = TRUE)
    L119.CapacityFactor_CSP_state <- get_data(all_data, "L119.CapacityFactor_CSP_state_gcamusa", strip_attributes = TRUE)
    L119.CapacityFactor_CSP_state_segment <- get_data(all_data, "L119.CapacityFactor_CSP_state_segment_gcamusa", strip_attributes = TRUE)
    L123.in_EJ_state_elec_F_tech <- get_data(all_data, "L123.in_EJ_state_elec_F_tech", strip_attributes = TRUE)
    L123.out_EJ_state_elec_F_tech <- get_data(all_data, "L123.out_EJ_state_elec_F_tech", strip_attributes = TRUE)
    L123.capacity_EJ_state_elec_F_tech <- get_data(all_data, "L123.capacity_EJ_state_elec_F_tech", strip_attributes = TRUE)

    L102.load_segments <- get_data(all_data, "L102.load_segments_gcamusa", strip_attributes = TRUE)
    L102.invest_segments <- get_data(all_data, "L102.invest_segments_gcamusa", strip_attributes = TRUE)
    A23.dispatch_sector <- get_data(all_data, "gcam-usa/A23.dispatch_sector", strip_attributes = TRUE)
    A23.dispatch_sector_state_share <- get_data(all_data, "gcam-usa/A23.dispatch_sector_state_share", strip_attributes = TRUE)
    A23.dispatch_subsector_interp <- get_data(all_data, "gcam-usa/A23.dispatch_subsector_interp", strip_attributes = TRUE)
    A23.dispatch_subsector_shrwt <- get_data(all_data, "gcam-usa/A23.dispatch_subsector_shrwt", strip_attributes = TRUE)
    A23.dispatch_subsector_shrwt_state_adj <- get_data(all_data, "gcam-usa/A23.dispatch_subsector_shrwt_state_adj")
    A23.dispatch_subsector_shrwt_interpto_state_adj <- get_data(all_data, "gcam-usa/A23.dispatch_subsector_shrwt_interpto_state_adj", strip_attributes = TRUE)
    A23.dispatch_subsector_logit <- get_data(all_data, "gcam-usa/A23.dispatch_subsector_logit", strip_attributes = TRUE)
    A23.dispatch_globaltech_eff_additional <- get_data(all_data, "gcam-usa/A23.dispatch_globaltech_eff_additional")
    A23.dispatch_globaltech_OMfixed_additional <- get_data(all_data, "gcam-usa/A23.dispatch_globaltech_OMfixed_additional")
    A23.dispatch_globaltech_OMvar_additional <- get_data(all_data, "gcam-usa/A23.dispatch_globaltech_OMvar_additional")
    A23.dispatch_globaltech_capital_additional <- get_data(all_data, "gcam-usa/A23.dispatch_globaltech_capital_additional")
    A23.dispatch_globaltech_retirement_additional <- get_data(all_data, "gcam-usa/A23.dispatch_globaltech_retirement_additional")
    A23.dispatch_globaltech_shrwt_additional <- get_data(all_data, "gcam-usa/A23.dispatch_globaltech_shrwt_additional")
    A23.globaltech_keyword_additional <- get_data(all_data, "gcam-usa/A23.globaltech_keyword_additional")
    A23.dispatch_additional_mapping <- get_data(all_data, "gcam-usa/A23.dispatch_additional_mapping")
    A23.dispatch_capacitytech_min_cap_fac <- get_data(all_data, "gcam-usa/A23.dispatch_capacitytech_min_cap_fac", strip_attributes = TRUE)
    calibrated_techs_dispatch_usa <- get_data(all_data, "gcam-usa/calibrated_techs_dispatch_usa", strip_attributes = TRUE)
    capacity_credit_calculator <- get_data(all_data, "gcam-usa/dispatch/capacity_credit_calculator")
    TechTrialMarket_mapping <- get_data(all_data, "gcam-usa/dispatch/TechTrialMarket_mapping")
    L223.PrimaryRenewKeyword_elec <- get_data(all_data, "L223.PrimaryRenewKeyword_elec", strip_attributes = TRUE)
    L2233.GlobalTechCoef_elec_cool <- get_data(all_data, "L2233.GlobalTechCoef_elec_cool", strip_attributes = TRUE)
    L2233.GlobalIntTechCoef_elec_cool <- get_data(all_data, "L2233.GlobalIntTechCoef_elec_cool", strip_attributes = TRUE)

    L210.GrdRenewRsrcCurves_USA <- get_data(all_data, "L210.GrdRenewRsrcCurves_USA", strip_attributes = TRUE)
    L120.GridCost_offshore_wind_USA <- get_data(all_data, "L120.GridCost_offshore_wind_USA", strip_attributes = TRUE)
    L2237.StubTechCost_wind_reeds_USA <- get_data(all_data, "L2237.StubTechCost_wind_reeds_USA", strip_attributes = TRUE)
    L2238.StubTechCost_PV_reeds_USA <- get_data(all_data, "L2238.StubTechCost_PV_reeds_USA", strip_attributes = TRUE)
    L2239.StubTechCost_CSP_reeds_USA <- get_data(all_data, "L2239.StubTechCost_CSP_reeds_USA", strip_attributes = TRUE)


    # -----------------------------------------------------------------------------
    # 2. Perform computations

    # A vector indicate states where geothermal electric technologies will not be created
    NREL_us_re_technical_potential %>%
      left_join(states_subregions, by = c("State" = "state_name")) %>%
      filter(Geothermal_Hydrothermal_GWh == 0) %>%
      transmute(geo_state_noresource = paste(state, gcamusa.GEOTHERMAL, sep = " ")) %>%
      unlist ->
      geo_states_noresource

    # A vector indicating states where CSP electric technologies will not be created
    L119.CapacityFactor_CSP_state %>%
      # states with effectively no resource has a very minor capacity.factor (<0.01)
      # remove these states to avoid creating CSP technologies there
      filter(capacity.factor < 0.01) ->
      csp_states_noresource
    csp_states_noresource <- unique(csp_states_noresource$state)

    # Define states and basins that have access to seawater where seawater cooling will be allowed
    seawater_states_basins <- unique(usa_seawater_states_basins$seawater_region)

    # To account for new nesting-subsector structure and to add cooling technologies,
    # we must expand certain outputs
    add_cooling_techs <- function(data, table.type){

      if(table.type == "GlobalTech") {
        data %>%
          # join will duplicate rows because multiple cooling systems are available by technology
          # LJENM will error, so left_join() is used
          left_join(A23.elec_tech_mapping_cool %>%
                      select(technology, to.technology),
                    by = "technology") %>%
          rename(sector.name = sector,
                 subsector.name0 = subsector,
                 subsector.name = technology,
                 technology = to.technology) -> data_new
      }

      if(table.type == "StubTech") {
        data %>%
          # join will duplicate rows because multiple cooling systems are available by technology
          # LJENM will error, so left_join() is used
          left_join(A23.elec_tech_mapping_cool,
                    by = c("stub.technology" = "technology")) %>%
          # for stub.technologies, we need to remove seawater techs in non-coastal regions
          filter(water_type != gcamusa.WATER_TYPE_SEAWATER | region %in% seawater_states_basins) %>%
          rename(subsector0 = subsector,
                 subsector = stub.technology,
                 stub.technology = to.technology) %>%
          select(-cooling_system, -water_type) -> data_new
      }

      if(table.type == "Tech") {
        data %>%
          # join will duplicate rows because multiple cooling systems are available by technology
          # LJENM will error, so left_join() is used
          left_join(A23.elec_tech_mapping_cool,
                    by = "technology") %>%
          # for stub.technologies, we need to remove seawater techs in non-coastal regions
          filter(water_type != gcamusa.WATER_TYPE_SEAWATER | region %in% seawater_states_basins) %>%
          rename(subsector0 = subsector,
                 subsector = technology,
                 technology = to.technology) %>%
          select(-cooling_system, -water_type) -> data_new
      }

      return(data_new)

    }


    #############################################################################
    # Set up investment sectors
    #############################################################################


    # ===========================================================================
    # L223 sector_investment
    # ===========================================================================
    A23.dispatch_sector %>%
      filter(supplysector %in% gcamusa.ELEC_INV_NAMES) %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME)) ->
      L223.Sector_Investment

    # ===========================================================================
    # L223 nesting subsector_investment
    # Fuel level subsector
    # ===========================================================================
    A23.dispatch_subsector_logit %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME)) %>%
      rename(subsector0 = subsector) ->
      L223.SubsectorLogit_Investment_Fuel

    # subsector with specified shareweigt for each year
    A23.dispatch_subsector_shrwt %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["SubsectorShrwt"]] ) -> L223.SubsectorShrwt_Investment_Fuel_base

    # calculate historical capacity shares to obtain historical share-weights
    L123.capacity_EJ_state_elec_F_tech %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(calibrated_techs_dispatch_usa %>%
                                 select(-minicam.energy.input, -secondary.output),
                               by=c("gcam_fuel" = "fuel", "elec_tech" = "elec_tech")) %>%
      rename(region = state) %>%
      group_by(region, subsector) %>%
      summarise(capacity = sum(capacity)) %>%
      ungroup() %>%
      group_by(region) %>%
      mutate(capacity_share = capacity / sum(capacity)) %>%
      ungroup() %>%
      # # here actually all capacity are above 0 in this table, but still want a safety check
      # mutate(subsector.cal.value = ifelse(capacity > 0, 1, 0)) %>%
      # select(region, subsector, subsector.cal.value) ->
      select(region, subsector, capacity_share) ->
      L123.capacity_EJ_state_elec_F_tech_final_cal_year

    L223.SubsectorShrwt_Investment_Fuel_base %>%
      filter(year == min(MODEL_BASE_YEARS)) %>%
      select(-year) %>%
      # L123.capacity_EJ_state_elec_F_tech_final_cal_year does not include fuels with zero capacity in a state historically
      # thus it is missing entries - mostly for nuclear, wind, and solar - relative to LHS
      # LJENM will throw error because of NAs, left_join is used, and NAs are resolved below
      left_join(L123.capacity_EJ_state_elec_F_tech_final_cal_year, by = c("region", "subsector")) %>%
      # when there is no historical capacity, assign 0
      mutate(share.weight = if_else(is.na(capacity_share), 0, capacity_share)) %>%
      select(-capacity_share) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_BASE_YEARS)) %>%
      rename(subsector0 = subsector) ->
      L223.SubsectorShrwt_Investment_Fuel_calibration

    L223.SubsectorShrwt_Investment_Fuel_base %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      # For nuclear - don't allow new investment (use zero  shareweights) if there is no capacity historically
      # L123.capacity_EJ_state_elec_F_tech_final_cal_year does not include fuels with zero capacity in a state historically
      # thus it is missing entries - mostly for nuclear, wind, and solar - relative to LHS
      # LJENM will throw error because of NAs, left_join is used, and NAs are resolved below
      left_join(L123.capacity_EJ_state_elec_F_tech_final_cal_year, by = c("region", "subsector")) %>%
      # when there is no historical capacity, assign 0
      replace_na(list(capacity_share = 0)) %>%
      mutate(share.weight = as.numeric(share.weight),
             share.weight = if_else(subsector == "nuclear" & capacity_share == 0, 0, share.weight)) %>%
      select(-capacity_share) %>%
      rename(subsector0 = subsector) ->
      L223.SubsectorShrwt_Investment_Fuel_future

    # State-specific adjustments to electricity generation subsector shareweights
    A23.dispatch_subsector_shrwt_state_adj %>%
      set_years() %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwt"]]) -> A23.dispatch_subsector_shrwt_state_adj

    L223.SubsectorShrwt_Investment_Fuel_future %>%
      anti_join(A23.dispatch_subsector_shrwt_state_adj,
                by = c("region", "supplysector", "subsector0" = "subsector")) %>%
      bind_rows(A23.dispatch_subsector_shrwt_state_adj %>%
                  rename(subsector0 = subsector)) %>%
      arrange(region, supplysector, subsector0, year) -> L223.SubsectorShrwt_Investment_Fuel_future

    L223.SubsectorShrwt_Investment_Fuel_calibration %>%
      bind_rows(L223.SubsectorShrwt_Investment_Fuel_future) ->
      L223.SubsectorShrwt_Investment_Fuel

    # subsectors interp-to
    # subsectors with s-curve shareweigts - gas, coal, nuclear
    A23.dispatch_subsector_interp %>%
      filter(!is.na(to.value)) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]]) %>%
      # For nuclear - don't allow new investment (use zero  shareweights) if there is no capacity historically
      # L123.capacity_EJ_state_elec_F_tech_final_cal_year does not include fuels with zero capacity in a state historically
      # thus it is missing entries - mostly for nuclear, wind, and solar - relative to LHS
      # LJENM will throw error because of NAs, left_join is used, and NAs are resolved below
      left_join(L123.capacity_EJ_state_elec_F_tech_final_cal_year, by = c("region", "subsector")) %>%
      # when there is no historical capacity, assign 0
      mutate(capacity_share = if_else(is.na(capacity_share), 0, capacity_share),
             to.value = if_else(subsector == "nuclear" & capacity_share == 0, as.integer(0), to.value)) %>%
      select(-capacity_share) %>%
      rename(subsector0 = subsector) ->
      L223.SubsectorInterpTo_Investment_Fuel

    # State-specific adjustments to electricity generation subsector shareweights
    A23.dispatch_subsector_shrwt_interpto_state_adj %>%
      set_years() %>%
      mutate(from.year = as.integer(from.year)) %>%
      mutate(to.year = as.integer(to.year)) -> A23.dispatch_subsector_shrwt_interpto_state_adj

    L223.SubsectorInterpTo_Investment_Fuel %>%
      mutate(from.year = as.integer(from.year)) %>%
      mutate(to.year = as.integer(to.year)) %>%
      anti_join(A23.dispatch_subsector_shrwt_interpto_state_adj, by = c("region", "supplysector", "subsector0" = "subsector")) %>%
      bind_rows(A23.dispatch_subsector_shrwt_interpto_state_adj %>%
                  rename(subsector0 = subsector)) %>%
      arrange(region, subsector0, from.year, supplysector) -> L223.SubsectorInterpTo_Investment_Fuel


    # ===========================================================================
    # L223 subsector_investment
    # Subsector, corresponding to generation technology
    # Technologies are generation tech / cooling system combinations
    # ===========================================================================
    L223.SubsectorLogit_Investment_Fuel %>%
      # join will duplicate rows because multiple subsectors are available by technology
      # LJENM will error, so left_join() is used
      left_join(calibrated_techs_dispatch_usa %>%
                  filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
                  distinct(supplysector = sector, subsector0 = subsector, subsector = technology),
                by = c("supplysector", "subsector0")) %>%
      select(region, supplysector, subsector0, subsector, logit.year.fillout, logit.exponent, logit.type) ->
      L223.SubsectorLogit_Investment

    calibrated_techs_dispatch_usa %>%
      filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
      select(sector, supplysector, subsector, technology) %>%
      left_join_error_no_match(bind_rows(A23.globaltech_shrwt,
                                         A23.dispatch_globaltech_shrwt_additional),
                               by = c("supplysector", "subsector", "technology")) %>%
      select(-supplysector) %>%
      gather_years(value_col = "share.weight") %>%
      complete(nesting(sector, subsector, technology), year = c(year, MODEL_YEARS)) %>%
      distinct() %>%
      group_by(sector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight)) %>%
      ungroup() %>%
      repeat_add_columns(tibble::tibble(region = gcamusa.STATES)) %>%
      filter(year %in% MODEL_YEARS) %>%
      select(region, supplysector = sector, subsector0 = subsector, subsector = technology, year, share.weight) ->
      L223.SubsectorShrwt_Investment


    # ===========================================================================
    ## L223 StubTech_investment
    # ===========================================================================
    # create a stub.technology template
    calibrated_techs_dispatch_usa %>%
      filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
      select(supplysector = sector, subsector, stub.technology = technology) %>%
      write_to_all_states( LEVEL2_DATA_NAMES[["StubTech"]] ) %>%
      add_cooling_techs(table.type = "StubTech") ->
      L223.StubTech_Investment

    # ---------------------------------------------------------------------------
    calibrated_techs_dispatch_usa %>%
      filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
      select(sector, supplysector, subsector, technology, minicam.energy.input) %>%
      # using left_join because efficiency table has NAs for technology improvement variables
      # cannot filter them out becuase fill_exp_decay_extrapolate function needs these columns (even with NAs)
      left_join(bind_rows(mutate(A23.globaltech_eff, minicam.energy.input = if_else(minicam.energy.input == "global solar resource", gsub('_storage', '', paste0(technology, '_resource')), minicam.energy.input)),
                          A23.dispatch_globaltech_eff_additional),
                by = c("supplysector", "subsector", "technology", "minicam.energy.input")) %>%
      nest(-sector) %>%
      mutate(data = lapply(data, fill_exp_decay_extrapolate, MODEL_YEARS)) %>%
      unnest() %>%
      add_cooling_techs(table.type = "GlobalTech") %>%
      rename(efficiency = value) %>%
      mutate(year = as.integer(year)) %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name, subsector.name0, subsector.name, technology, year, minicam.energy.input, efficiency) ->
      L223.GlobalTechEff_Investment

    L223.GlobalTechEff_Investment %>%
      filter(subsector.name0 %in% c("solar", "wind")) %>%
      mutate(type = "Resource") ->
      L223.GlobalIntTechEff_Investment

    # Create a complete list of all global investment-technologies
    L223.GlobalTechEff_Investment %>%
      rename(invest.technology = technology) %>%
      select(sector.name, subsector.name0, subsector.name, invest.technology) %>%
      distinct() ->
      L233.GlobalInvestTech_Investment

    # ===========================================================================
    ## L223 StubTechMarket_Investment
    # ===========================================================================

    # built market names into L223.StubTechMarket_Investment
    L223.GlobalTechEff_Investment %>%
      select(-efficiency) %>%
      rename(supplysector = sector.name,
             subsector0 = subsector.name0,
             subsector = subsector.name,
             stub.technology = technology) %>%
      repeat_add_columns(tibble::tibble(region = gcamusa.STATES)) %>%
      filter(!grepl(gcamusa.WATER_TYPE_SEAWATER, stub.technology) | region %in% seawater_states_basins) %>%
      mutate(market.name = if_else(minicam.energy.input %in% c(gcamusa.STATE_RENEWABLE_RESOURCES, "PV_resource", "CSP_resource", gcamusa.STATE_BIOMASS_SECTORS),
                                   region, gcam.USA_REGION)) %>%
      select(region, supplysector, subsector0, subsector, stub.technology, year, minicam.energy.input, market.name) ->
      L223.StubTechMarket_Investment

    # assign regional fuel markets if gcamusa.USE_REGIONAL_FUEL_MARKETS is TRUE
    if(gcamusa.USE_REGIONAL_FUEL_MARKETS) {
      L223.StubTechMarket_Investment %>%
        left_join_error_no_match(states_subregions %>%
                                   select(state, grid_region),
                                 by = c("region" = "state")) %>%
        mutate(market.name = if_else(minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS, grid_region, market.name)) %>%
        select(-grid_region) ->
        L223.StubTechMarket_Investment
    }

    # ===========================================================================
    ## L223 GlobalTechCoef_Investment_cool
    # ===========================================================================

    # built market names into L223.StubTechMarket_Investment
    L223.GlobalTechEff_Investment %>%
      select(-minicam.energy.input, -efficiency) %>%
      left_join_error_no_match(A23.elec_tech_mapping_cool %>%
                                 select(to.technology, water_type),
                               by = c("technology" = "to.technology")) %>%
      # remove technologies with no water consumption (water type = none)
      filter(water_type != gcamusa.ELEC_COOLING_SYSTEM_NONE) %>%
      # need to make sure gas and RL steam and CT technologies are correctly
      # mapped to the corresponding global (steam/CT) technologies
      left_join(A23.dispatch_additional_mapping %>%
                  select(to.technology.cool, from.technology.cool),
                by = c("technology" = "to.technology.cool")) %>%
      mutate(from.technology.cool = if_else(is.na(from.technology.cool), technology, from.technology.cool)) %>%
      # map in water withdrawal & consumption coefficients from the global model
      # join will duplicate rows because there are withdrawal & consumption
      # coefficients for each technology. LJENM will error, so left_join is used
      left_join(L2233.GlobalTechCoef_elec_cool %>%
                  bind_rows(L2233.GlobalIntTechCoef_elec_cool) %>%
                  select(technology, year, minicam.energy.input, coefficient),
                by = c("from.technology.cool" = "technology", "year")) %>%
      select(sector.name, subsector.name0, subsector.name, technology, year, minicam.energy.input, coefficient) ->
      L223.GlobalTechCoef_Investment_cool

    # ===========================================================================
    ## L223 StubTechCoef_Investment_cool
    # ===========================================================================

    L223.GlobalTechCoef_Investment_cool %>%
      rename(supplysector = sector.name,
             subsector0 = subsector.name0,
             subsector = subsector.name,
             stub.technology = technology) %>%
      repeat_add_columns(tibble::tibble(region = gcamusa.STATES)) %>%
      filter(!grepl(gcamusa.WATER_TYPE_SEAWATER, stub.technology) | region %in% seawater_states_basins) %>%
      # market is region (state) for fresh water, USA for seawater
      mutate(market.name = if_else(minicam.energy.input == gcamusa.WATER_TYPE_SEAWATER, gcam.USA_REGION, region)) %>%
      select(region, supplysector, subsector0, subsector, stub.technology,
             year, minicam.energy.input, coefficient, market.name) ->
      L223.StubTechCoef_Investment_cool

    # ===========================================================================
    ## L223 GlobalTechOMvar_Investment  OM fix investment
    # ===========================================================================
    calibrated_techs_dispatch_usa %>%
      filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
      select(sector, supplysector, subsector, technology) %>%
      left_join_error_no_match(bind_rows(L113.globaltech_OMfixed_ATB, A23.dispatch_globaltech_OMfixed_additional) %>%
                                 select(-improvement.shadow.technology),
                               by = c("supplysector", "subsector", "technology")) %>%
      nest(-sector) %>%
      mutate(data = lapply(data, fill_exp_decay_extrapolate, MODEL_YEARS)) %>%
      unnest() %>%
      add_cooling_techs(table.type = "GlobalTech") %>%
      rename(OM.fixed = value) %>%
      mutate(year = as.integer(year)) %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name, subsector.name0, subsector.name, technology, year, input.OM.fixed, OM.fixed) ->
      L223.GlobalTechOMfixed_Investment

    # ===========================================================================
    ## L223 GlobalTechOMvar_Investment  OM var investment
    # ===========================================================================

    calibrated_techs_dispatch_usa %>%
      filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
      select(sector, supplysector, subsector, technology) %>%
      filter(technology %in% c(L113.globaltech_OMvar_ATB$technology, A23.dispatch_globaltech_OMvar_additional$technology)) %>%
      left_join_error_no_match(bind_rows(L113.globaltech_OMvar_ATB, A23.dispatch_globaltech_OMvar_additional) %>%
                                 select(-improvement.shadow.technology),
                               by = c("supplysector", "subsector", "technology")) %>%
      nest(-sector) %>%
      mutate(data = lapply(data, fill_exp_decay_extrapolate, MODEL_YEARS)) %>%
      unnest() %>%
      add_cooling_techs(table.type = "GlobalTech") %>%
      rename(OM.var = value) %>%
      mutate(year = as.integer(year)) %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name, subsector.name0, subsector.name, technology, year, input.OM.var, OM.var) ->
      L223.GlobalTechOMvar_Investment

    # ===========================================================================
    ## L223 GlobalTechCapital_Investment  capital.overnight investment
    # ===========================================================================

    calibrated_techs_dispatch_usa %>%
      filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
      select(sector, supplysector, subsector, technology) %>%
      left_join_error_no_match(bind_rows(L113.globaltech_capital_ATB,
                                         A23.dispatch_globaltech_capital_additional) %>%
                                 select(-improvement.shadow.technology),
                               by = c("supplysector", "subsector", "technology")) %>%
      nest(-sector) %>%
      mutate(data = lapply(data, fill_exp_decay_extrapolate, MODEL_YEARS)) %>%
      unnest() %>%
      add_cooling_techs(table.type = "GlobalTech") %>%
      rename(capital.overnight = value) %>%
      mutate(year = as.integer(year)) %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name, subsector.name0, subsector.name, technology, year, input.capital, capital.overnight, fixed.charge.rate) ->
      L223.GlobalTechCapital_Investment

    # ===========================================================================
    ## L223.GlobalTechCapital_Investment_cool  capital.overnight investment
    # ===========================================================================
    A23.CoolingSystemCosts %>%
      gather_years %>%
      complete(nesting(cooling_system, input.capital),
               year = c(year, MODEL_YEARS)) %>%
      group_by(cooling_system, input.capital) %>%
      mutate(value = approx_fun(year, value)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      # convert 2005 $/kW to 1975$
      mutate(capital.overnight = round(value * gdp_deflator(1975, 2005), 0)) %>%
      select(-value)->
      L223.CoolingSystemCosts

    L223.GlobalTechCapital_Investment %>%
      select(-input.capital, -capital.overnight) %>%
      left_join_error_no_match(A23.elec_tech_mapping_cool %>%
                                 select(to.technology, cooling_system),
                               by = c("technology" = "to.technology")) %>%
      left_join_error_no_match(L223.CoolingSystemCosts,
                               by = c("cooling_system", "year")) %>%
      select(sector.name, subsector.name0, subsector.name, technology, year, input.capital, capital.overnight, fixed.charge.rate) ->
      L223.GlobalTechCapital_Investment_cool

    # ===========================================================================
    ## L223 GlobalTechShrwt_Investment  shareweight investment
    # ===========================================================================
    calibrated_techs_dispatch_usa %>%
      filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
      select(sector, supplysector, subsector, technology) %>%
      left_join_error_no_match(bind_rows(A23.globaltech_shrwt,
                                         A23.dispatch_globaltech_shrwt_additional),
                               by = c("supplysector", "subsector", "technology")) %>%
      select(-supplysector) %>%
      gather_years(value_col = "share.weight") %>%
      complete(nesting(sector, subsector, technology), year = c(year, MODEL_YEARS)) %>%
      distinct() %>%
      group_by(sector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight)) %>%
      ungroup() %>%
      add_cooling_techs(table.type = "GlobalTech") %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name, subsector.name0, subsector.name, technology, year, share.weight) ->
      L223.GlobalTechShrwt_Investment

    # ===========================================================================
    # L223 investment technology share weights and interpolation rules
    # ===========================================================================

    # Prepare interpolation rules for all power plant + cooling system combinations
    # First, we assume that if the generation technology exists in the historical period
    # and is allowed to continue into future periods, the cooling technology shares will
    # be held constant into the future.  The exception is once through cooling, whose
    # share weights will be 0 from 2020-2100 to mirror GCAM-core.

    L123.capacity_EJ_state_elec_F_tech %>%
      filter(year %in% MODEL_YEARS) %>%
      left_join_error_no_match(calibrated_techs_dispatch_usa %>%
                                 # map in cooling systems; join will duplicate rows because multiple cooling systems
                                 # are available by technology;  LJENM will error, so left_join() is used
                                 left_join(A23.elec_tech_mapping_cool, by = "technology") %>%
                                 # if water type is seawater, set cooling system to seawater
                                 mutate(cooling_system = if_else(water_type == gcamusa.WATER_TYPE_SEAWATER,
                                                                 gcamusa.WATER_TYPE_SEAWATER,
                                                                 cooling_system)) %>%
                                 select(fuel, elec_tech, cooling_system, subsector, technology, to.technology),
                               by = c("gcam_fuel" = "fuel", "elec_tech", "cooling_system")) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(region = state, subsector0 = subsector, subsector = technology, stub.technology = to.technology, year, capacity) ->
      L223.hist_cap_USA

    L223.StubTech_Investment %>%
      # L223.StubTech_Investment_cap_USA includes only technologies with non-zero capacity historically
      # joining with L223.StubTech_Investment to get a complete list
      # join results in NAs (addressed below); LJENM throws error, so left_join is used instead
      # note that join will also duplicate rows because some techs are available in multiple investment segments
      left_join(L223.hist_cap_USA,
                by = c("region", "subsector0", "subsector", "stub.technology")) %>%
      mutate(year = max(MODEL_BASE_YEARS)) %>%
      replace_na(list(capacity = 0)) -> L223.StubTech_Investment_cap_USA

    # Set all once through technologies to zero in all future periods.
    L223.StubTech_Investment_cap_USA %>%
      filter(grepl(gcamusa.DISALLOWED_COOLING_TECH, stub.technology)) %>%
      mutate(from.year = min(MODEL_FUTURE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = gcamusa.FIXED_SHAREWEIGHT,
             to.value = 0) %>%
      mutate(apply.to = gcamusa.INTERP_APPLY_TO) %>%
      select(region, supplysector, subsector0, subsector, stub.technology, apply.to,
             from.year, to.year, to.value, interpolation.function) -> L223.StubTechInterpTo_Investment_oncethrough_USA

    L223.StubTech_Investment_cap_USA %>%
      filter(!grepl(gcamusa.DISALLOWED_COOLING_TECH, stub.technology)) %>%
      # calculate capacity shares (excluding once through, which is not allowed in the future)
      group_by(region, supplysector, subsector0, subsector, year) %>%
      mutate(subs_capacity = sum(capacity),
             capacity_share = round(capacity / subs_capacity, energy.DIGITS_SHRWT)) %>%
      ungroup() %>%
      left_join(L223.SubsectorShrwt_Investment_Fuel %>%
                  filter(year %in% (MODEL_FUTURE_YEARS)) %>%
                  group_by(region, supplysector, subsector0) %>%
                  # check if a fuel has non-zero share-weight in any future period (i.e. sum > 0)
                  summarise(future.subs.shrwt = if_else(sum(share.weight) > 0, 1, 0)) %>%
                  ungroup(),
                by = c("region", "supplysector", "subsector0")) -> L223.StubTech_Investment_cap_SW_USA

    # If the particular load segment / generation technology produced historically,
    # fix technology (cooling system) share weights to historical shares
    # for all future periods.  We do this even if the particular load segment /
    # generation technology is not allowed to deploy in the future - these assumptions
    # are handled at the subsector (generation technology) level.
    L223.StubTech_Investment_cap_SW_USA %>%
      filter(!grepl(gcamusa.DISALLOWED_COOLING_TECH, stub.technology),
             subs_capacity > 0 & future.subs.shrwt > 0) %>%
      mutate(from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = gcamusa.FIXED_SHAREWEIGHT,
             to.value = capacity_share) %>%
      mutate(apply.to = gcamusa.INTERP_APPLY_TO) %>%
      select(region, supplysector, subsector0, subsector, stub.technology, apply.to,
             from.year, to.year, to.value, interpolation.function)  -> L223.StubTechInterpTo_Investment_hist_USA

    # Second, if a generation technology did not exist in the historical period (i.e. CSP, IGCC, etc.),
    # but exists in the future, then the share weights for all non-once through cooling technologies
    # will be set to 1.
    L223.StubTech_Investment_cap_SW_USA %>%
      filter(!grepl(gcamusa.DISALLOWED_COOLING_TECH, stub.technology),
             subs_capacity == 0 & future.subs.shrwt > 0) %>%
      mutate(from.year = min(MODEL_FUTURE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = gcamusa.FIXED_SHAREWEIGHT,
             to.value = gcamusa.DEFAULT_SHAREWEIGHT) %>%
      mutate(apply.to = gcamusa.INTERP_APPLY_TO) %>%
      select(region, supplysector, subsector0, subsector, stub.technology, apply.to,
             from.year, to.year, to.value, interpolation.function) -> L223.StubTechInterpTo_Investment_fut_USA

    # Third, if a fuel and power plant combination did exist in the historical period,
    # but switches to a new power plant type (i.e. Nuclear Gen 2 -> Gen 3), we assume
    # that share weights of cooling technologies for that particular state remain similar
    # to the old power plant (e.g. Gen 3 will have the same cooling tech share weights as Gen 2).

    # Create a table with future techs whose cooling shares we can infer from existing ones
    L223.StubTech_Investment_cap_SW_USA %>%
      filter(!grepl(gcamusa.DISALLOWED_COOLING_TECH, stub.technology),
             subs_capacity == 0 & future.subs.shrwt > 0) %>%
      # Get rid of info related to calibrated values, which we've just confirmed are zero
      select(-capacity, -subs_capacity, -capacity_share) %>%
      # use semi_join to filter for future generation technologies which are mapped to current gen techs
      semi_join(A23.elec_tech_mapping_cool_shares_fut,
                by = "subsector") %>%
      left_join_error_no_match(A23.elec_tech_mapping_cool_shares_fut,
                               by = "subsector") %>%
      left_join_error_no_match(A23.elec_tech_mapping_cool %>%
                                 # if water type is seawater, set cooling system to seawater
                                 mutate(cooling_system = if_else(water_type == gcamusa.WATER_TYPE_SEAWATER,
                                                                 gcamusa.WATER_TYPE_SEAWATER,
                                                                 cooling_system)) %>%
                                 select(to.technology, cooling_system),
                               by = c("stub.technology" = "to.technology")) -> L223.elec_Investment_SW_future_techs_USA

    # Create a table with existing techs whose cooling shares we can use to inform future ones
    L223.StubTech_Investment_cap_SW_USA %>%
      select(-supplysector, -future.subs.shrwt) %>%
      distinct() %>%
      # nuclear gen II gets dropped out of the above table because it's not allowed to be
      # deployed in future periods and thus not set up as an investment technology
      # we need to add it back in to this table so we can use gen II cooling shares to
      # inform future gen III cooling shares
      bind_rows(L223.hist_cap_USA %>%
                  filter(!grepl(gcamusa.DISALLOWED_COOLING_TECH, stub.technology),
                         subsector0 == "nuclear") %>%
                  group_by(region, subsector0, subsector, year) %>%
                  mutate(subs_capacity = sum(capacity),
                         capacity_share = round(capacity / subs_capacity, energy.DIGITS_SHRWT)) %>%
                  ungroup()) %>%
      filter(!grepl(gcamusa.DISALLOWED_COOLING_TECH, stub.technology)) %>%
      # use semi_join to filter for current generation technologies which are used to inform
      # future gen tech cooling shares
      semi_join(A23.elec_tech_mapping_cool_shares_fut,
                by = c("subsector" = "mapped_subsector")) %>%
      left_join_error_no_match(A23.elec_tech_mapping_cool %>%
                                 # if water type is seawater, set cooling system to seawater
                                 mutate(cooling_system = if_else(water_type == gcamusa.WATER_TYPE_SEAWATER,
                                                                 gcamusa.WATER_TYPE_SEAWATER,
                                                                 cooling_system)) %>%
                                 select(to.technology, cooling_system),
                               by = c("stub.technology" = "to.technology")) %>%
      select(-capacity, -subs_capacity) -> L2233.elec_Investment_SW_mapped_techs_USA

    # Join tables and match up share info
    L223.elec_Investment_SW_future_techs_USA %>%
      # because nuclear gen II is not set up as an investment tech (as discussed above)
      # not all cooling tech combos for nuclear gen III are present in nuclear gen III in
      # L2233.elec_Investment_SW_mapped_techs_USA.  LJENM throws error because of NAs,
      # which are dealt with below. left_join is used
      left_join(L2233.elec_Investment_SW_mapped_techs_USA %>%
                  select(-stub.technology),
                by = c("region", "subsector0", "year",
                       "mapped_subsector" = "subsector", "cooling_system")) %>%
      replace_na(list(capacity_share = 0)) %>%
      # group_by(region, supplysector, subsector0, subsector) %>%
      # # check if a generation tech has non-zero share-weight in any future period (i.e. sum > 0)
      # mutate(subs.share.weight = if_else(sum(calOutputValue) > 0, 1, 0)) %>%
      # # Remove any generation techs with zero generation by mapped technologies,
      # # which will result in zero shares for every cooling tech.
      # # This could happen if once through had 100% share historically, since
      # # once through is not included in this (future-oriented) table.
      # filter(subs.share.weight != 0) %>%
      # # calculate new cooling tech shares (share weights) without once through
      # mutate(share.weight = round(calOutputValue / sum(calOutputValue), energy.DIGITS_SHRWT)) %>%
      # ungroup() %>%
    mutate(from.year = min(MODEL_FUTURE_YEARS),
           to.year = max(MODEL_YEARS),
           interpolation.function = gcamusa.FIXED_SHAREWEIGHT,
           to.value = capacity_share,
           apply.to = gcamusa.INTERP_APPLY_TO) %>%
      select(region, supplysector, subsector0, subsector, stub.technology, apply.to,
             from.year, to.year, to.value, interpolation.function) -> L223.StubTechInterpTo_Investment_mapped_USA

    # Finally, all power plant and cooling technology combinations that do not exist in
    # in future years are given 0 share weights in the future periods.
    L223.StubTech_Investment_cap_SW_USA %>%
      filter(future.subs.shrwt == 0) %>%
      mutate(from.year = min(MODEL_FUTURE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = gcamusa.FIXED_SHAREWEIGHT,
             to.value = 0) %>%
      mutate(apply.to = gcamusa.INTERP_APPLY_TO) %>%
      select(region, supplysector, subsector0, subsector, stub.technology, apply.to,
             from.year, to.year, to.value, interpolation.function) ->
      L223.StubTechInterpTo_Investment_nofut_USA

    # Combine all "InterpTo" cases which specify a particular future shareweight value
    L223.StubTechInterpTo_Investment_oncethrough_USA %>%
      bind_rows(L223.StubTechInterpTo_Investment_hist_USA,
                L223.StubTechInterpTo_Investment_fut_USA,
                L223.StubTechInterpTo_Investment_nofut_USA) %>%
      # use anti-join to avoid duplicates, since the scope of L223.StubTechInterpTo_Investment_mapped_USA
      # partially overlaps with that of L223.StubTechInterpTo_Investment_fut_USA
      anti_join(L223.StubTechInterpTo_Investment_mapped_USA,
                by = c("region", "supplysector", "subsector0", "subsector", "stub.technology")) %>%
      bind_rows(L223.StubTechInterpTo_Investment_mapped_USA) ->
      L223.StubTechInterpTo_Investment_USA

    # Make a table with shareweights for start and end points of "InterpTo" rules
    L223.StubTechInterpTo_Investment_USA %>%
      gather(drop, year, from.year, to.year) %>%
      select(region, supplysector, subsector0, subsector, stub.technology, year, share.weight = to.value) ->
      L223.StubTechShrwt_Investment_USA

    # Combine all interpolation cases
    L223.StubTechInterpTo_Investment_USA %>%
      # get rid of to.value to consolidate to one table
      # to.value is now reflected in L2233.StubTechShrwt_elecS_cool_USA
      select(-to.value) -> L223.StubTechInterp_Investment_USA


    # ===========================================================================
    ## L223 GlobalTechCapFac_Investment  capacity investment
    # ===========================================================================

    # 1) Here need to get the time share of each investment segment
    # Currently assumed the same for all gird
    L102.invest_segments %>%
      filter(grid_region == "Alaska grid") %>% # pick any grid is OK as they are the same for all gird
      select(invest_segment, hours) %>%
      mutate(invest_segment = as.character(invest_segment),
             seg_fraction = hours / gcamusa.ELEC_BASELOAD_HRS) %>%
      rename(sector = invest_segment) %>%
      select(-hours) ->
      L223.TechCapFac_Investment_SegAdjust

    # 2) map capacity factor for each investment segment based on the load shares estimated above
    calibrated_techs_dispatch_usa %>%
      filter(sector == "electricity generation") %>%
      select(technology, capacity.factor) %>%
      repeat_add_columns(tibble::tibble(sector = L223.TechCapFac_Investment_SegAdjust$sector)) %>%
      left_join_error_no_match(L223.TechCapFac_Investment_SegAdjust, by = "sector") %>%
      mutate(sector = as.character(sector)) %>%
      mutate(capacity.factor = capacity.factor * seg_fraction) %>%
      right_join(calibrated_techs_dispatch_usa %>%
                   filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
                   select(sector, supplysector, subsector, technology),
                 by = c("sector", "technology")) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      add_cooling_techs(table.type = "GlobalTech") %>%
      select(sector.name, subsector.name0, subsector.name, technology, year, capacity.factor) ->
      L223.GlobalTechCapFac_Investment

    L223.GlobalTechCapFac_Investment %>%
      filter(subsector.name0 %in% c("solar", "wind")) %>%
      left_join_error_no_match(L223.TechCapFac_Investment_SegAdjust, by = c("sector.name" = "sector")) %>%
      mutate(max.capacity.factor = seg_fraction) %>%
      select(-seg_fraction) ->
      L223.GlobalIntInvTechMaxCapFac_Investment

    # ===========================================================================
    ## L223 GlobalTechCapture_Investment  storage market investment
    # ===========================================================================

    calibrated_techs_dispatch_usa %>%
      filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
      select(sector, supplysector, subsector, technology) %>%
      filter(technology %in% A23.globaltech_co2capture$technology) %>%
      left_join_error_no_match(A23.globaltech_co2capture, by = c("supplysector", "subsector", "technology")) %>%
      select(-supplysector) %>%
      gather_years(value_col = "remove.fraction") %>%
      complete(nesting(sector, supplysector, subsector, technology), year = c(year, MODEL_FUTURE_YEARS)) %>%
      distinct() %>%
      group_by(sector, subsector, technology) %>%
      mutate(remove.fraction = approx_fun(year, remove.fraction)) %>%
      ungroup() %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      add_cooling_techs(table.type = "GlobalTech") %>%
      mutate(storage.market = energy.CO2.STORAGE.MARKET) %>%
      select(sector.name, subsector.name0, subsector.name, technology, year, remove.fraction, storage.market) ->
      L223.GlobalTechCapture_Investment

    # ===========================================================================
    ## L223 GlobalTechCost_Investment  capacity credit investment
    # ===========================================================================

    # capacity-market-price:the capital overnight cost of a gas CT in 1975USD/KW
    L223.GlobalTechCapital_Investment %>%
      filter(subsector.name == "gas (CT)",
             sector.name == gcamusa.ELEC_INV_NAMES[1]) %>%
      mutate(input.capital = "capacity credit",
             capital.overnight = capital.overnight * -1) %>%
      select(year, input.capital, capital.overnight, fixed.charge.rate) ->
      gas_CT_cost

    calibrated_techs_dispatch_usa %>%
      filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
      select(sector, subsector, technology) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      mutate(input.capital = "capacity credit") %>%
      left_join(gas_CT_cost, by = c("year", "input.capital")) %>%
      add_cooling_techs(table.type = "GlobalTech") %>%
      select(sector.name, subsector.name0, subsector.name, technology, year, input.capital, capital.overnight, fixed.charge.rate) ->
      L223.GlobalTechCost_Investment

    # ===========================================================================
    ## L223 GlobalTechCost_CapacityCreditCalulator  capacity credit calculator for non-dispatchable techs
    # ===========================================================================

    L223.GlobalTechCost_Investment %>%
      filter(subsector.name0 %in% capacity_credit_calculator$subsector) %>%
      left_join_error_no_match(capacity_credit_calculator, by = c("subsector.name0" = "subsector")) %>%
      rename(invest.technology = technology) %>%
      # select(LEVEL2_DATA_NAMES[['GlobalInvestTechCapacityCredit']]) ->
      select(sector.name, subsector.name0, subsector.name, invest.technology, year, steepness, x.mid, Cmax, Cmin) ->
      L223.GlobalTechCost_CapacityCreditCalulator

    # ===========================================================================
    ## L223 TechCapFac_Investment wind/solar capacity factor investment
    # ===========================================================================
    L210.GrdRenewRsrcCurves_USA %>%
      filter(renewresource != gcamusa.GEOTHERMAL) %>%
      group_by(region, renewresource) %>%
      summarize(capacity.factor = 1.0 - min(extractioncost)) %>%
      ungroup() %>%
      repeat_add_columns(tibble::tibble(sector = L223.TechCapFac_Investment_SegAdjust$sector)) %>%
      # here using inner_join as a cross-reference process to filter wind and solar for those only exist
      # in certain segment defined in calibrated_techs_dispatch_usa
      # currently wind and solar only exsit in base and intermediate segments
      mutate(sector = as.character(sector)) %>%
      inner_join(calibrated_techs_dispatch_usa %>%
                   filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
                   select(sector, supplysector, subsector, technology, minicam.energy.input),
                 by=c("sector", "renewresource" = "minicam.energy.input")) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      add_cooling_techs(table.type = "Tech") %>%
      mutate(supplysector = sector) %>%
      select(region, supplysector, subsector0, subsector, technology, year, capacity.factor) ->
      L223.TechCapFac_Investment

    # investment technology use trial at grid level
    L223.StubTechMarket_Investment %>%
      filter(subsector0 %in% capacity_credit_calculator$subsector) %>%
      left_join_error_no_match(select(states_subregions, state, grid_region), by = c("region" = "state")) %>%
      left_join_error_no_match(TechTrialMarket_mapping, by = c("region", "subsector0" = "subsector")) %>%
      rename(invest.technology = stub.technology) %>%
      # select(LEVEL2_DATA_NAMES[['InvestTechTrialMktName']]) ->
      select(region, supplysector, subsector0, subsector, invest.technology, year, trial.market.name) ->
      L223.TechTrialMarket_Investment

    # ===========================================================================
    ## L223 Sector_Investment_StateShare investment sharing sectors at the grid region
    # ===========================================================================

    # although called "state_share", it is essentially grid_region share
    # for supplysector, the logit is -6
    A23.dispatch_sector_state_share %>%
      repeat_add_columns(tibble::tibble(region = gcamusa.GRID_REGIONS)) %>%
      mutate(logit.year.fillout = MODEL_YEARS[1]) %>% #1975
      select(region, supplysector, output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent, logit.type) ->
      L223.Sector_Investment_StateShare

    # ===========================================================================
    ## L223 Subsector_Investment_StateShare investment sharing subsectors at the grid region
    # ===========================================================================

    # although called "state_share", it is essentially grid_region share
    # for subsector, the logit is -3
    states_subregions %>%
      select(state, grid_region) %>%
      # using right_join becuase logit.type in L223.Sector_Investment is NA, but this column is required
      right_join(L223.Sector_Investment, by = c("state" = "region")) %>%
      rename(subsector = state) %>% # not sure why subsector should be states
      rename(region = grid_region) %>%
      select(region, supplysector, subsector, logit.year.fillout, logit.exponent, logit.type)->
      L223.Subsector_Investment_StateShare

    # ===========================================================================
    ## L223.SubsectorShrwtFllt_Investment_StateShare subsector share.weigth normalized by 2015 capaicity shares in grid
    # ===========================================================================
    # Distribute State Investment share-weights within a grid region based on capacity distribution normalized to maximum in region
    # Normalize capacity by grid region

    # !!! note that here the capacity is in EJ (MW * hrs)
    L123.capacity_EJ_state_elec_F_tech %>%
      select(state, year, capacity) %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      group_by(state) %>%
      summarise(capacity = sum(capacity)) %>%
      left_join_error_no_match(states_subregions %>% select(state, grid_region), by = "state") %>%
      group_by(grid_region) %>%
      mutate(grid_region_max = max(capacity),
             grid_region_norm = capacity/grid_region_max) %>%
      ungroup() ->
      L123.capacity_EJ_state_elec_F_tech_Norm

    # Create table for normalized investment share weights by state

    L223.Subsector_Investment_StateShare %>%
      rename(year.fillout = logit.year.fillout) %>%
      left_join_error_no_match(L123.capacity_EJ_state_elec_F_tech_Norm %>%
                                 dplyr::select(subsector=state, region=grid_region, grid_region_norm), by = c("subsector", "region")) %>%
      rename(share.weight=grid_region_norm) %>%
      select(-logit.exponent, -logit.type) ->
      L223.SubsectorShrwtFllt_Investment_StateShare

    # ===========================================================================
    ## L223.TechCoef_Investment_StateShare technology coefficients
    # ===========================================================================

    # The dispatch sector will calculate investment in terms of capacity and investment
    # is done in terms of energy.  So we need to convert.  The investment state share
    # sector provides a good place to do it.
    L223.SubsectorShrwtFllt_Investment_StateShare %>%
      select(region, supplysector, subsector) %>%
      mutate(technology = subsector) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      mutate(minicam.energy.input = supplysector) %>%
      # convert from capacity to energy by adjusting by the number of hours in each
      # investment segment
      left_join_error_no_match(L102.invest_segments %>% mutate(invest_segment = as.character(invest_segment)),
                               by=c("region" = "grid_region", "supplysector" = "invest_segment")) %>%
      mutate(coefficient = hours / gcamusa.ELEC_BASELOAD_HRS * (1 - gcamusa.ELEC_INV_MARGIN)) %>%
      mutate(market.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
      L223.TechCoef_Investment_StateShare

    # The coefficient above will also adjust the prices which we don't actualy want to do.
    # We use the pMult to counter act that.
    L223.TechCoef_Investment_StateShare %>%
      mutate(pMult = 1.0 / coefficient) %>%
      select(LEVEL2_DATA_NAMES[['TechPmult']]) ->
      L223.TechPmult_Investment_StateShare

    # ===========================================================================
    ## L223.TechShrwt_Investment_StateShare technology shareweight
    # ===========================================================================

    L223.TechCoef_Investment_StateShare %>%
      select(region, supplysector, subsector, technology, year) %>%
      mutate(share.weight = gcamusa.DEFAULT_SHAREWEIGHT) ->
      L223.TechShrwt_Investment_StateShare


    # ===========================================================================
    ## Dispatch sectors
    # ===========================================================================

    # ===========================================================================
    ## sectors logit dispatch
    # ===========================================================================

    A23.dispatch_sector %>%
      filter(supplysector %in% gcamusa.ELEC_GEN_NAMES) %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type")) ->
      L223.Sector_Dispatch

    # ===========================================================================
    ## subsector logit dispatch and share-weight
    # TODO: these values are not actually going to be used
    # ===========================================================================

    calibrated_techs_dispatch_usa %>%
      filter(sector == "electricity generation") %>%
      select(supplysector, subsector) %>%
      distinct() %>%
      mutate(logit.exponent = -3, logit.type = NA) %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], "logit.type")) ->
      L223.SubsectorLogit_Dispatch

    # subsector shareweight
    L223.SubsectorLogit_Dispatch %>%
      select(region, supplysector, subsector) %>%
      mutate(year.fillout = MODEL_YEARS[1], share.weight = 1) ->
      L223.SubsectorShrwtFllt_Dispatch

    # technolgoy shareweight
    calibrated_techs_dispatch_usa %>%
      filter(sector == "electricity generation") %>%
      select(supplysector, subsector, technology) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = gcamusa.DEFAULT_SHAREWEIGHT) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["TechShrwt"]]) %>%
      add_cooling_techs(table.type = "Tech") %>%
      select(region, supplysector, subsector = subsector0, technology, year, share.weight) ->
      L223.TechShrwt_Dispatch

    # technolgoy efficiency
    calibrated_techs_dispatch_usa %>%
      filter(sector == "electricity generation") %>%
      select(sector, supplysector, subsector, technology, minicam.energy.input) %>%
      # filter out hydro in this table as it does not have an input
      filter(!is.na(minicam.energy.input)) %>%
      # using left_join becuase efficiency table has NAs for technology improvement parameters for some technologies
      # but fill_exp_decay_extrapolate needs those columns
      left_join(bind_rows(mutate(A23.globaltech_eff, minicam.energy.input = if_else(minicam.energy.input == "global solar resource", gsub('_storage', '', paste0(technology, '_resource')), minicam.energy.input)),
                          A23.dispatch_globaltech_eff_additional),
                by = c("supplysector", "subsector", "technology", "minicam.energy.input")) %>%
      fill_exp_decay_extrapolate(MODEL_YEARS) %>%
      select(-sector) %>%
      rename(efficiency = value) %>%
      mutate(market.name = "temp") %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["TechEff"]]) %>%
      add_cooling_techs(table.type = "Tech") %>%
      mutate(market.name = if_else(minicam.energy.input %in% c(gcamusa.STATE_RENEWABLE_RESOURCES, "PV_resource", "CSP_resource", gcamusa.STATE_BIOMASS_SECTORS),
                                   region, gcam.USA_REGION)) %>%
      select(region, supplysector, subsector = subsector0, technology, year, minicam.energy.input, efficiency, market.name) ->
      L223.TechEff_Dispatch

    # assign regional fuel markets if gcamusa.USE_REGIONAL_FUEL_MARKETS is TRUE
    if(gcamusa.USE_REGIONAL_FUEL_MARKETS) {
      L223.TechEff_Dispatch %>%
        left_join_error_no_match(states_subregions %>%
                                   select(state, grid_region),
                                 by = c("region" = "state")) %>%
        mutate(market.name = if_else(minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS, grid_region, market.name)) %>%
        select(-grid_region) ->
        L223.TechEff_Dispatch
    }

    L223.TechEff_Dispatch %>%
      filter(subsector %in% c("solar", "wind")) %>%
      mutate(flag = "Resource") ->
      L223.StubTechEffFlag_Dispatch

    # L223.TechCoef_Dispatch_cool - water withdrawal and consumption coefficients
    L223.TechShrwt_Dispatch %>%
      select(-share.weight) %>%
      left_join_error_no_match(A23.elec_tech_mapping_cool %>%
                                 select(to.technology, water_type),
                               by = c("technology" = "to.technology")) %>%
      # remove technologies with no water consumption (water type = none)
      filter(water_type != gcamusa.ELEC_COOLING_SYSTEM_NONE) %>%
      # need to make sure gas and RL steam and CT technologies are correctly
      # mapped to the corresponding global (steam/CT) technologies
      left_join(A23.dispatch_additional_mapping %>%
                  select(to.technology.cool, from.technology.cool),
                by = c("technology" = "to.technology.cool")) %>%
      mutate(from.technology.cool = if_else(is.na(from.technology.cool), technology, from.technology.cool)) %>%
      # map in water withdrawal & consumption coefficients from the global model
      # join will duplicate rows because there are withdrawal & consumption
      # coefficients for each technology. LJENM will error, so left_join is used
      left_join(L2233.GlobalTechCoef_elec_cool %>%
                  bind_rows(L2233.GlobalIntTechCoef_elec_cool) %>%
                  select(technology, year, minicam.energy.input, coefficient),
                by = c("from.technology.cool" = "technology", "year")) %>%
      mutate(market.name = if_else(minicam.energy.input == gcamusa.WATER_TYPE_SEAWATER, gcam.USA_REGION, region)) %>%
      select(region, supplysector, subsector, technology, year, minicam.energy.input, coefficient, market.name) ->
      L223.TechCoef_Dispatch_cool

    # technology OM_fixed
    calibrated_techs_dispatch_usa %>%
      filter(sector == "electricity generation") %>%
      select(sector, supplysector, subsector, technology) %>%
      filter(technology %in% c(L113.globaltech_OMfixed_ATB$technology,
                               A23.dispatch_globaltech_OMfixed_additional$technology)) %>%
      left_join_error_no_match(bind_rows(L113.globaltech_OMfixed_ATB,
                                         A23.dispatch_globaltech_OMfixed_additional) %>%
                                 select(-improvement.shadow.technology),
                               by = c("supplysector", "subsector", "technology")) %>%
      fill_exp_decay_extrapolate(MODEL_YEARS) %>%
      select(-sector) %>%
      rename(OM.fixed = value) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["TechOMfixed"]]) %>%
      add_cooling_techs(table.type = "Tech") %>%
      select(region, supplysector, subsector = subsector0, technology, year, input.OM.fixed, OM.fixed) ->
      L223.TechOMfixed_Dispatch

    # technology OM_Var
    calibrated_techs_dispatch_usa %>%
      filter(sector == "electricity generation") %>%
      select(sector, supplysector, subsector, technology) %>%
      filter(technology %in% c(L113.globaltech_OMvar_ATB$technology,
                               A23.dispatch_globaltech_OMvar_additional$technology)) %>%
      left_join_error_no_match(bind_rows(L113.globaltech_OMvar_ATB,
                                         A23.dispatch_globaltech_OMvar_additional) %>%
                                 select(-improvement.shadow.technology),
                               by = c("supplysector", "subsector", "technology")) %>%
      # TODO: double check why solar doesn't have OMvar
      fill_exp_decay_extrapolate(MODEL_YEARS) %>%
      select(-sector) %>%
      rename(OM.var = value) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["TechOMvar"]]) %>%
      add_cooling_techs(table.type = "Tech") %>%
      select(region, supplysector, subsector = subsector0, technology, year, input.OM.var, OM.var) ->
      L223.TechOMvar_Dispatch

    # technology lifetime (three parts)
    # "final-calibration-year", "final-historical-year"
    A23.globaltech_retirement %>%
      anti_join(A23.dispatch_globaltech_retirement_additional,
                by = c("supplysector", "subsector", "technology", "year")) %>%
      bind_rows(A23.dispatch_globaltech_retirement_additional) %>%
      filter(year %in% c("final-calibration-year", "final-historical-year")) %>%
      set_years() %>%
      mutate(year_int = as.integer(year)) %>%
      select(supplysector, subsector, technology, year_int, lifetime) ->
      L223.TechLifetime_Dispatch

    # "initial-future-year"
    A23.globaltech_retirement %>%
      anti_join(A23.dispatch_globaltech_retirement_additional,
                by = c("supplysector", "subsector", "technology", "year")) %>%
      bind_rows(A23.dispatch_globaltech_retirement_additional) %>%
      filter(year == "initial-future-year") %>%
      set_years() %>%
      mutate(year_temp = as.integer(year)) %>%
      select(supplysector, subsector, technology, year_temp, lifetime) %>%
      expand(., ., year_int = MODEL_YEARS[MODEL_YEARS >= as.integer(set_years(tibble::tibble("initial-future-year")))]) %>%
      select(-year_temp) %>%
      bind_rows(L223.TechLifetime_Dispatch, .) %>%
      distinct() ->
      L223.TechLifetime_Dispatch

    # "initial-nonhistorical-year"
    A23.globaltech_retirement %>%
      anti_join(A23.dispatch_globaltech_retirement_additional,
                by = c("supplysector", "subsector", "technology", "year")) %>%
      bind_rows(A23.dispatch_globaltech_retirement_additional) %>%
      filter(year == "initial-nonhistorical-year") %>%
      set_years() %>%
      mutate(year_temp = as.integer(year)) %>%
      select(supplysector, subsector, technology, year_temp, lifetime) %>%
      expand(., .,
             year_int = MODEL_YEARS[MODEL_YEARS >= as.integer(set_years(tibble::tibble("initial-nonhistorical-year")))]) %>%
      select(-year_temp) %>%
      bind_rows(L223.TechLifetime_Dispatch, .) %>%
      distinct() %>%
      rename(year = year_int) %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["TechYr"]], "lifetime")) %>%
      add_cooling_techs(table.type = "Tech") %>%
      select(region, supplysector, subsector = subsector0, technology, year, lifetime) %>%
      filter(technology %in% unique(L223.TechOMfixed_Dispatch$technology) | technology == "hydro") ->
      L223.TechLifetime_Dispatch

    # Capacity technology profit shutdown which will actually be used during
    # calculations to determine new investment only. The cost of building
    # and operating new capacity compared to operating the existing and will
    # discount that existing capacity towards the capacity reserve margin when
    # it was cheaper to invest in new
    L223.TechShrwt_Dispatch %>%
      select(-share.weight) %>%
      mutate(median.shutdown.point = gcamusa.ELEC_CAP_INV_MEDIAN,
             profit.shutdown.steepness = gcamusa.ELEC_CAP_INV_STEEPNESS) %>%
      rename(stub.technology = technology) ->
      L223.TechProfitShutdown_Dispatch

    # technology S-Curve
    A23.globaltech_retirement %>%
      bind_rows(A23.dispatch_globaltech_retirement_additional) %>%
      filter(year %in% c("final-calibration-year", "final-historical-year")) %>%
      filter(!is.na(steepness)) %>%
      set_years() %>%
      mutate(year = as.integer(year)) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["TechSCurve"]]) %>%
      add_cooling_techs(table.type = "Tech") %>%
      select(region, supplysector, subsector = subsector0, technology, year, lifetime, steepness, half.life) %>%
      filter(technology %in% unique(L223.TechOMfixed_Dispatch$technology) | technology == "hydro") ->
      L223.TechSCurve_Dispatch

    # technology capacity factor
    calibrated_techs_dispatch_usa %>%
      filter(sector == "electricity generation") %>%
      select(supplysector, subsector, technology, capacity.factor) %>%
      expand(., ., year = MODEL_YEARS) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["TechCapFac"]]) %>%
      add_cooling_techs(table.type = "Tech") %>%
      select(region, supplysector, subsector = subsector0, technology, year, capacity.factor) ->
      L223.TechCapFac_Dispatch

    # technology capacity
    L223.TechCapFac_Dispatch %>%
      rename(dispatch.sector = supplysector,
             capacity.technology = technology,
             capacity = capacity.factor) %>%
      mutate(capacity = 0.0 ) ->
      L223.CapacityTech_FutureTechs

    # renewable capacity factor by load segment

    # We need to make sure hydro continues to use the historical capacity factor into
    # the future.  We can do that by using the segment specific capacity factors.
    # TODO: get actual seasonal variations for hydro instead
    # L123.out_EJ_state_elec_F_tech %>%
    #   filter(fuel == "hydro", year == MODEL_FINAL_BASE_YEAR) %>%
    #   left_join_error_no_match(L123.capacity_EJ_state_elec_F_tech, by= c("state", "fuel" = "gcam_fuel", "year")) %>%
    #   mutate(capacity.factor = value / capacity) %>%
    #   select(state, sector, fuel, capacity.factor) %>%
    #   expand(., ., segment = gcamusa.ELEC_LOAD_SEGMENT_ORDER) ->
    #   L223.hydro_CapFac_segment


    bind_rows(L114.CapacityFactor_wind_state_segment,
              L114.CapacityFactor_wind_offshore_state_segment,
              L119.CapacityFactor_PV_state_segment,
              L119.CapacityFactor_CSP_state_segment) %>%
      #L223.hydro_CapFac_segment) %>%
      select(-sector) %>%
      rename(technology = fuel, region = state) %>%
      # need to account for cooling techs before performing the join below
      # join will duplicate rows because multiple cooling systems are available by technology
      # LJENM will error, so left_join() is used
      left_join(A23.elec_tech_mapping_cool %>%
                  select(technology, to.technology),
                by = "technology") %>%
      select(-technology) %>%
      rename(technology = to.technology) %>%
      # note expanding by rows here so just regular left join
      left_join(L223.TechCapFac_Dispatch %>%
                  select(-capacity.factor),
                by = c("region", "technology")) %>%
      # hydro is currently only produced out of the final calibration year, while
      # it is not an error to include the segment specific capacity factor in the
      # future, not including it helps keep the size of the XML down
      # filter((technology == "hydro" & year == MODEL_FINAL_BASE_YEAR) |
      #          (technology != "hydro" & year >= MODEL_FINAL_BASE_YEAR)) %>%
      rename(dispatch.sector = supplysector,
             capacity.technology = technology) %>%
      select(LEVEL2_DATA_NAMES[['CapacityTechSegmentCapFac']]) ->
      L223.CapacityTechSegmentCapFac

    A23.dispatch_capacitytech_min_cap_fac %>%
      expand(., ., year = MODEL_YEARS) %>%
      rename(dispatch.sector = supplysector,
             capacity.technology = technology) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["CapacityTechMinCapFac"]]) %>%
      # add_cooling_techs needs a technology - create a temporary one
      mutate(technology = capacity.technology) %>%
      add_cooling_techs(table.type = "Tech") %>%
      select(region, dispatch.sector, subsector = subsector0, capacity.technology = technology, year, min.capacity.factor) ->
      L223.CapacityTechMinCapFac

    # carbon storage market and remove.fraction
    A23.globaltech_co2capture %>%
      left_join_error_no_match(calibrated_techs_dispatch_usa %>%
                                 filter(sector == "electricity generation") %>%
                                 select(supplysector, subsector, technology),
                               by = c("supplysector", "subsector", "technology")) %>%
      gather(key="year", value="remove.fraction", -supplysector, -subsector, -technology) %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_FUTURE_YEARS)) %>%
      distinct() %>%
      mutate(year = as.integer(year)) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(remove.fraction = approx_fun(year, remove.fraction)) %>%
      ungroup() %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      mutate(storage.market = energy.CO2.STORAGE.MARKET) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["CarbonCapture"]]) %>%
      add_cooling_techs(table.type = "Tech") %>%
      select(region, supplysector, subsector = subsector0, technology, year, storage.market, remove.fraction) ->
      L223.TechCarbonCapture_Dispatch

    # logit table template, check later
    tibble(region = gcamusa.GRID_REGIONS,
           supplysector = "electricity",
           output.unit = "EJ",
           input.unit = "EJ",
           price.unit = "1975$/GJ",
           logit.year.fillout = MODEL_YEARS[1],
           logit.exponent = -3,
           logit.type = NA) ->
      L223.Sector_Dispatch_Grid

    # dispatch sector
    L223.Sector_Dispatch %>%
      select(region, supplysector) %>%
      rename(dispatch.sector = supplysector) %>%
      expand(., ., generation.sector = gcamusa.ELEC_INV_NAMES) %>%
      mutate(generation.sector.market = region) %>%
      bind_rows(
        states_subregions %>%
          select(state, grid_region) %>%
          rename(region = grid_region) %>%
          rename(generation.sector.market = state) %>%
          mutate(dispatch.sector = "electricity") %>%
          mutate(generation.sector = "electricity")
      ) ->
      L223.DispatchSector

    # dispatch sector demand segment
    L102.load_segments %>%
      rename(region = grid_region) %>%
      mutate(dispatch.sector = "electricity") %>%
      select(region, dispatch.sector, segment, hours, relative.generation, generation.fraction) %>%
      mutate(demand.segment.name = dispatch.sector) ->
      L223.DispatchSectorDispatchSegments

    # Next we need to map which dispatch segment most closely alligns with
    # the investment segment so that it can be used to determine capacity
    # investment
    L102.load_segments %>%
      group_by(grid_region) %>%
      arrange(desc(relative.generation)) %>%
      # create the total hours so we can use it to match up to the
      # hour when the invesement segments "transition" from one to
      # the other
      mutate(hours_cumm = cumsum(hours)) %>%
      left_join(L102.invest_segments %>%
                  select(grid_region, invest_segment, hours) %>%
                  group_by(grid_region) %>%
                  # hours refer to the total hours at the "end" of the segement
                  # for the purposes of this calculation we want the hour at the
                  # "transition" so we lag
                  mutate(hours = lag(hours, default = !!gcamusa.ELEC_SUPERPEAK_HRS)) %>%
                  ungroup() %>%
                  spread(invest_segment, hours), by = c("grid_region")) %>%
      # first mark if a dispatch segment occurs before the transition
      # to each investment segment
      mutate_at(gcamusa.ELEC_INV_NAMES, funs(. <= hours_cumm)) %>%
      # we want to select the first dispatch segment after the transition
      # and to do that we can "summarize" by just choosing the first TRUE
      # for each investment segment
      summarize_at(gcamusa.ELEC_INV_NAMES, funs(first(segment[.]))) %>%
      gather(invest.segment, dispatch.segment, gcamusa.ELEC_INV_NAMES) ->
      DispSegmentInvestSegmentMap

    # Pull the dispatch segment to investment segment map into L223.DispatchSectorDispatchSegments
    L223.DispatchSectorDispatchSegments %>%
      # using left_join as we expect NAs for all the dispatch sectors that
      # won't be used to calculate capacity investment
      left_join(DispSegmentInvestSegmentMap,
                by = c("region" = "grid_region", "segment" = "dispatch.segment")) %>%
      # replace the NAs with just an empty string which will indicate
      # that segment is not used to calculate investment
      mutate(invest.segment = if_else(is.na(invest.segment), "", invest.segment)) ->
      L223.DispatchSectorDispatchSegments


    # Calibration
    # calibrated capacity for electricity technology
    L123.capacity_EJ_state_elec_F_tech %>%
      filter(year %in% MODEL_YEARS) %>%
      left_join_error_no_match(calibrated_techs_dispatch_usa %>%
                                 # map in cooling systems; join will duplicate rows because multiple cooling systems
                                 # are available by technology;  LJENM will error, so left_join() is used
                                 left_join(A23.elec_tech_mapping_cool, by = "technology") %>%
                                 # if water type is seawater, set cooling system to seawater
                                 mutate(cooling_system = if_else(water_type == gcamusa.WATER_TYPE_SEAWATER,
                                                                 gcamusa.WATER_TYPE_SEAWATER,
                                                                 cooling_system)) %>%
                                 select(fuel, elec_tech, cooling_system, supplysector, subsector, to.technology),
                               by = c("gcam_fuel" = "fuel", "elec_tech", "cooling_system")) %>%
      # select(LEVEL2_DATA_NAMES[['CapacityTech']]) ->
      select(region = state, dispatch.sector = supplysector, subsector, capacity.technology = to.technology, year, capacity) ->
      L223.CapacityTech

    # # Make sure that we're not inflating capacity or generation by mapping in cooling system info
    # L123.capacity_EJ_state_elec_F_tech %>%
    #   group_by(year) %>%
    #   summarize(capacity = sum(capacity)) -> TEST1
    #
    # L223.CapacityTech %>%
    #   group_by(year) %>%
    #   summarize(capacity = sum(capacity)) -> TEST2

    # calibrated output for electricity technology
    L123.out_EJ_state_elec_F_tech %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = value) %>%
      left_join_error_no_match(calibrated_techs_dispatch_usa %>%
                                 # map in cooling systems; join will duplicate rows because multiple cooling systems
                                 # are available by technology;  LJENM will error, so left_join() is used
                                 left_join(A23.elec_tech_mapping_cool, by = "technology") %>%
                                 # if water type is seawater, set cooling system to seawater
                                 mutate(cooling_system = if_else(water_type == gcamusa.WATER_TYPE_SEAWATER,
                                                                 gcamusa.WATER_TYPE_SEAWATER,
                                                                 cooling_system)) %>%
                                 select(sector, fuel, elec_tech, cooling_system, supplysector, subsector, to.technology),
                               by = c("sector", "fuel", "elec_tech", "cooling_system")) %>%
      # select(LEVEL2_DATA_NAMES[["TechYr"]], calOutputValue) -> L223.Production_Dispatch_temp
      select(region = state, supplysector, subsector, capacity.technology = to.technology, year, calOutputValue) ->
      L223.Production_Dispatch_temp

    # # Make sure that we're not inflating generation by mapping in cooling system info
    # L123.out_EJ_state_elec_F_tech %>%
    #   group_by(year) %>%
    #   summarize(calOutputValue = sum(value)) %>%
    #   filter(year %in% MODEL_BASE_YEARS) -> TEST1
    #
    # L223.Production_Dispatch_temp %>%
    #   group_by(year) %>%
    #   summarize(calOutputValue = sum(calOutputValue)) -> TEST2

    # L123.out_EJ_state_elec_F_tech contains only technologies which have non-zero historical generation
    # start with full set of dispatch techs, join in non-zero calibrated values, and assign
    # zero calibrated output for the others
    L223.TechShrwt_Dispatch %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      # L223.Production_Dispatch_temp contains subset of technologies which have non-zero historical generation
      # LJENM throws error; left_join() is used; NAs are assigned zero calOutputValue below
      left_join(L223.Production_Dispatch_temp, by = c("region", "supplysector", "subsector",
                                                      "technology" = "capacity.technology", "year")) %>%
      # technologies missing calOutputValue have no generation in that region / period
      mutate(calOutputValue = if_else(is.na(calOutputValue), 0, calOutputValue),
             share.weight.year = year,
             # share weights are 1 for all dispatch subsectors / technologies
             subs.share.weight = gcamusa.DEFAULT_SHAREWEIGHT,
             tech.share.weight = gcamusa.DEFAULT_SHAREWEIGHT) %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) -> L223.Production_Dispatch

    # calibrated capacity factor for electricity technology
    L123.in_EJ_state_elec_F_tech %>%
      mutate(variable = "input") %>%
      bind_rows(mutate(L123.out_EJ_state_elec_F_tech, variable = "output")) %>%
      filter(fuel != "hydro") %>%
      filter(year %in% MODEL_YEARS) %>%
      spread(variable, value) %>%
      mutate(efficiency = output / input) %>%
      # These are renewables and nuclear
      filter(!is.na(efficiency)) %>%
      select(-input, -output) %>%
      left_join_error_no_match(calibrated_techs_dispatch_usa %>%
                                 # map in cooling systems; join will duplicate rows because multiple cooling systems
                                 # are available by technology;  LJENM will error, so left_join() is used
                                 left_join(A23.elec_tech_mapping_cool, by = "technology") %>%
                                 # if water type is seawater, set cooling system to seawater
                                 mutate(cooling_system = if_else(water_type == gcamusa.WATER_TYPE_SEAWATER,
                                                                 gcamusa.WATER_TYPE_SEAWATER,
                                                                 cooling_system)) %>%
                                 select(sector, fuel, elec_tech, cooling_system, supplysector, subsector, to.technology, minicam.energy.input),
                               by = c("sector", "fuel", "elec_tech", "cooling_system")) %>%
      select(region = state, supplysector, subsector, technology = to.technology, year, minicam.energy.input, efficiency) %>%
      mutate(market.name = if_else(minicam.energy.input %in% c(gcamusa.STATE_RENEWABLE_RESOURCES, "PV_resource", "CSP_resource", gcamusa.STATE_BIOMASS_SECTORS),
                                   region, gcam.USA_REGION)) ->
      L223.TechEff_Cal

    # assign regional fuel markets if gcamusa.USE_REGIONAL_FUEL_MARKETS is TRUE
    if(gcamusa.USE_REGIONAL_FUEL_MARKETS) {
      L223.TechEff_Cal %>%
        left_join_error_no_match(states_subregions %>%
                                   select(state, grid_region),
                                 by = c("region" = "state")) %>%
        mutate(market.name = if_else(minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS, grid_region, market.name)) %>%
        select(-grid_region) ->
        L223.TechEff_Cal
    }

    # capacity technology use trail market at grid level
    L223.TechShrwt_Dispatch %>%
      filter(subsector %in% capacity_credit_calculator$subsector) %>%
      left_join_error_no_match(select(states_subregions, state, grid_region), by = c("region" = "state")) %>%
      left_join_error_no_match(TechTrialMarket_mapping, by = c("region", "subsector")) %>%
      rename(dispatch.sector = supplysector,
             capacity.technology = technology) %>%
      select(LEVEL2_DATA_NAMES[['CapacityTechTrialMktName']]) ->
      L223.TechTrialMarket_Dispatch

    # Primary energy keywords for capacity technologies
    # combine default and additional dispatch technology tables
    A23.globaltech_keyword %>%
      bind_rows(A23.globaltech_keyword_additional) -> L223.globaltech_keyword

    # segment technologies with renewable keywords
    L223.globaltech_keyword %>%
      filter(!is.na(primary.renewable)) -> L223.renew_keyword

    # segment technologies with fossil keywords
    L223.globaltech_keyword %>%
      filter(!is.na(average.fossil.efficiency)) -> L223.fossil_keyword

    # L223.PrimaryRenewKeyword_Dispatch_USA - primary energy keywords for renewable dispatch technologies
    L223.CapacityTech %>%
      bind_rows(L223.CapacityTech_FutureTechs) %>%
      select(-capacity) %>%
      # filter for techs with renewable energy keywords (solar, wind, geo, nuc)
      semi_join(L223.renew_keyword %>%
                  distinct(subsector, primary.renewable),
                by = c("subsector")) %>%
      left_join_error_no_match(L223.renew_keyword %>%
                                 distinct(subsector, primary.renewable),
                               by = c("subsector")) %>%
      rename(supplysector = dispatch.sector,
             technology = capacity.technology) ->
      L223.PrimaryRenewKeyword_Dispatch_USA

    # L223.AvgFossilEffKeyword_Dispatch_USA - primary energy keywords for fossil dispatch technologies
    L223.CapacityTech %>%
      bind_rows(L223.CapacityTech_FutureTechs) %>%
      select(-capacity) %>%
      # filter for techs with renewable energy keywords (solar, wind, geo, nuc)
      semi_join(L223.fossil_keyword %>%
                  distinct(subsector, average.fossil.efficiency),
                by = c("subsector")) %>%
      left_join_error_no_match(L223.fossil_keyword %>%
                                 distinct(subsector, average.fossil.efficiency),
                               by = c("subsector")) %>%
      rename(supplysector = dispatch.sector,
             technology = capacity.technology) ->
      L223.AvgFossilEffKeyword_Dispatch_USA


    # ===========================================================================
    # Socioeconomic information in the electricity grid regions (required for GCAM to run with these regions)
    # ===========================================================================

    # L223.InterestRate_FERC: Interest rates in the FERC grid regions
    tibble(region = gcamusa.GRID_REGIONS,
           interest.rate = socioeconomics.DEFAULT_INTEREST_RATE) ->
      L223.InterestRate_FERC

    # L223.Pop_FERC: Population
    tibble(region = gcamusa.GRID_REGIONS,
           totalPop = 1) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) ->
      L223.Pop_FERC

    # L223.BaseGDP_FERC: Base GDP in FERC grid regions
    tibble(region = gcamusa.GRID_REGIONS,
           baseGDP = 1)  ->
      L223.BaseGDP_FERC

    # L223.LaborForceFillout_FERC: labor force in the grid regions
    tibble(region = gcamusa.GRID_REGIONS,
           year.fillout = min(MODEL_BASE_YEARS),
           laborforce = socioeconomics.DEFAULT_LABORFORCE) ->
      L223.LaborForceFillout_FERC


    # Some updates related to geothermal power
    # Remove geothermal option from states that do not have potential
    L223.SubsectorLogit_Investment_Fuel %<>% filter(!(paste(region, subsector0) %in% geo_states_noresource))
    L223.SubsectorShrwt_Investment_Fuel %<>% filter(!(paste(region, subsector0) %in% geo_states_noresource))
    L223.SubsectorInterpTo_Investment_Fuel %<>% filter(!(paste(region, subsector0) %in% geo_states_noresource))
    L223.SubsectorLogit_Investment %<>% filter(!(paste(region, subsector0) %in% geo_states_noresource))
    L223.SubsectorShrwt_Investment %<>% filter(!(paste(region, subsector0) %in% geo_states_noresource))
    L223.StubTech_Investment %<>% filter(!(paste(region, subsector0) %in% geo_states_noresource))
    L223.StubTechMarket_Investment %<>% filter(!(paste(region, subsector0) %in% geo_states_noresource))
    L223.StubTechCoef_Investment_cool %<>% filter(!(paste(region, subsector0) %in% geo_states_noresource))
    L223.StubTechShrwt_Investment_USA %<>% filter(!(paste(region, subsector0) %in% geo_states_noresource))
    L223.StubTechInterp_Investment_USA %<>% filter(!(paste(region, subsector0) %in% geo_states_noresource))
    L223.SubsectorLogit_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.SubsectorShrwtFllt_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.CapacityTech %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.CapacityTech_FutureTechs %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.CapacityTechSegmentCapFac %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.CapacityTechMinCapFac %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechShrwt_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechEff_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechCoef_Dispatch_cool %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechOMfixed_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechOMvar_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechLifetime_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechProfitShutdown_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechSCurve_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechCapFac_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.Production_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.PrimaryRenewKeyword_Dispatch_USA %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))

    # Geothermal resource cost should not impact variable cost of generation / dispatch order
    # Create a pmult table to zero out resource cost in the dispatch sector
    # Note that we can't delete the input entirely because the resource is "consumed" in the dispatch sector
    L223.TechEff_Dispatch %>%
      filter(subsector == gcamusa.GEOTHERMAL) %>%
      select(-efficiency, -market.name) %>%
      mutate(price.unit.conversion = 0) %>%
      rename(dispatch.sector = supplysector,
             capacity.technology = technology) -> L223.CapacityTechInputPMult_geo


    # Remove CSP option from states that do not have potential
    L223.SubsectorLogit_Investment %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", subsector))))
    L223.SubsectorShrwt_Investment %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", subsector))))
    L223.StubTech_Investment %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", subsector))))
    L223.StubTechMarket_Investment %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", subsector))))
    L223.StubTechCoef_Investment_cool %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", subsector))))
    L223.StubTechShrwt_Investment_USA %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", subsector))))
    L223.StubTechInterp_Investment_USA %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", subsector))))
    L223.CapacityTech %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", capacity.technology))))
    L223.CapacityTech_FutureTechs %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", capacity.technology))))
    L223.CapacityTechSegmentCapFac %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", capacity.technology))))
    L223.CapacityTechMinCapFac %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", capacity.technology))))
    L223.TechShrwt_Dispatch %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", technology))))
    L223.TechEff_Dispatch %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", technology))))
    L223.StubTechEffFlag_Dispatch %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", technology))))
    L223.TechCoef_Dispatch_cool %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", technology))))
    L223.TechOMfixed_Dispatch %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", technology))))
    L223.TechOMvar_Dispatch %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", technology))))
    L223.TechLifetime_Dispatch %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", technology))))
    L223.TechProfitShutdown_Dispatch %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", stub.technology))))
    L223.TechCapFac_Dispatch %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", technology))))
    L223.Production_Dispatch %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", technology))))
    L223.TechCapFac_Investment %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", technology))))
    L223.TechTrialMarket_Dispatch %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", capacity.technology))))
    L223.TechTrialMarket_Investment %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", subsector))))
    L223.PrimaryRenewKeyword_Dispatch_USA %<>% filter(!((region %in% csp_states_noresource) & (grepl("CSP", technology))))

    # Modifications for offshore wind
    # Remove states with no offshore wind resources
    offshore_wind_states <- L210.GrdRenewRsrcCurves_USA %>%
      filter(renewresource == "offshore wind resource") %>%
      pull(region) %>%
      unique()

    L223.SubsectorLogit_Investment %<>% filter(region %in% offshore_wind_states | subsector != "wind_offshore")
    L223.SubsectorShrwt_Investment %<>% filter(region %in% offshore_wind_states | subsector != "wind_offshore")
    L223.StubTech_Investment %<>% filter(region %in% offshore_wind_states | stub.technology != "wind_offshore")
    L223.StubTechMarket_Investment %<>% filter(region %in% offshore_wind_states | stub.technology != "wind_offshore")
    L223.StubTechCoef_Investment_cool %<>% filter(region %in% offshore_wind_states | stub.technology != "wind_offshore")
    L223.StubTechShrwt_Investment_USA %<>% filter(region %in% offshore_wind_states | stub.technology != "wind_offshore")
    L223.StubTechInterp_Investment_USA %<>% filter(region %in% offshore_wind_states | stub.technology != "wind_offshore")
    L223.CapacityTech_FutureTechs %<>% filter(region %in% offshore_wind_states | capacity.technology != "wind_offshore")
    L223.CapacityTechSegmentCapFac %<>% filter(region %in% offshore_wind_states | capacity.technology != "wind_offshore")
    L223.CapacityTechMinCapFac %<>% filter(region %in% offshore_wind_states | capacity.technology != "wind_offshore")
    L223.TechShrwt_Dispatch %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")
    L223.TechEff_Dispatch %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")
    L223.StubTechEffFlag_Dispatch %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")
    L223.TechCoef_Dispatch_cool %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")
    L223.TechOMfixed_Dispatch %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")
    L223.TechOMvar_Dispatch %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")
    L223.TechLifetime_Dispatch %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")
    L223.TechProfitShutdown_Dispatch %<>% filter(region %in% offshore_wind_states | stub.technology != "wind_offshore")
    L223.TechSCurve_Dispatch %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")
    L223.Production_Dispatch %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")
    L223.TechTrialMarket_Dispatch %<>% filter(region %in% offshore_wind_states | capacity.technology != "wind_offshore")
    L223.TechTrialMarket_Investment %<>% filter(region %in% offshore_wind_states | invest.technology != "wind_offshore")
    L223.PrimaryRenewKeyword_Dispatch_USA %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")

    # Wind & utility-scale (i.e. non-rooftop) solar are assumed to be infeasible in DC.
    # Thus, no wind & solar subsectors should be created in DC's electricity sector.
    # Use anti_join to remove them from the table.
    L223.SubsectorLogit_Investment_Fuel %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector0" = "resource_elec_subsector"))
    L223.SubsectorShrwt_Investment_Fuel %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector0" = "resource_elec_subsector"))
    L223.SubsectorInterpTo_Investment_Fuel %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector0" = "resource_elec_subsector"))
    L223.SubsectorLogit_Investment %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector0" = "resource_elec_subsector"))
    L223.SubsectorShrwt_Investment %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector0" = "resource_elec_subsector"))
    L223.StubTech_Investment %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector0" = "resource_elec_subsector"))
    L223.StubTechMarket_Investment %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector0" = "resource_elec_subsector"))
    L223.StubTechCoef_Investment_cool %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector0" = "resource_elec_subsector"))
    L223.StubTechShrwt_Investment_USA %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector0" = "resource_elec_subsector"))
    L223.StubTechInterp_Investment_USA %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector0" = "resource_elec_subsector"))
    L223.TechCapFac_Investment %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector0" = "resource_elec_subsector"))
    L223.SubsectorLogit_Dispatch %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector" = "resource_elec_subsector"))
    L223.SubsectorShrwtFllt_Dispatch %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector" = "resource_elec_subsector"))
    L223.CapacityTech_FutureTechs %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector" = "resource_elec_subsector"))
    L223.CapacityTechSegmentCapFac %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector" = "resource_elec_subsector"))
    L223.CapacityTechMinCapFac %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector" = "resource_elec_subsector"))
    L223.TechShrwt_Dispatch %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector" = "resource_elec_subsector"))
    L223.TechEff_Dispatch %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector" = "resource_elec_subsector"))
    L223.StubTechEffFlag_Dispatch %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector" = "resource_elec_subsector"))
    L223.TechCoef_Dispatch_cool %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector" = "resource_elec_subsector"))
    L223.TechOMfixed_Dispatch %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector" = "resource_elec_subsector"))
    L223.TechOMvar_Dispatch %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector" = "resource_elec_subsector"))
    L223.TechLifetime_Dispatch %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector" = "resource_elec_subsector"))
    L223.TechProfitShutdown_Dispatch %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector" = "resource_elec_subsector"))
    L223.TechSCurve_Dispatch %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector" = "resource_elec_subsector"))
    L223.Production_Dispatch %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector" = "resource_elec_subsector"))
    L223.TechTrialMarket_Dispatch %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector" = "resource_elec_subsector"))
    L223.TechTrialMarket_Investment %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector0" = "resource_elec_subsector"))
    L223.PrimaryRenewKeyword_Dispatch_USA %<>% anti_join(A10.renewable_resource_delete, by = c("region", "subsector" = "resource_elec_subsector"))

    L223.TechCapFac_Dispatch %>%
      left_join(left_join(L210.GrdRenewRsrcCurves_geo_USA %>%
                                 filter(renewresource != "geothermal") %>%
                                 group_by(region, renewresource) %>%
                                 summarize(CFmax = 1.0 - min(extractioncost)) %>%
                                 ungroup(), select(L223.TechEff_Dispatch, region, technology, minicam.energy.input) %>% distinct(), by=c("region", "renewresource" = "minicam.energy.input")),
                               ., by= c("region", "technology")) %>%
      filter(region != "DC") %>% # TODO: DC wind and PV
      mutate(capacity.factor= round(CFmax,energy.DIGITS_CAPACITY_FACTOR)) %>%
      select(region, supplysector, subsector, technology, year,
             capacity.factor) -> L223.TechCapFac_offshore_wind_Dispatch

    L223.TechCapFac_Dispatch %>%
      filter(!subsector %in% c("solar", "wind")) %>%
      bind_rows(L223.TechCapFac_offshore_wind_Dispatch) %>%
      # Wind & utility-scale (i.e. non-rooftop) solar are assumed to be infeasible in DC.
      # Thus, no wind & solar subsectors should be created in DC's electricity sector.
      # Use anti_join to remove them from the table.
      anti_join(A10.renewable_resource_delete,
                by = c("region", "subsector" = "resource_elec_subsector")) ->
      L223.TechCapFac_Dispatch

    # We need to make sure hydro continues to use the historical capacity factor into
    # the future.
    L123.out_EJ_state_elec_F_tech %>%
      filter(fuel == "hydro", year == MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L123.capacity_EJ_state_elec_F_tech, by= c("state", "fuel" = "gcam_fuel", "year")) %>%
      mutate(capacity.factor.hydro = value / capacity) %>%
      select(state, fuel, capacity.factor.hydro) ->
      L223.hydro_CapFac

    L223.TechCapFac_Dispatch %>%
      filter(subsector == "hydro") %>%
      # the base table will have hydro even in states that do not have production
      # so we will get NAs here
      left_join(L223.hydro_CapFac, by=c("region" = "state", "subsector" = "fuel")) %>%
      mutate(capacity.factor = if_else(is.na(capacity.factor.hydro), capacity.factor, capacity.factor.hydro)) %>%
      select(-capacity.factor.hydro) %>%
      bind_rows(filter(L223.TechCapFac_Dispatch, subsector != "hydro")) ->
      L223.TechCapFac_Dispatch

    # Add segment capacity factors for hydropower
    # First, hydropower segment capacity factors need to be scaled to be consistent with annual average
    L115.CapacityFactor_hydro_state_segment_gcamusa %>%
      left_join_error_no_match(L115.CapacityFactor_hydro_state_gcamusa %>%
                                 rename(EIA_CF_annual = capacity.factor),
                               by = c("state", "sector", "fuel"))  %>%
      left_join_error_no_match(L223.hydro_CapFac,
                               by = c("state", "fuel")) %>%
      # calculate scaler (ratio between annual capacity factor and one implied by segments)
      mutate(CF_scaler = capacity.factor.hydro / EIA_CF_annual,
             # apply scaler to segment capacity factor
             capacity.factor = capacity.factor * CF_scaler) %>%
      rename(region = state) %>%
      mutate(dispatch.sector = "electricity",
             subsector = fuel,
             capacity.technology = fuel) %>%
      # We only need to provide this info for model base years because
      # we don't allow new investment in hydropower
      repeat_add_columns(tibble::tibble(year = MODEL_BASE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[['CapacityTechSegmentCapFac']]) ->
      L223.CapacityTechSegmentCapFac_hydro

    L223.CapacityTechSegmentCapFac %>%
      bind_rows(L223.CapacityTechSegmentCapFac_hydro) ->
      L223.CapacityTechSegmentCapFac

    # Finally, add resource input (unlimited) for hydropower,
    # which is needed in order to utilize segment capacity factors
    L223.CapacityTech_FutureTechs %>%
      filter(subsector == "hydro") %>%
      select(-capacity) %>%
      # not all states have hydropower capacity, filter for those that do
      semi_join(L223.hydro_CapFac, by = c("region" = "state")) %>%
      mutate(minicam.energy.input = gcamusa.HYDRO_RESOURCE,
             efficiency = gcamusa.DEFAULT_COEFFICIENT,
             market.name = region,
             flag = "Resource") %>%
      rename(supplysector = dispatch.sector,
             technology = capacity.technology) -> L223.StubTechEffFlag_Dispatch_hydro

    L223.StubTechEffFlag_Dispatch %>%
      bind_rows(L223.StubTechEffFlag_Dispatch_hydro) -> L223.StubTechEffFlag_Dispatch


    # Grid connection costs for investment technologies
    bind_rows(
      mutate(L120.GridCost_offshore_wind_USA, minicam.energy.input = "offshore wind resource"),
      mutate(L2237.StubTechCost_wind_reeds_USA, minicam.energy.input = "onshore wind resource"),
      mutate(L2238.StubTechCost_PV_reeds_USA, minicam.energy.input = "PV_resource"),
      mutate(L2239.StubTechCost_CSP_reeds_USA, minicam.energy.input = "CSP_resource")
    )  ->
      L223.grid_cost

    L223.StubTechMarket_Investment %>%
      # select(LEVEL2_DATA_NAMES[["StubTechYr"]]) %>%
      select(region, supplysector, subsector0, subsector, stub.technology, year, minicam.energy.input) %>%
      left_join(L223.grid_cost, by = c("region" = "State", "minicam.energy.input")) %>%
      filter(!is.na(grid.cost)) %>%
      mutate(minicam.energy.input = "grid connection cost") %>%
      rename(minicam.non.energy.input = minicam.energy.input,
             input.cost = grid.cost) ->
      L223.StubTechCost_offshore_wind_Investment


    # -----------------------------------------------------------------------------
    # Produce outputs

    L223.Sector_Investment %>%
      add_title("Investment supplysector logit-exponent by state") %>%
      add_units("Unitless") %>%
      add_comments("Set supplysector logit-exponent for states") %>%
      add_legacy_name("L223.Sector_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/A23.dispatch_sector_state_share") ->
      L223.Sector_Investment

    L223.SubsectorLogit_Investment_Fuel %>%
      add_title("Investment subsector logit-exponent by state") %>%
      add_units("Unitless") %>%
      add_comments("Set subsector logit-exponent for states") %>%
      add_precursors("gcam-usa/A23.dispatch_subsector_logit",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.SubsectorLogit_Investment_Fuel

    L223.SubsectorShrwt_Investment_Fuel %>%
      add_title("Investment subsector share-weight by state") %>%
      add_units("Unitless") %>%
      add_comments("Set subsector share-weight for states") %>%
      add_precursors("gcam-usa/A23.dispatch_subsector_shrwt",
                     "gcam-usa/A23.dispatch_subsector_shrwt_state_adj",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.SubsectorShrwt_Investment_Fuel

    L223.SubsectorInterpTo_Investment_Fuel %>%
      add_title("Investment subsector s-curve interpolation-function by state") %>%
      add_units("Unitless") %>%
      add_comments("Set subsector s-curve interpolation-function for states") %>%
      add_precursors("gcam-usa/A23.dispatch_subsector_interp",
                     "gcam-usa/A23.dispatch_subsector_shrwt_interpto_state_adj",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.SubsectorInterpTo_Investment_Fuel

    L223.SubsectorLogit_Investment %>%
      add_title("Investment subsector logit-exponent by state") %>%
      add_units("Unitless") %>%
      add_comments("Set subsector logit-exponent for states") %>%
      add_legacy_name("L223.SubsectorLogit_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/A23.dispatch_subsector_logit",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.SubsectorLogit_Investment

    L223.SubsectorShrwt_Investment %>%
      add_title("Investment subsector share-weight by state") %>%
      add_units("Unitless") %>%
      add_comments("Set subsector share-weight for states") %>%
      add_legacy_name("L223.SubsectorShrwt_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/A23.dispatch_subsector_shrwt",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.SubsectorShrwt_Investment

    L223.StubTech_Investment %>%
      add_title("Investment stub.technology names by state") %>%
      add_units("Unitless") %>%
      add_comments("Set stub.technology names as a template for states") %>%
      add_legacy_name("L223.StubTech_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L210.GrdRenewRsrcCurves_USA",
                     "L120.GridCost_offshore_wind_USA",
                     "gcam-usa/A23.elec_tech_mapping_cool",
                     "gcam-usa/A23.elec_tech_mapping_cool_shares_fut",
                     "gcam-usa/usa_seawater_states_basins",
                     "gcam-usa/A23.dispatch_additional_mapping",
                     "water/A23.CoolingSystemCosts",
                     "L2233.GlobalTechCoef_elec_cool",
                     "L2233.GlobalIntTechCoef_elec_cool") ->
      L223.StubTech_Investment

    L233.GlobalInvestTech_Investment %>%
      add_title("List of all investment-technology to create") %>%
      add_units("NA") %>%
      add_comments("Mostly used to set the proper XML tags") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa") ->
      L233.GlobalInvestTech_Investment

    L223.GlobalTechEff_Investment %>%
      add_title("Investment technology efficiency") %>%
      add_units("Unitless") %>%
      add_comments("Set technology efficiency") %>%
      add_legacy_name("L223.GlobalTechEff_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "energy/A23.globaltech_eff",
                     "gcam-usa/A23.dispatch_globaltech_eff_additional") ->
      L223.GlobalTechEff_Investment

    L223.StubTechMarket_Investment %>%
      add_title("Investment technology market names for state") %>%
      add_units("Unitless") %>%
      add_comments("Set technology market for state") %>%
      add_legacy_name("L223.StubTechMarket_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L210.GrdRenewRsrcCurves_USA",
                     "L120.GridCost_offshore_wind_USA") ->
      L223.StubTechMarket_Investment

    L223.GlobalTechCoef_Investment_cool %>%
      add_title("Investment technology water withdrawal and consumption coefficients") %>%
      add_units("Unitless") %>%
      add_comments("Set technology water withdrawal and consumption coefficients") %>%
      same_precursors_as("L223.GlobalTechEff_Investment") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/A23.elec_tech_mapping_cool",
                     "gcam-usa/usa_seawater_states_basins",
                     "gcam-usa/A23.dispatch_additional_mapping",
                     "L2233.GlobalTechCoef_elec_cool",
                     "L2233.GlobalIntTechCoef_elec_cool") ->
      L223.GlobalTechCoef_Investment_cool

    L223.StubTechCoef_Investment_cool %>%
      add_title("Investment stub technology water withdrawal and consumption coefficients") %>%
      add_units("Unitless") %>%
      add_comments("Set technology water withdrawal and consumption coefficients at state-level") %>%
      same_precursors_as("L223.GlobalTechCoef_Investment_cool") ->
      L223.StubTechCoef_Investment_cool

    L223.GlobalTechOMfixed_Investment %>%
      add_title("Investment technology OM-fix") %>%
      add_units("1975$US/kW/year") %>%
      add_comments("Set technology OM-fix") %>%
      add_legacy_name("L223.GlobalTechOMfixed_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "L113.globaltech_OMfixed_ATB",
                     "gcam-usa/A23.dispatch_globaltech_OMfixed_additional") ->
      L223.GlobalTechOMfixed_Investment

    L223.GlobalTechOMvar_Investment %>%
      add_title("Investment technology OM-var") %>%
      add_units("1975$US/MWh") %>%
      add_comments("Set technology OM-var") %>%
      add_legacy_name("L223.GlobalTechOMvar_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "L113.globaltech_OMvar_ATB",
                     "gcam-usa/A23.dispatch_globaltech_OMvar_additional") ->
      L223.GlobalTechOMvar_Investment

    L223.GlobalTechCapital_Investment %>%
      add_title("Investment technology capital-overnight and fixed-charge-rate") %>%
      add_units("1975$US/kw") %>%
      add_comments("Set technology capital-overnight and fixed-charge-rate") %>%
      add_legacy_name("L223.L223.GlobalTechCapital_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "L113.globaltech_capital_ATB",
                     "gcam-usa/A23.dispatch_globaltech_capital_additional") ->
      L223.GlobalTechCapital_Investment

    L223.GlobalTechCapital_Investment_cool %>%
      add_title("Investment technology cooling system capital-overnight and fixed-charge-rate") %>%
      add_units("1975$US/kw") %>%
      add_comments("Set cooling system capital-overnight and fixed-charge-rate") %>%
      add_legacy_name("L223.L223.GlobalTechCapital_Investment (dispatch branch)") %>%
      same_precursors_as("L223.GlobalTechCapital_Investment") %>%
      add_precursors("water/A23.CoolingSystemCosts",
                     "gcam-usa/A23.elec_tech_mapping_cool") ->
      L223.GlobalTechCapital_Investment_cool

    L223.GlobalTechShrwt_Investment %>%
      add_title("Investment technology share-weight") %>%
      add_units("Unitless") %>%
      add_comments("Set technology share-weight") %>%
      add_legacy_name("L223.GlobalTechShrwt_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "energy/A23.globaltech_shrwt",
                     "gcam-usa/A23.dispatch_globaltech_shrwt_additional") ->
      L223.GlobalTechShrwt_Investment

    L223.GlobalIntTechEff_Investment %>%
      add_title("Add flag for intermittent investment technologies") %>%
      add_units("NA") %>%
      add_comments("Adds a resource flag so the intermittent tech can use it to look up the market CF") %>%
      same_precursors_as("L223.GlobalTechEff_Investment") ->
      L223.GlobalIntTechEff_Investment

    L223.GlobalIntInvTechMaxCapFac_Investment %>%
      add_title("Sets the max CF for intermittent investment technologies") %>%
      add_units("NA") %>%
      add_comments("Because the resource CF will be dynamic we need to ensure it never exceeds") %>%
      add_comments("the number of hours in the investment segment.") %>%
      same_precursors_as("L223.GlobalTechEff_Investment") ->
      L223.GlobalIntInvTechMaxCapFac_Investment

    L223.StubTechInterp_Investment_USA %>%
      add_title("Investment technology share-weight interpolation rules by state") %>%
      add_units("Unitless") %>%
      add_comments("Set technology share-weight interpolation rules by state") %>%
      # TODO:  set precursors
      same_precursors_as("L223.GlobalTechShrwt_Investment") ->
      L223.StubTechInterp_Investment_USA

    L223.StubTechShrwt_Investment_USA %>%
      add_title("Investment technology share-weights by state") %>%
      add_units("Unitless") %>%
      add_comments("Set technology share-weights by state") %>%
      # TODO:  set precursors
      same_precursors_as("L223.GlobalTechShrwt_Investment") ->
      L223.StubTechShrwt_Investment_USA

    L223.GlobalTechCapFac_Investment %>%
      add_title("Investment technology capacity factor") %>%
      add_units("Unitless") %>%
      add_comments("Set technology capacity factor used for investment decision-making") %>%
      add_legacy_name("L223.GlobalTechCapFac_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "L102.invest_segments_gcamusa") ->
      L223.GlobalTechCapFac_Investment

    L223.TechCapFac_Investment %>%
      add_title("Investment technology capacity factor for wind PV CSP for state") %>%
      add_units("Unitless") %>%
      add_comments("Set technology capacity factor for wind PV CSP for state") %>%
      add_legacy_name("L223.TechCapFac_Investment (dispatch branch)") %>%
      add_precursors("L119.CapacityFactor_PV_state_gcamusa",
                     "L119.CapacityFactor_CSP_state_gcamusa",
                     "gcam-usa/calibrated_techs_dispatch_usa",
                     "L102.invest_segments_gcamusa") ->
      L223.TechCapFac_Investment

    L223.GlobalTechCapture_Investment %>%
      add_title("Investment technology storage-market and remove-fraction for CCS") %>%
      add_units("Unitless") %>%
      add_comments("Set technology storage-market and remove-fraction for CCS") %>%
      add_legacy_name("L223.GlobalTechCapture_Investment (dispatch branch)") %>%
      add_precursors("energy/A23.globaltech_co2capture",
                     "gcam-usa/calibrated_techs_dispatch_usa") ->
      L223.GlobalTechCapture_Investment

    L223.GlobalTechCost_Investment %>%
      add_title("Investment technology capacity credit") %>%
      add_units("1975 $/GJ") %>%
      add_comments("Set technology capacity credit") %>%
      add_legacy_name("L223.GlobalTechCost_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/dispatch/capacity_credit_calculator") ->
      L223.GlobalTechCost_Investment

    L223.GlobalTechCost_CapacityCreditCalulator %>%
      add_title("Investment technology capacity credit calculator for dispatchable technologies") %>%
      add_units("NA") %>%
      add_comments("wind and solar only") %>%
      add_legacy_name("L223.GlobalTechCost_CapacityCreditCalulator (dispatch branch)") %>%
      same_precursors_as("L223.GlobalTechCost_Investment") ->
      L223.GlobalTechCost_CapacityCreditCalulator

    L223.Sector_Investment_StateShare %>%
      add_title("Investment supplysector logit-exponent for grid") %>%
      add_units("Unitless") %>%
      add_comments("Set supplysector logit-exponent for grid") %>%
      add_legacy_name("L223.Sector_Investment_StateShare (dispatch branch)") %>%
      add_precursors("gcam-usa/A23.dispatch_sector_state_share") ->
      L223.Sector_Investment_StateShare

    L223.Subsector_Investment_StateShare %>%
      add_title("Investment subsector (state) logit-exponent for grid") %>%
      add_units("Unitless") %>%
      add_comments("Set subsector (state) logit-exponent for grid") %>%
      add_legacy_name("L223.Subsector_Investment_StateShare (dispatch branch)") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A23.dispatch_sector") ->
      L223.Subsector_Investment_StateShare

    L223.SubsectorShrwtFllt_Investment_StateShare %>%
      add_title("Investment subsector (state) share-weight for grid") %>%
      add_units("Unitless") %>%
      add_comments("Set subsector (state) share-weight for grid") %>%
      add_legacy_name("L223.SubsectorShrwtFllt_Investment_StateShare (dispatch branch)") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A23.dispatch_sector",
                     "L123.capacity_EJ_state_elec_F_tech") ->
      L223.SubsectorShrwtFllt_Investment_StateShare

    L223.TechCoef_Investment_StateShare %>%
      add_title("Investment subsector (state) and technology (new) coefficients and market for grid") %>%
      add_units("Unitless") %>%
      add_comments("Converts from capacity to energy as appropriate for the segment") %>%
      add_legacy_name("L223.TechCoef_Investment_StateShare (dispatch branch)") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A23.dispatch_sector",
                     "L102.invest_segments_gcamusa") ->
      L223.TechCoef_Investment_StateShare

    L223.TechPmult_Investment_StateShare %>%
      add_title("Investment subsector (state) and technology (new) pMult and market for grid") %>%
      add_units("Unitless") %>%
      add_comments("To counter act the capacity to energy conversion as we didn't want to adjust prices") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A23.dispatch_sector",
                     "L102.invest_segments_gcamusa") ->
      L223.TechPmult_Investment_StateShare

    L223.TechShrwt_Investment_StateShare %>%
      add_title("Investment subsector (state) and technology (new) share-weight for grid") %>%
      add_units("Unitless") %>%
      add_comments("Set subsector (state) and technology (new) share-weight for grid") %>%
      add_legacy_name("L223.TechShrwt_Investment_StateShare (dispatch branch)") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A23.dispatch_sector") ->
      L223.TechShrwt_Investment_StateShare

    L223.StubTechCost_offshore_wind_Investment %>%
      add_title("State-specific non-energy cost adder for offshore wind grid connection cost") %>%
      add_units("1975$ / GJ") %>%
      add_comments("Grid connection cost adder for offshore wind") %>%
      same_precursors_as("L223.StubTechMarket_Investment") %>%
      add_precursors("L120.GridCost_offshore_wind_USA",
                     "L2237.StubTechCost_wind_reeds_USA",
                     "L2238.StubTechCost_PV_reeds_USA",
                     "L2239.StubTechCost_CSP_reeds_USA")->
      L223.StubTechCost_offshore_wind_Investment

    # ------------------------------------------------------------------------------------------------------------
    # dispatch
    L223.DispatchSector %>%
      add_title("Dispatch dispatchsector generation_sector generation_sector_market for state") %>%
      add_units("Unitless") %>%
      add_comments("Set dispatchsector generation_sector generation_sector_market names for state") %>%
      add_legacy_name("L223.DispatchSector (dispatch branch)") %>%
      add_precursors("gcam-usa/A23.dispatch_sector") ->
      L223.DispatchSector

    L223.Sector_Dispatch %>%
      add_title("Dispatch supplysector logit-exponent for state") %>%
      add_units("Unitless") %>%
      add_comments("Set supplysector logit-exponent for state") %>%
      add_legacy_name("L223.Sector_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/A23.dispatch_sector") ->
      L223.Sector_Dispatch

    L223.SubsectorLogit_Dispatch %>%
      add_title("Dispatch subsector logit-exponent for state") %>%
      add_units("Unitless") %>%
      add_comments("Set subsector logit-exponent for state") %>%
      add_legacy_name("L223.SubsectorLogit_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/A10.renewable_resource_delete",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.SubsectorLogit_Dispatch

    L223.SubsectorShrwtFllt_Dispatch %>%
      add_title("Dispatch subsector shareweight for state") %>%
      add_units("Unitless") %>%
      add_comments("Set subsector shareweight for state") %>%
      add_legacy_name("L223.SubsectorShrwtFllt_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/A10.renewable_resource_delete",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.SubsectorShrwtFllt_Dispatch

    L223.CapacityTech_FutureTechs %>%
      add_title("Dispatch technology capacity for state") %>%
      add_units("EJ") %>%
      add_comments("Set technology capacity for state") %>%
      add_comments("In this dataset all capacity is set to 0 (as a template") %>%
      add_legacy_name("L223.CapacityTech_FutureTechs (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L210.GrdRenewRsrcCurves_USA",
                     "L120.GridCost_offshore_wind_USA") ->
      L223.CapacityTech_FutureTechs

    L223.CapacityTechSegmentCapFac %>%
      add_title("Dispatch technology capacity factor by segment for state") %>%
      add_units("Unitless") %>%
      add_comments("Set technology capacity factor by segment for state") %>%
      add_legacy_name("L223.CapacityTechSegmentCapFac (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/A10.renewable_resource_delete",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L114.CapacityFactor_wind_state_segment_gcamusa",
                     "L114.CapacityFactor_wind_offshore_state_segment_gcamusa",
                     "L115.CapacityFactor_hydro_state_gcamusa",
                     "L115.CapacityFactor_hydro_state_segment_gcamusa",
                     "L119.CapacityFactor_CSP_state_segment_gcamusa",
                     "L119.CapacityFactor_PV_state_segment_gcamusa",
                     "L210.GrdRenewRsrcCurves_USA",
                     "L120.GridCost_offshore_wind_USA") ->
      L223.CapacityTechSegmentCapFac

    L223.CapacityTechMinCapFac %>%
      add_title("Capacity technology minimum capacity factor") %>%
      add_units("Unitless") %>%
      add_comments("Technologies will not be allowed to dispatch when it's capacity factor") %>%
      add_comments("would fall below this minimum value.") %>%
      add_precursors("gcam-usa/A23.dispatch_capacitytech_min_cap_fac") ->
      L223.CapacityTechMinCapFac

    L223.CapacityTech %>%
      add_title("Dispatch technology capacity for state (Calculated)") %>%
      add_units("EJ") %>%
      add_comments("Set technology capacity for state") %>%
      add_legacy_name("L223.CapacityTech (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "L123.capacity_EJ_state_elec_F_tech",
                     "gcam-usa/A10.renewable_resource_delete",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.CapacityTech

    L223.TechShrwt_Dispatch %>%
      add_title("Dispatch technology shareweight for state") %>%
      add_units("Unitless") %>%
      add_comments("Set technology shareweight for state all as 1") %>%
      add_legacy_name("L223.TechShrwt_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/A10.renewable_resource_delete",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L210.GrdRenewRsrcCurves_USA",
                     "L120.GridCost_offshore_wind_USA") ->
      L223.TechShrwt_Dispatch

    L223.TechEff_Dispatch %>%
      add_title("Dispatch technology efficiency for state") %>%
      add_units("Unitless") %>%
      add_comments("Set technology efficiency for state") %>%
      add_legacy_name("L223.TechEff_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/A10.renewable_resource_delete",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "energy/A23.globaltech_eff",
                     "gcam-usa/A23.dispatch_globaltech_eff_additional",
                     "L210.GrdRenewRsrcCurves_USA",
                     "L120.GridCost_offshore_wind_USA") ->
      L223.TechEff_Dispatch

    L223.CapacityTechInputPMult_geo %>%
      add_title("Dispatch technology price multiplier for geothermal resource") %>%
      add_units("multiplier") %>%
      add_comments("Set zero price multiplier for geothermal resource for dispatch techs") %>%
      same_precursors_as(L223.TechEff_Dispatch) ->
      L223.CapacityTechInputPMult_geo

    L223.StubTechEffFlag_Dispatch %>%
      add_title("Add flag for intermittent capacity technologies") %>%
      add_units("NA") %>%
      add_comments("Adds the resource flag so it can be used to set the renewable CF from the market") %>%
      same_precursors_as(L223.TechEff_Dispatch) ->
      L223.StubTechEffFlag_Dispatch

    L223.TechCoef_Dispatch_cool %>%
      add_title("Dispatch technology water withdrawal and consumption coefficients by state") %>%
      add_units("Unitless") %>%
      add_comments("Set technology water withdrawal and consumption coefficients by state") %>%
      same_precursors_as("L223.TechEff_Dispatch") %>%
      same_precursors_as("L223.StubTechCoef_Investment_cool") ->
      L223.TechCoef_Dispatch_cool

    L223.TechOMfixed_Dispatch %>%
      add_title("Dispatch technology OM fixed for state") %>%
      add_units("1975$US/MWh") %>%
      add_comments("Set technology OM fixed for state") %>%
      add_legacy_name("L223.TechOMfixed_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/A10.renewable_resource_delete",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L113.globaltech_OMfixed_ATB",
                     "gcam-usa/A23.dispatch_globaltech_OMvar_additional",
                     "L210.GrdRenewRsrcCurves_USA",
                     "L120.GridCost_offshore_wind_USA") ->
      L223.TechOMfixed_Dispatch

    L223.TechOMvar_Dispatch %>%
      add_title("Dispatch technology OM var for state") %>%
      add_units("1975$US/MWh") %>%
      add_comments("Set technology OM var for state") %>%
      add_legacy_name("L223.TechOMvar_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/A10.renewable_resource_delete",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L113.globaltech_OMvar_ATB",
                     "gcam-usa/A23.dispatch_globaltech_OMvar_additional",
                     "L210.GrdRenewRsrcCurves_USA",
                     "L120.GridCost_offshore_wind_USA") ->
      L223.TechOMvar_Dispatch

    L223.TechLifetime_Dispatch %>%
      add_title("Dispatch technology lifetime for state") %>%
      add_units("yrs") %>%
      add_comments("Set technology lifetime for state") %>%
      add_legacy_name("L223.TechLifetime_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/A10.renewable_resource_delete",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "energy/A23.globaltech_retirement",
                     "gcam-usa/A23.dispatch_globaltech_retirement_additional",
                     "L210.GrdRenewRsrcCurves_USA",
                     "L120.GridCost_offshore_wind_USA") ->
      L223.TechLifetime_Dispatch

    L223.TechProfitShutdown_Dispatch %>%
      add_title("Dispatch technology capacity investment discount params") %>%
      add_units("NA") %>%
      add_comments("Profit shutdown param that are used to discount existing") %>%
      add_comments("capacity in investment decisions.") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/A10.renewable_resource_delete") ->
      L223.TechProfitShutdown_Dispatch

    L223.TechSCurve_Dispatch %>%
      add_title("Dispatch technology lifetime steepness and half.life for state") %>%
      add_units("unitless") %>%
      add_comments("Set technology lifetime steepness and half.life for state") %>%
      add_legacy_name("L223.TechSCurve_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/A10.renewable_resource_delete",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "energy/A23.globaltech_retirement",
                     "gcam-usa/A23.dispatch_globaltech_retirement_additional",
                     "L210.GrdRenewRsrcCurves_USA",
                     "L120.GridCost_offshore_wind_USA") ->
      L223.TechSCurve_Dispatch

    L223.TechCapFac_Dispatch %>%
      add_title("Dispatch technology capacity factor for state") %>%
      add_units("unitless") %>%
      add_comments("Set technology capacity factor for state") %>%
      add_legacy_name("L223.TechCapFac_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/A10.renewable_resource_delete",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L210.GrdRenewRsrcCurves_USA") ->
      L223.TechCapFac_Dispatch

    L223.TechCarbonCapture_Dispatch %>%
      add_title("Dispatch CCS storage market and remove-fraction for state") %>%
      add_units("unitless") %>%
      add_comments("Set CCS storage market and remove-fraction for state") %>%
      add_legacy_name("L223.TechCarbonCapture_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "energy/A23.globaltech_co2capture") ->
      L223.TechCarbonCapture_Dispatch

    L223.Production_Dispatch %>%
      add_title("Dispatch technology calibrated output by state") %>%
      add_units("EJ") %>%
      add_comments("Set technology calibrated output by state") %>%
      same_precursors_as("L223.TechShrwt_Dispatch") %>%
      add_precursors("L123.out_EJ_state_elec_F_tech") ->
      L223.Production_Dispatch

    L223.TechEff_Cal %>%
      add_title("Dispatch technology calibrated efficiency for state") %>%
      add_units("unitless") %>%
      add_comments("Set technology calibrated efficiency for state") %>%
      add_legacy_name("L223.TechEff_Cal (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "L123.in_EJ_state_elec_F_tech",
                     "L123.out_EJ_state_elec_F_tech") ->
      L223.TechEff_Cal

    L223.PrimaryRenewKeyword_Dispatch_USA %>%
      add_title("Dispatch technology primary energy renewable keyword") %>%
      add_units("NA") %>%
      add_comments("Set primary energy renewable keyword for GCAM-USA electricity capacity technologies (dispatch sector)") %>%
      add_precursors("energy/A23.globaltech_keyword",
                     "gcam-usa/A23.globaltech_keyword_additional") ->
      L223.PrimaryRenewKeyword_Dispatch_USA

    L223.AvgFossilEffKeyword_Dispatch_USA %>%
      add_title("Dispatch technology primary energy renewable keyword") %>%
      add_units("NA") %>%
      add_comments("Set primary energy renewable keyword for GCAM-USA electricity capacity technologies (dispatch sector)") %>%
      add_precursors("energy/A23.globaltech_keyword",
                     "gcam-usa/A23.globaltech_keyword_additional") ->
      L223.AvgFossilEffKeyword_Dispatch_USA

    L223.TechTrialMarket_Dispatch %>%
      add_title("Dispatch technology set trial market for renewables") %>%
      add_units("unitless") %>%
      add_comments("Set set trial market for renewables") %>%
      add_legacy_name("L223.TechTrialMarket_Dispatch (dispatch branch)") %>%
      same_precursors_as("L223.TechShrwt_Dispatch") %>%
      add_precursors("gcam-usa/dispatch/capacity_credit_calculator",
                     "gcam-usa/dispatch/TechTrialMarket_mapping") ->
      L223.TechTrialMarket_Dispatch

    L223.TechTrialMarket_Investment %>%
      add_title("Investment technology set trial market for renewables") %>%
      add_units("unitless") %>%
      add_comments("Set set trial market for renewables") %>%
      add_legacy_name("L223.TechTrialMarket_Investment (dispatch branch)") %>%
      same_precursors_as("L223.StubTechMarket_Investment") %>%
      add_precursors("gcam-usa/dispatch/capacity_credit_calculator",
                     "gcam-usa/dispatch/TechTrialMarket_mapping") ->
      L223.TechTrialMarket_Investment

    L223.Sector_Dispatch_Grid %>%
      add_title("Dispatch supplysector (electricity) logit.exponent for grid") %>%
      add_units("unitless") %>%
      add_comments("Set supplysector (electricity) logit.exponent for grid") %>%
      add_legacy_name("L223.Sector_Dispatch_Grid (dispatch branch)") %>%
      same_precursors_as("L223.Sector_Dispatch_Grid") ->
      L223.Sector_Dispatch_Grid

    L223.DispatchSectorDispatchSegments %>%
      add_title("Dispatch dispatch.sector (electricity) relative generation and fraction for grid") %>%
      add_units("unitless") %>%
      add_comments("Set dispatch.sector (electricity) relative generation and fraction for grid") %>%
      add_legacy_name("L223.DispatchSectorDemandSegments (dispatch branch)") %>%
      add_precursors("L102.load_segments_gcamusa", "L102.invest_segments_gcamusa") ->
      L223.DispatchSectorDispatchSegments

    L223.InterestRate_FERC %>%
      add_title("Interest rates in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Use the default interest rate") %>%
      add_legacy_name("L223.InterestRate_FERC") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L223.InterestRate_FERC

    L223.Pop_FERC %>%
      add_title("Population in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("The same value is copied to all model years") %>%
      add_legacy_name("L223.Pop_FERC") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L223.Pop_FERC

    L223.BaseGDP_FERC %>%
      add_title("Base GDP in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("") %>%
      add_legacy_name("L223.BaseGDP_FERC") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L223.BaseGDP_FERC

    L223.LaborForceFillout_FERC %>%
      add_title("Labor force in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Use the default labor force") %>%
      add_legacy_name("L223.LaborForceFillout_FERC") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L120.GridCost_offshore_wind_USA",
                     "L223.PrimaryRenewKeyword_elec",
                     "L2233.GlobalTechCoef_elec_cool",
                     "L2233.GlobalIntTechCoef_elec_cool",
                     "L210.GrdRenewRsrcCurves_USA",
                     "L120.GridCost_offshore_wind_USA",
                     "L2237.StubTechCost_wind_reeds_USA",
                     "L2238.StubTechCost_PV_reeds_USA",
                     "L2239.StubTechCost_CSP_reeds_USA") ->
      L223.LaborForceFillout_FERC

    return_data(L223.Sector_Investment,
                L223.SubsectorLogit_Investment_Fuel,
                L223.SubsectorShrwt_Investment_Fuel,
                L223.SubsectorInterpTo_Investment_Fuel,
                L223.SubsectorLogit_Investment,
                L223.SubsectorShrwt_Investment,
                L223.StubTech_Investment,
                L233.GlobalInvestTech_Investment,
                L223.GlobalTechEff_Investment,
                L223.StubTechMarket_Investment,
                L223.GlobalTechCoef_Investment_cool,
                L223.StubTechCoef_Investment_cool,
                L223.GlobalTechOMfixed_Investment,
                L223.GlobalTechOMvar_Investment,
                L223.GlobalTechCapital_Investment,
                L223.GlobalTechCapital_Investment_cool,
                L223.GlobalIntTechEff_Investment,
                L223.GlobalIntInvTechMaxCapFac_Investment,
                L223.GlobalTechShrwt_Investment,
                L223.StubTechInterp_Investment_USA,
                L223.StubTechShrwt_Investment_USA,
                L223.GlobalTechCapFac_Investment,
                L223.TechCapFac_Investment,
                L223.GlobalTechCapture_Investment,
                L223.GlobalTechCost_Investment,
                L223.GlobalTechCost_CapacityCreditCalulator,
                L223.Sector_Investment_StateShare,
                L223.Subsector_Investment_StateShare,
                L223.SubsectorShrwtFllt_Investment_StateShare,
                L223.TechCoef_Investment_StateShare,
                L223.TechPmult_Investment_StateShare,
                L223.TechShrwt_Investment_StateShare,
                L223.DispatchSector,
                L223.Sector_Dispatch,
                L223.SubsectorLogit_Dispatch,
                L223.SubsectorShrwtFllt_Dispatch,
                L223.CapacityTech_FutureTechs,
                L223.CapacityTechSegmentCapFac,
                L223.CapacityTechMinCapFac,
                L223.CapacityTech,
                L223.TechShrwt_Dispatch,
                L223.TechEff_Dispatch,
                L223.CapacityTechInputPMult_geo,
                L223.StubTechEffFlag_Dispatch,
                L223.TechCoef_Dispatch_cool,
                L223.TechOMfixed_Dispatch,
                L223.TechOMvar_Dispatch,
                L223.TechLifetime_Dispatch,
                L223.TechProfitShutdown_Dispatch,
                L223.TechSCurve_Dispatch,
                L223.TechCapFac_Dispatch,
                L223.TechCarbonCapture_Dispatch,
                L223.Production_Dispatch,
                L223.TechEff_Cal,
                L223.PrimaryRenewKeyword_Dispatch_USA,
                L223.AvgFossilEffKeyword_Dispatch_USA,
                L223.TechTrialMarket_Dispatch,
                L223.TechTrialMarket_Investment,
                L223.Sector_Dispatch_Grid,
                L223.DispatchSectorDispatchSegments,
                L223.InterestRate_FERC,
                L223.Pop_FERC,
                L223.BaseGDP_FERC,
                L223.LaborForceFillout_FERC,
                L223.StubTechCost_offshore_wind_Investment)
  } else {
    stop("Unknown command")
  }
}
