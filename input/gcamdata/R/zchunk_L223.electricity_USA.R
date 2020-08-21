# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L223.electricity_USA
#'
#' Generates GCAM-USA model inputs for electrcity sector by grid regions and states.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L223.Sector_Investment},
#' \code{L223.SubsectorLogit_Investment}, \code{L223.SubsectorInterp_Investment}, \code{L223.SubsectorInterpTo_Investment},
#' \code{L223.StubTech_Investment}, \code{L223.GlobalTechEff_Investment}, \code{L223.StubTechMarket_Investment},
#' \code{L223.GlobalTechOMfixed_Investment},\code{L223.GlobalTechOMvar_Investment}, \code{L223.GlobalTechCapital_Investment},
#' \code{L223.GlobalTechShrwt_Investment}, \code{L223.GlobalTechCapFac_Investment}, \code{L223.TechCapFac_Investment},
#' \code{L223.GlobalTechCapture_Investment}, \code{L223.GlobalTechCost_Investment}, \code{L223.GlobalTechCost_CapacityCreditCalulator},
#' \code{L223.Sector_Investment_StateShare}, \code{L223.Subsector_Investment_StateShare}, \code{L223.CapacityTech},
#' \code{L223.SubsectorShrwtFllt_Investment_StateShare}, \code{L223.TechCoef_Investment_StateShare},
#' \code{L223.TechShrwt_Investment_StateShare}, \code{L223.TechShrwt_Dispatch},
#' \code{L223.TechEff_Dispatch},
#' \code{L223.DispatchSector}, \code{L223.Sector_Dispatch},
#' \code{L223.SubsectorLogit_Dispatch}, \code{L223.SubsectorShrwtFllt_Dispatch}, \code{L223.CapacityTech_FutureTechs},
#' \code{L223.TechOMvar_Dispatch}, \code{L223.TechLifetime_Dispatch}, \code{L223.TechSCurve_Dispatch},
#' \code{L223.TechCapFac_Dispatch}, \code{L223.TechCarbonCapture_Dispatch}, \code{L223.Production_Dispatch},
#' \code{L223.TechEff_Cal}, \code{L223.TechTrialMarket_Dispatch},\code{L223.TechTrialMarket_Investment}, \code{L223.Sector_Dispatch_Grid}, \code{L223.DispatchSectorCalProd},
#' \code{L223.DispatchSectorDispatchSegments}, \code{L223.InterestRate_FERC}, \code{L223.Pop_FERC}, \code{L223.BaseGDP_FERC},
#' \code{L223.LaborForceFillout_FERC}, \code{L223.TechCost_offshore_wind_Dispatch}.
#' The corresponding file in the
#' original data system was \code{L223.electricity_USA.R} (gcam-usa level2 - dispatch branch).
#' @details This chunk generates input files to create an annualized electricity generation sector for each state
#' and creates the demand for the state-level electricity sectors in the grid regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select mutate_at
#' @importFrom tidyr gather spread nest unnest expand
#' @author YO Feb 2020
module_gcamusa_L223.electricity_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/NREL_us_re_technical_potential",
             FILE = "energy/A23.globaltech_eff",
             FILE = "energy/A23.globaltech_OMfixed",
             FILE = "energy/A23.globaltech_OMvar",
             FILE = "energy/A23.globaltech_capital",
             FILE = "energy/A23.globaltech_retirement",
             FILE = "energy/A23.globaltech_shrwt",
             FILE = "energy/A23.globaltech_co2capture",
             "L114.CapacityFactor_wind_state_gcamusa",
             "L119.CapacityFactor_PV_state_gcamusa",
             "L119.CapacityFactor_CSP_state_gcamusa",
             "L114.CapacityFactor_wind_state_segment_gcamusa",
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
             FILE = "gcam-usa/A23.dispatch_subsector_logit",
             FILE = "gcam-usa/A23.dispatch_globaltech_eff_additional",
             FILE = "gcam-usa/A23.dispatch_globaltech_OMfixed_additional",
             FILE = "gcam-usa/A23.dispatch_globaltech_OMvar_additional",
             FILE = "gcam-usa/A23.dispatch_globaltech_capital_additional",
             FILE = "gcam-usa/A23.dispatch_globaltech_retirement_additional",
             FILE = "gcam-usa/A23.dispatch_globaltech_shrwt_additional",
             FILE = "gcam-usa/A23.dispatch_capacitytech_min_cap_fac",
             FILE = "gcam-usa/calibrated_techs_dispatch_usa",
             FILE = "gcam-usa/dispatch/capacity_credit_calculator",
             FILE = "gcam-usa/dispatch/TechTrialMarket_mapping",
             "L120.RsrcCurves_EJ_R_offshore_wind_USA",
             "L120.RegCapFactor_offshore_wind_USA",
             "L120.GridCost_offshore_wind_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L223.Sector_Investment",
             "L223.SubsectorLogit_Investment",
             "L223.SubsectorShrwt_Investment",
             "L223.SubsectorInterp_Investment",
             "L223.SubsectorInterpTo_Investment",
             "L223.StubTech_Investment",
             "L223.GlobalTechEff_Investment",
             "L223.StubTechMarket_Investment",
             "L223.GlobalTechOMfixed_Investment",
             "L223.GlobalTechOMvar_Investment",
             "L223.GlobalTechCapital_Investment",
             "L223.GlobalTechShrwt_Investment",
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
             "L223.TechOMfixed_Dispatch",
             "L223.TechOMvar_Dispatch",
             "L223.TechLifetime_Dispatch",
             "L223.TechSCurve_Dispatch",
             "L223.TechCapFac_Dispatch",
             "L223.TechCarbonCapture_Dispatch",
             "L223.Production_Dispatch",
             "L223.TechEff_Cal",
             "L223.TechTrialMarket_Dispatch",
             "L223.TechTrialMarket_Investment",
             "L223.Sector_Dispatch_Grid",
             "L223.DispatchSectorCalProd",
             "L223.DispatchSectorDispatchSegments",
             "L223.InterestRate_FERC",
             "L223.Pop_FERC",
             "L223.BaseGDP_FERC",
             "L223.TechCost_offshore_wind_Dispatch",
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
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    NREL_us_re_technical_potential <- get_data(all_data, "gcam-usa/NREL_us_re_technical_potential")

    A23.globaltech_eff <- get_data(all_data, "energy/A23.globaltech_eff")
    A23.globaltech_OMfixed <- get_data(all_data, "energy/A23.globaltech_OMfixed")
    A23.globaltech_OMvar <- get_data(all_data, "energy/A23.globaltech_OMvar")
    A23.globaltech_capital <- get_data(all_data, "energy/A23.globaltech_capital")
    A23.globaltech_retirement <- get_data(all_data, "energy/A23.globaltech_retirement")
    A23.globaltech_shrwt <- get_data(all_data, "energy/A23.globaltech_shrwt")
    A23.globaltech_co2capture <- get_data(all_data, "energy/A23.globaltech_co2capture")

    L114.CapacityFactor_wind_state <- get_data(all_data, "L114.CapacityFactor_wind_state_gcamusa")
    L114.CapacityFactor_wind_state_segment <- get_data(all_data, "L114.CapacityFactor_wind_state_segment_gcamusa")
    L119.CapacityFactor_PV_state <- get_data(all_data, "L119.CapacityFactor_PV_state_gcamusa")
    L119.CapacityFactor_PV_state_segment <- get_data(all_data, "L119.CapacityFactor_PV_state_segment_gcamusa")
    L119.CapacityFactor_CSP_state <- get_data(all_data, "L119.CapacityFactor_CSP_state_gcamusa")
    L119.CapacityFactor_CSP_state_segment <- get_data(all_data, "L119.CapacityFactor_CSP_state_segment_gcamusa")

    L123.in_EJ_state_elec_F_tech <- get_data(all_data, "L123.in_EJ_state_elec_F_tech")
    L123.out_EJ_state_elec_F_tech <- get_data(all_data, "L123.out_EJ_state_elec_F_tech")
    L123.capacity_EJ_state_elec_F_tech <- get_data(all_data, "L123.capacity_EJ_state_elec_F_tech")

    L102.load_segments <- get_data(all_data, "L102.load_segments_gcamusa")
    L102.invest_segments <- get_data(all_data, "L102.invest_segments_gcamusa")
    A23.dispatch_sector <- get_data(all_data, "gcam-usa/A23.dispatch_sector")
    A23.dispatch_sector_state_share <- get_data(all_data, "gcam-usa/A23.dispatch_sector_state_share")
    A23.dispatch_subsector_interp <- get_data(all_data, "gcam-usa/A23.dispatch_subsector_interp")
    A23.dispatch_subsector_shrwt <- get_data(all_data, "gcam-usa/A23.dispatch_subsector_shrwt")
    A23.dispatch_subsector_logit <- get_data(all_data, "gcam-usa/A23.dispatch_subsector_logit")
    A23.dispatch_globaltech_eff_additional <- get_data(all_data, "gcam-usa/A23.dispatch_globaltech_eff_additional")
    A23.dispatch_globaltech_OMfixed_additional <- get_data(all_data, "gcam-usa/A23.dispatch_globaltech_OMfixed_additional")
    A23.dispatch_globaltech_OMvar_additional <- get_data(all_data, "gcam-usa/A23.dispatch_globaltech_OMvar_additional")
    A23.dispatch_globaltech_capital_additional <- get_data(all_data, "gcam-usa/A23.dispatch_globaltech_capital_additional")
    A23.dispatch_globaltech_retirement_additional <- get_data(all_data, "gcam-usa/A23.dispatch_globaltech_retirement_additional")
    A23.dispatch_globaltech_shrwt_additional <- get_data(all_data, "gcam-usa/A23.dispatch_globaltech_shrwt_additional")
    A23.dispatch_capacitytech_min_cap_fac <- get_data(all_data, "gcam-usa/A23.dispatch_capacitytech_min_cap_fac")
    calibrated_techs_dispatch_usa <- get_data(all_data, "gcam-usa/calibrated_techs_dispatch_usa")
    capacity_credit_calculator <- get_data(all_data, "gcam-usa/dispatch/capacity_credit_calculator")
    TechTrialMarket_mapping <- get_data(all_data, "gcam-usa/dispatch/TechTrialMarket_mapping")

    L120.RsrcCurves_EJ_R_offshore_wind_USA <- get_data(all_data, "L120.RsrcCurves_EJ_R_offshore_wind_USA")
    L120.RegCapFactor_offshore_wind_USA <- get_data(all_data, "L120.RegCapFactor_offshore_wind_USA")
    L120.GridCost_offshore_wind_USA <- get_data(all_data, "L120.GridCost_offshore_wind_USA")

    # -----------------------------------------------------------------------------
    # 2. Perform computations

    #A vector indicate states where geothermal electric technologies will not be created
    NREL_us_re_technical_potential %>%
      left_join(states_subregions, by = c("State" = "state_name")) %>%
      filter(Geothermal_Hydrothermal_GWh == 0) %>%
      transmute(geo_state_noresource = paste(state, "geothermal", sep = " ")) %>%
      unlist ->
      geo_states_noresource

    # # A vector indicating states where CSP electric technologies will not be created
    L119.CapacityFactor_CSP_state %>%
      # states with effectively no resource has a very minor capacity.factor (<0.001)
      # remove these states to avoid creating CSP technologies there
      filter(capacity.factor < 0.01) %>%
      transmute(csp_state_noresource = paste(state, "CSP", sep = " ")) %>%
      unlist ->
      csp_states_noresource

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
    # L223 subsector_investment
    # ===========================================================================
    A23.dispatch_subsector_logit %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME)) ->
      L223.SubsectorLogit_Investment

    # subsector with specified shareweigt for each year
    A23.dispatch_subsector_shrwt %>%
      gather(key="year", value="share.weight", -supplysector, -subsector) %>%
      complete(nesting(supplysector, subsector), year = c(year, MODEL_YEARS)) %>%
      distinct() %>%
      mutate(year = as.integer(year)) %>%
      group_by(supplysector, subsector) %>%
      mutate(share.weight = approx_fun(year, share.weight)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(share.weight = ifelse(year %in% MODEL_BASE_YEARS, gcamusa.DEFAULT_SHAREWEIGHT, share.weight)) %>%
      # TODO: currently set geothermal shareweight as 0 to help solve
      mutate(share.weight = ifelse(subsector == "geothermal", 0, share.weight)) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["SubsectorShrwt"]] ) ->
      L223.SubsectorShrwt_Investment

    # subsectors with linear shareweigts - refined liquids, solar, biomass, nuclear, wind, geotheraml
    A23.dispatch_subsector_interp %>%
      filter(is.na(to.value)) %>%
      write_to_all_states( LEVEL2_DATA_NAMES[["SubsectorInterp"]] ) ->
      L223.SubsectorInterp_Investment

    # subsectors with s-curve shareweigts - gas, coal, nuclear
    A23.dispatch_subsector_interp %>%
      filter(!is.na(to.value)) %>%
      write_to_all_states( LEVEL2_DATA_NAMES[["SubsectorInterpTo"]] ) ->
      L223.SubsectorInterpTo_Investment

    # ===========================================================================
    ## L223 StubTech_investment
    # ===========================================================================
    # create a stub.technology template
    calibrated_techs_dispatch_usa %>%
      filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
      select(sector, subsector, technology) %>%
      rename(supplysector = sector, stub.technology = technology) %>%
      write_to_all_states( LEVEL2_DATA_NAMES[["StubTech"]] ) ->
      L223.StubTech_Investment

    # ---------------------------------------------------------------------------
    calibrated_techs_dispatch_usa %>%
      filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
      select(sector, supplysector, subsector, technology, minicam.energy.input) %>%
      # using left_join because efficiency table has NAs for technology improvement variables
      # cannot filter them out becuase fill_exp_decay_extrapolate function needs these columns (even with NAs)
      left_join(bind_rows(A23.globaltech_eff, A23.dispatch_globaltech_eff_additional),
                by = c("supplysector", "subsector", "technology", "minicam.energy.input")) %>%
      nest(-sector) %>%
      mutate(data = lapply(data, fill_exp_decay_extrapolate, MODEL_YEARS)) %>%
      unnest() %>%
      mutate(supplysector = sector) %>%
      select(-sector) %>%
      rename(efficiency = value) %>%
      mutate(year = as.integer(year)) %>%
      filter(year %in% MODEL_YEARS) %>%
      select(supplysector, subsector, technology, year, minicam.energy.input, efficiency) ->
      L223.GlobalTechEff_Investment

    # ===========================================================================
    ## L223 StubTechMarket_Investment
    # ===========================================================================

    # built market names into L223.StubTechMarket_Investment
    L223.GlobalTechEff_Investment %>%
      select(-efficiency) %>%
      rename(stub.technology = technology) %>%
      mutate(market.name = "temp") %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["StubTechMarket"]]) %>%
      mutate(market.name = if_else(minicam.energy.input %in% c(gcamusa.STATE_RENEWABLE_RESOURCES, "global solar resource"), region, "USA")) ->
      L223.StubTechMarket_Investment

    # ===========================================================================
    ## L223 GlobalTechOMvar_Investment  OM fix investment
    # ===========================================================================
    calibrated_techs_dispatch_usa %>%
      filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
      select(sector, supplysector, subsector, technology) %>%
      left_join_error_no_match(bind_rows(A23.globaltech_OMfixed, A23.dispatch_globaltech_OMfixed_additional) %>%
                                 select(-improvement.shadow.technology),
                               by = c("supplysector", "subsector", "technology")) %>%
      nest(-sector) %>%
      mutate(data = lapply(data, fill_exp_decay_extrapolate, MODEL_YEARS)) %>%
      unnest() %>%
      mutate(supplysector = sector) %>%
      select(-sector) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      rename(OM.fixed = value) %>%
      mutate(year = as.integer(year)) %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name, subsector.name, technology, year, input.OM.fixed, OM.fixed) ->
      L223.GlobalTechOMfixed_Investment

    # ===========================================================================
    ## L223 GlobalTechOMvar_Investment  OM var investment
    # ===========================================================================

    calibrated_techs_dispatch_usa %>%
      filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
      select(sector, supplysector, subsector, technology) %>%
      filter(technology %in% c(A23.globaltech_OMvar$technology, A23.dispatch_globaltech_OMvar_additional$technology)) %>%
      left_join_error_no_match(bind_rows(A23.globaltech_OMvar, A23.dispatch_globaltech_OMvar_additional) %>%
                                 select(-improvement.shadow.technology),
                               by = c("supplysector", "subsector", "technology")) %>%
      nest(-sector) %>%
      mutate(data = lapply(data, fill_exp_decay_extrapolate, MODEL_YEARS)) %>%
      unnest() %>%
      mutate(supplysector = sector) %>%
      select(-sector) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      rename(OM.var = value) %>%
      mutate(year = as.integer(year)) %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name, subsector.name, technology, year, input.OM.var, OM.var) ->
      L223.GlobalTechOMvar_Investment

    # ===========================================================================
    ## L223 GlobalTechCapital_Investment  capital.overnight investment
    # ===========================================================================

    calibrated_techs_dispatch_usa %>%
      filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
      select(sector, supplysector, subsector, technology) %>%
      left_join_error_no_match(bind_rows(A23.globaltech_capital, A23.dispatch_globaltech_capital_additional) %>%
                                 select(-improvement.shadow.technology),
                               by = c("supplysector", "subsector", "technology")) %>%
      nest(-sector) %>%
      mutate(data = lapply(data, fill_exp_decay_extrapolate, MODEL_YEARS)) %>%
      unnest() %>%
      mutate(supplysector = sector) %>%
      select(-sector) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      rename(capital.overnight = value) %>%
      mutate(year = as.integer(year)) %>%
      filter(year %in% MODEL_YEARS) %>%
      select(sector.name, subsector.name, technology, year, input.capital, capital.overnight, fixed.charge.rate) ->
      L223.GlobalTechCapital_Investment

    # ===========================================================================
    ## L223 GlobalTechShrwt_Investment  shareweight investment
    # ===========================================================================
    calibrated_techs_dispatch_usa %>%
      filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
      select(sector, supplysector, subsector, technology) %>%
      left_join_error_no_match(bind_rows(A23.globaltech_shrwt, A23.dispatch_globaltech_shrwt_additional),
                               by = c("supplysector", "subsector", "technology")) %>%
      gather(key="year", value="share.weight", -sector, -supplysector, -subsector, -technology) %>%
      complete(nesting(sector, supplysector, subsector, technology), year = c(year, MODEL_YEARS)) %>%
      mutate(supplysector = sector) %>%
      select(-sector) %>%
      distinct() %>%
      mutate(year = as.integer(year)) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) ->
      L223.GlobalTechShrwt_Investment

    # ===========================================================================
    ## L223 GlobalTechCapFac_Investment  capacity investment
    # ===========================================================================

    # 1) Here need to get the time share of each investment segment
    # Currently assumed the same for all gird
    L102.invest_segments %>%
      filter(grid_region == "Alaska grid") %>% # pick any grid is OK as they are the same for all gird
      select(invest_segment, hours) %>%
      mutate(seg_fraction = hours / gcamusa.ELEC_BASELOAD_HRS) %>%
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
                 by=c("sector", "technology")) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      mutate(supplysector = sector) %>%
      select(supplysector, subsector, technology, year, capacity.factor) ->
      L223.GlobalTechCapFac_Investment

    # ===========================================================================
    ## L223 GlobalTechCapture_Investment  storage market investment
    # ===========================================================================

    calibrated_techs_dispatch_usa %>%
      filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
      select(sector, supplysector, subsector, technology) %>%
      filter(technology %in% A23.globaltech_co2capture$technology) %>%
      left_join_error_no_match(A23.globaltech_co2capture, by = c("supplysector", "subsector", "technology")) %>%
      gather(key="year", value="remove.fraction", -sector, -supplysector, -subsector, -technology) %>%
      complete(nesting(sector, supplysector, subsector, technology), year = c(year, MODEL_FUTURE_YEARS)) %>%
      mutate(supplysector = sector) %>%
      select(-sector) %>%
      distinct() %>%
      mutate(year = as.integer(year)) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(remove.fraction = approx_fun(year, remove.fraction)) %>%
      ungroup() %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      mutate(storage.market = energy.CO2.STORAGE.MARKET) ->
      L223.GlobalTechCapture_Investment

    # ===========================================================================
    ## L223 GlobalTechCost_Investment  capacity credit investment
    # ===========================================================================

    # capacity-market-price：the capital overnight cost of a gas CT in 1975USD/KW
    # use 2015 value
    gas_CT_cost <- A23.dispatch_globaltech_capital_additional$`2015`[A23.dispatch_globaltech_capital_additional$technology == "gas (CT)"]

    calibrated_techs_dispatch_usa %>%
      filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
      select(sector, supplysector, subsector, technology) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      mutate(supplysector = sector) %>%
      mutate(capacity.market.price = gas_CT_cost) %>%
      select(supplysector, subsector, technology, year, capacity.market.price) ->
      L223.GlobalTechCost_Investment

    # ===========================================================================
    ## L223 GlobalTechCost_CapacityCreditCalulator  capacity credit calculator for non-dispatchable techs
    # ===========================================================================
    L223.GlobalTechCost_Investment %>%
      filter(subsector %in% capacity_credit_calculator$subsector) %>%
      # currently need to create a blank column otherwise the xml structure will be incorrect
      mutate(capacity.credit.calculator = NA) %>%
      left_join_error_no_match(capacity_credit_calculator, by = "subsector") %>%
      select(-capacity.market.price) ->
      L223.GlobalTechCost_CapacityCreditCalulator

    # ===========================================================================
    ## L223 TechCapFac_Investment wind/solar capacity factor investment
    # ===========================================================================

    bind_rows(L114.CapacityFactor_wind_state %>% mutate(technology = "wind"),
              L119.CapacityFactor_PV_state %>% mutate(technology = "PV"),
              L119.CapacityFactor_CSP_state %>% mutate(technology = "CSP")) %>%
      rename(region = "state") %>%
      select(-sector) %>%
      repeat_add_columns(tibble::tibble(sector = L223.TechCapFac_Investment_SegAdjust$sector)) %>%
      left_join_error_no_match(L223.TechCapFac_Investment_SegAdjust, by = "sector") %>%
      mutate(capacity.factor = pmin(capacity.factor, seg_fraction)) %>% # takes the smaller of these two
      # here using inner_join as a cross-reference process to filter wind and solar for those only exist
      # in certain segment defined in calibrated_techs_dispatch_usa
      # currently wind and solar only exsit in base and intermediate segments
      mutate(sector = as.character(sector)) %>%
      inner_join(calibrated_techs_dispatch_usa %>%
                   filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
                   select(sector, supplysector, subsector, technology),
                 by=c("sector", "technology")) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      mutate(supplysector = sector) %>%
      select(region, supplysector, subsector, technology, year, capacity.factor) ->
      L223.TechCapFac_Investment

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
      right_join(L223.Sector_Investment, by=c("state" = "region")) %>%
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
      write_to_all_states(LEVEL2_DATA_NAMES[["TechShrwt"]]) ->
      L223.TechShrwt_Dispatch

    # technolgoy efficiency
    calibrated_techs_dispatch_usa %>%
      filter(sector == "electricity generation") %>%
      select(sector, supplysector, subsector, technology, minicam.energy.input) %>%
      # filter out hydro in this table as it does not have an input
      filter(!is.na(minicam.energy.input)) %>%
      # using left_join becuase efficiency table has NAs for technology improvement parameters for some technologies
      # but fill_exp_decay_extrapolate needs those columns
      left_join(bind_rows(A23.globaltech_eff, A23.dispatch_globaltech_eff_additional),
                by = c("supplysector", "subsector", "technology", "minicam.energy.input")) %>%
      fill_exp_decay_extrapolate(MODEL_YEARS) %>%
      select(-sector) %>%
      rename(efficiency = value) %>%
      mutate(market.name = "temp") %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["TechEff"]]) %>%
      mutate(market.name = if_else(minicam.energy.input %in%
                                     c(gcamusa.STATE_RENEWABLE_RESOURCES, "global solar resource"), region, "USA")) ->
      L223.TechEff_Dispatch

    # technology OM_fixed
    calibrated_techs_dispatch_usa %>%
      filter(sector == "electricity generation") %>%
      select(sector, supplysector, subsector, technology) %>%
      filter(technology %in% c(A23.globaltech_OMfixed$technology, A23.dispatch_globaltech_OMfixed_additional$technology)) %>%
      left_join_error_no_match(bind_rows(A23.globaltech_OMfixed, A23.dispatch_globaltech_OMfixed_additional) %>%
                                 select(-improvement.shadow.technology),
                               by = c("supplysector", "subsector", "technology")) %>%
      fill_exp_decay_extrapolate(MODEL_YEARS) %>%
      select(-sector) %>%
      rename(OM.fixed = value) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["TechOMfixed"]]) ->
      L223.TechOMfixed_Dispatch

    # technology OM_Var
    calibrated_techs_dispatch_usa %>%
      filter(sector == "electricity generation") %>%
      select(sector, supplysector, subsector, technology) %>%
      filter(technology %in% c(A23.globaltech_OMvar$technology, A23.dispatch_globaltech_OMvar_additional$technology)) %>%
      left_join_error_no_match(bind_rows(A23.globaltech_OMvar, A23.dispatch_globaltech_OMvar_additional) %>%
                                 select(-improvement.shadow.technology),
                               by = c("supplysector", "subsector", "technology")) %>%
      # TODO: double check why solar doesn't have OMvar
      fill_exp_decay_extrapolate(MODEL_YEARS) %>%
      select(-sector) %>%
      rename(OM.var = value) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["TechOMvar"]]) ->
      L223.TechOMvar_Dispatch

    # technology lifetime (three parts)
    # "final-calibration-year", "final-historical-year"
    A23.globaltech_retirement %>%
      bind_rows(A23.dispatch_globaltech_retirement_additional) %>%
      filter(year %in% c("final-calibration-year", "final-historical-year")) %>%
      filter(technology %in% unique(L223.TechOMfixed_Dispatch$technology) | technology == "hydro") %>%
      set_years() %>%
      mutate(year_int = as.integer(year)) %>%
      select(supplysector, subsector, technology, year_int, lifetime) ->
      L223.TechLifetime_Dispatch

    # "initial-future-year"
    A23.globaltech_retirement %>%
      bind_rows(A23.dispatch_globaltech_retirement_additional) %>%
      filter(year == "initial-future-year") %>%
      filter(technology %in% unique(L223.TechOMfixed_Dispatch$technology)) %>%
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
      bind_rows(A23.dispatch_globaltech_retirement_additional) %>%
      filter(year == "initial-nonhistorical-year") %>%
      filter(technology %in% unique(L223.TechOMfixed_Dispatch$technology)) %>%
      set_years() %>%
      mutate(year_temp = as.integer(year)) %>%
      select(supplysector, subsector, technology, year_temp, lifetime) %>%
      expand(., .,
             year_int = MODEL_YEARS[MODEL_YEARS >= as.integer(set_years(tibble::tibble("initial-nonhistorical-year")))]) %>%
      select(-year_temp) %>%
      bind_rows(L223.TechLifetime_Dispatch, .) %>%
      distinct() %>%
      rename(year = year_int) %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["TechYr"]], "lifetime")) ->
      L223.TechLifetime_Dispatch

    # technology S-Curve
    A23.globaltech_retirement %>%
      bind_rows(A23.dispatch_globaltech_retirement_additional) %>%
      filter(year %in% c("final-calibration-year", "final-historical-year")) %>%
      filter(technology %in% unique(L223.TechOMfixed_Dispatch$technology) | technology == "hydro") %>%
      filter(!is.na(steepness)) %>%
      set_years() %>%
      mutate(year = as.integer(year)) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["TechSCurve"]]) ->
      L223.TechSCurve_Dispatch

    # technology capacity factor
    calibrated_techs_dispatch_usa %>%
      filter(sector == "electricity generation") %>%
      select(supplysector, subsector, technology, capacity.factor) %>%
      expand(., ., year = MODEL_YEARS) %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["TechYr"]], "capacity.factor")) ->
      L223.TechCapFac_Dispatch

    # technology capacity
    L223.TechCapFac_Dispatch %>%
      rename(capacity = capacity.factor) %>%
      mutate(capacity = 0.0 ) ->
      L223.CapacityTech_FutureTechs

    # renewable capacity factor by load segment

    # We need to make sure hydro continues to use the historical capacity factor into
    # the future.  We can do that by using the segment specific capacity factors.
    # TODO: get actual seasonal variations for hydro instead
    L123.out_EJ_state_elec_F_tech %>%
      filter(fuel == "hydro", year == MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L123.capacity_EJ_state_elec_F_tech, by= c("state", "fuel" = "gcam_fuel", "year")) %>%
      mutate(capacity.factor = value / capacity) %>%
      select(state, sector, fuel, capacity.factor) %>%
      expand(., ., segment = gcamusa.ELEC_LOAD_SEGMENT_ORDER) ->
      L223.hydro_CapFac_segment

    bind_rows(L114.CapacityFactor_wind_state_segment,
              L119.CapacityFactor_PV_state_segment,
              L119.CapacityFactor_CSP_state_segment,
              L223.hydro_CapFac_segment) %>%
      select(-sector) %>%
      rename(technology = fuel, region = state) %>%
      # note expanding by rows here so just regular left join
      left_join(L223.TechCapFac_Dispatch %>% select(-capacity.factor), by = c("region", "technology")) %>%
      # hydro is currently only produced out of the final calibration year, while
      # it is not an error to include the segment specific capacity factor in the
      # future, not including it helps keep the size of the XML down
      filter((technology == "hydro" & year == MODEL_FINAL_BASE_YEAR) |
               (technology != "hydro" & year >= MODEL_FINAL_BASE_YEAR)) %>%
      select(region, supplysector, subsector, technology, year, segment, capacity.factor) ->
      L223.CapacityTechSegmentCapFac

    A23.dispatch_capacitytech_min_cap_fac %>%
      expand(., ., year = MODEL_YEARS) %>%
      #write_to_all_states(LEVEL2_DATA_NAMES[["CapacityTechMinCapFac"]]) ->
      write_to_all_states(c(LEVEL2_DATA_NAMES[["TechYr"]], "min.capacity.factor")) ->
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
      write_to_all_states(LEVEL2_DATA_NAMES[["CarbonCapture"]]) ->
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
      rename(dispatchsector = supplysector) %>%
      expand(., ., generation_sector = gcamusa.ELEC_INV_NAMES) %>%
      mutate(generation_sector_market = region) %>%
      bind_rows(
        states_subregions %>%
          select(state, grid_region) %>%
          rename(region = grid_region) %>%
          rename(generation_sector_market = state) %>%
          mutate(dispatchsector = "electricity") %>%
          mutate(generation_sector = "electricity")
      ) ->
      L223.DispatchSector

    # dispatch sector demand segment
    L102.load_segments %>%
      rename(region = grid_region) %>%
      mutate(dispatch.sector = "electricity") %>%
      select(region, dispatch.sector, segment, hours, relative.generation, generation.fraction) ->
      L223.DispatchSectorDispatchSegments

    # When the demand side has no segments we keep the load curve as part of
    # the dispatch segments so it can downscale the total electricity demand
    # L223.DispatchSectorDispatchSegments %>%
    #   mutate(demand.segment.name = dispatch.sector) ->
    #   L223.DispatchSectorDispatchSegments

    # When the demand side is divided into segments we need to map the dispatch
    # segments to those sectors and update the relative.generation accordingly
    L223.DispatchSectorDispatchSegments %>%
      # right now the dispatch segments exactly match the demand segments
      # so the generation fraction is just 1.0.
      mutate(demand.segment.name = paste(dispatch.sector, segment, sep = "_"),
             generation.fraction = 1.0) ->
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
                  mutate(hours = as.integer(lag(hours, default = gcamusa.ELEC_SUPERPEAK_HRS))) %>%
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
                                 select(-minicam.energy.input, -secondary.output),
                               by=c("gcam_fuel" = "fuel", "elec_tech" = "elec_tech")) %>%
      rename(region = state) %>%
      select(region, supplysector, subsector, technology, year, capacity) ->
      L223.CapacityTech

    # calibrated output for electricity technology
    L123.out_EJ_state_elec_F_tech %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      # mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT)) %>%
      mutate(calOutputValue = value) %>%
      left_join_error_no_match(calibrated_techs_dispatch_usa %>%
                                 select(-minicam.energy.input, -secondary.output),
                               by = c("sector", "fuel", "elec_tech")) %>%
      rename(region = state) %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]], calOutputValue)-> L223.Production_Dispatch_temp

    # L123.out_EJ_state_elec_F_tech contains only technologies which have non-zero historical generation
    # start with full set of dispatch techs, join in non-zero calibrated values, and assign
    # zero calibrated output for the others
    L223.TechShrwt_Dispatch %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      # L123.out_EJ_state_elec_F_tech contains subset of technologies which have non-zero historical generation
      # LJENM throws error; left_join() is used; NAs are assigned zero calOutputValue below
      left_join(L223.Production_Dispatch_temp, by = c(LEVEL2_DATA_NAMES[["TechYr"]])) %>%
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
                                 select(-secondary.output),
                               by = c("sector", "fuel", "elec_tech")) %>%
      rename(region = state) %>%
      select(region, supplysector, subsector, technology, year, minicam.energy.input, efficiency) %>%
      mutate(market.name = if_else(minicam.energy.input %in%
                                     c(gcamusa.STATE_RENEWABLE_RESOURCES, "global solar resource"), region, "USA")) ->
      L223.TechEff_Cal

    # calibrated production for grid
    L123.out_EJ_state_elec_F_tech %>%
      left_join_error_no_match(select(states_subregions, state, grid_region), by = "state") %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(dispatchsector = "electricity") %>%
      select(grid_region, dispatchsector, year, value) %>%
      group_by(grid_region, dispatchsector, year) %>%
      summarize(cal_production = sum(value)) %>%
      ungroup() ->
      L223.DispatchSectorCalProd

    # capacity technology use trail market at grid level
    L223.TechShrwt_Dispatch %>%
      filter(subsector %in% capacity_credit_calculator$subsector) %>%
      left_join_error_no_match(select(states_subregions, state, grid_region), by = c("region" = "state")) %>%
      left_join_error_no_match(TechTrialMarket_mapping, by = c("region", "subsector")) %>%
      select(region, supplysector, subsector, technology, year, trial.market.name, capacity.market.name) ->
      L223.TechTrialMarket_Dispatch

    # investment technology use trial at grid level
    L223.StubTechMarket_Investment %>%
      filter(subsector %in% capacity_credit_calculator$subsector) %>%
      left_join_error_no_match(select(states_subregions, state, grid_region), by = c("region" = "state")) %>%
      left_join_error_no_match(TechTrialMarket_mapping, by = c("region", "subsector")) %>%
      rename(technology = stub.technology) %>%
      select(region, supplysector, subsector, technology, year, trial.market.name) ->
      L223.TechTrialMarket_Investment

    # Socioeconomic information in the electricity grid regions (required for GCAM to run with these regions)

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


    # Remove geothermal option from states that do not have potential
    L223.SubsectorLogit_Investment %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.SubsectorShrwt_Investment %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.SubsectorInterp_Investment %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.SubsectorInterpTo_Investment %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.StubTech_Investment %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.StubTechMarket_Investment %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.SubsectorLogit_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.SubsectorShrwtFllt_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.CapacityTech %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.CapacityTech_FutureTechs %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.CapacityTechSegmentCapFac %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.CapacityTechMinCapFac %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechShrwt_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechEff_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechOMfixed_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechOMvar_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechLifetime_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechSCurve_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechCapFac_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.Production_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))

    # Remove CSP option from states that do not have potential
    L223.StubTech_Investment %<>% filter(!(paste(region, stub.technology) %in% csp_states_noresource))
    L223.StubTechMarket_Investment %<>% filter(!(paste(region, stub.technology) %in% csp_states_noresource))
    L223.CapacityTech %<>% filter(!(paste(region, technology) %in% csp_states_noresource))
    L223.CapacityTech_FutureTechs %<>% filter(!(paste(region, technology) %in% csp_states_noresource))
    L223.CapacityTechSegmentCapFac %<>% filter(!(paste(region, technology) %in% csp_states_noresource))
    L223.CapacityTechMinCapFac %<>% filter(!(paste(region, technology) %in% csp_states_noresource))
    L223.TechShrwt_Dispatch %<>% filter(!(paste(region, technology) %in% csp_states_noresource))
    L223.TechEff_Dispatch %<>% filter(!(paste(region, technology) %in% csp_states_noresource))
    L223.TechOMfixed_Dispatch %<>% filter(!(paste(region, technology) %in% csp_states_noresource))
    L223.TechOMvar_Dispatch %<>% filter(!(paste(region, technology) %in% csp_states_noresource))
    L223.TechLifetime_Dispatch %<>% filter(!(paste(region, technology) %in% csp_states_noresource))
    L223.TechCapFac_Dispatch %<>% filter(!(paste(region, technology) %in% csp_states_noresource))
    L223.Production_Dispatch %<>% filter(!(paste(region, technology) %in% csp_states_noresource))
    L223.TechCapFac_Investment %<>% filter(!(paste(region, technology) %in% csp_states_noresource))
    L223.TechTrialMarket_Dispatch %<>% filter(!(paste(region, technology) %in% csp_states_noresource))
    L223.TechTrialMarket_Investment %<>% filter(!(paste(region, technology) %in% csp_states_noresource))

    # Modifications for offshore wind
    # Remove states with no offshore wind resources
    offshore_wind_states <- unique(L120.RsrcCurves_EJ_R_offshore_wind_USA$region)

    L223.StubTech_Investment %<>% filter(region %in% offshore_wind_states | stub.technology != "wind_offshore")
    L223.StubTechMarket_Investment %<>% filter(region %in% offshore_wind_states | stub.technology != "wind_offshore")
    L223.CapacityTech_FutureTechs %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")
    L223.CapacityTechSegmentCapFac %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")
    L223.CapacityTechMinCapFac %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")
    L223.TechShrwt_Dispatch %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")
    L223.TechEff_Dispatch %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")
    L223.TechOMfixed_Dispatch %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")
    L223.TechOMvar_Dispatch %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")
    L223.TechLifetime_Dispatch %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")
    L223.TechSCurve_Dispatch %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")
    L223.Production_Dispatch %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")
    L223.TechTrialMarket_Dispatch %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")
    L223.TechTrialMarket_Investment %<>% filter(region %in% offshore_wind_states | technology != "wind_offshore")

    L223.TechCapFac_Dispatch %>%
      filter(technology == "wind_offshore",
             region %in% offshore_wind_states) %>%
      select(-capacity.factor) %>%
      left_join_error_no_match(L120.RegCapFactor_offshore_wind_USA,
                               by= c("region" = "State")) %>%
      mutate(capacity.factor= round(CFmax,energy.DIGITS_CAPACITY_FACTOR)) %>%
      select(region, supplysector, subsector, technology, year,
             capacity.factor) -> L223.TechCapFac_offshore_wind_Dispatch


    L223.TechCapFac_Dispatch %>%
      filter(technology != "wind_offshore") %>%
      bind_rows(L223.TechCapFac_offshore_wind_Dispatch) -> L223.TechCapFac_Dispatch


    L223.TechCapFac_offshore_wind_Dispatch %>%
      select(region, supplysector, subsector, technology, year) %>%
      mutate(minicam.non.energy.input = "regional price adjustment") %>%
      left_join_error_no_match(L120.GridCost_offshore_wind_USA, by = c("region" = "State")) %>%
      rename(input.cost = grid.cost) ->
      L223.TechCost_offshore_wind_Dispatch

    # ----------------------------------------------------------------------------------------------------------------------------
    L223.Sector_Investment %>%
      add_title("Investment supplysector logit-exponent by state") %>%
      add_units("Unitless") %>%
      add_comments("Set supplysector logit-exponent for states") %>%
      add_legacy_name("L223.Sector_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/A23.dispatch_sector_state_share") ->
      L223.Sector_Investment

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

    L223.SubsectorInterp_Investment %>%
      add_title("Investment subsector linear interpolation-function by state") %>%
      add_units("Unitless") %>%
      add_comments("Set subsector linear interpolation-function for states") %>%
      add_legacy_name("L223.SubsectorInterp_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/A23.dispatch_subsector_interp",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.SubsectorInterp_Investment

    L223.SubsectorInterpTo_Investment %>%
      add_title("Investment subsector s-curve interpolation-function by state") %>%
      add_units("Unitless") %>%
      add_comments("Set subsector s-curve interpolation-function for states") %>%
      add_legacy_name("L223.SubsectorInterpTo_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/A23.dispatch_subsector_interp",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.SubsectorInterpTo_Investment

    L223.StubTech_Investment %>%
      add_title("Investment stub.technology names by state") %>%
      add_units("Unitless") %>%
      add_comments("Set stub.technology names as a template for states") %>%
      add_legacy_name("L223.StubTech_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L120.RsrcCurves_EJ_R_offshore_wind_USA",
                     "L120.RegCapFactor_offshore_wind_USA",
                     "L120.GridCost_offshore_wind_USA") ->
      L223.StubTech_Investment

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
                     "L120.RsrcCurves_EJ_R_offshore_wind_USA",
                     "L120.RegCapFactor_offshore_wind_USA",
                     "L120.GridCost_offshore_wind_USA") ->
      L223.StubTechMarket_Investment

    L223.GlobalTechOMfixed_Investment %>%
      add_title("Investment technology OM-fix") %>%
      add_units("1975$US/kW/year") %>%
      add_comments("Set technology OM-fix") %>%
      add_legacy_name("L223.GlobalTechOMfixed_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "energy/A23.globaltech_OMfixed",
                     "gcam-usa/A23.dispatch_globaltech_OMfixed_additional") ->
      L223.GlobalTechOMfixed_Investment

    L223.GlobalTechOMvar_Investment %>%
      add_title("Investment technology OM-var") %>%
      add_units("1975$US/MWh") %>%
      add_comments("Set technology OM-var") %>%
      add_legacy_name("L223.GlobalTechOMvar_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "energy/A23.globaltech_OMvar",
                     "gcam-usa/A23.dispatch_globaltech_OMvar_additional") ->
      L223.GlobalTechOMvar_Investment

    L223.GlobalTechCapital_Investment %>%
      add_title("Investment technology capital-overnight and fixed-charge-rate") %>%
      add_units("1975$US/kw") %>%
      add_comments("Set technology capital-overnight and fixed-charge-rate") %>%
      add_legacy_name("L223.L223.GlobalTechCapital_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "energy/A23.globaltech_capital",
                     "gcam-usa/A23.dispatch_globaltech_capital_additional") ->
      L223.GlobalTechCapital_Investment

    L223.GlobalTechShrwt_Investment %>%
      add_title("Investment technology share-weight") %>%
      add_units("Unitless") %>%
      add_comments("Set technology share-weight") %>%
      add_legacy_name("L223.GlobalTechShrwt_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "energy/A23.globaltech_shrwt",
                     "gcam-usa/A23.dispatch_globaltech_shrwt_additional") ->
      L223.GlobalTechShrwt_Investment

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
      add_precursors("L114.CapacityFactor_wind_state_gcamusa",
                     "L119.CapacityFactor_PV_state_gcamusa",
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
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.SubsectorLogit_Dispatch

    L223.SubsectorShrwtFllt_Dispatch %>%
      add_title("Dispatch subsector shareweight for state") %>%
      add_units("Unitless") %>%
      add_comments("Set subsector shareweight for state") %>%
      add_legacy_name("L223.SubsectorShrwtFllt_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
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
                     "L120.RsrcCurves_EJ_R_offshore_wind_USA",
                     "L120.RegCapFactor_offshore_wind_USA",
                     "L120.GridCost_offshore_wind_USA") ->
      L223.CapacityTech_FutureTechs

    L223.CapacityTechSegmentCapFac %>%
      add_title("Dispatch technology capacity factor by segment for state") %>%
      add_units("Unitless") %>%
      add_comments("Set technology capacity factor by segment for state") %>%
      add_legacy_name("L223.CapacityTechSegmentCapFac (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L114.CapacityFactor_wind_state_segment_gcamusa",
                     "L119.CapacityFactor_CSP_state_segment_gcamusa",
                     "L119.CapacityFactor_PV_state_segment_gcamusa",
                     "L120.RsrcCurves_EJ_R_offshore_wind_USA",
                     "L120.RegCapFactor_offshore_wind_USA",
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
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.CapacityTech

    L223.TechShrwt_Dispatch %>%
      add_title("Dispatch technology shareweight for state") %>%
      add_units("Unitless") %>%
      add_comments("Set technology shareweight for state all as 1") %>%
      add_legacy_name("L223.TechShrwt_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L120.RsrcCurves_EJ_R_offshore_wind_USA",
                     "L120.RegCapFactor_offshore_wind_USA",
                     "L120.GridCost_offshore_wind_USA") ->
      L223.TechShrwt_Dispatch

    L223.TechEff_Dispatch %>%
      add_title("Dispatch technology efficiency for state") %>%
      add_units("Unitless") %>%
      add_comments("Set technology efficiency for state") %>%
      add_legacy_name("L223.TechEff_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "energy/A23.globaltech_eff",
                     "gcam-usa/A23.dispatch_globaltech_eff_additional",
                     "L120.RsrcCurves_EJ_R_offshore_wind_USA",
                     "L120.RegCapFactor_offshore_wind_USA",
                     "L120.GridCost_offshore_wind_USA") ->
      L223.TechEff_Dispatch

    L223.TechOMfixed_Dispatch %>%
      add_title("Dispatch technology OM fixed for state") %>%
      add_units("1975$US/MWh") %>%
      add_comments("Set technology OM fixed for state") %>%
      add_legacy_name("L223.TechOMfixed_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "energy/A23.globaltech_OMfixed",
                     "gcam-usa/A23.dispatch_globaltech_OMvar_additional",
                     "L120.RsrcCurves_EJ_R_offshore_wind_USA",
                     "L120.RegCapFactor_offshore_wind_USA",
                     "L120.GridCost_offshore_wind_USA") ->
      L223.TechOMfixed_Dispatch

    L223.TechOMvar_Dispatch %>%
      add_title("Dispatch technology OM var for state") %>%
      add_units("1975$US/MWh") %>%
      add_comments("Set technology OM var for state") %>%
      add_legacy_name("L223.TechOMvar_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "energy/A23.globaltech_OMvar",
                     "gcam-usa/A23.dispatch_globaltech_OMvar_additional",
                     "L120.RsrcCurves_EJ_R_offshore_wind_USA",
                     "L120.RegCapFactor_offshore_wind_USA",
                     "L120.GridCost_offshore_wind_USA") ->
      L223.TechOMvar_Dispatch

    L223.TechLifetime_Dispatch %>%
      add_title("Dispatch technology lifetime for state") %>%
      add_units("yrs") %>%
      add_comments("Set technology lifetime for state") %>%
      add_legacy_name("L223.TechLifetime_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "energy/A23.globaltech_retirement",
                     "gcam-usa/A23.dispatch_globaltech_retirement_additional",
                     "L120.RsrcCurves_EJ_R_offshore_wind_USA",
                     "L120.RegCapFactor_offshore_wind_USA",
                     "L120.GridCost_offshore_wind_USA") ->
      L223.TechLifetime_Dispatch

    L223.TechSCurve_Dispatch %>%
      add_title("Dispatch technology lifetime steepness and half.life for state") %>%
      add_units("unitless") %>%
      add_comments("Set technology lifetime steepness and half.life for state") %>%
      add_legacy_name("L223.TechSCurve_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "energy/A23.globaltech_retirement",
                     "gcam-usa/A23.dispatch_globaltech_retirement_additional",
                     "L120.RsrcCurves_EJ_R_offshore_wind_USA",
                     "L120.RegCapFactor_offshore_wind_USA",
                     "L120.GridCost_offshore_wind_USA") ->
      L223.TechSCurve_Dispatch

    L223.TechCapFac_Dispatch %>%
      add_title("Dispatch technology capacity factor for state") %>%
      add_units("unitless") %>%
      add_comments("Set technology capacity factor for state") %>%
      add_legacy_name("L223.TechCapFac_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L120.RsrcCurves_EJ_R_offshore_wind_USA",
                     "L120.RegCapFactor_offshore_wind_USA",
                     "L120.GridCost_offshore_wind_USA") ->
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

    L223.DispatchSectorCalProd %>%
      add_title("Dispatch dispatchsector (electricity) calibrated production for grid") %>%
      add_units("unitless") %>%
      add_comments("Set dispatchsector (electricity) calibrated production for grid") %>%
      add_legacy_name("L223.DispatchSectorCalProd (dispatch branch)") %>%
      add_precursors("L123.out_EJ_state_elec_F_tech") ->
      L223.DispatchSectorCalProd

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
      add_precursors("gcam-usa/states_subregions") ->
      L223.LaborForceFillout_FERC

    L223.TechCost_offshore_wind_Dispatch %>%
      add_title("State-specific non-energy cost adder for offshore wind grid connection cost") %>%
      add_units("Unitless") %>%
      add_comments("Adder") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L120.RsrcCurves_EJ_R_offshore_wind_USA",
                     "L120.RegCapFactor_offshore_wind_USA",
                     "L120.GridCost_offshore_wind_USA")->
      L223.TechCost_offshore_wind_Dispatch

    return_data(L223.Sector_Investment,
                L223.SubsectorLogit_Investment,
                L223.SubsectorInterp_Investment,
                L223.SubsectorShrwt_Investment,
                L223.SubsectorInterpTo_Investment,
                L223.StubTech_Investment,
                L223.GlobalTechEff_Investment,
                L223.StubTechMarket_Investment,
                L223.GlobalTechOMfixed_Investment,
                L223.GlobalTechOMvar_Investment,
                L223.GlobalTechCapital_Investment,
                L223.GlobalTechShrwt_Investment,
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
                L223.TechOMfixed_Dispatch,
                L223.TechOMvar_Dispatch,
                L223.TechLifetime_Dispatch,
                L223.TechSCurve_Dispatch,
                L223.TechCapFac_Dispatch,
                L223.TechCarbonCapture_Dispatch,
                L223.Production_Dispatch,
                L223.TechEff_Cal,
                L223.TechTrialMarket_Dispatch,
                L223.TechTrialMarket_Investment,
                L223.Sector_Dispatch_Grid,
                L223.DispatchSectorCalProd,
                L223.DispatchSectorDispatchSegments,
                L223.InterestRate_FERC,
                L223.Pop_FERC,
                L223.BaseGDP_FERC,
                L223.LaborForceFillout_FERC,
                L223.TechCost_offshore_wind_Dispatch)
  } else {
    stop("Unknown command")
  }
}
