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
#' \code{L223.GlobalTechCapture_Investment}, \code{L223.GlobalTechCost_Investment}, \code{L223.GlobalTechHack_Investment},
#' \code{L223.Sector_Investment_StateShare}, \code{L223.Subsector_Investment_StateShare}, \code{L223.CapacityTech},
#' \code{L223.SubsectorShrwtFllt_Investment_StateShare}, \code{L223.TechCoef_Investment_StateShare},
#' \code{L223.TechShrwt_Investment_StateShare},\code{L223.Sector_Investment_LoadCurve}, \code{L223.TechShrwt_Dispatch},
#' \code{L223.SectorUseTrialMarket_Investment_LoadCurve}, \code{L223.SubsectorLogit_Investment_LoadCurve},
#' \code{L223.SubsectorShrwtFllt_Investment_LoadCurve}, \code{L223.TechShrwt_Investment_LoadCurve},
#' \code{L223.TechCoef_Investment_LoadCurve}, \code{L223.TechPMult_Investment_LoadCurve}, \code{L223.TechEff_Dispatch},
#' \code{L223.TechCost_Investment_LoadCurve}, \code{L223.DispatchSector}, \code{L223.Sector_Dispatch},
#' \code{L223.SubsectorLogit_Dispatch}, \code{L223.SubsectorShrwtFllt_Dispatch}, \code{L223.CapacityTech_FutureTechs},
#' \code{L223.TechOMvar_Dispatch}, \code{L223.TechLifetime_Dispatch}, \code{L223.TechSCurve_Dispatch},
#' \code{L223.TechCapFac_Dispatch}, \code{L223.TechCarbonCapture_Dispatch}, \code{L223.TechCapFac_Cal},
#' \code{L223.TechEff_Cal}, \code{L223.Sector_Dispatch_Grid}, \code{L223.DispatchSectorCalProd},
#' \code{L223.DispatchSectorDemandSegments}, \code{L223.InterestRate_FERC}, \code{L223.Pop_FERC}, \code{L223.BaseGDP_FERC},
#' \code{L223.LaborForceFillout_FERC}.
#' The corresponding file in the
#' original data system was \code{L223.electricity_USA.R} (gcam-usa level2 - dispatch branch).
#' @details This chunk generates input files to create an annualized electricity generation sector for each state
#' and creates the demand for the state-level electricity sectors in the grid regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
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

             "L114.CapacityFactor_wind_state",
             "L119.CapacityFactor_PV_state",
             "L119.CapFacScaler_PV_state",
             "L119.CapacityFactor_CSP_state",
             "L119.CapFacScaler_CSP_state",
             "L114.CapacityFactor_wind_state_segment",
             "L119.CapacityFactor_PV_state_segment",
             "L119.CapacityFactor_CSP_state_segment",

             "L123.in_EJ_state_elec_F_tech",
             "L123.out_EJ_state_elec_F_tech",
             "L123.capacity_EJ_state_elec_F_tech",
             "L123.capacity_factor_EJ_state_elec_F_tech",

             "L223.GlobalIntTechBackup_elec",

             FILE = "gcam-usa/dispatch/L102.load_segments",
             FILE = "gcam-usa/dispatch/L102.invest_segments",
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
             FILE = "gcam-usa/calibrated_techs_dispatch_usa"))
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
             "L223.GlobalTechHack_Investment",
             "L223.Sector_Investment_StateShare",
             "L223.Subsector_Investment_StateShare",
             "L223.SubsectorShrwtFllt_Investment_StateShare",
             "L223.TechCoef_Investment_StateShare",
             "L223.TechShrwt_Investment_StateShare",
             "L223.Sector_Investment_LoadCurve",
             "L223.SectorUseTrialMarket_Investment_LoadCurve",
             "L223.SubsectorLogit_Investment_LoadCurve",
             "L223.SubsectorShrwtFllt_Investment_LoadCurve",
             "L223.TechShrwt_Investment_LoadCurve",
             "L223.TechCoef_Investment_LoadCurve",
             "L223.TechPMult_Investment_LoadCurve",
             "L223.TechCost_Investment_LoadCurve",
             "L223.DispatchSector",
             "L223.Sector_Dispatch",
             "L223.SubsectorLogit_Dispatch",
             "L223.SubsectorShrwtFllt_Dispatch",
             "L223.CapacityTech_FutureTechs",
             "L223.CapacityTechSegmentCapFac",
             "L223.CapacityTech",
             "L223.TechShrwt_Dispatch",
             "L223.TechEff_Dispatch",
             "L223.TechOMvar_Dispatch",
             "L223.TechLifetime_Dispatch",
             "L223.TechSCurve_Dispatch",
             "L223.TechCapFac_Dispatch",
             "L223.TechCarbonCapture_Dispatch",
             "L223.TechCapFac_Cal",
             "L223.TechEff_Cal",
             "L223.Sector_Dispatch_Grid",
             "L223.DispatchSectorCalProd",
             "L223.DispatchSectorDemandSegments",
             "L223.InterestRate_FERC",
             "L223.Pop_FERC",
             "L223.BaseGDP_FERC",
             "L223.LaborForceFillout_FERC"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    grid_region <- Geothermal_Hydrothermal_GWh <- state <- geo_state_noresource <-
      region <- supplysector <- subsector <- technology <- year <- value <-
      sector <- calOutputValue <- fuel <- elec <- share <- avg.share <- pref <-
      share.weight.mult <- share.weight <- market.name <- sector.name <- subsector.name <-
      minicam.energy.input <- calibration <- secondary.output <- stub.technology <-
      capacity.factor <- scaler <- capacity.factor.capital <- . <- NULL  # silence package check notes

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

    L114.CapacityFactor_wind_state <- get_data(all_data, "L114.CapacityFactor_wind_state")
    L114.CapacityFactor_wind_state_segment <- get_data(all_data, "L114.CapacityFactor_wind_state_segment")
    L119.CapFacScaler_PV_state <- get_data(all_data, "L119.CapFacScaler_PV_state")
    L119.CapacityFactor_PV_state <- get_data(all_data, "L119.CapacityFactor_PV_state")
    L119.CapacityFactor_PV_state_segment <- get_data(all_data, "L119.CapacityFactor_PV_state_segment")
    L119.CapacityFactor_CSP_state <- get_data(all_data, "L119.CapacityFactor_CSP_state")
    L119.CapFacScaler_CSP_state <- get_data(all_data, "L119.CapFacScaler_CSP_state")
    L119.CapacityFactor_CSP_state_segment <- get_data(all_data, "L119.CapacityFactor_CSP_state_segment")

    L123.in_EJ_state_elec_F_tech <- get_data(all_data, "L123.in_EJ_state_elec_F_tech")
    L123.out_EJ_state_elec_F_tech <- get_data(all_data, "L123.out_EJ_state_elec_F_tech")
    L123.capacity_EJ_state_elec_F_tech <- get_data(all_data, "L123.capacity_EJ_state_elec_F_tech")
    L123.capacity_factor_EJ_state_elec_F_tech <- get_data(all_data, "L123.capacity_factor_EJ_state_elec_F_tech")

    L223.GlobalIntTechBackup_elec <- get_data(all_data, "L223.GlobalIntTechBackup_elec")

    L102.load_segments <- get_data(all_data, "gcam-usa/dispatch/L102.load_segments")
    L102.invest_segments <- get_data(all_data, "gcam-usa/dispatch/L102.invest_segments")
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
    calibrated_techs_dispatch_usa <- get_data(all_data, "gcam-usa/calibrated_techs_dispatch_usa")


    #A vector indicate states where geothermal electric technologies will not be created
    NREL_us_re_technical_potential %>%
      left_join(states_subregions, by = c("State" = "state_name")) %>%
      filter(Geothermal_Hydrothermal_GWh == 0) %>%
      transmute(geo_state_noresource = paste(state, "geothermal", sep = " ")) %>%
      unlist ->
      geo_states_noresource

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
      left_join(bind_rows(A23.globaltech_OMfixed, A23.dispatch_globaltech_OMfixed_additional),
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
      left_join(bind_rows(A23.globaltech_OMvar, A23.dispatch_globaltech_OMvar_additional),
                by = c("supplysector", "subsector", "technology")) %>%
      filter(!is.na(input.OM.var)) %>% # TODO: double check why solar doesn't have OMvar
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
      left_join(bind_rows(A23.globaltech_capital, A23.dispatch_globaltech_capital_additional),
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
      filter(year %in% MODEL_YEARS)  ->
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
      right_join(A23.globaltech_co2capture) %>%
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

    # current assumption
    # capacity credit = -2.5 * capacity factor
    # 2.5 (1975$/GJ) basically comes from the levelized capital cost of a gas combustion turbine.
    # And the idea is that all electricity users contribute to this investment credit
    # (thus gets added on to the dispatch price) and the on the investment tech side it is
    # given as a credit / subsidy (with the intermittent techs getting a reduced credit).

    calibrated_techs_dispatch_usa %>%
      filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
      select(sector, supplysector, subsector, technology, capacity.factor) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      mutate(supplysector = sector) %>%
      mutate(minicam.non.energy.input = "capacity credit") %>%
      mutate(input.cost = -2.5 * capacity.factor) %>%
      select(supplysector, subsector, technology, year, minicam.non.energy.input, input.cost) ->
      L223.GlobalTechCost_Investment

    # ===========================================================================
    ## L223 GlobalTechHack_Investment  total hack investment
    # ===========================================================================

    # current assumption
    # total.hack = 1
    # TODO: check what does this mean

    calibrated_techs_dispatch_usa %>%
      filter(sector %in% gcamusa.ELEC_INV_NAMES) %>%
      select(sector, supplysector, subsector, technology) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      mutate(supplysector = sector) %>%
      select(-sector) %>%
      mutate(total.hack = 1) ->
      L223.GlobalTechHack_Investment

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
      right_join(L223.Sector_Investment, by=c("state" = "region")) %>%
      rename(subsector = state) %>% # not sure why subsector should be states
      rename(region = grid_region) %>%
      select(region, supplysector, subsector, logit.year.fillout, logit.exponent, logit.type)->
      L223.Subsector_Investment_StateShare

    # ===========================================================================
    ## L223.SubsectorShrwtFllt_Investment_StateShare subsector share.weigth normalized by 2010 capaicity shares in grid
    # ===========================================================================
    # Distribute State Investment share-weights within a grid region based on capacity distribution normalized to maximum in region
    # Normalize capacity by grid region

    # !!! note that here the capacity is in EJ (MW * hrs)
    L123.capacity_EJ_state_elec_F_tech %>%
      select(state, year, capacity) %>%
      filter(year == 2010) %>%
      group_by(state) %>%
      summarise(X2010 = sum(capacity)) %>%
      left_join_error_no_match(states_subregions %>% select(state, grid_region), by = "state") %>%
      group_by(grid_region) %>%
      mutate(grid_region_max = max(X2010),
             grid_region_norm = X2010/grid_region_max) %>%
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

    # TODO: check whether here the technology is named as "new"

    L223.SubsectorShrwtFllt_Investment_StateShare %>%
      select(region, supplysector, subsector) %>%
      mutate(technology = "new") %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      mutate(minicam.energy.input = supplysector) %>%
      mutate(coefficient = 1.0) %>%
      mutate(market.name = subsector) ->
      L223.TechCoef_Investment_StateShare

    # ===========================================================================
    ## L223.TechShrwt_Investment_StateShare technology shareweight
    # ===========================================================================

    L223.TechCoef_Investment_StateShare %>%
      select(region, supplysector, subsector, technology, year) %>%
      mutate(share.weight = 1.0) ->
      L223.TechShrwt_Investment_StateShare


    # ===========================================================================
    ## L223.Sector_Investment_LoadCurve sector load curve
    # ===========================================================================

    tibble(region = gcamusa.GRID_REGIONS,
           supplysector = "capacity investment",
           output.unit = "EJ",
           input.unit = "EJ",
           price.unit = "1975$/GJ",
           logit.year.fillout = MODEL_YEARS[1],
           logit.exponent = -3,
           logit.type = NA) %>%
      select(region, supplysector, output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent, logit.type) ->
      L223.Sector_Investment_LoadCurve

    # ===========================================================================
    ## L223.SubsectorLogit_Investment_LoadCurve subsector load curve
    # ===========================================================================

    L223.Sector_Investment_LoadCurve %>%
      mutate(subsector = supplysector) %>%
      select(region, supplysector, subsector, logit.year.fillout, logit.exponent, logit.type) ->
      L223.SubsectorLogit_Investment_LoadCurve

    # ===========================================================================
    ## L223.SectorUseTrialMarket_Investment_LoadCurve sector trial market
    # ===========================================================================

    L223.Sector_Investment_LoadCurve %>%
      select(region, supplysector) %>%
      mutate(use.trial.market = 1) ->
      L223.SectorUseTrialMarket_Investment_LoadCurve

    # ===========================================================================
    ## L223.SubsectorLogit_Investment_LoadCurve subsector shareweight
    ## L223.TechShrwt_Investment_LoadCurve technology shareweight
    # ===========================================================================

    L223.SubsectorLogit_Investment_LoadCurve %>%
      rename(year.fillout = logit.year.fillout) %>%
      rename(share.weight = logit.exponent) %>%
      select(-logit.type) %>%
      mutate(share.weight = 1.0) ->
      L223.SubsectorShrwtFllt_Investment_LoadCurve

    L223.SubsectorShrwtFllt_Investment_LoadCurve %>%
      rename(year = year.fillout) %>%
      complete(nesting(region, supplysector, subsector, share.weight), year = MODEL_YEARS, fill=list(share.weight = 1.0)) %>%
      mutate(technology = subsector) %>%
      select(region, supplysector, subsector, technology, year, share.weight) ->
      L223.TechShrwt_Investment_LoadCurve

    # ===========================================================================
    ## L223.TechCoef_Investment_LoadCurve  technlogy coefficient load curve
    # ===========================================================================

    L223.TechShrwt_Investment_LoadCurve %>%
      select(-share.weight) %>%
      left_join(L102.invest_segments, by=c("region" = "grid_region")) %>%
      mutate(coefficient = hours / 8760 * 0.8 * `generation-fraction`) %>%
      rename(minicam.energy.input = invest_segment) %>%
      select(-hours, -generation, -area, -`generation-fraction`) %>%
      mutate(market.name = region) ->
      L223.TechCoef_Investment_LoadCurve

    # ===========================================================================
    ## L223.TechPMult_Investment_LoadCurve  p multiplier load curve
    # ===========================================================================

    L223.TechCoef_Investment_LoadCurve %>%
      select(-market.name, -coefficient) %>%
      mutate(pMultiplier = 0.0) ->
      L223.TechPMult_Investment_LoadCurve

    # 2.5 1975$/GJ basically come from the levelized capital cost of a gas combustion turbine.
    # # And the idea is that all electricity users contribute to this investment credit
    # # (thus gets added on to the dispatch price) and the on the investment tech side it is
    # # given as a credit / subsidy (with the intermittent techs getting a reduced credit).

    L223.TechShrwt_Investment_LoadCurve %>%
      select(-share.weight) %>%
      mutate(minicam.non.energy.input = "non-energy") %>%
      mutate(input.cost = 2.5) ->
      L223.TechCost_Investment_LoadCurve

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
      mutate(share.weight = 1.0) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["TechShrwt"]]) ->
      L223.TechShrwt_Dispatch

    # technolgoy efficiency
    calibrated_techs_dispatch_usa %>%
      filter(sector == "electricity generation") %>%
      select(sector, supplysector, subsector, technology, minicam.energy.input) %>%
      # filter out hydro in this table as it does not have an input
      filter(!is.na(minicam.energy.input)) %>%
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

    # TODO: check later
    # if( use_regional_fuel_markets & "market.name" %in% names( L223.TechEff_Dispatch ) ){
    #   L223.TechEff_Dispatch$market.name[ L223.TechEff_Dispatch[[input]] %in% regional_fuel_markets ] <- states_subregions$grid_region[
    #     match( L223.TechEff_Dispatch$region[ L223.TechEff_Dispatch[[input]] %in% regional_fuel_markets], states_subregions$state ) ]
    # }

    # technology OM_fixed
    calibrated_techs_dispatch_usa %>%
      filter(sector == "electricity generation") %>%
      select(sector, supplysector, subsector, technology) %>%
      left_join(bind_rows(A23.globaltech_OMfixed, A23.dispatch_globaltech_OMfixed_additional),
                by = c("supplysector", "subsector", "technology")) %>%
      # filter out hydro in this table as it does not have an OMfixed
      filter(!is.na(input.OM.fixed)) %>%
      fill_exp_decay_extrapolate(MODEL_YEARS) %>%
      select(-sector) %>%
      rename(OM.fixed = value) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["TechOMfixed"]]) ->
      L223.TechOMfixed_Dispatch

    # technology OM_Var
    calibrated_techs_dispatch_usa %>%
      filter(sector == "electricity generation") %>%
      select(sector, supplysector, subsector, technology) %>%
      left_join(bind_rows(A23.globaltech_OMvar, A23.dispatch_globaltech_OMvar_additional),
                by = c("supplysector", "subsector", "technology")) %>%
      # TODO: double check why solar doesn't have OMvar
      filter(!is.na(input.OM.var)) %>%
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
    L114.CapacityFactor_wind_state_segment %>%
      bind_rows(L119.CapacityFactor_PV_state_segment, L119.CapacityFactor_CSP_state_segment) %>%
      select(-sector) %>%
      rename(technology = fuel, region = state) %>%
      spread(segment, capacity.factor) ->
      L223.renew_seg_cap_fac

    # all technology capacity factor by load segment
    L223.TechCapFac_Dispatch %>%
      filter(year >= FINAL_MODEL_BASE_YEARS) %>%
      filter(technology != "hydro") %>%
      left_join(L223.renew_seg_cap_fac, by=c("region", "technology")) %>%
      filter(!is.na(Jan_day) | year == FINAL_MODEL_BASE_YEARS) %>%
      mutate_at(vars(Apr_day:superpeak), funs(if_else(is.na(.), capacity.factor, .))) %>%
      select(-capacity.factor) %>%
      gather("segment", "capacity.factor", Apr_day:superpeak) ->
      L223.CapacityTechSegmentCapFac

    # carbon storage market and remove.fraction
    calibrated_techs_dispatch_usa %>%
      filter(sector == "electricity generation") %>%
      select(supplysector, subsector, technology) %>%
      right_join(A23.globaltech_co2capture) %>%
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
      L223.DispatchSectorDemandSegments

    # Calibration
    # calibrated capacity for electricity technology
    L123.capacity_EJ_state_elec_F_tech %>%
      filter(year %in% MODEL_YEARS) %>%
      left_join(calibrated_techs_dispatch_usa, by=c("gcam_fuel" = "fuel", "elec_tech" = "elec_tech")) %>%
      rename(region = state) %>%
      select(region, supplysector, subsector, technology, year, capacity) ->
      L223.CapacityTech

    # calibrated capacity factor for electricity technology
    L123.capacity_factor_EJ_state_elec_F_tech %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(capacity_factor = pmax(capacity_factor, 0.001)) %>%
      left_join(calibrated_techs_dispatch_usa, by=c("fuel" = "fuel", "elec_tech" = "elec_tech")) %>%
      rename(region = state) %>%
      select(region, supplysector, subsector, technology, year, capacity_factor) ->
      L223.TechCapFac_Cal

    # calibrated capacity factor for electricity technology
    L123.in_EJ_state_elec_F_tech %>%
      mutate(variable = "input") %>%
      bind_rows(mutate(L123.out_EJ_state_elec_F_tech, variable = "output")) %>%
      filter(fuel != "hydro") %>%
      filter(year %in% MODEL_YEARS) %>%
      spread(variable, value) %>%
      mutate(efficiency = output / input) %>%
      # TODO: these are renewables and nuclear
      filter(!is.na(efficiency)) %>%
      select(-input, -output) %>%
      left_join(calibrated_techs_dispatch_usa, by = c("sector", "fuel", "elec_tech")) %>%
      rename(region = state) %>%
      select(region, supplysector, subsector, technology, year, minicam.energy.input, efficiency) %>%
      mutate(market.name = if_else(minicam.energy.input %in%
                                     c(gcamusa.STATE_RENEWABLE_RESOURCES, "global solar resource"), region, "USA")) ->
      L223.TechEff_Cal

    # TODO: check later
    # if( use_regional_fuel_markets & "market.name" %in% names( L223.TechEff_Cal ) ){
    #   L223.TechEff_Cal$market.name[ L223.TechEff_Cal[[input]] %in% regional_fuel_markets ] <- states_subregions$grid_region[
    #     match( L223.TechEff_Cal$region[ L223.TechEff_Cal[[input]] %in% regional_fuel_markets], states_subregions$state ) ]
    # }

    # calibrated production for grid
    L123.out_EJ_state_elec_F_tech %>%
      left_join(select(states_subregions, state, grid_region), by = "state") %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(dispatchsector = "electricity") %>%
      select(grid_region, dispatchsector, year, value) %>%
      group_by(grid_region, dispatchsector, year) %>%
      summarize(cal_production = sum(value)) %>%
      ungroup() ->
      L223.DispatchSectorCalProd

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
    L223.TechShrwt_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechEff_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechOMfixed_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechOMvar_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechLifetime_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechSCurve_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))
    L223.TechCapFac_Dispatch %<>% filter(!(paste(region, subsector) %in% geo_states_noresource))

    # # TODO: new features, decide later how to implement
    # # currently all disabled
    #
    # # 1) DISABLED Feature 1
    # if(1==2) {
    #   # Adding in future hydropower generation here
    #   # L223.StubTechFixOut_hydro_USA: fixed output of future hydropower
    #   # TODO: This just holds it constant for now; at some point, should downscale of the (almost completely flat) nation-level projection
    #   L223.StubTechFixOut_hydro_USA <- repeat_and_add_vector(
    #     subset( L223.StubTechFixOut_elec_USA, grepl( "hydro", stub.technology ) & year == max( historical_years ) ),
    #     Y, model_future_years )
    #
    #   # L223.StubTechProd_elec_USA: calibrated output of electricity generation technologies
    #   L223.StubTechProd_elec_USA <- L223.calout_EJ_state_elec_F_tech[ c( names_StubTechYr, "calOutputValue" ) ]
    #   L223.StubTechProd_elec_USA$share.weight.year <- L223.StubTechProd_elec_USA$year
    #   L223.StubTechProd_elec_USA <- set_subsector_shrwt( L223.StubTechProd_elec_USA, value.name="calOutputValue" )
    #   L223.StubTechProd_elec_USA$share.weight <- ifelse( L223.StubTechProd_elec_USA$calOutputValue > 0, 1, 0 )
    #   L223.StubTechProd_elec_USA <- subset( L223.StubTechProd_elec_USA, !paste( region, subsector ) %in% geo_states_noresource )
    #
    #   # L223.StubTechMarket_elec_USA: market names of inputs to state electricity sectors
    #   L223.StubTechMarket_elec_USA <- repeat_and_add_vector( L223.StubTech_elec_USA, Y, model_years )
    #   L223.StubTechMarket_elec_USA[[input]] <- A23.globaltech_eff[[input]][
    #     match( vecpaste( L223.StubTechMarket_elec_USA[ c( supp, subs, "stub.technology" ) ] ),
    #            vecpaste( A23.globaltech_eff[ c( supp, subs, tech ) ] ) ) ]
    #   #Remove NA rows for hydro
    #   L223.StubTechMarket_elec_USA <- subset( L223.StubTechMarket_elec_USA, complete.cases( L223.StubTechMarket_elec_USA ) )
    #   L223.StubTechMarket_elec_USA$market.name <- "USA"
    #   L223.StubTechMarket_elec_USA$market.name[ L223.StubTechMarket_elec_USA[[input]] %in% c( state_renewable_resources, state_unlimited_resources ) ] <-
    #     L223.StubTechMarket_elec_USA$region[ L223.StubTechMarket_elec_USA[[input]] %in% c( state_renewable_resources, state_unlimited_resources ) ]
    #   L223.StubTechMarket_elec_USA <- subset( L223.StubTechMarket_elec_USA, !paste( region, subsector ) %in% geo_states_noresource )
    #
    #   if( use_regional_fuel_markets ){
    #     L223.StubTechMarket_elec_USA$market.name[ L223.StubTechMarket_elec_USA[[input]] %in% regional_fuel_markets ] <- states_subregions$grid_region[
    #       match( L223.StubTechMarket_elec_USA$region[ L223.StubTechMarket_elec_USA[[input]] %in% regional_fuel_markets ],
    #              states_subregions$state ) ]
    #   }
    # }
    #
    # # 2) DISABLED Feature 2
    # if(1==2) {
    #   # L223.StubTechMarket_backup_USA: market names of backup inputs to state electricity sectors
    #   L223.GlobalIntTechBackup_elec[ c( supp, subs ) ] <- L223.GlobalIntTechBackup_elec[ c( "sector.name", "subsector.name" ) ]
    #   L223.StubTechMarket_backup_USA <- repeat_and_add_vector( L223.GlobalIntTechBackup_elec[ c( s_s_t_i, Y ) ], reg, states )
    #   L223.StubTechMarket_backup_USA$market.name <- "USA"
    #   L223.StubTechMarket_backup_USA$stub.technology <- L223.StubTechMarket_backup_USA$technology
    #   L223.StubTechMarket_backup_USA <- L223.StubTechMarket_backup_USA[ names_StubTechMarket ]
    #
    #   #The backup electric market is only set here if regional electricity markets are not used (i.e., one national grid)
    #   if( !use_regional_elec_markets ){
    #     printlog( "L223.StubTechElecMarket_backup_USA: market name of electricity sector for backup calculations" )
    #     L223.StubTechElecMarket_backup_USA <- L223.StubTechMarket_backup_USA[ names_StubTechYr ]
    #     L223.StubTechElecMarket_backup_USA$electric.sector.market <- "USA"
    #   }
    #
    #   printlog( "L223.StubTechCapFactor_elec_wind_USA: capacity factors for wind electricity in the states" )
    #   #Just use the subsector for matching - technologies include storage technologies as well
    #   L114.CapacityFactor_wind_state[ c( supp, subs ) ] <- calibrated_techs[
    #     match( vecpaste( L114.CapacityFactor_wind_state[ S_F ] ),
    #            vecpaste( calibrated_techs[ S_F ] ) ),
    #     c( supp, subs ) ]
    #   L223.StubTechCapFactor_elec_wind_USA <- repeat_and_add_vector(
    #     subset( L223.StubTechCapFactor_elec, region == "USA" &
    #               paste( supplysector, subsector ) %in% vecpaste( L114.CapacityFactor_wind_state[ c( supp, subs ) ] ) ),
    #     reg, states )
    #   L223.StubTechCapFactor_elec_wind_USA$capacity.factor <- round(
    #     L114.CapacityFactor_wind_state$capacity.factor[
    #       match( vecpaste( L223.StubTechCapFactor_elec_wind_USA[ c( reg, supp, subs ) ] ),
    #              vecpaste( L114.CapacityFactor_wind_state[ c( state, supp, subs ) ] ) ) ],
    #     digits_capacity_factor )
    #   L223.StubTechCapFactor_elec_wind_USA <- L223.StubTechCapFactor_elec_wind_USA[ names_StubTechCapFactor ]
    #
    #   printlog( "L223.StubTechCapFactor_elec_solar_USA: capacity factors by state and solar electric technology" )
    #   L223.CapFacScaler_solar_state <- rbind( L119.CapFacScaler_PV_state, L119.CapFacScaler_CSP_state )
    #   L223.CapFacScaler_solar_state[ s_s_t ] <- calibrated_techs[
    #     match( vecpaste( L223.CapFacScaler_solar_state[ S_F ] ),
    #            vecpaste( calibrated_techs[ S_F ] ) ),
    #     s_s_t ]
    #   #Just use the subsector for matching - technologies include storage technologies as well
    #   L223.StubTechCapFactor_elec_solar_USA <- repeat_and_add_vector(
    #     subset( L223.StubTechCapFactor_elec, region == "USA" &
    #               paste( supplysector, subsector ) %in% vecpaste( L223.CapFacScaler_solar_state[ c( supp, subs ) ] ) ),
    #     reg, states )
    #   #For matching capacity factors to technologies, need to have a name that matches what's in the capacity factor table (which doesn't include storage techs)
    #   L223.StubTechCapFactor_elec_solar_USA$match_tech <- sub( "_storage", "", L223.StubTechCapFactor_elec_solar_USA$stub.technology )
    #   L223.StubTechCapFactor_elec_solar_USA$capacity.factor <- round(
    #     L223.StubTechCapFactor_elec_solar_USA$capacity.factor * L223.CapFacScaler_solar_state$scaler[
    #       match( vecpaste( L223.StubTechCapFactor_elec_solar_USA[ c( reg, supp, subs, "match_tech" ) ] ),
    #              vecpaste( L223.CapFacScaler_solar_state[ c( state, s_s_t ) ] ) ) ],
    #     digits_cost )
    #   L223.StubTechCapFactor_elec_solar_USA <- L223.StubTechCapFactor_elec_solar_USA[ names_StubTechCapFactor ]
    # }

    # -----------------------------------------------------------------------------
    # 3. produce output

    # TODO: will be replaced by add_node_equiv_xml, but need to check first keep these here for now

    # write_mi_data( L223.SectorNodeEquiv, "EQUIV_TABLE", "GCAMUSA_LEVEL2_DATA", "L223.SectorNodeEquiv", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )
    # write_mi_data( L223.TechNodeEquiv, "EQUIV_TABLE", "GCAMUSA_LEVEL2_DATA", "L223.TechNodeEquiv", "GCAMUSA_XML_BATCH", "batch_electricity_USA.xml" )

    # TODO: check later
    # all related to logit table
    # ----------------------------------------------------------------------------------------------------------------------------
    # for( curr_table in names ( L223.SectorLogitTables_Investment ) ) {
    #   write_mi_data( L223.SectorLogitTables_Investment[[ curr_table ]]$data, L223.SectorLogitTables_Investment[[ curr_table ]]$header,
    #                  "GCAMUSA_LEVEL2_DATA", paste0("L223.", L223.SectorLogitTables_Investment[[ curr_table ]]$header, "_Investment" ), "GCAMUSA_XML_BATCH",
    #                  "batch_electricity_USA.xml" )
    # }

    # for( curr_table in names ( L223.SubsectorLogitTables_Investment ) ) {
    #   write_mi_data( L223.SubsectorLogitTables_Investment[[ curr_table ]]$data, L223.SubsectorLogitTables_Investment[[ curr_table ]]$header,
    #                  "GCAMUSA_LEVEL2_DATA", paste0("L223.", L223.SubsectorLogitTables_Investment[[ curr_table ]]$header, "_Investment" ), "GCAMUSA_XML_BATCH",
    #                  "batch_electricity_USA.xml" )
    # }

    # for( curr_table in names ( L223.SectorLogitTables_Investment_StateShare ) ) {
    #   write_mi_data( L223.SectorLogitTables_Investment_StateShare[[ curr_table ]]$data, L223.SectorLogitTables_Investment_StateShare[[ curr_table ]]$header,
    #                  "GCAMUSA_LEVEL2_DATA", paste0("L223.", L223.SectorLogitTables_Investment_StateShare[[ curr_table ]]$header, "_Investment_StateShare" ), "GCAMUSA_XML_BATCH",
    #                  "batch_electricity_USA.xml" )
    # }

    # for( curr_table in names ( L223.SubsectorLogitTables_Investment_StateShare ) ) {
    #   write_mi_data( L223.SubsectorLogitTables_Investment_StateShare[[ curr_table ]]$data, L223.SubsectorLogitTables_Investment_StateShare[[ curr_table ]]$header,
    #                  "GCAMUSA_LEVEL2_DATA", paste0("L223.", L223.SubsectorLogitTables_Investment_StateShare[[ curr_table ]]$header, "_Investment_StateShare" ), "GCAMUSA_XML_BATCH",
    #                  "batch_electricity_USA.xml" )
    # }

    # for( curr_table in names ( L223.SectorLogitTables_Investment_LoadCurve ) ) {
    #   write_mi_data( L223.SectorLogitTables_Investment_LoadCurve[[ curr_table ]]$data, L223.SectorLogitTables_Investment_LoadCurve[[ curr_table ]]$header,
    #                  "GCAMUSA_LEVEL2_DATA", paste0("L223.", L223.SectorLogitTables_Investment_LoadCurve[[ curr_table ]]$header, "_Investment_LoadCurve" ), "GCAMUSA_XML_BATCH",
    #                  "batch_electricity_USA.xml" )
    # }

    # for( curr_table in names ( L223.SubsectorLogitTables_Investment_LoadCurve ) ) {
    #   write_mi_data( L223.SubsectorLogitTables_Investment_LoadCurve[[ curr_table ]]$data, L223.SubsectorLogitTables_Investment_LoadCurve[[ curr_table ]]$header,
    #                  "GCAMUSA_LEVEL2_DATA", paste0("L223.", L223.SubsectorLogitTables_Investment_LoadCurve[[ curr_table ]]$header, "_Investment_LoadCurve" ), "GCAMUSA_XML_BATCH",
    #                  "batch_electricity_USA.xml" )
    # }

    # for( curr_table in names ( L223.SectorLogitTables_Dispatch ) ) {
    #   write_mi_data( L223.SectorLogitTables_Dispatch[[ curr_table ]]$data, L223.SectorLogitTables_Dispatch[[ curr_table ]]$header,
    #                  "GCAMUSA_LEVEL2_DATA", paste0("L223.", L223.SectorLogitTables_Dispatch[[ curr_table ]]$header, "_Dispatch" ), "GCAMUSA_XML_BATCH",
    #                  "batch_electricity_USA.xml" )
    # }

    # for( curr_table in names ( L223.SubsectorLogitTables_Dispatch ) ) {
    #   write_mi_data( L223.SubsectorLogitTables_Dispatch[[ curr_table ]]$data, L223.SubsectorLogitTables_Dispatch[[ curr_table ]]$header,
    #                  "GCAMUSA_LEVEL2_DATA", paste0("L223.", L223.SubsectorLogitTables_Dispatch[[ curr_table ]]$header, "_Dispatch" ), "GCAMUSA_XML_BATCH",
    #                  "batch_electricity_USA.xml" )
    # }

    # for( curr_table in names ( L223.SectorLogitTables_Dispatch_Grid ) ) {
    #   write_mi_data( L223.SectorLogitTables_Dispatch_Grid[[ curr_table ]]$data, L223.SectorLogitTables_Dispatch_Grid[[ curr_table ]]$header,
    #                  "GCAMUSA_LEVEL2_DATA", paste0("L223.", L223.SectorLogitTables_Dispatch_Grid[[ curr_table ]]$header, "_Dispatch_Grid" ), "GCAMUSA_XML_BATCH",
    #                  "batch_electricity_USA.xml" )
    # }

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
                     "gcam-usa/NREL_us_re_technical_potential") ->
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
                     "gcam-usa/NREL_us_re_technical_potential") ->
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
                     "gcam-usa/dispatch/L102.invest_segments") ->
      L223.GlobalTechCapFac_Investment

    L223.TechCapFac_Investment %>%
      add_title("Investment technology capacity factor for wind PV CSP for state") %>%
      add_units("Unitless") %>%
      add_comments("Set technology capacity factor for wind PV CSP for state") %>%
      add_legacy_name("L223.TechCapFac_Investment (dispatch branch)") %>%
      add_precursors("L114.CapacityFactor_wind_state",
                     "L119.CapacityFactor_PV_state",
                     "L119.CapacityFactor_CSP_state",
                     "gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/dispatch/L102.invest_segments") ->
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
      add_comments("Assuming -2.5 * capacity factor") %>%
      add_legacy_name("L223.GlobalTechCost_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa") ->
      L223.GlobalTechCost_Investment

    L223.GlobalTechHack_Investment %>%
      add_title("Investment technology total-hack") %>%
      add_units("Unitless") %>%
      add_comments("Set technology total-hack") %>%
      add_legacy_name("L223.GlobalTechHack_Investment (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa") ->
      L223.GlobalTechHack_Investment

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
      add_comments("Set subsector (state) and technology (new) coefficients and market for grid") %>%
      add_legacy_name("L223.TechCoef_Investment_StateShare (dispatch branch)") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A23.dispatch_sector",
                     "L123.capacity_EJ_state_elec_F_tech") ->
      L223.TechCoef_Investment_StateShare

    L223.TechShrwt_Investment_StateShare %>%
      add_title("Investment subsector (state) and technology (new) share-weight for grid") %>%
      add_units("Unitless") %>%
      add_comments("Set subsector (state) and technology (new) share-weight for grid") %>%
      add_legacy_name("L223.TechShrwt_Investment_StateShare (dispatch branch)") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A23.dispatch_sector") ->
      L223.TechShrwt_Investment_StateShare

    L223.Sector_Investment_LoadCurve %>%
      add_title("Investment supplysector (capacity investment) logit-exponent for grid") %>%
      add_units("Unitless") %>%
      add_comments("Set supplysector (capacity investment) logit-exponent for grid") %>%
      add_legacy_name("L223.Sector_Investment_LoadCurve (dispatch branch)") %>%
      same_precursors_as("L223.Sector_Investment_LoadCurve") ->
      L223.Sector_Investment_LoadCurve

    L223.SectorUseTrialMarket_Investment_LoadCurve %>%
      add_title("Investment supplysector (capacity investment) use-trail-market for grid") %>%
      add_units("Unitless") %>%
      add_comments("Set supplysector (capacity investment) use-trail-market for grid") %>%
      add_legacy_name("L223.SectorUseTrialMarket_Investment_LoadCurve (dispatch branch)") %>%
      same_precursors_as("L223.SectorUseTrialMarket_Investment_LoadCurve") ->
      L223.SectorUseTrialMarket_Investment_LoadCurve

    L223.SubsectorLogit_Investment_LoadCurve %>%
      add_title("Investment supplysector and subsector (capacity investment) logit-exponent for grid") %>%
      add_units("Unitless") %>%
      add_comments("Set supplysector and subsector (capacity investment) logit-exponent for grid") %>%
      add_legacy_name("L223.SubsectorLogit_Investment_LoadCurve (dispatch branch)") %>%
      same_precursors_as("L223.SubsectorLogit_Investment_LoadCurve") ->
      L223.SubsectorLogit_Investment_LoadCurve

    L223.SubsectorShrwtFllt_Investment_LoadCurve %>%
      add_title("Investment supplysector and subsector (capacity investment) share-weight for grid") %>%
      add_units("Unitless") %>%
      add_comments("Set supplysector and subsector (capacity investment) share-weight for grid") %>%
      add_legacy_name("L223.SubsectorShrwtFllt_Investment_LoadCurve (dispatch branch)") %>%
      same_precursors_as("L223.SubsectorShrwtFllt_Investment_LoadCurve") ->
      L223.SubsectorShrwtFllt_Investment_LoadCurve

    L223.TechShrwt_Investment_LoadCurve %>%
      add_title("Investment supplysector/subsector/technology (capacity investment) share-weight for grid") %>%
      add_units("Unitless") %>%
      add_comments("Set supplysector/subsector/technology (capacity investment) share-weight for grid") %>%
      add_legacy_name("L223.TechShrwt_Investment_LoadCurve (dispatch branch)") %>%
      same_precursors_as("L223.TechShrwt_Investment_LoadCurve") ->
      L223.TechShrwt_Investment_LoadCurve

    L223.TechCoef_Investment_LoadCurve %>%
      add_title("Investment supplysector/subsector/technology (capacity investment) coefficient for grid") %>%
      add_units("Unitless") %>%
      add_comments("Set supplysector/subsector/technology (capacity investment) coefficient for grid") %>%
      add_legacy_name("L223.TechCoef_Investment_LoadCurve (dispatch branch)") %>%
      add_precursors("gcam-usa/dispatch/L102.invest_segments") ->
      L223.TechCoef_Investment_LoadCurve

    L223.TechPMult_Investment_LoadCurve %>%
      add_title("Investment supplysector/subsector/technology (capacity investment) pMultiplier for grid") %>%
      add_units("Unitless") %>%
      add_comments("Set supplysector/subsector/technology (capacity investment) pMultiplier for grid") %>%
      add_legacy_name("L223.TechPMult_Investment_LoadCurve (dispatch branch)") %>%
      add_precursors("gcam-usa/dispatch/L102.invest_segments") ->
      L223.TechPMult_Investment_LoadCurve

    L223.TechCost_Investment_LoadCurve %>%
      add_title("Investment supplysector/subsector/technology (capacity investment) input.cost for grid") %>%
      add_units("1975$/GJ") %>%
      add_comments("Set supplysector/subsector/technology (capacity investment) input.cost for grid") %>%
      add_legacy_name("L223.TechCost_Investment_LoadCurve (dispatch branch)") %>%
      add_precursors("gcam-usa/dispatch/L102.invest_segments") ->
      L223.TechCost_Investment_LoadCurve

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
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.CapacityTech_FutureTechs

    L223.CapacityTechSegmentCapFac %>%
      add_title("Dispatch technology capacity factor by segment for state") %>%
      add_units("Unitless") %>%
      add_comments("Set technology capacity factor by segment for state") %>%
      add_legacy_name("L223.CapacityTechSegmentCapFac (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "L114.CapacityFactor_wind_state_segment",
                     "L119.CapacityFactor_CSP_state_segment",
                     "L119.CapacityFactor_PV_state_segment") ->
      L223.CapacityTechSegmentCapFac

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
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.TechShrwt_Dispatch

    L223.TechEff_Dispatch %>%
      add_title("Dispatch technology efficiency for state") %>%
      add_units("Unitless") %>%
      add_comments("Set technology efficiency for state") %>%
      add_legacy_name("L223.TechEff_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "energy/A23.globaltech_eff",
                     "gcam-usa/A23.dispatch_globaltech_eff_additional") ->
      L223.TechEff_Dispatch

    L223.TechOMvar_Dispatch %>%
      add_title("Dispatch technology OM var for state") %>%
      add_units("1975$US/MWh") %>%
      add_comments("Set technology OM var for state") %>%
      add_legacy_name("L223.TechOMvar_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "energy/A23.globaltech_OMvar",
                     "gcam-usa/A23.dispatch_globaltech_OMvar_additional") ->
      L223.TechOMvar_Dispatch

    L223.TechLifetime_Dispatch %>%
      add_title("Dispatch technology lifetime for state") %>%
      add_units("yrs") %>%
      add_comments("Set technology lifetime for state") %>%
      add_legacy_name("L223.TechLifetime_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "energy/A23.globaltech_retirement",
                     "gcam-usa/A23.dispatch_globaltech_retirement_additional") ->
      L223.TechLifetime_Dispatch

    L223.TechSCurve_Dispatch %>%
      add_title("Dispatch technology lifetime steepness and half.life for state") %>%
      add_units("unitless") %>%
      add_comments("Set technology lifetime steepness and half.life for state") %>%
      add_legacy_name("L223.TechSCurve_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/NREL_us_re_technical_potential",
                     "energy/A23.globaltech_retirement",
                     "gcam-usa/A23.dispatch_globaltech_retirement_additional") ->
      L223.TechSCurve_Dispatch

    L223.TechCapFac_Dispatch %>%
      add_title("Dispatch technology capacity factor for state") %>%
      add_units("unitless") %>%
      add_comments("Set technology capacity factor for state") %>%
      add_legacy_name("L223.TechCapFac_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "gcam-usa/NREL_us_re_technical_potential") ->
      L223.TechCapFac_Dispatch

    L223.TechCarbonCapture_Dispatch %>%
      add_title("Dispatch CCS storage market and remove-fraction for state") %>%
      add_units("unitless") %>%
      add_comments("Set CCS storage market and remove-fraction for state") %>%
      add_legacy_name("L223.TechCarbonCapture_Dispatch (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "energy/A23.globaltech_co2capture") ->
      L223.TechCarbonCapture_Dispatch

    L223.TechCapFac_Cal %>%
      add_title("Dispatch technology calibrated capacity-factor for state") %>%
      add_units("unitless") %>%
      add_comments("Set technology calibrated capacity-factor for state") %>%
      add_legacy_name("L223.TechCapFac_Cal (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "L123.capacity_factor_EJ_state_elec_F_tech") ->
      L223.TechCapFac_Cal

    L223.TechEff_Cal %>%
      add_title("Dispatch technology calibrated efficiency for state") %>%
      add_units("unitless") %>%
      add_comments("Set technology calibrated efficiency for state") %>%
      add_legacy_name("L223.TechEff_Cal (dispatch branch)") %>%
      add_precursors("gcam-usa/calibrated_techs_dispatch_usa",
                     "L123.in_EJ_state_elec_F_tech",
                     "L123.out_EJ_state_elec_F_tech") ->
      L223.TechEff_Cal

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

    L223.DispatchSectorDemandSegments %>%
      add_title("Dispatch dispatch.sector (electricity) relative generation and fraction for grid") %>%
      add_units("unitless") %>%
      add_comments("Set dispatch.sector (electricity) relative generation and fraction for grid") %>%
      add_legacy_name("L223.DispatchSectorDemandSegments (dispatch branch)") %>%
      add_precursors("gcam-usa/dispatch/L102.load_segments") ->
      L223.DispatchSectorDemandSegments

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
                L223.GlobalTechHack_Investment,
                L223.Sector_Investment_StateShare,
                L223.Subsector_Investment_StateShare,
                L223.SubsectorShrwtFllt_Investment_StateShare,
                L223.TechCoef_Investment_StateShare,
                L223.TechShrwt_Investment_StateShare,
                L223.Sector_Investment_LoadCurve,
                L223.SectorUseTrialMarket_Investment_LoadCurve,
                L223.SubsectorLogit_Investment_LoadCurve,
                L223.SubsectorShrwtFllt_Investment_LoadCurve,
                L223.TechShrwt_Investment_LoadCurve,
                L223.TechCoef_Investment_LoadCurve,
                L223.TechPMult_Investment_LoadCurve,
                L223.TechCost_Investment_LoadCurve,
                L223.DispatchSector,
                L223.Sector_Dispatch,
                L223.SubsectorLogit_Dispatch,
                L223.SubsectorShrwtFllt_Dispatch,
                L223.CapacityTech_FutureTechs,
                L223.CapacityTechSegmentCapFac,
                L223.CapacityTech,
                L223.TechShrwt_Dispatch,
                L223.TechEff_Dispatch,
                L223.TechOMvar_Dispatch,
                L223.TechLifetime_Dispatch,
                L223.TechSCurve_Dispatch,
                L223.TechCapFac_Dispatch,
                L223.TechCarbonCapture_Dispatch,
                L223.TechCapFac_Cal,
                L223.TechEff_Cal,
                L223.Sector_Dispatch_Grid,
                L223.DispatchSectorCalProd,
                L223.DispatchSectorDemandSegments,
                L223.InterestRate_FERC,
                L223.Pop_FERC,
                L223.BaseGDP_FERC,
                L223.LaborForceFillout_FERC)
  } else {
    stop("Unknown command")
  }
}
