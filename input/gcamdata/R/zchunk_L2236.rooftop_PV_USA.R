# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2236.rooftop_PV_USA
#'
#' Create rooftop PV technologies for GCAM-USA.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2236.Supplysector_RPV_USA}, \code{L2236.ElecReserve_RPV_USA},
#' \code{L2236.SubsectorLogit_RPV_USA}, \code{L2236.SubsectorShrwtFllt_RPV_USA},
#' \code{L2236.SubsectorInterpTo_RPV_USA}, \code{L2236.StubTech_RPV_USA}, \code{L2236.StubTechCapFactor_RPV_USA},
#' \code{L2236.StubTechMarket_RPV_USA}, \code{L2236.StubTechElecMarket_RPV_USA}.
#' @details Create rooftop PV technologies for GCAM-USA
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter lag mutate select semi_join
#' @importFrom tidyr complete nesting
#' @author MTB September 2018
module_gcamusa_L2236.rooftop_PV_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             "L223.Supplysector_elec",
             "L223.ElecReserve",
             "L223.SubsectorLogit_elec",
             "L223.SubsectorShrwtFllt_elec",
             "L223.SubsectorInterpTo_elec",
             "L223.StubTech_elec",
             "L223.GlobalIntTechEff_elec",
             "L223.StubTechCapFactor_elec",
             "L223.TechCapFac_Dispatch"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2236.Supplysector_RPV_USA",
             "L2236.ElecReserve_RPV_USA",
             "L2236.SubsectorLogit_RPV_USA",
             "L2236.SubsectorShrwtFllt_RPV_USA",
             "L2236.SubsectorInterpTo_RPV_USA",
             "L2236.StubTech_RPV_USA",
             "L2236.StubTechCapFactor_RPV_USA",
             "L2236.StubTechMarket_RPV_USA",
             "L2236.StubTechElecMarket_RPV_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    L223.Supplysector_elec <- get_data(all_data, "L223.Supplysector_elec")
    L223.ElecReserve <- get_data(all_data, "L223.ElecReserve")
    L223.SubsectorLogit_elec <- get_data(all_data, "L223.SubsectorLogit_elec")
    L223.SubsectorShrwtFllt_elec <- get_data(all_data, "L223.SubsectorShrwtFllt_elec")
    L223.SubsectorInterpTo_elec <- get_data(all_data, "L223.SubsectorInterpTo_elec")
    L223.StubTech_elec <- get_data(all_data, "L223.StubTech_elec")
    L223.GlobalIntTechEff_elec <- get_data(all_data, "L223.GlobalIntTechEff_elec")
    L223.StubTechCapFactor_elec <- get_data(all_data, "L223.StubTechCapFactor_elec")
    L223.TechCapFac_Dispatch <- get_data(all_data, "L223.TechCapFac_Dispatch")

    # Silence package checks
    subsector <- year <- fixedOutput <- state <- EIA <- EIA_ratio <- fixedOutput_2015 <-
      AEO <- AEO_2015_ratio <- region <- supplysector <- stub.technology <-
      share.weight.year <- subs.share.weight <- tech.share.weight <-
      technology <- subsector_1 <- to.technology <- NULL

    # ===================================================
    # Data Processing

    # Function to copy elect_td_bld / rooftop_pv info from USA region to states
    PV_to_states <- function(data, names){
      data %>%
        filter(supplysector == gcamusa.ROOFTOP_PV_SECTOR,
               region == gcam.USA_REGION) %>%
        write_to_all_states(names, gcamusa.STATES)
    }

    # Sector information
    L2236.Supplysector_RPV_USA <- PV_to_states(L223.Supplysector_elec, LEVEL2_DATA_NAMES[["Supplysector"]]) %>%
      mutate(logit.exponent = gcamusa.DEFAULT_LOGITEXP,
             logit.type = NA)

    L2236.ElecReserve_RPV_USA <- PV_to_states(L223.ElecReserve, LEVEL2_DATA_NAMES[["ElecReserve"]])

    # Subsector information
    L2236.SubsectorLogit_RPV_USA <- PV_to_states(L223.SubsectorLogit_elec, LEVEL2_DATA_NAMES[["SubsectorLogit"]]) %>%
      mutate(logit.exponent = gcamusa.DEFAULT_LOGITEXP,
             logit.type = NA)

    L2236.SubsectorShrwtFllt_RPV_USA <- PV_to_states(L223.SubsectorShrwtFllt_elec, LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]])
    L2236.SubsectorInterpTo_RPV_USA <- PV_to_states(L223.SubsectorInterpTo_elec, LEVEL2_DATA_NAMES[["SubsectorInterpTo"]])

    # Technology information
    L2236.StubTech_RPV_USA <- PV_to_states(L223.StubTech_elec, LEVEL2_DATA_NAMES[["StubTech"]])

    # Capacity factor.  Scale USA average RPV capacity factor to states based on ratio of
    # state PV (utility scale) capacity facotr to USA average PV capacity factor
    USA_PV_CF <- L223.StubTechCapFactor_elec %>%
      filter(region == gcam.USA_REGION,
             stub.technology == "PV") %>%
      distinct(capacity.factor) %>%
      pull()

    L223.TechCapFac_Dispatch %>%
      filter(technology == "PV") %>%
      mutate(US_PV_CF = USA_PV_CF,
             CF_ratio = capacity.factor / US_PV_CF) %>%
      distinct(region, CF_ratio) ->
      L2236.RPV_CF_scaler

    L2236.StubTechCapFactor_RPV_USA <- PV_to_states(L223.StubTechCapFactor_elec, LEVEL2_DATA_NAMES[["StubTechCapFactor"]]) %>%
      # DC doesn't have utility scale PV and thus is not included in L2236.RPV_CF_scaler
      # LJENM will throw an error, so use left_join()
      left_join(L2236.RPV_CF_scaler, by = "region") %>%
      # replace NA capacity factor scalers with 1
      replace_na(list(CF_ratio = 1)) %>%
      mutate(capacity.factor = capacity.factor * CF_ratio) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCapFactor"]])

    # Market information for distributed_solar resource and backup market
    L2236.StubTechMarket_RPV_USA <- L223.GlobalIntTechEff_elec %>%
      filter(sector.name == gcamusa.ROOFTOP_PV_SECTOR) %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = intermittent.technology) %>%
      # placeholder market name
      mutate(market.name = gcam.USA_REGION) %>%
      write_to_all_states(LEVEL2_DATA_NAMES[["StubTechMarket"]], gcamusa.STATES) %>%
      # set market name - state for distributed_solar resource, USA for backup market
      mutate(market.name = if_else(minicam.energy.input == "distributed_solar", region, gcam.USA_REGION))

    # Calibrated production
    # There is no calibrated RPV production at the USA level, so assign 0 production to all states
    L2236.StubTech_RPV_USA %>%
      repeat_add_columns(tibble::tibble(year = MODEL_BASE_YEARS)) %>%
      mutate(calOutputValue = 0,
             share.weight.year = year,
             subs.share.weight = 0,
             tech.share.weight = 0) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2236.StubTechProd_RPV

    # Last bit of market info for backup
    L2236.StubTech_RPV_USA %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      left_join_error_no_match(states_subregions %>%
                                 select(state, grid_region), by = c("region" = "state"))  %>%
      mutate(electric.sector.market = grid_region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechElecMarket"]]) ->
      L2236.StubTechElecMarket_RPV_USA


    # ===================================================
    # Produce outputs

    L2236.Supplysector_RPV_USA %>%
      add_title("Supply sector information for elect_td_bld sector") %>%
      add_units("Unitless") %>%
      add_comments("Written to all states from L223.Supplysector_elec") %>%
      add_precursors("L223.Supplysector_elec") ->
      L2236.Supplysector_RPV_USA

    L2236.ElecReserve_RPV_USA %>%
      add_title("Electricity reserve margins and grid capacity factors by state") %>%
      add_units("unitless") %>%
      add_comments("Reserve margin: Average fraction of capacity reserved for backup") %>%
      add_comments("Grid Capacity: Conversion factor of grid capacity to output for entire grid") %>%
      add_precursors("L223.ElecReserve") ->
      L2236.ElecReserve_RPV_USA

    L2236.SubsectorLogit_RPV_USA %>%
      add_title("Subsector logit exponents for rooftop_pv subsector") %>%
      add_units("Unitless") %>%
      add_comments("Written to all states from L223.SubsectorLogit_elec") %>%
      add_precursors("L223.SubsectorLogit_elec") ->
      L2236.SubsectorLogit_RPV_USA

    L2236.SubsectorShrwtFllt_RPV_USA %>%
      add_title("Subsector shareweights for rooftop_pv subsector") %>%
      add_units("Unitless") %>%
      add_comments("Written to all states from L223.SubsectorShrwtFllt_elec") %>%
      add_precursors("L223.SubsectorShrwtFllt_elec") ->
      L2236.SubsectorShrwtFllt_RPV_USA

    L2236.SubsectorInterpTo_RPV_USA %>%
      add_title("Interpolation rules using a to.value for rooftop_pv subsectors") %>%
      add_units("unitless") %>%
      add_comments("Written to all states from L223.SubsectorInterpTo_elec") %>%
      add_precursors("L223.SubsectorInterpTo_elec") ->
      L2236.SubsectorInterpTo_RPV_USA

    L2236.StubTech_RPV_USA %>%
      add_title("Stub technologies for rooftop_pv") %>%
      add_units("unitless") %>%
      add_comments("Written to all states from L223.StubTech_elec") %>%
      add_precursors("L223.StubTech_elec") ->
      L2236.StubTech_RPV_USA

    L2236.StubTechCapFactor_RPV_USA %>%
      add_title("Capacity factors of rooftop_pv technologies (state-level)") %>%
      add_units("unitless fraction") %>%
      add_comments("USA average rooftop PV capacity factor scaled to states on the basis of state:USA utility scale PV capacity factor ratio") %>%
      add_precursors("L223.StubTechCapFactor_elec",
                     "L223.TechCapFac_Dispatch") ->
      L2236.StubTechCapFactor_RPV_USA

    L2236.StubTechMarket_RPV_USA %>%
      add_title("Energy Inputs for rooftop_pv stub technologies") %>%
      add_units("NA") %>%
      add_comments("Energy inputs and market names - including resource input and backup_electricity - for rooftop_pv technologies") %>%
      add_precursors("L223.GlobalIntTechEff_elec") ->
      L2236.StubTechMarket_RPV_USA

    L2236.StubTechElecMarket_RPV_USA%>%
      add_title("Rooftop_pv sector name for backup markets") %>%
      add_units("NA") %>%
      add_comments("Backup market sector names for intermittent rooftop_pv technologies") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L223.StubTech_elec") ->
      L2236.StubTechElecMarket_RPV_USA

    return_data(L2236.Supplysector_RPV_USA,
                L2236.ElecReserve_RPV_USA,
                L2236.SubsectorLogit_RPV_USA,
                L2236.SubsectorShrwtFllt_RPV_USA,
                L2236.SubsectorInterpTo_RPV_USA,
                L2236.StubTech_RPV_USA,
                L2236.StubTechCapFactor_RPV_USA,
                L2236.StubTechMarket_RPV_USA,
                L2236.StubTechElecMarket_RPV_USA)

  } else {
    stop("Unknown command")
  }
}
