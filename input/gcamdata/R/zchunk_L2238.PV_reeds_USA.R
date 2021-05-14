# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2238.PV_reeds_USA
#'
#' Create updated solar PV resource supply curves consistent with ReEDS.
#' Also add non-ReEDS states (AK,DC,HI) based on NREL technical potential data.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2238.DeleteStubTechMinicamEnergyInput_investment_PV_reeds_USA},
#' \code{L2238.DeleteInput_dispatch_PV_reeds_USA}, \code{L2238.RenewRsrc_PV_reeds_USA},
#' \code{L2238.GrdRenewRsrcCurves_PV_reeds_USA}, \code{L2238.GrdRenewRsrcMax_PV_reeds_USA},
#' \code{L2238.StubTechEffFlag_investment_PV_reeds_USA}, \code{L2238.TechEff_Dispatch_PV_reeds_USA},
#' \code{L2238.RenewRsrcTechChange_PV_reeds_USA}, and \code{L2238.StubTechCost_PV_reeds_USA},
#' \code{L2238.ResTechShrwt_PV_reeds_USA}, \code{L2238.CapacityTechInputPMult_dispatch_PV_reeds_USA}.
#' The corresponding file in the original data system was \code{L2238.PV_reeds_USA.R} (gcam-usa level2).
#' @details Create state-level solar PV resource supply curves
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author MTB September 2018 / AJS June 2019 / YO April 2020

module_gcamusa_L2238.PV_reeds_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = 'gcam-usa/states_subregions',
             FILE = 'gcam-usa/reeds_regions_states',
             FILE = 'gcam-usa/reeds_PV_curve_capacity',
             FILE = 'gcam-usa/reeds_PV_curve_CF_avg',
             FILE = 'gcam-usa/reeds_PV_curve_grid_cost',
             FILE = 'gcam-usa/non_reeds_PV_grid_cost',
             FILE = 'gcam-usa/NREL_us_re_technical_potential',
             FILE = 'gcam-usa/NREL_us_re_capacity_factors',
             FILE = "gcam-usa/A10.renewable_resource_delete",
             "L113.globaltech_capital_ATB"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2238.RenewRsrc_PV_reeds_USA",
             "L2238.GrdRenewRsrcCurves_PV_reeds_USA",
             "L2238.GrdRenewRsrcMax_PV_reeds_USA",
             "L2238.StubTechCost_PV_reeds_USA",
             "L2238.ResTechShrwt_PV_reeds_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, 'gcam-usa/states_subregions')
    reeds_regions_states <- get_data(all_data, 'gcam-usa/reeds_regions_states')
    reeds_PV_curve_capacity <- get_data(all_data, 'gcam-usa/reeds_PV_curve_capacity')
    reeds_PV_curve_CF_avg <- get_data(all_data, 'gcam-usa/reeds_PV_curve_CF_avg')
    reeds_PV_curve_grid_cost <- get_data(all_data, 'gcam-usa/reeds_PV_curve_grid_cost')
    non_reeds_PV_grid_cost <- get_data(all_data, 'gcam-usa/non_reeds_PV_grid_cost')
    NREL_us_re_technical_potential <- get_data(all_data, 'gcam-usa/NREL_us_re_technical_potential')
    NREL_us_re_capacity_factors <- get_data(all_data, 'gcam-usa/NREL_us_re_capacity_factors')
    A10.renewable_resource_delete <- get_data(all_data, "gcam-usa/A10.renewable_resource_delete")
    L113.globaltech_capital_ATB <- get_data(all_data, "L113.globaltech_capital_ATB")

    # Silence package checks
    region <- state <- states_list <- sector.name <- subsector.name <- intermittent.technology <-
      supplysector <- subsector <- stub.technology <- year <- input.capital <- capital.overnight <-
      fixed.charge.rate <- input.OM.fixed <- OM.fixed <- BA <- State <- PV.class <- CF <- upvsc1 <-
      upvsc2 <- upvsc3 <- upvsc4 <- upvsc5 <- resource.potential.MW <- resource.potential.EJ <-
      fcr <- price <- Pmin <- Pvar <- CFmax <- available <- grade <- extractioncost <-
      maxSubResource <- k1 <- capital.tech.change.5yr <- k2 <- tech.change.5yr <- tech.change <-
      bin <- cost <- grid.cost <- renewresource <- sub.renewable.resource <- year.fillout <-
      minicam.energy.input <- efficiency <- market.name <- flag <- capacity.factor <-
      input.cost <- capital.tech.change.period <- tech.change.period <- time.change <-
      subresource <- Urban_Utility_scale_PV_GWh <- Rural_Utility_scale_PV_GWh <- Urban_Utility_scale_PV <-
      Rural_Utility_scale_PV <- Total_Utility_scale_PV_GWh <- resource <- value <- non_reeds_state <-
      technology <- subsector_1 <- to.technology <- NULL

    # ===================================================
    # Data Processing

    # First, process the states not included in the REEDS data, so they can be easily merged into the ReEDS data
    # and associated processing pipeline

    # L2238.non_reeds_states: Create a list of states not in the ReEDS data
    reeds_regions_states %>%
      distinct(State) -> reeds_states

    states_subregions %>%
      select(State=state) %>%
      anti_join(reeds_states, by = "State") %>%
      pull(State) -> L2238.non_reeds_states

    # L2238.non_reeds_states_PV_technical_potential : Total technical potential (urban + rural)
    # for non-ReEDS states from the NREL RE Technical Potential Database
    NREL_us_re_technical_potential %>%
      # semi-join states_subregions to filter out "TOTAL" row
      semi_join(states_subregions, by = c("State" = "state_name")) %>%
      left_join_error_no_match(states_subregions, by = c("State" = "state_name")) %>%
      select(State = state, Urban_Utility_scale_PV_GWh, Rural_Utility_scale_PV_GWh) %>%
      filter(State %in% L2238.non_reeds_states) %>%
      mutate(Total_Utility_scale_PV_GWh = Urban_Utility_scale_PV_GWh + Rural_Utility_scale_PV_GWh) ->
      L2238.non_reeds_states_PV_technical_potential

    # L2238.non_reeds_states_PV_capacity_factor - combined (weighted for urban / rural potential contribution)
    # capacity factor for non-ReEDS states from the NREL RE Capacity Factor Database
    NREL_us_re_capacity_factors %>%
      # semi-join states_subregions to filter out "TOTAL" row
      semi_join(states_subregions, by = c("State" = "state_name")) %>%
      left_join_error_no_match(states_subregions, by = c("State" = "state_name")) %>%
      select(State = state, Urban_Utility_scale_PV, Rural_Utility_scale_PV) %>%
      filter(State %in% L2238.non_reeds_states) %>%
      left_join_error_no_match(L2238.non_reeds_states_PV_technical_potential, by = "State") %>%
      mutate(CF = Urban_Utility_scale_PV * (Urban_Utility_scale_PV_GWh / Total_Utility_scale_PV_GWh) +
               Rural_Utility_scale_PV * (Rural_Utility_scale_PV_GWh / Total_Utility_scale_PV_GWh)) %>%
      select(State, CF) -> L2238.non_reeds_states_PV_capacity_factor

    # L2238.PV_potential_EJ_non_reeds_states: NREL data set does not include resource class, which is needed for data processing below
    # Create a Dummy Class for Resource Potential - assume a Class 4 resource, which is the lowest starting class for most other states
    # Also convert GWh to EJ values
    L2238.non_reeds_states_PV_technical_potential %>%
      select(State, Total_Utility_scale_PV_GWh) %>%
      mutate(PV.class = "class4",
             resource.potential.EJ = Total_Utility_scale_PV_GWh * CONV_GWH_EJ) %>%
      select(-Total_Utility_scale_PV_GWh) -> L2238.PV_potential_EJ_non_reeds_states

    # L2238.PV_CF_non_reeds_states: Process data for capacity factor.
    # The class data will be joined from the potential table in order to create a capacity factor table by state and class
    L2238.non_reeds_states_PV_capacity_factor %>%
      left_join_error_no_match(L2238.PV_potential_EJ_non_reeds_states, by = "State") %>%
      select(State, PV.class, CF) -> L2238.PV_CF_non_reeds_states


    # Second, process ReEDS data and combine with data from other states

    # L2238.PV_CF: Capacity factor by state and PV class
    reeds_PV_curve_CF_avg %>%
      left_join_error_no_match(reeds_regions_states %>%
                                 distinct(BA, State),
                               by = "BA") %>%
      select(State, PV.class, CF ) %>%
      group_by(State, PV.class) %>%
      summarise_if(is.numeric, mean) %>%
      ungroup() -> L2238.PV_CF

    # Merge capacity factor data from non-ReEDS states
    L2238.PV_CF %>%
      bind_rows(L2238.PV_CF_non_reeds_states) -> L2238.PV_CF

    # L2238.PV_potential_EJ: Resource potential in EJ by state and class
    # We first calculate the resource potential in EJ in each ReEDS region and class using the
    # potential in MW with the average capacity factor for each region and class.
    # We then aggregate this to the state-level.
    reeds_PV_curve_capacity %>%
      mutate(resource.potential.MW = upvsc1 + upvsc2 + upvsc3 + upvsc4 + upvsc5) %>%
      select(BA, PV.class, resource.potential.MW) %>%
      left_join_error_no_match(reeds_PV_curve_CF_avg, by = c("BA", "PV.class")) %>%
      mutate(resource.potential.EJ = resource.potential.MW * CONV_YEAR_HOURS * CF * CONV_MWH_EJ) %>%
      left_join_error_no_match(reeds_regions_states %>%
                                 distinct(BA, State),
                               by = "BA") %>%
      select(State, PV.class, resource.potential.EJ) %>%
      group_by(State, PV.class) %>%
      summarise_if(is.numeric, sum) %>%
      ungroup() -> L2238.PV_potential_EJ

    # Merge potential data from non-ReEDS states
    L2238.PV_potential_EJ %>%
      bind_rows(L2238.PV_potential_EJ_non_reeds_states) -> L2238.PV_potential_EJ

    # join the capacity factors with the PV potential to create a graded supply curve
    L2238.PV_CF %>%
      left_join_error_no_match(L2238.PV_potential_EJ, by = c("State", "PV.class")) %>%
      # in order to have an upward sloping supply curve we will make the price 1 - capacity factor
      # We don't want "price" points too close together. Round price (capacity factor) to three digits
      # and summarize potential by region & price point.
      mutate(price = round(1.0 - CF, 3),
             renewresource = "PV_resource",
             sub.renewable.resource = "PV_resource") %>%
      group_by(region = State, renewresource, sub.renewable.resource, extractioncost = price) %>%
      summarise(available = sum(resource.potential.EJ)) %>%
      ungroup() %>%
      arrange(region, extractioncost) %>%
      group_by(region) %>%
      mutate(grade = paste0("grade ", row_number())) %>%
      ungroup() %>%
      select(region, renewresource, sub.renewable.resource, grade, available, extractioncost) ->
      pv_cf_curve

    # we need to add a "grade 0" and sub.renewable.resource expects the cumulative quantity
    # in the available for each grade
    pv_cf_curve %>%
      select(region, renewresource, sub.renewable.resource) %>%
      distinct() %>%
      mutate(grade="grade 0", available = 0, extractioncost = 0) %>%
      bind_rows(pv_cf_curve) %>%
      arrange(region, extractioncost) %>%
      group_by(region) %>%
      # sub.renewable.resource are shifted and implicitly anchored to zero, so we need to
      # shift the extraction cost accordingly and the top of the curve is always cost = 1 (aka zero capacity factor)
      mutate(extractioncost = lead(extractioncost, default = 1), available = cumsum(available)) %>%
      ungroup() ->
      L2238.GrdRenewRsrcCurves_PV_reeds_USA

    # Calculating maxSubResource for the graded renewable resource supply curve
    L2238.GrdRenewRsrcCurves_PV_reeds_USA %>%
      select(region, renewresource, sub.renewable.resource) %>%
      distinct() %>%
      mutate(year.fillout = min(MODEL_YEARS),
             maxSubResource = 1) -> L2238.GrdRenewRsrcMax_PV_reeds_USA

    L113.globaltech_capital_ATB %>%
      filter(technology == "PV") %>%
      pull(fixed.charge.rate) ->
      L2238.fcr

    # Grid connection costs are read in as fixed non-energy cost adders (in $/GJ) that vary by state.
    # Our starting data comprises of grid connection costs in $/MW by ReEDS region and PV class.
    # This data also categorizes the connection cost into five bins in each region and class.
    # We first calculate the minimum cost for a region and class.
    # Using this data, we then obtain grid connection cost in $/GJ for each region and class as
    # FCR * (grid connection cost in $/MW) / (CONV_YEAR_HOURS * CF * MWh_GJ).
    # Costs are then obtained for a state by taking the minimum.
    # In the future, we might think about a separate state-level curve for grid connection costs.
    reeds_PV_curve_grid_cost %>%
      gather(bin, cost, -BA, -PV.class) %>%
      group_by(BA, PV.class) %>%
      summarise(cost = min(cost[cost>0])) %>%
      ungroup() %>%
      left_join_error_no_match(reeds_PV_curve_CF_avg, by = c("BA", "PV.class")) %>%
      mutate(fcr = L2238.fcr,
             grid.cost = fcr * cost / (CONV_YEAR_HOURS * CF * CONV_MWH_GJ),
             grid.cost = grid.cost * gdp_deflator(1975, 2005))  %>%
      left_join_error_no_match(reeds_regions_states %>%
                                 distinct(BA, State),
                               by = "BA") %>%
      select(State, BA, PV.class, grid.cost) %>%
      group_by(State) %>%
      summarise(grid.cost = min(grid.cost)) %>%
      ungroup() %>%
      mutate(grid.cost = round(grid.cost, energy.DIGITS_COST)) -> L2238.grid_cost

    # L2238.grid_cost_non_reeds_states: Calculate grid costs for non-ReEDS states
    # grid costs based on mapping to states with similar geography or grid costs
    # these assumptions can be changed in gcam-usa/non_reeds_PV_grid_cost
    non_reeds_PV_grid_cost %>%
      left_join_error_no_match(L2238.grid_cost, by = c("comparison_state" = "State")) %>%
      select(State = non_reeds_state, grid.cost) -> L2238.grid_cost_non_reeds_states

    # Bind tables
    L2238.grid_cost %>%
      bind_rows(L2238.grid_cost_non_reeds_states) -> L2238.grid_cost

    # Format tables for output
    # Table to read in renewresource, output.unit, price.unit and market
    L2238.GrdRenewRsrcCurves_PV_reeds_USA %>%
      distinct(region, renewresource) %>%
      mutate(output.unit = "EJ",
             price.unit = "1975$/GJ",
             market = region) ->
      L2238.RenewRsrc_PV_reeds_USA

    # Establishing shareweights
    L2238.GrdRenewRsrcMax_PV_reeds_USA %>%
      select(region, resource = renewresource, subresource = sub.renewable.resource) %>%
      unique() %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = subresource,
             share.weight = 1.0) %>%
      # Utility-scale (i.e. non-rooftop) solar is assumed to be infeasible in DC.
      # Thus, it should not be assigned "PV_resource".
      # Use anti_join to remove it from the table.
      anti_join(A10.renewable_resource_delete, by = c("region", "resource" = "resource_elec_subsector")) %>%
      select(LEVEL2_DATA_NAMES[["ResTechShrwt"]]) ->
      L2238.ResTechShrwt_PV_reeds_USA


    # ===================================================
    # Produce outputs

    L2238.RenewRsrc_PV_reeds_USA %>%
      add_title("Market Information for Solar PV Resources") %>%
      add_units("NA") %>%
      add_comments("Applies to all states") %>%
      add_legacy_name("L2238.RenewRsrc_PV_USA_reeds") %>%
      add_precursors('gcam-usa/reeds_regions_states',
                     'gcam-usa/states_subregions',
                     'gcam-usa/reeds_PV_curve_capacity',
                     'gcam-usa/reeds_PV_curve_CF_avg',
                     'gcam-usa/NREL_us_re_technical_potential',
                     'gcam-usa/NREL_us_re_capacity_factors',
                     'gcam-usa/A10.renewable_resource_delete') ->
      L2238.RenewRsrc_PV_reeds_USA

    L2238.GrdRenewRsrcCurves_PV_reeds_USA %>%
      add_title("Graded Supply Curves of Solar PV Resources at the State-Level") %>%
      add_units("available: fraction of maxSubResource; extractioncost: $1975/GJ") %>%
      add_comments("Data from ReEDS") %>%
      add_legacy_name("L2238.GrdRenewRsrcCurves_PV_USA_reeds") %>%
      same_precursors_as("L2238.RenewRsrc_PV_reeds_USA") ->
      L2238.GrdRenewRsrcCurves_PV_reeds_USA

    L2238.GrdRenewRsrcMax_PV_reeds_USA %>%
      add_title("Maximum Subresource Availability for Solar PV Resources at the State-Level") %>%
      add_units("EJ") %>%
      add_comments("Each grade represents the (cumulative) fraction of maxSubResource available at a given price") %>%
      add_comments("Data from ReEDS") %>%
      add_legacy_name("L2238.GrdRenewRsrcMax_PV_USA_reeds") %>%
      same_precursors_as("L2238.RenewRsrc_PV_reeds_USA") ->
      L2238.GrdRenewRsrcMax_PV_reeds_USA

    L2238.grid_cost %>%
      add_title("State-specific Grid Connection Cost Adders for Solar PV Technologies") %>%
      add_units("$1975/GJ") %>%
      add_comments("Data from ReEDS") %>%
      add_legacy_name("L2238.StubTechCost_PV_USA_reeds") %>%
      add_precursors('gcam-usa/reeds_regions_states',
                     'gcam-usa/states_subregions',
                     'gcam-usa/reeds_PV_curve_capacity',
                     'gcam-usa/reeds_PV_curve_CF_avg',
                     'gcam-usa/NREL_us_re_technical_potential',
                     'gcam-usa/NREL_us_re_capacity_factors',
                     'gcam-usa/reeds_PV_curve_grid_cost',
                     'gcam-usa/non_reeds_PV_grid_cost',
                     'L113.globaltech_capital_ATB') ->
      L2238.StubTechCost_PV_reeds_USA

    L2238.ResTechShrwt_PV_reeds_USA %>%
      add_title("Technology share-weights for the renewable resources") %>%
      add_units("NA") %>%
      add_comments("Mostly just to provide a shell of a technology for the resource to use") %>%
      same_attributes_as(L2238.GrdRenewRsrcMax_PV_reeds_USA) ->
      L2238.ResTechShrwt_PV_reeds_USA


    return_data(L2238.RenewRsrc_PV_reeds_USA,
                L2238.GrdRenewRsrcCurves_PV_reeds_USA,
                L2238.GrdRenewRsrcMax_PV_reeds_USA,
                L2238.StubTechCost_PV_reeds_USA,
                L2238.ResTechShrwt_PV_reeds_USA)

  } else {
    stop("Unknown command")
  }
}
