# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2237.wind_reeds_USA
#'
#' Create wind resource supply curves for USA states based on data from NREL ReEDS model.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2237.SmthRenewRsrcCurves_wind_reeds_USA},
#' \code{L2237.SmthRenewRsrcTechChange_wind_reeds_USA}, \code{L2237.StubTechCost_wind_reeds_USA},
#' \code{L2237.ResTechShrwt_wind_reeds_USA}.
#' The corresponding file in the original data system was \code{L2237.wind_reeds_USA.R} (gcam-usa level2).
#' @details Create state-level wind resource supply curves
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter lag mutate select row_number semi_join summarise_if group_by bind_rows
#' @importFrom tidyr gather spread
#' @author MTB September 2018 / YO April 2020

module_gcamusa_L2237.wind_reeds_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = 'gcam-usa/reeds_regions_states',
             FILE = 'gcam-usa/reeds_wind_curve_capacity',
             FILE = 'gcam-usa/reeds_wind_curve_CF_avg',
             FILE = 'gcam-usa/reeds_wind_curve_grid_cost',
             FILE = "gcam-usa/us_state_wind",
             "L113.globaltech_capital_ATB",
             "L114.CapacityFactor_wind_state_gcamusa"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c('L2237.RenewRsrc_wind_reeds_USA',
             'L2237.GrdRenewRsrcCurves_wind_reeds_USA',
             'L2237.StubTechCost_wind_reeds_USA',
             'L2237.ResTechShrwt_wind_reeds_USA'))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    reeds_regions_states <- get_data(all_data, 'gcam-usa/reeds_regions_states')
    reeds_wind_curve_capacity <- get_data(all_data, 'gcam-usa/reeds_wind_curve_capacity')
    reeds_wind_curve_CF_avg <- get_data(all_data, 'gcam-usa/reeds_wind_curve_CF_avg')
    reeds_wind_curve_grid_cost <- get_data(all_data, 'gcam-usa/reeds_wind_curve_grid_cost')
    us_state_wind <- get_data(all_data, "gcam-usa/us_state_wind")
    L113.globaltech_capital_ATB <- get_data(all_data, "L113.globaltech_capital_ATB")
    L114.CapacityFactor_wind_state_gcamusa <- get_data(all_data, "L114.CapacityFactor_wind_state_gcamusa")

    # Silence package checks
    region <- state <- states_list <- sector.name <- subsector.name <- intermittent.technology <-
      supplysector <- subsector <- stub.technology <- year <- input.capital <- capital.overnight <-
      fixed.charge.rate <- input.OM.fixed <- OM.fixed <- State <- TRG <- CF <- Wind.Class <- wsc1 <-
      wsc2 <- wsc3 <- wsc4 <- wsc5 <- Wind.Resource.Region <- resource.potential.MW <- resource.potential.EJ <-
      fcr <- price <- supply <- CFmax <- base.price <- maxSubResource <- percent.supply <- Pvar <- P2 <- P1 <- Q2 <-
      Q1 <- mid.price <- optimize <- curve.exponent <- k1 <- capital.tech.change.5yr <- k2 <- tech.change.5yr <-
      tech.change <- Wind.Type <- bin <- cost <- grid.cost <- Region <- renewresource <-
      smooth.renewable.subresource <- year.fillout <- capacity.factor <- input.cost <-
      capital.tech.change.period <- tech.change.period <- time.change <-
      subresource <- technology <- subsector_1 <- to.technology <- NULL

    # ===================================================
    # Data Processing

    # L2237.wind_CF: Capacity factor by state and wind class
    reeds_wind_curve_CF_avg %>%
      left_join_error_no_match(reeds_regions_states, by = c("Wind.Resource.Region" = "Region")) %>%
      select(State, Wind.Class = TRG, CF) %>%
      group_by(State, Wind.Class) %>%
      summarise_if(is.numeric, mean) %>%
      ungroup() -> L2237.wind_CF

    # L2237.wind_potential_EJ: Resource potential in EJ by state and class
    # We first calculate the resource potential in EJ in each ReEDS region and class using the
    # potential in MW with the average capacity factor for each region and class.
    # We then aggregate this to the state-level.
    reeds_wind_curve_capacity %>%
      mutate(resource.potential.MW = wsc1 + wsc2 + wsc3 + wsc4 + wsc5) %>%
      select(Wind.Resource.Region, Wind.Class, resource.potential.MW) %>%
      left_join_error_no_match(reeds_wind_curve_CF_avg, by = c("Wind.Resource.Region" , "Wind.Class" = "TRG")) %>%
      mutate(resource.potential.EJ = resource.potential.MW * CONV_YEAR_HOURS * CF * CONV_MWH_EJ) %>%
      left_join_error_no_match(reeds_regions_states, by = c("Wind.Resource.Region" = "Region")) %>%
      select(State, Wind.Class, resource.potential.EJ) %>%
      group_by(State, Wind.Class) %>%
      summarise_if(is.numeric, sum) %>%
      ungroup() -> L2237.wind_potential_EJ

    # join the capacity factors with the wind potential to create a graded supply curve
    L2237.wind_potential_EJ %>%
      left_join_error_no_match(L2237.wind_CF, by = c("State", "Wind.Class")) %>%
      # in order to have an upward sloping supply curve we will make the price 1 - capacity factor
      # We don't want "price" points too close together. Round price (capacity factor) to two digits
      # and summarize potential by region & price point.
      mutate(price = round(1.0 - CF, 2),
             renewresource = "onshore wind resource",
             sub.renewable.resource = "onshore wind resource") %>%
      group_by(region = State, renewresource, sub.renewable.resource, extractioncost = price) %>%
      summarise(available = sum(resource.potential.EJ)) %>%
      ungroup() %>%
      arrange(region, extractioncost) %>%
      group_by(region) %>%
      mutate(grade = paste0("grade ", row_number())) %>%
      ungroup() %>%
      select(region, renewresource, sub.renewable.resource, grade, available, extractioncost) ->
      wind_cf_curve

    # fall back to old data for states not included in the reeds data
    us_state_wind %>%
      filter(!region %in% unique(wind_cf_curve$region)) %>%
      left_join_error_no_match(L114.CapacityFactor_wind_state_gcamusa, by=c("region" = "state")) %>%
      # in order to have an upward sloping supply curve we will make the price 1 - capacity factor
      mutate(price = 1.0 - capacity.factor,
             renewresource = "onshore wind resource",
             sub.renewable.resource = "onshore wind resource",
             grade = "grade 1") %>%
      select(region, renewresource, sub.renewable.resource, grade, available = maxResource, extractioncost = price) %>%
      bind_rows(wind_cf_curve) ->
      wind_cf_curve

    # we need to add a "grade 0" and sub.renewable.resource expects the cumulative quantity
    # in the available for each grade
    wind_cf_curve %>%
      select(region, renewresource, sub.renewable.resource) %>%
      distinct() %>%
      mutate(grade="grade 0", available = 0, extractioncost = 0) %>%
      bind_rows(wind_cf_curve) %>%
      arrange(region, extractioncost) %>%
      group_by(region) %>%
      # sub.renewable.resource are shifted and implicitly anchored to zero, so we need to
      # shift the extraction cost accordingly and the top of the curve is always cost = 1 (aka zero capacity factor)
      mutate(extractioncost = lead(extractioncost, default = 1), available = cumsum(available)) %>%
      ungroup() ->
      L2237.GrdRenewRsrcCurves_wind_reeds_USA

    L2237.GrdRenewRsrcCurves_wind_reeds_USA %>%
      distinct(region, renewresource) %>%
      mutate(output.unit = "EJ",
             price.unit = "1975$/GJ",
             market = region) ->
      L2237.RenewRsrc_wind_reeds_USA

    L113.globaltech_capital_ATB %>%
      filter(technology == "wind") %>%
      pull(fixed.charge.rate) ->
      L2237.fcr

    # Grid connection costs are read in as fixed non-energy cost adders (in $/GJ) that vary by state.
    # Our starting data consists of grid connection costs in $/MW by ReEDS region and wind class.
    # This data also categorizes the connection cost into five bins in each region and class.
    # Using this data, we obtain a grid connection cost in $/GJ for each region/ class/ bin data
    # point as FCR * (grid connection cost in $/MW) / (CONV_YEAR_HOURS * CF * MWh_GJ).
    # Costs are then obtained for a state by averaging.
    # In the future, we might think about a separate state-level curve for grid connection costs.
    reeds_wind_curve_grid_cost %>%
      select(-Wind.Type) %>%
      gather(bin, cost, -Wind.Resource.Region, -Wind.Class) %>%
      filter(cost != 0) %>%
      left_join_error_no_match(reeds_wind_curve_CF_avg, by = c("Wind.Resource.Region", "Wind.Class" = "TRG")) %>%
      mutate(fcr = L2237.fcr,
             grid.cost = fcr * cost / (CONV_YEAR_HOURS * CF * CONV_MWH_GJ),
             grid.cost = grid.cost * gdp_deflator(1975, 2013)) %>%
      left_join_error_no_match(reeds_regions_states %>%
                                 select(Region, State),
                               by = c("Wind.Resource.Region" = "Region")) %>%
      group_by(State) %>%
      summarise(grid.cost = round(min(grid.cost), energy.DIGITS_COST)) %>%
      ungroup() -> L2237.grid.cost

    L2237.GrdRenewRsrcCurves_wind_reeds_USA %>%
      select(region, resource = renewresource, subresource = sub.renewable.resource) %>%
      unique() %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = subresource,
             share.weight = 1.0) %>%
      select(LEVEL2_DATA_NAMES[["ResTechShrwt"]]) ->
      L2237.ResTechShrwt_wind_reeds_USA


    # ===================================================
    # Produce outputs

    L2237.GrdRenewRsrcCurves_wind_reeds_USA %>%
      add_title("Wind Resource Supply Curve") %>%
      add_units("available: EJ; extractioncost: 1 - Capacity Factor (%)") %>%
      add_comments("Data from ReEDS") %>%
      add_precursors('gcam-usa/reeds_regions_states',
                     'gcam-usa/reeds_wind_curve_capacity',
                     'gcam-usa/reeds_wind_curve_CF_avg',
                     'gcam-usa/us_state_wind',
                     'L114.CapacityFactor_wind_state_gcamusa') ->
      L2237.GrdRenewRsrcCurves_wind_reeds_USA

    L2237.RenewRsrc_wind_reeds_USA %>%
      add_title("Wind Resource") %>%
      add_units("NA") %>%
      add_comments("Data from ReEDS") %>%
      same_precursors_as(L2237.GrdRenewRsrcCurves_wind_reeds_USA) ->
      L2237.RenewRsrc_wind_reeds_USA

    L2237.grid.cost %>%
      add_title("State-specific Grid Connection Cost Adders for Wind Power Technologies") %>%
      add_units("$1975/GJ") %>%
      add_comments("Data from ReEDS") %>%
      add_legacy_name("L2237.StubTechCost_wind_USA_reeds") %>%
      add_precursors('gcam-usa/reeds_regions_states',
                     'gcam-usa/reeds_wind_curve_CF_avg',
                     'gcam-usa/reeds_wind_curve_grid_cost',
                     'L113.globaltech_capital_ATB') ->
      L2237.StubTechCost_wind_reeds_USA

    L2237.ResTechShrwt_wind_reeds_USA %>%
      add_title("Technology share-weights for the renewable resources") %>%
      add_units("NA") %>%
      add_comments("Mostly just to provide a shell of a technology for the resource to use") %>%
      same_precursors_as(L2237.GrdRenewRsrcCurves_wind_reeds_USA) ->
      L2237.ResTechShrwt_wind_reeds_USA

    return_data(L2237.RenewRsrc_wind_reeds_USA,
                L2237.GrdRenewRsrcCurves_wind_reeds_USA,
                L2237.StubTechCost_wind_reeds_USA,
                L2237.ResTechShrwt_wind_reeds_USA)

  } else {
    stop("Unknown command")
  }
}
