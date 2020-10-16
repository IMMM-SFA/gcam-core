# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2231.nonewcoal_USA
#'
#' Generates optional moratorium on new pulverized coal plants in USA states.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2231.SubsectorShrwt_elec_nonewcoal_USA}, \code{L2231.StubTechShrwt_nonewcoal_USA},
#' \code{L2231.SubsectorShrwt_elec_coal_delay_USA}, \code{L2231.StubTechShrwt_coal_delay_USA}.
#' The corresponding file in the
#' original data system was \code{L2231.nonewcoal_USA.R} (gcam-usa level2).
#' @details This chunk sets zero share-weights of pulverized coal technologies, which assumes
#' no new pulverized coal plants without CCS will be built in USA states.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join distinct filter mutate select
#' @author RC Aug 2018
module_gcamusa_L2231.nonewcoal_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L223.SubsectorShrwtFllt_Investment",
             "L222.StubTechMarket_en_USA",
             "L232.StubTechMarket_ind_USA",
             "L222.StubTech_en",
             "L225.StubTech_h2"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2231.SubsectorShrwt_elec_nonewcoal_USA",
             "L2231.StubTechShrwt_nonewcoal_USA",
             "L2231.SubsectorShrwt_elec_coal_delay_USA",
             "L2231.StubTechShrwt_coal_delay_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    region <- year <- supplysector <- subsector <- stub.technology <- share.weight <-
      Electric.sector.technology <- Electric.sector <- subsector0 <- NULL  # silence package check notes

    # Load required inputs
    L223.SubsectorShrwtFllt_Investment <- get_data(all_data, "L223.SubsectorShrwtFllt_Investment")
    L222.StubTechMarket_en_USA <- get_data(all_data, "L222.StubTechMarket_en_USA")
    L232.StubTechMarket_ind_USA <- get_data(all_data, "L232.StubTechMarket_ind_USA")
    L222.StubTech_en <- get_data(all_data, "L222.StubTech_en")
    L225.StubTech_h2 <- get_data(all_data, "L225.StubTech_h2")


    # ===================================================
    # Perform computations

    L223.SubsectorShrwtFllt_Investment %>%
      # Get the conventional coal technology without CCS
      filter(subsector0 == "coal", !grepl("CCS", subsector)) %>%
      # bind_rows(tibble(supplysector = "industrial energy use", subsector = "coal", stub.technology = "coal cogen")) %>%
      # distinct(region, supplysector, subsector0, subsector) %>%
      select(-year.fillout, -share.weight) %>%
      distinct() %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      mutate(share.weight = 0) ->
      L2231.SubsectorShrwt_elec_nonewcoal_USA

    L232.StubTechMarket_ind_USA %>%
      filter(stub.technology == "coal cogen") %>%
      select(-minicam.energy.input, market.name) %>%
      bind_rows(L222.StubTechMarket_en_USA %>%
                  filter(subsector == "coal to liquids", !grepl("CCS",stub.technology))) %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      distinct(region, supplysector, subsector, stub.technology, year) %>%
      mutate(share.weight = 0) ->
      L2231.StubTechShrwt_ind_ref_coal_USA

    L222.StubTech_en %>%
      filter(region == gcam.USA_REGION, subsector == "coal gasification", !grepl("CCS",stub.technology)) %>%
      bind_rows(L225.StubTech_h2 %>%
                  filter(region == gcam.USA_REGION, subsector == "coal", !grepl("CCS",stub.technology))) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      mutate(share.weight = 0) %>%
      select(LEVEL2_DATA_NAMES[["StubTechShrwt"]]) ->
      L2231.StubTechShrwt_en_coal_USA

    bind_rows(L2231.StubTechShrwt_ind_ref_coal_USA,
              L2231.StubTechShrwt_en_coal_USA) ->
      L2231.StubTechShrwt_nonewcoal_USA

    # Coal delay
    L2231.SubsectorShrwt_elec_nonewcoal_USA %>%
      filter(year <= gcamusa.FIRST_NEW_COAL_YEAR) ->
      L2231.SubsectorShrwt_elec_coal_delay_USA

    L2231.StubTechShrwt_nonewcoal_USA %>%
      filter(year <= gcamusa.FIRST_NEW_COAL_YEAR) ->
      L2231.StubTechShrwt_coal_delay_USA


    # ===================================================
    # Produce outputs

    L2231.SubsectorShrwt_elec_nonewcoal_USA %>%
      add_title("Power sector share-weights for coal without CCS in USA states") %>%
      add_units("Unitless") %>%
      add_comments("Set zero share-weights for coal without CCS in all USA states and future years") %>%
      add_legacy_name("L2231.SubsectorShrwt_nonewcoal_elecS_cool_USA") %>%
      add_precursors("L223.SubsectorShrwtFllt_Investment") ->
      L2231.SubsectorShrwt_elec_nonewcoal_USA

    L2231.StubTechShrwt_nonewcoal_USA %>%
      add_title("Share-weights for coal without CCS in USA states") %>%
      add_units("Unitless") %>%
      add_comments("Set zero share-weights for coal without CCS in all USA states and future years") %>%
      add_legacy_name("L2231.StubTechShrwt_coal_USA") %>%
      add_precursors("L232.StubTechMarket_ind_USA",
                     "L222.StubTechMarket_en_USA",
                     "L222.StubTech_en",
                     "L225.StubTech_h2") ->
      L2231.StubTechShrwt_nonewcoal_USA

    L2231.SubsectorShrwt_elec_coal_delay_USA %>%
      add_title("Power sector share-weights for coal without CCS in USA states") %>%
      add_units("Unitless") %>%
      add_comments("Set zero share-weights for coal without CCS in all USA states for near future") %>%
      add_comments("New coal power deployment can begin in gcamusa.FIRST_NEW_COAL_YEAR (see constants.R; default is 2035) ") %>%
      add_legacy_name("L2231.SubsectorShrwt_coal_delay_elecS_cool_USA") %>%
      same_precursors_as("L2231.SubsectorShrwt_elec_nonewcoal_USA") ->
      L2231.SubsectorShrwt_elec_coal_delay_USA

    L2231.StubTechShrwt_coal_delay_USA %>%
      add_title("Share-weights for coal without CCS in USA states") %>%
      add_units("Unitless") %>%
      add_comments("Set zero share-weights for coal without CCS in all USA states for near future") %>%
      add_comments("New coal power deployment can begin in gcamusa.FIRST_NEW_COAL_YEAR (see constants.R; default is 2035) ") %>%
      same_precursors_as("L2231.StubTechShrwt_nonewcoal_USA") ->
      L2231.StubTechShrwt_coal_delay_USA

    return_data(L2231.SubsectorShrwt_elec_nonewcoal_USA,
                L2231.StubTechShrwt_nonewcoal_USA,
                L2231.SubsectorShrwt_elec_coal_delay_USA,
                L2231.StubTechShrwt_coal_delay_USA)
  } else {
    stop("Unknown command")
  }
}
