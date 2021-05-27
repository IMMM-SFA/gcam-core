# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_ccs_supply_low_xml
#'
#' Construct XML data structure for \code{ccs_supply_low.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ccs_supply_low.xml}. The corresponding file in the
#' original data system was \code{batch_ccs_supply_low.xml.R} (energy XML).
module_energy_batch_ccs_supply_low_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L261.RsrcCurves_C",
             "L261.RsrcCurves_C_low",
             "L261.RsrcCurves_FERC",
             FILE = "gcam-usa/states_subregions"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ccs_supply_low.xml",
             XML = "ccs_supply_low_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L261.RsrcCurves_C <- get_data(all_data, "L261.RsrcCurves_C")
    L261.RsrcCurves_C_low <- get_data(all_data, "L261.RsrcCurves_C_low")
    L261.RsrcCurves_FERC <- get_data(all_data, "L261.RsrcCurves_FERC")
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")

    # ===================================================

    # Produce outputs
    create_xml("ccs_supply_low.xml") %>%
      add_xml_data(L261.RsrcCurves_C_low, "RsrcCurves") %>%
      add_precursors("L261.RsrcCurves_C_low") ->
      ccs_supply_low.xml

    # Produce outputs for USA States
    # Get Ratio of US National change
    L261.RsrcCurves_C %>%
      filter(region=="USA") %>%
      left_join_error_no_match(L261.RsrcCurves_C_low %>%
                                 filter(region=="USA") %>%
                                 rename(available_x = available, extractioncost_x = extractioncost)) %>%
      mutate(available_ratio = if_else(available_x==available, 1, available_x/available),
             extractioncost_ratio = if_else(extractioncost_x==extractioncost,1,extractioncost_x/extractioncost)) %>%
      select(-extractioncost_x, - extractioncost, -available_x, -available, -region)->
      L261.RsrcCurves_C_Ratios


    L261.RsrcCurves_C_low_USA <- L261.RsrcCurves_FERC  %>%
      left_join_error_no_match(L261.RsrcCurves_C_Ratios) %>%
      mutate(available = available * available_ratio,
             extractioncost = extractioncost * extractioncost_ratio) %>%
      select(-available_ratio,-extractioncost_ratio)


    create_xml("ccs_supply_low_USA.xml") %>%
      add_xml_data(L261.RsrcCurves_C_low_USA, "RsrcCurves") %>%
      add_precursors("L261.RsrcCurves_C_low", "L261.RsrcCurves_FERC", "L261.RsrcCurves_C", "gcam-usa/states_subregions") ->
      ccs_supply_low_USA.xml

    return_data(ccs_supply_low.xml, ccs_supply_low_USA.xml)
  } else {
    stop("Unknown command")
  }
}

