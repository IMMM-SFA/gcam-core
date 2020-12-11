# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_liquids_limits_usa_xml
#'
#' Construct XML data structure for \code{liquids_limits.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{liquids_limits_USA.xml}.
module_gcamusa_batch_liquids_limits_usa_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L270.CreditMkt_USA",
              "L270.CreditOutput_USA",
              "L270.CapacityTechYr_LiqLim_Dispatch",
              "L270.GlobalTechCoef_LiqLim_Investment",
              "L270.TechCoef_LiqLim_Dispatch"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "liquids_limits_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L270.CreditMkt_USA <- get_data(all_data, "L270.CreditMkt_USA")
    L270.CreditOutput_USA <- get_data(all_data, "L270.CreditOutput_USA")
    L270.GlobalTechCoef_LiqLim_Investment <- get_data(all_data, "L270.GlobalTechCoef_LiqLim_Investment")
    L270.CapacityTechYr_LiqLim_Dispatch <- get_data(all_data, "L270.CapacityTechYr_LiqLim_Dispatch")
    L270.TechCoef_LiqLim_Dispatch <- get_data(all_data, "L270.TechCoef_LiqLim_Dispatch")


    # ===================================================

    # Produce outputs
    create_xml("liquids_limits_USA.xml") %>%
      add_xml_data(L270.CreditMkt_USA, "PortfolioStd") %>%
      add_xml_data(L270.CreditOutput_USA, "GlobalTechRESSecOut") %>%
      add_xml_data(L270.GlobalTechCoef_LiqLim_Investment, "GlobalTechCoef") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("subsector") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L270.CapacityTechYr_LiqLim_Dispatch, "CapacityTechYr") %>%
      add_xml_data(L270.TechCoef_LiqLim_Dispatch, "TechCoef") %>%
      add_xml_data(L270.CreditOutput_USA, "GlobalTechRESSecOut") %>%
      add_precursors("L270.CreditMkt_USA",
                     "L270.CreditOutput_USA",
                     "L270.GlobalTechCoef_LiqLim_Investment",
                     "L270.CapacityTechYr_LiqLim_Dispatch",
                     "L270.TechCoef_LiqLim_Dispatch") ->
      liquids_limits_USA.xml

    return_data(liquids_limits_USA.xml)
  } else {
    stop("Unknown command")
  }
}
