# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_rooftop_PV_USA_xml
#'
#' Construct XML data structure for \code{rooftop_PV_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{rooftop_PV_USA.xml}.
module_gcamusa_batch_rooftop_PV_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2236.Supplysector_RPV_USA",
             "L2236.ElecReserve_RPV_USA",
             "L2236.SubsectorLogit_RPV_USA",
             "L2236.SubsectorShrwtFllt_RPV_USA",
             "L2236.SubsectorInterpTo_RPV_USA",
             "L2236.StubTech_RPV_USA",
             "L2236.StubTechCapFactor_RPV_USA",
             "L2236.StubTechMarket_RPV_USA",
             "L2236.StubTechElecMarket_RPV_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "rooftop_PV_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    technology <- stub.technology <- NULL  # silence package check notes

    # Load required inputs
    L2236.Supplysector_RPV_USA <- get_data(all_data, "L2236.Supplysector_RPV_USA")
    L2236.ElecReserve_RPV_USA <- get_data(all_data, "L2236.ElecReserve_RPV_USA")
    L2236.SubsectorLogit_RPV_USA <- get_data(all_data, "L2236.SubsectorLogit_RPV_USA")
    L2236.SubsectorShrwtFllt_RPV_USA <- get_data(all_data, "L2236.SubsectorShrwtFllt_RPV_USA")
    L2236.SubsectorInterpTo_RPV_USA <- get_data(all_data, "L2236.SubsectorInterpTo_RPV_USA")
    L2236.StubTech_RPV_USA <- get_data(all_data, "L2236.StubTech_RPV_USA")
    L2236.StubTechCapFactor_RPV_USA <- get_data(all_data, "L2236.StubTechCapFactor_RPV_USA")
    L2236.StubTechMarket_RPV_USA <- get_data(all_data, "L2236.StubTechMarket_RPV_USA")
    L2236.StubTechElecMarket_RPV_USA <- get_data(all_data, "L2236.StubTechElecMarket_RPV_USA")

    # ===================================================

    # Produce outputs
    create_xml("rooftop_PV_USA.xml") %>%
      add_logit_tables_xml(L2236.Supplysector_RPV_USA, "Supplysector") %>%
      add_xml_data(L2236.ElecReserve_RPV_USA, "ElecReserve") %>%
      add_logit_tables_xml(L2236.SubsectorLogit_RPV_USA, "SubsectorLogit") %>%
      add_xml_data(L2236.SubsectorShrwtFllt_RPV_USA, "SubsectorShrwtFllt") %>%
      add_xml_data(L2236.SubsectorInterpTo_RPV_USA, "SubsectorInterpTo") %>%
      add_xml_data(L2236.StubTech_RPV_USA, "StubTech") %>%
      add_xml_data(L2236.StubTechCapFactor_RPV_USA, "StubTechCapFactor") %>%
      add_xml_data(L2236.StubTechMarket_RPV_USA, "StubTechMarket") %>%
      add_xml_data(L2236.StubTechElecMarket_RPV_USA, "StubTechElecMarket") %>%
      add_precursors("L2236.Supplysector_RPV_USA",
                     "L2236.ElecReserve_RPV_USA",
                     "L2236.SubsectorLogit_RPV_USA",
                     "L2236.SubsectorShrwtFllt_RPV_USA",
                     "L2236.SubsectorInterpTo_RPV_USA",
                     "L2236.StubTech_RPV_USA",
                     "L2236.StubTechCapFactor_RPV_USA",
                     "L2236.StubTechMarket_RPV_USA",
                     "L2236.StubTechElecMarket_RPV_USA") ->
      rooftop_PV_USA.xml

    return_data(rooftop_PV_USA.xml)
  } else {
    stop("Unknown command")
  }
}
