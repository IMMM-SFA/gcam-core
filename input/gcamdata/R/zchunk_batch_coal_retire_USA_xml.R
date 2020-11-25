# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_coal_retire_USA_xml
#'
#' Construct XML data structure for \code{coal_retire_vintage_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{coal_retire_USA.xml}. The corresponding file in the
#' original data system was \code{batch_coal_retire_USA.xml} (gcamusa xml-batch).
#' the generated outputs: \code{coal_retire_vintage_USA.xml}.
module_gcamusa_batch_coal_retire_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2241.Production_coalret_vintage_dispatch_gcamusa",
             "L2241.CapacityTech_coalret_vintage_dispatch_gcamusa",
             "L2241.TechEff_coalret_vintage_dispatch_gcamusa",
             "L2241.TechSCurve_coalret_vintage_dispatch_gcamusa",
             "L2241.TechShrwt_coalret_vintage_dispatch_gcamusa",
             "L2241.TechOMfixed_coalret_vintage_dispatch_gcamusa",
             "L2241.TechOMvar_coalret_vintage_dispatch_gcamusa",
             "L2241.CapacityTechMinCapFac_coalret_vintage_dispatch_gcamusa",
             "L2241.TechProfitShutdown_coalret_vintage_dispatch_gcamusa",
             "L2241.TechCapFac_coalret_vintage_dispatch_gcamusa",
             "L2241.CapacityTechAvail_coalret_vintage_dispatch_gcamusa",
             "L2241.TechCoef_cool_coalret_vintage_dispatch_gcamusa"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "coal_retire_vintage_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # silence package check notes
    tech.share.weight <- share.weight <- sector.name <- supplysector <- subsector.name <- subsector <- NULL

    # Load required inputs
    L2241.Production_coalret_vintage_dispatch_gcamusa <- get_data(all_data, "L2241.Production_coalret_vintage_dispatch_gcamusa")
    L2241.CapacityTech_coalret_vintage_dispatch_gcamusa <- get_data(all_data, "L2241.CapacityTech_coalret_vintage_dispatch_gcamusa")
    L2241.TechEff_coalret_vintage_dispatch_gcamusa <- get_data(all_data, "L2241.TechEff_coalret_vintage_dispatch_gcamusa")
    L2241.TechSCurve_coalret_vintage_dispatch_gcamusa <- get_data(all_data, "L2241.TechSCurve_coalret_vintage_dispatch_gcamusa")
    L2241.TechShrwt_coalret_vintage_dispatch_gcamusa <- get_data(all_data, "L2241.TechShrwt_coalret_vintage_dispatch_gcamusa")
    L2241.TechOMfixed_coalret_vintage_dispatch_gcamusa <- get_data(all_data, "L2241.TechOMfixed_coalret_vintage_dispatch_gcamusa")
    L2241.TechOMvar_coalret_vintage_dispatch_gcamusa <- get_data(all_data, "L2241.TechOMvar_coalret_vintage_dispatch_gcamusa")
    L2241.CapacityTechMinCapFac_coalret_vintage_dispatch_gcamusa <- get_data(all_data, "L2241.CapacityTechMinCapFac_coalret_vintage_dispatch_gcamusa")
    L2241.TechProfitShutdown_coalret_vintage_dispatch_gcamusa <- get_data(all_data, "L2241.TechProfitShutdown_coalret_vintage_dispatch_gcamusa")
    L2241.TechCapFac_coalret_vintage_dispatch_gcamusa <- get_data(all_data, "L2241.TechCapFac_coalret_vintage_dispatch_gcamusa")
    L2241.CapacityTechAvail_coalret_vintage_dispatch_gcamusa <- get_data(all_data, "L2241.CapacityTechAvail_coalret_vintage_dispatch_gcamusa")
    L2241.TechCoef_cool_coalret_vintage_dispatch_gcamusa <- get_data(all_data, "L2241.TechCoef_cool_coalret_vintage_dispatch_gcamusa")

    # Produce outputs
    create_xml("coal_retire_vintage_USA.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L2241.CapacityTech_coalret_vintage_dispatch_gcamusa, "CapacityTech") %>%
      add_xml_data(L2241.Production_coalret_vintage_dispatch_gcamusa, "Production") %>%
      add_xml_data(L2241.TechEff_coalret_vintage_dispatch_gcamusa, "TechEff") %>%
      add_xml_data(L2241.TechSCurve_coalret_vintage_dispatch_gcamusa, "TechSCurve") %>%
      add_xml_data(L2241.TechShrwt_coalret_vintage_dispatch_gcamusa, "TechShrwt") %>%
      add_xml_data(L2241.TechOMfixed_coalret_vintage_dispatch_gcamusa, "TechOMfixed") %>%
      add_xml_data(L2241.TechOMvar_coalret_vintage_dispatch_gcamusa, "TechOMvar") %>%
      add_xml_data(L2241.CapacityTechMinCapFac_coalret_vintage_dispatch_gcamusa, "CapacityTechMinCapFac") %>%
      add_xml_data(L2241.TechProfitShutdown_coalret_vintage_dispatch_gcamusa, "StubTechProfitShutdown") %>%
      add_xml_data(L2241.TechCapFac_coalret_vintage_dispatch_gcamusa, "TechCapFac") %>%
      add_xml_data(L2241.CapacityTechAvail_coalret_vintage_dispatch_gcamusa, "TechAvail") %>%
      add_xml_data(L2241.TechCoef_cool_coalret_vintage_dispatch_gcamusa, "TechCoef") %>%
      add_precursors("L2241.Production_coalret_vintage_dispatch_gcamusa",
                     "L2241.CapacityTech_coalret_vintage_dispatch_gcamusa",
                     "L2241.TechEff_coalret_vintage_dispatch_gcamusa",
                     "L2241.TechSCurve_coalret_vintage_dispatch_gcamusa",
                     "L2241.TechShrwt_coalret_vintage_dispatch_gcamusa",
                     "L2241.TechOMfixed_coalret_vintage_dispatch_gcamusa",
                     "L2241.TechOMvar_coalret_vintage_dispatch_gcamusa",
                     "L2241.CapacityTechMinCapFac_coalret_vintage_dispatch_gcamusa",
                     "L2241.TechProfitShutdown_coalret_vintage_dispatch_gcamusa",
                     "L2241.TechCapFac_coalret_vintage_dispatch_gcamusa",
                     "L2241.CapacityTechAvail_coalret_vintage_dispatch_gcamusa",
                     "L2241.TechCoef_cool_coalret_vintage_dispatch_gcamusa") ->
      coal_retire_vintage_USA.xml

    return_data(coal_retire_vintage_USA.xml)
  } else {
    stop("Unknown command")
  }
}
