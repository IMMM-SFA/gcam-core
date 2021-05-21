# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_elec_hydro_USA_xml
#'
#' Construct XML data structure for \code{elec_hydro_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{elec_hydro_USA.xml}.
#' The corresponding file in the original data system was \code{batch_elec_hydro_USA.xml} (gcamusa XML batch).
module_gcamusa_batch_elec_hydro_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2242.CapacityTech_hydro_future",
             "L2242.TechLifetime_hydro",
             "L2242.TechCapFac_hydro",
             "L2242.CapacityTechSegmentCapFac_hydro",
             "L2242.UnlimitRsrcPrice_hydro"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "elec_hydro_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    technology <- stub.technology <- NULL  # silence package check notes

    # Load required inputs
    L2242.CapacityTech_hydro_future <- get_data(all_data, "L2242.CapacityTech_hydro_future")
    L2242.TechLifetime_hydro <- get_data(all_data, "L2242.TechLifetime_hydro")
    L2242.TechCapFac_hydro <- get_data(all_data, "L2242.TechCapFac_hydro")
    L2242.CapacityTechSegmentCapFac_hydro <- get_data(all_data, "L2242.CapacityTechSegmentCapFac_hydro")
    L2242.UnlimitRsrcPrice_hydro <- get_data(all_data, "L2242.UnlimitRsrcPrice_hydro")

    # ===================================================

    # Produce outputs
    create_xml("elec_hydro_USA.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L2242.CapacityTech_hydro_future, "CapacityTech") %>%
      add_xml_data(L2242.TechCapFac_hydro, "TechCapFac")  %>%
      add_xml_data(L2242.TechLifetime_hydro, "TechLifetime") %>%
      add_xml_data(L2242.CapacityTechSegmentCapFac_hydro, "CapacityTechSegmentCapFac") %>%
      add_xml_data(L2242.UnlimitRsrcPrice_hydro, "UnlimitRsrcPrice") %>%
      add_precursors("L2242.CapacityTech_hydro_future",
                     "L2242.TechLifetime_hydro",
                     "L2242.TechCapFac_hydro",
                     "L2242.CapacityTechSegmentCapFac_hydro",
                     "L2242.UnlimitRsrcPrice_hydro") ->
      elec_hydro_USA.xml

    return_data(elec_hydro_USA.xml)
  } else {
    stop("Unknown command")
  }
}
