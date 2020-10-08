# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_solar_reeds_USA_xml
#'
#' Construct XML data structure for \code{solar_reeds_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{solar_reeds_USA.xml}.
#' The corresponding file in the original data system was \code{batch_solar_USA_reeds.xml} (gcamusa XML batch).
module_gcamusa_batch_solar_reeds_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2238.DeleteStubTechMinicamEnergyInput_investment_PV_reeds_USA",
             "L2238.DeleteInput_dispatch_PV_reeds_USA",
             "L2238.RenewRsrc_PV_reeds_USA",
             "L2238.GrdRenewRsrcCurves_PV_reeds_USA",
             "L2238.GrdRenewRsrcMax_PV_reeds_USA",
             "L2238.StubTechEffFlag_investment_PV_reeds_USA",
             "L2238.TechEff_Dispatch_PV_reeds_USA",
             "L2238.CapacityTechInputPMult_dispatch_PV_reeds_USA",
             "L2238.RenewRsrcTechChange_PV_reeds_USA",
             "L2238.StubTechCost_PV_reeds_USA",
             "L2238.ResTechShrwt_PV_reeds_USA",
             "L2239.DeleteUnlimitRsrc_reeds_USA",
             "L2239.DeleteStubTechMinicamEnergyInput_investment_CSP_reeds_USA",
             "L2239.DeleteStubTechMinicamEnergyInput_dispatch_CSP_reeds_USA",
             "L2239.RenewRsrc_CSP_reeds_USA",
             "L2239.GrdRenewRsrcCurves_CSP_reeds_USA",
             "L2239.GrdRenewRsrcMax_CSP_reeds_USA",
             "L2239.StubTechEffFlag_investment_CSP_reeds_USA",
             "L2239.StubTechEffFlag_dispatch_CSP_reeds_USA",
             "L2239.StubTechPmultFlag_dispatch_CSP_reeds_USA",
             "L2239.RenewRsrcTechChange_CSP_reeds_USA",
             "L2239.StubTechCost_CSP_reeds_USA",
             "L2239.ResTechShrwt_CSP_reeds_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "solar_reeds_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    technology <- stub.technology <- NULL  # silence package check notes

    # Load required inputs
    L2238.DeleteStubTechMinicamEnergyInput_investment_PV_reeds_USA <- get_data(all_data,"L2238.DeleteStubTechMinicamEnergyInput_investment_PV_reeds_USA")
    L2238.DeleteInput_dispatch_PV_reeds_USA <- get_data(all_data, "L2238.DeleteInput_dispatch_PV_reeds_USA")
    L2238.RenewRsrc_PV_reeds_USA <- get_data(all_data, "L2238.RenewRsrc_PV_reeds_USA")
    L2238.GrdRenewRsrcCurves_PV_reeds_USA <- get_data(all_data, "L2238.GrdRenewRsrcCurves_PV_reeds_USA")
    L2238.GrdRenewRsrcMax_PV_reeds_USA <- get_data(all_data, "L2238.GrdRenewRsrcMax_PV_reeds_USA")
    L2238.StubTechEffFlag_investment_PV_reeds_USA <- get_data(all_data, "L2238.StubTechEffFlag_investment_PV_reeds_USA")
    L2238.TechEff_Dispatch_PV_reeds_USA <- get_data(all_data, "L2238.TechEff_Dispatch_PV_reeds_USA")
    L2238.CapacityTechInputPMult_dispatch_PV_reeds_USA <- get_data(all_data, "L2238.CapacityTechInputPMult_dispatch_PV_reeds_USA")
    L2238.RenewRsrcTechChange_PV_reeds_USA <- get_data(all_data, "L2238.RenewRsrcTechChange_PV_reeds_USA")
    L2238.StubTechCost_PV_reeds_USA <- get_data(all_data, "L2238.StubTechCost_PV_reeds_USA")
    L2238.ResTechShrwt_PV_reeds_USA <- get_data(all_data, "L2238.ResTechShrwt_PV_reeds_USA")

    L2239.DeleteUnlimitRsrc_reeds_USA <- get_data(all_data, "L2239.DeleteUnlimitRsrc_reeds_USA")
    L2239.DeleteStubTechMinicamEnergyInput_investment_CSP_reeds_USA <- get_data(all_data, "L2239.DeleteStubTechMinicamEnergyInput_investment_CSP_reeds_USA")
    L2239.DeleteStubTechMinicamEnergyInput_dispatch_CSP_reeds_USA <- get_data(all_data, "L2239.DeleteStubTechMinicamEnergyInput_dispatch_CSP_reeds_USA")
    L2239.RenewRsrc_CSP_reeds_USA <- get_data(all_data, "L2239.RenewRsrc_CSP_reeds_USA")
    L2239.GrdRenewRsrcCurves_CSP_reeds_USA <- get_data(all_data, "L2239.GrdRenewRsrcCurves_CSP_reeds_USA")
    L2239.GrdRenewRsrcMax_CSP_reeds_USA <- get_data(all_data, "L2239.GrdRenewRsrcMax_CSP_reeds_USA")
    L2239.StubTechEffFlag_investment_CSP_reeds_USA <- get_data(all_data, "L2239.StubTechEffFlag_investment_CSP_reeds_USA")
    L2239.StubTechEffFlag_dispatch_CSP_reeds_USA <- get_data(all_data, "L2239.StubTechEffFlag_dispatch_CSP_reeds_USA")
    L2239.StubTechPmultFlag_dispatch_CSP_reeds_USA <- get_data(all_data, "L2239.StubTechPmultFlag_dispatch_CSP_reeds_USA")
    L2239.RenewRsrcTechChange_CSP_reeds_USA <- get_data(all_data, "L2239.RenewRsrcTechChange_CSP_reeds_USA")
    L2239.StubTechCost_CSP_reeds_USA <- get_data(all_data, "L2239.StubTechCost_CSP_reeds_USA")
    L2239.ResTechShrwt_CSP_reeds_USA <- get_data(all_data, "L2239.ResTechShrwt_CSP_reeds_USA")

    # ===================================================
    # Produce outputs

    # Rename columns in two dispatch sector | capacity technology tables
    # Note that these tables are not stub.technology tables (not connected to the global tech db)
    # However, because we've set up the node equivalance, we can print into to XML correctly this
    # way without adding a new LEVEL2DATANAMES for CapacityTechEffFlag
    L2239.StubTechEffFlag_dispatch_CSP_reeds_USA %>%
      rename(stub.technology = technology) ->
      L2239.StubTechEffFlag_dispatch_CSP_reeds_USA

    L2238.TechEff_Dispatch_PV_reeds_USA %>%
      rename(stub.technology = technology) ->
      L2238.TechEff_Dispatch_PV_reeds_USA

    create_xml("solar_reeds_USA.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data_generate_levels(L2238.DeleteStubTechMinicamEnergyInput_investment_PV_reeds_USA, "DeleteStubTechMinicamEnergyInput",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data(L2238.CapacityTechInputPMult_dispatch_PV_reeds_USA, "CapacityTechInputPMult") %>%
      add_xml_data(L2238.DeleteInput_dispatch_PV_reeds_USA, "DeleteInput") %>%
      add_xml_data(L2238.RenewRsrc_PV_reeds_USA, "RenewRsrc") %>%
      add_xml_data(L2238.GrdRenewRsrcCurves_PV_reeds_USA, "GrdRenewRsrcCurves") %>%
      add_xml_data(L2238.GrdRenewRsrcMax_PV_reeds_USA, "GrdRenewRsrcMax") %>%
      add_xml_data_generate_levels(L2238.StubTechEffFlag_investment_PV_reeds_USA, "StubTechEffFlag",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data(L2238.TechEff_Dispatch_PV_reeds_USA, "StubTechEffFlag") %>%
      add_xml_data(L2238.RenewRsrcTechChange_PV_reeds_USA, "RenewRsrcTechChange") %>%
      add_xml_data_generate_levels(L2238.StubTechCost_PV_reeds_USA, "StubTechCost",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data(L2239.DeleteUnlimitRsrc_reeds_USA, "DeleteUnlimitRsrc") %>%
      add_xml_data_generate_levels(L2239.DeleteStubTechMinicamEnergyInput_investment_CSP_reeds_USA, "DeleteStubTechMinicamEnergyInput",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data(L2239.StubTechPmultFlag_dispatch_CSP_reeds_USA, "CapacityTechInputPMult") %>%
      add_xml_data(L2239.DeleteStubTechMinicamEnergyInput_dispatch_CSP_reeds_USA, "DeleteInput") %>%
      add_xml_data(L2239.RenewRsrc_CSP_reeds_USA, "RenewRsrc") %>%
      add_xml_data(L2239.GrdRenewRsrcCurves_CSP_reeds_USA, "GrdRenewRsrcCurves") %>%
      add_xml_data(L2239.GrdRenewRsrcMax_CSP_reeds_USA, "GrdRenewRsrcMax") %>%
      add_xml_data_generate_levels(L2239.StubTechEffFlag_investment_CSP_reeds_USA, "StubTechEffFlag",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data(L2239.StubTechEffFlag_dispatch_CSP_reeds_USA, "StubTechEffFlag") %>%
      add_xml_data(L2239.RenewRsrcTechChange_CSP_reeds_USA, "RenewRsrcTechChange") %>%
      add_xml_data_generate_levels(L2239.StubTechCost_CSP_reeds_USA, "StubTechCost",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_xml_data(L2238.ResTechShrwt_PV_reeds_USA, "ResTechShrwt") %>%
      add_xml_data(L2239.ResTechShrwt_CSP_reeds_USA, "ResTechShrwt") %>%
      add_precursors("L2238.DeleteStubTechMinicamEnergyInput_investment_PV_reeds_USA",
                     "L2238.DeleteInput_dispatch_PV_reeds_USA",
                     "L2238.RenewRsrc_PV_reeds_USA",
                     "L2238.GrdRenewRsrcCurves_PV_reeds_USA",
                     "L2238.GrdRenewRsrcMax_PV_reeds_USA",
                     "L2238.StubTechEffFlag_investment_PV_reeds_USA",
                     "L2238.TechEff_Dispatch_PV_reeds_USA",
                     "L2238.CapacityTechInputPMult_dispatch_PV_reeds_USA",
                     "L2238.RenewRsrcTechChange_PV_reeds_USA",
                     "L2238.StubTechCost_PV_reeds_USA",
                     "L2238.ResTechShrwt_PV_reeds_USA",
                     "L2239.DeleteUnlimitRsrc_reeds_USA",
                     "L2239.DeleteStubTechMinicamEnergyInput_investment_CSP_reeds_USA",
                     "L2239.DeleteStubTechMinicamEnergyInput_dispatch_CSP_reeds_USA",
                     "L2239.RenewRsrc_CSP_reeds_USA",
                     "L2239.GrdRenewRsrcCurves_CSP_reeds_USA",
                     "L2239.GrdRenewRsrcMax_CSP_reeds_USA",
                     "L2239.StubTechEffFlag_investment_CSP_reeds_USA",
                     "L2239.StubTechEffFlag_dispatch_CSP_reeds_USA",
                     "L2239.StubTechPmultFlag_dispatch_CSP_reeds_USA",
                     "L2239.RenewRsrcTechChange_CSP_reeds_USA",
                     "L2239.StubTechCost_CSP_reeds_USA",
                     "L2239.ResTechShrwt_CSP_reeds_USA") ->
      solar_reeds_USA.xml

    return_data(solar_reeds_USA.xml)
  } else {
    stop("Unknown command")
  }
}
