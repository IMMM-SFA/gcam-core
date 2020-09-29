#' module_gcamusa_batch_building_segments_USA_xml
#'
#' Construct XML data structure for \code{building_segments_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_segments_USA.xml}, \code{building_segments_USA_rcp4p5.xml},
#' \code{building_segments_USA_rcp8p5.xml}. The corresponding file in the
#' original data system was \code{batch_segments_building_USA.xml} (dispatch branch gcamusa XML).
module_gcamusa_batch_building_segments_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2441.DeleteThermalService_gcamusa",
             "L2441.ThermalBaseService_gcamusa",
             "L2441.ThermalServiceSatiation_gcamusa",
             "L2441.ThermalServiceCalSatiationValue_gcamusa",
             "L2441.ThermalDefaultCoef_gcamusa",
             "L2441.Intgains_scalar_gcamusa",
             "L2441.Supplysector_bld_gcamusa",
             "L2441.FinalEnergyKeyword_bld_gcamusa",
             "L2441.SubsectorLogit_bld_gcamusa",
             "L2441.SubsectorShrwtFllt_bld_gcamusa",
             "L2441.SubsectorInterp_bld_gcamusa",
             "L2441.SubsectorInterpTo_bld_gcamusa",
             "L2441.StubTechFromSector_bld_gcamusa",
             "L2441.StubTechDeleteInput_gcamusa",
             "L2441.StubTechEff_segmentinputs_gcamusa",
             "L2441.StubTechCalInput_bld_gcamusa",
             "L2441.StubTechMarket_bld_gcamusa",
             "L2441.TechCoef_nonthermal_load_curve_gcamusa",
             "L2441.HDDCDD_Fixed_gcamusa",
             "L2441.HDDCDD_Fixed_rcp4p5_gcamusa",
             "L2441.HDDCDD_Fixed_rcp8p5_gcamusa"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_segments_USA.xml",
             XML = "building_segments_USA_rcp4p5.xml",
             XML = "building_segments_USA_rcp8p5.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Only process data if using end-use "demand" segments (gcamusa.USE_ELEC_DEMAND_SEGMENTS == TRUE)
    if(gcamusa.USE_ELEC_DEMAND_SEGMENTS) {

      # Load required inputs
      L2441.DeleteThermalService <- get_data(all_data, "L2441.DeleteThermalService_gcamusa")
      L2441.ThermalBaseService <- get_data(all_data, "L2441.ThermalBaseService_gcamusa")
      L2441.ThermalServiceSatiation <- get_data(all_data, "L2441.ThermalServiceSatiation_gcamusa")
      L2441.ThermalServiceCalSatiationValue <- get_data(all_data, "L2441.ThermalServiceCalSatiationValue_gcamusa")
      L2441.ThermalDefaultCoef <- get_data(all_data, "L2441.ThermalDefaultCoef_gcamusa")
      L2441.Intgains_scalar <- get_data(all_data, "L2441.Intgains_scalar_gcamusa")
      L2441.Supplysector_bld <- get_data(all_data, "L2441.Supplysector_bld_gcamusa")
      L2441.FinalEnergyKeyword_bld <- get_data(all_data, "L2441.FinalEnergyKeyword_bld_gcamusa")
      L2441.SubsectorLogit_bld <- get_data(all_data, "L2441.SubsectorLogit_bld_gcamusa")
      L2441.SubsectorShrwtFllt_bld <- get_data(all_data, "L2441.SubsectorShrwtFllt_bld_gcamusa")
      L2441.SubsectorInterp_bld <- get_data(all_data, "L2441.SubsectorInterp_bld_gcamusa")
      L2441.SubsectorInterpTo_bld <- get_data(all_data, "L2441.SubsectorInterpTo_bld_gcamusa")
      L2441.StubTechFromSector_bld <- get_data(all_data, "L2441.StubTechFromSector_bld_gcamusa")
      L2441.StubTechDeleteInput <- get_data(all_data, "L2441.StubTechDeleteInput_gcamusa")
      L2441.StubTechEff_segmentinputs <- get_data(all_data, "L2441.StubTechEff_segmentinputs_gcamusa")
      L2441.StubTechCalInput_bld <- get_data(all_data, "L2441.StubTechCalInput_bld_gcamusa")
      L2441.StubTechMarket_bld <- get_data(all_data, "L2441.StubTechMarket_bld_gcamusa")
      L2441.TechCoef_nonthermal_load_curve <- get_data(all_data, "L2441.TechCoef_nonthermal_load_curve_gcamusa")
      L2441.HDDCDD_Fixed <- get_data(all_data, "L2441.HDDCDD_Fixed_gcamusa")
      L2441.HDDCDD_Fixed_rcp4p5 <- get_data(all_data, "L2441.HDDCDD_Fixed_rcp4p5_gcamusa")
      L2441.HDDCDD_Fixed_rcp8p5 <- get_data(all_data, "L2441.HDDCDD_Fixed_rcp8p5_gcamusa")

      # ===================================================

      # Produce outputs
      create_xml("building_segments_USA.xml") %>%
        add_xml_data(L2441.DeleteThermalService, "DeleteThermalService") %>%
        add_xml_data(L2441.ThermalBaseService, "ThermalBaseService") %>%
        add_xml_data(L2441.ThermalServiceSatiation, "ThermalServiceSatiation") %>%
        add_xml_data(L2441.ThermalServiceCalSatiationValue, "ThermalServiceCalSatiationValue", NULL) %>%
        add_xml_data(L2441.ThermalDefaultCoef, "ThermalServiceCoef", NULL) %>%
        add_xml_data(L2441.Intgains_scalar, "Intgains_scalar") %>%
        add_logit_tables_xml(L2441.Supplysector_bld, "Supplysector") %>%
        add_xml_data(L2441.FinalEnergyKeyword_bld, "FinalEnergyKeyword") %>%
        add_logit_tables_xml(L2441.SubsectorLogit_bld, "SubsectorLogit") %>%
        add_xml_data(L2441.SubsectorShrwtFllt_bld, "SubsectorShrwtFllt") %>%
        add_xml_data(L2441.SubsectorInterp_bld, "SubsectorInterp") %>%
        add_xml_data(L2441.SubsectorInterpTo_bld, "SubsectorInterpTo") %>%
        add_xml_data(L2441.StubTechFromSector_bld, "StubTechFromSector", NULL) %>%
        add_xml_data(L2441.StubTechDeleteInput, "DeleteStubTechMinicamEnergyInput") %>%
        add_xml_data(L2441.StubTechEff_segmentinputs, "StubTechEff") %>%
        add_xml_data(L2441.StubTechCalInput_bld, "StubTechCalInput") %>%
        add_xml_data(L2441.StubTechMarket_bld, "StubTechMarket") %>%
        add_xml_data(L2441.HDDCDD_Fixed, "HDDCDD") %>%
        add_xml_data(L2441.TechCoef_nonthermal_load_curve, "TechCoef") %>%
        add_precursors("L2441.DeleteThermalService_gcamusa",
                       "L2441.ThermalBaseService_gcamusa",
                       "L2441.ThermalServiceSatiation_gcamusa",
                       "L2441.ThermalServiceCalSatiationValue_gcamusa",
                       "L2441.ThermalDefaultCoef_gcamusa",
                       "L2441.Intgains_scalar_gcamusa",
                       "L2441.Supplysector_bld_gcamusa",
                       "L2441.FinalEnergyKeyword_bld_gcamusa",
                       "L2441.SubsectorLogit_bld_gcamusa",
                       "L2441.SubsectorShrwtFllt_bld_gcamusa",
                       "L2441.SubsectorInterp_bld_gcamusa",
                       "L2441.SubsectorInterpTo_bld_gcamusa",
                       "L2441.StubTechFromSector_bld_gcamusa",
                       "L2441.StubTechDeleteInput_gcamusa",
                       "L2441.StubTechEff_segmentinputs_gcamusa",
                       "L2441.StubTechCalInput_bld_gcamusa",
                       "L2441.StubTechMarket_bld_gcamusa",
                       "L2441.TechCoef_nonthermal_load_curve_gcamusa",
                       "L2441.HDDCDD_Fixed_gcamusa") ->
        building_segments_USA.xml

      create_xml("building_segments_USA_rcp4p5.xml") %>%
        add_xml_data(L2441.HDDCDD_Fixed_rcp4p5, "HDDCDD") %>%
        add_precursors("L2441.HDDCDD_Fixed_rcp4p5_gcamusa") ->
        building_segments_USA_rcp4p5.xml

      create_xml("building_segments_USA_rcp8p5.xml") %>%
        add_xml_data(L2441.HDDCDD_Fixed_rcp8p5, "HDDCDD") %>%
        add_precursors("L2441.HDDCDD_Fixed_rcp8p5_gcamusa") ->
        building_segments_USA_rcp8p5.xml

    } else {

      # If not using end-use "demand" segments (gcamusa.USE_ELEC_DEMAND_SEGMENTS == FALSE),
      # return a set of empty tables

      create_xml("building_segments_USA.xml") %>%
        add_precursors("L2441.DeleteThermalService_gcamusa",
                       "L2441.ThermalBaseService_gcamusa",
                       "L2441.ThermalServiceSatiation_gcamusa",
                       "L2441.ThermalServiceCalSatiationValue_gcamusa",
                       "L2441.ThermalDefaultCoef_gcamusa",
                       "L2441.Intgains_scalar_gcamusa",
                       "L2441.Supplysector_bld_gcamusa",
                       "L2441.FinalEnergyKeyword_bld_gcamusa",
                       "L2441.SubsectorLogit_bld_gcamusa",
                       "L2441.SubsectorShrwtFllt_bld_gcamusa",
                       "L2441.SubsectorInterp_bld_gcamusa",
                       "L2441.SubsectorInterpTo_bld_gcamusa",
                       "L2441.StubTechFromSector_bld_gcamusa",
                       "L2441.StubTechDeleteInput_gcamusa",
                       "L2441.StubTechEff_segmentinputs_gcamusa",
                       "L2441.StubTechCalInput_bld_gcamusa",
                       "L2441.StubTechMarket_bld_gcamusa",
                       "L2441.TechCoef_nonthermal_load_curve_gcamusa",
                       "L2441.HDDCDD_Fixed_gcamusa") ->
        building_segments_USA.xml

      create_xml("building_segments_USA_rcp4p5.xml") %>%
        add_precursors("L2441.HDDCDD_Fixed_rcp4p5_gcamusa") ->
        building_segments_USA_rcp4p5.xml

      create_xml("building_segments_USA_rcp8p5.xml") %>%
        add_precursors("L2441.HDDCDD_Fixed_rcp8p5_gcamusa") ->
        building_segments_USA_rcp8p5.xml

    }

    return_data(building_segments_USA.xml,
                building_segments_USA_rcp4p5.xml,
                building_segments_USA_rcp8p5.xml)
  } else {
    stop("Unknown command")
  }
}
