#' module_gcamusa_batch_electricity_USA_xml
#'
#' Construct XML data structure for \code{electricity_USA.xml}.
#'()
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electricity_USA.xml}. The corresponding file in the
#' original data system was \code{batch_electricity_USA_xml.R} (gcamusa XML).
module_gcamusa_batch_electricity_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L223.Sector_Investment",
             "L223.SubsectorLogit_Investment",
             "L223.SubsectorShrwt_Investment",
             "L223.SubsectorInterp_Investment",
             "L223.SubsectorInterpTo_Investment",
             "L223.StubTech_Investment",
             "L223.GlobalTechEff_Investment",
             "L223.StubTechMarket_Investment",
             "L223.GlobalTechOMfixed_Investment",
             "L223.GlobalTechOMvar_Investment",
             "L223.GlobalTechCapital_Investment",
             "L223.GlobalTechShrwt_Investment",
             "L223.GlobalTechCapFac_Investment",
             "L223.TechCapFac_Investment",
             "L223.GlobalTechCapture_Investment",
             "L223.GlobalTechCost_Investment",
             "L223.GlobalTechHack_Investment",
             "L223.Sector_Investment_StateShare",
             "L223.Subsector_Investment_StateShare",
             "L223.SubsectorShrwtFllt_Investment_StateShare",
             "L223.TechCoef_Investment_StateShare",
             "L223.TechShrwt_Investment_StateShare",
             "L223.Sector_Investment_LoadCurve",
             "L223.SectorUseTrialMarket_Investment_LoadCurve",
             "L223.SubsectorLogit_Investment_LoadCurve",
             "L223.SubsectorShrwtFllt_Investment_LoadCurve",
             "L223.TechShrwt_Investment_LoadCurve",
             "L223.TechCoef_Investment_LoadCurve",
             "L223.TechPMult_Investment_LoadCurve",
             "L223.TechCost_Investment_LoadCurve",
             "L223.DispatchSector",
             "L223.Sector_Dispatch",
             "L223.SubsectorLogit_Dispatch",
             "L223.SubsectorShrwtFllt_Dispatch",
             "L223.CapacityTech_FutureTechs",
             "L223.CapacityTechSegmentCapFac",
             "L223.CapacityTech",
             "L223.TechShrwt_Dispatch",
             "L223.TechEff_Dispatch",
             "L223.TechOMvar_Dispatch",
             "L223.TechLifetime_Dispatch",
             "L223.TechSCurve_Dispatch",
             "L223.TechCapFac_Dispatch",
             "L223.TechCarbonCapture_Dispatch",
             "L223.TechCapFac_Cal",
             "L223.TechEff_Cal",
             "L223.Sector_Dispatch_Grid",
             "L223.DispatchSectorCalProd",
             "L223.DispatchSectorDemandSegments",
             "L223.InterestRate_FERC",
             "L223.Pop_FERC",
             "L223.BaseGDP_FERC",
             "L223.LaborForceFillout_FERC"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "electricity_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    passthrough.sector <- technology <- share.weight <- NULL # silence package check notes

    # Load required inputs

    L223.Sector_Investment <- get_data(all_data, "L223.Sector_Investment")
    L223.SubsectorLogit_Investment <- get_data(all_data, "L223.SubsectorLogit_Investment")
    L223.SubsectorShrwt_Investment <- get_data(all_data, "L223.SubsectorShrwt_Investment")
    L223.SubsectorInterp_Investment <- get_data(all_data, "L223.SubsectorInterp_Investment")
    L223.SubsectorInterpTo_Investment <- get_data(all_data, "L223.SubsectorInterpTo_Investment")
    L223.StubTech_Investment <- get_data(all_data, "L223.StubTech_Investment")
    L223.GlobalTechEff_Investment <- get_data(all_data, "L223.GlobalTechEff_Investment")
    L223.StubTechMarket_Investment <- get_data(all_data, "L223.StubTechMarket_Investment")
    L223.GlobalTechOMfixed_Investment <- get_data(all_data, "L223.GlobalTechOMfixed_Investment")
    L223.GlobalTechOMvar_Investment <- get_data(all_data, "L223.GlobalTechOMvar_Investment")
    L223.GlobalTechCapital_Investment <- get_data(all_data, "L223.GlobalTechCapital_Investment")
    L223.GlobalTechShrwt_Investment <- get_data(all_data, "L223.GlobalTechShrwt_Investment")
    L223.GlobalTechCapFac_Investment <- get_data(all_data, "L223.GlobalTechCapFac_Investment")
    L223.TechCapFac_Investment <- get_data(all_data, "L223.TechCapFac_Investment")
    L223.GlobalTechCapture_Investment <- get_data(all_data, "L223.GlobalTechCapture_Investment")
    L223.GlobalTechCost_Investment <- get_data(all_data, "L223.GlobalTechCost_Investment")
    L223.GlobalTechHack_Investment <- get_data(all_data, "L223.GlobalTechHack_Investment")
    L223.Sector_Investment_StateShare <- get_data(all_data, "L223.Sector_Investment_StateShare")
    L223.Subsector_Investment_StateShare <- get_data(all_data, "L223.Subsector_Investment_StateShare")
    L223.SubsectorShrwtFllt_Investment_StateShare <- get_data(all_data, "L223.SubsectorShrwtFllt_Investment_StateShare")
    L223.TechCoef_Investment_StateShare <- get_data(all_data, "L223.TechCoef_Investment_StateShare")
    L223.TechShrwt_Investment_StateShare <- get_data(all_data, "L223.TechShrwt_Investment_StateShare")
    L223.Sector_Investment_LoadCurve <- get_data(all_data, "L223.Sector_Investment_LoadCurve")
    L223.SectorUseTrialMarket_Investment_LoadCurve <- get_data(all_data, "L223.SectorUseTrialMarket_Investment_LoadCurve")
    L223.SubsectorLogit_Investment_LoadCurve <- get_data(all_data, "L223.SubsectorLogit_Investment_LoadCurve")
    L223.SubsectorShrwtFllt_Investment_LoadCurve <- get_data(all_data, "L223.SubsectorShrwtFllt_Investment_LoadCurve")
    L223.TechShrwt_Investment_LoadCurve <- get_data(all_data, "L223.TechShrwt_Investment_LoadCurve")
    L223.TechCoef_Investment_LoadCurve <- get_data(all_data, "L223.TechCoef_Investment_LoadCurve")
    L223.TechPMult_Investment_LoadCurve <- get_data(all_data, "L223.TechPMult_Investment_LoadCurve")
    L223.TechCost_Investment_LoadCurve <- get_data(all_data, "L223.TechCost_Investment_LoadCurve")
    L223.DispatchSector <- get_data(all_data, "L223.DispatchSector")
    L223.Sector_Dispatch <- get_data(all_data, "L223.Sector_Dispatch")
    L223.SubsectorLogit_Dispatch <- get_data(all_data, "L223.SubsectorLogit_Dispatch")
    L223.SubsectorShrwtFllt_Dispatch <- get_data(all_data, "L223.SubsectorShrwtFllt_Dispatch")
    L223.CapacityTech_FutureTechs <- get_data(all_data, "L223.CapacityTech_FutureTechs")
    L223.CapacityTechSegmentCapFac <- get_data(all_data, "L223.CapacityTechSegmentCapFac")
    L223.CapacityTech <- get_data(all_data, "L223.CapacityTech")
    L223.TechShrwt_Dispatch <- get_data(all_data, "L223.TechShrwt_Dispatch")
    L223.TechEff_Dispatch <- get_data(all_data, "L223.TechEff_Dispatch")
    L223.TechOMvar_Dispatch <- get_data(all_data, "L223.TechOMvar_Dispatch")
    L223.TechLifetime_Dispatch <- get_data(all_data, "L223.TechLifetime_Dispatch")
    L223.TechSCurve_Dispatch <- get_data(all_data, "L223.TechSCurve_Dispatch")
    L223.TechCapFac_Dispatch <- get_data(all_data, "L223.TechCapFac_Dispatch")
    L223.TechCarbonCapture_Dispatch <- get_data(all_data, "L223.TechCarbonCapture_Dispatch")
    L223.TechCapFac_Cal <- get_data(all_data, "L223.TechCapFac_Cal")
    L223.TechEff_Cal <- get_data(all_data, "L223.TechEff_Cal")
    L223.Sector_Dispatch_Grid <- get_data(all_data, "L223.Sector_Dispatch_Grid")
    L223.DispatchSectorCalProd <- get_data(all_data, "L223.DispatchSectorCalProd")
    L223.DispatchSectorDemandSegments <- get_data(all_data, "L223.DispatchSectorDemandSegments")
    L223.InterestRate_FERC <- get_data(all_data, "L223.InterestRate_FERC")
    L223.Pop_FERC <- get_data(all_data, "L223.Pop_FERC")
    L223.BaseGDP_FERC <- get_data(all_data, "L223.BaseGDP_FERC")
    L223.LaborForceFillout_FERC <- get_data(all_data, "L223.LaborForceFillout_FERC")

    # Produce outputs
    create_xml("electricity_USA.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_logit_tables_xml(L223.Sector_Investment, "Supplysector") %>%
      add_logit_tables_xml(L223.SubsectorLogit_Investment, "SubsectorLogit") %>%
      add_xml_data(L223.SubsectorShrwt_Investment, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorInterp_Investment, "SubsectorInterp") %>%
      add_xml_data(L223.SubsectorInterpTo_Investment, "SubsectorInterpTo") %>%
      add_xml_data(L223.StubTech_Investment, "StubTech") %>%
      add_xml_data(L223.GlobalTechEff_Investment, "GlobalTechEff", NULL) %>%
      add_xml_data(L223.StubTechMarket_Investment, "StubTechMarket") %>%
      add_xml_data(L223.GlobalTechOMfixed_Investment, "GlobalTechOMfixed") %>%
      add_xml_data(L223.GlobalTechOMvar_Investment, "GlobalTechOMvar") %>%
      add_xml_data(L223.GlobalTechCapital_Investment, "GlobalTechCapital") %>%
      add_xml_data(L223.GlobalTechShrwt_Investment, "GlobalTechShrwt", NULL) %>%
      add_xml_data(L223.GlobalTechCapFac_Investment, "GlobalTechCapFac", NULL) %>%
      add_xml_data(L223.TechCapFac_Investment, "TechCapFac", NULL) %>%
      add_xml_data(L223.GlobalTechCapture_Investment, "GlobalTechCapture", NULL) %>%
      add_xml_data(L223.GlobalTechCost_Investment, "GlobalTechCost", NULL) %>%
      add_xml_data(L223.GlobalTechHack_Investment, "GlobalTechHack", NULL) %>%
      add_logit_tables_xml(L223.Sector_Investment_StateShare, "Supplysector") %>%
      add_logit_tables_xml(L223.Subsector_Investment_StateShare, "SubsectorLogit") %>%
      add_xml_data(L223.SubsectorShrwtFllt_Investment_StateShare, "SubsectorShrwtFllt") %>%
      add_xml_data(L223.TechCoef_Investment_StateShare, "TechCoef") %>%
      add_xml_data(L223.TechShrwt_Investment_StateShare, "TechShrwt") %>%
      add_logit_tables_xml(L223.Sector_Investment_LoadCurve, "Supplysector") %>%
      add_xml_data(L223.SectorUseTrialMarket_Investment_LoadCurve, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L223.SubsectorLogit_Investment_LoadCurve, "SubsectorLogit") %>%
      add_xml_data(L223.SubsectorShrwtFllt_Investment_LoadCurve, "SubsectorShrwtFllt") %>%
      add_xml_data(L223.TechShrwt_Investment_LoadCurve, "TechShrwt") %>%
      add_xml_data(L223.TechCoef_Investment_LoadCurve, "TechCoef") %>%
      add_xml_data(L223.TechPMult_Investment_LoadCurve, "TechPMult", NULL) %>%
      add_xml_data(L223.TechCost_Investment_LoadCurve, "TechCost") %>%
      add_xml_data(L223.DispatchSector, "DispatchSector", NULL) %>%
      add_logit_tables_xml(L223.Sector_Dispatch, "Supplysector") %>%
      add_logit_tables_xml(L223.SubsectorLogit_Dispatch, "SubsectorLogit") %>%
      add_xml_data(L223.SubsectorShrwtFllt_Dispatch, "SubsectorShrwtFllt") %>%
      add_xml_data(L223.CapacityTech_FutureTechs, "CapacityTech", NULL) %>%
      add_xml_data(L223.CapacityTechSegmentCapFac, "CapacityTechSegmentCapFac", NULL) %>%
      add_xml_data(L223.CapacityTech, "CapacityTech", NULL) %>%
      add_xml_data(L223.TechShrwt_Dispatch, "TechShrwt") %>%
      add_xml_data(L223.TechEff_Dispatch, "TechEff") %>%
      add_xml_data(L223.TechOMvar_Dispatch, "TechOMvar") %>%
      add_xml_data(L223.TechLifetime_Dispatch, "TechLifetime") %>%
      add_xml_data(L223.TechSCurve_Dispatch, "TechSCurveDistpatch", NULL) %>%
      add_xml_data(L223.TechCapFac_Dispatch, "TechCapFac", NULL) %>%
      add_xml_data(L223.TechCarbonCapture_Dispatch, "CarbonCapture") %>%
      add_xml_data(L223.TechCapFac_Cal, "TechCapFac", NULL) %>%
      add_xml_data(L223.TechEff_Cal, "TechEff") %>%
      add_logit_tables_xml(L223.Sector_Dispatch_Grid, "Supplysector") %>%
      add_xml_data(L223.DispatchSectorCalProd, "DispatchSectorCalProd", NULL) %>%
      add_xml_data(L223.DispatchSectorDemandSegments, "DispatchSectorDemandSegments", NULL) %>%
      add_xml_data(L223.InterestRate_FERC, "InterestRate") %>%
      add_xml_data(L223.Pop_FERC, "Pop") %>%
      add_xml_data(L223.BaseGDP_FERC, "BaseGDP") %>%
      add_xml_data(L223.LaborForceFillout_FERC, "LaborForceFillout") %>%
      add_precursors("L223.Sector_Investment",
                     "L223.SubsectorLogit_Investment",
                     "L223.SubsectorShrwt_Investment",
                     "L223.SubsectorInterp_Investment",
                     "L223.SubsectorInterpTo_Investment",
                     "L223.StubTech_Investment",
                     "L223.GlobalTechEff_Investment",
                     "L223.StubTechMarket_Investment",
                     "L223.GlobalTechOMfixed_Investment",
                     "L223.GlobalTechOMvar_Investment",
                     "L223.GlobalTechCapital_Investment",
                     "L223.GlobalTechShrwt_Investment",
                     "L223.GlobalTechCapFac_Investment",
                     "L223.TechCapFac_Investment",
                     "L223.GlobalTechCapture_Investment",
                     "L223.GlobalTechCost_Investment",
                     "L223.GlobalTechHack_Investment",
                     "L223.Sector_Investment_StateShare",
                     "L223.Subsector_Investment_StateShare",
                     "L223.SubsectorShrwtFllt_Investment_StateShare",
                     "L223.TechCoef_Investment_StateShare",
                     "L223.TechShrwt_Investment_StateShare",
                     "L223.Sector_Investment_LoadCurve",
                     "L223.SectorUseTrialMarket_Investment_LoadCurve",
                     "L223.SubsectorLogit_Investment_LoadCurve",
                     "L223.SubsectorShrwtFllt_Investment_LoadCurve",
                     "L223.TechShrwt_Investment_LoadCurve",
                     "L223.TechCoef_Investment_LoadCurve",
                     "L223.TechPMult_Investment_LoadCurve",
                     "L223.TechCost_Investment_LoadCurve",
                     "L223.DispatchSector",
                     "L223.Sector_Dispatch",
                     "L223.SubsectorLogit_Dispatch",
                     "L223.SubsectorShrwtFllt_Dispatch",
                     "L223.CapacityTech_FutureTechs",
                     "L223.CapacityTechSegmentCapFac",
                     "L223.CapacityTech",
                     "L223.TechShrwt_Dispatch",
                     "L223.TechEff_Dispatch",
                     "L223.TechOMvar_Dispatch",
                     "L223.TechLifetime_Dispatch",
                     "L223.TechSCurve_Dispatch",
                     "L223.TechCapFac_Dispatch",
                     "L223.TechCarbonCapture_Dispatch",
                     "L223.TechCapFac_Cal",
                     "L223.TechEff_Cal",
                     "L223.Sector_Dispatch_Grid",
                     "L223.DispatchSectorCalProd",
                     "L223.DispatchSectorDemandSegments",
                     "L223.InterestRate_FERC",
                     "L223.Pop_FERC",
                     "L223.BaseGDP_FERC",
                     "L223.LaborForceFillout_FERC") ->
      electricity_USA.xml

    return_data(electricity_USA.xml)
  } else {
    stop("Unknown command")
  }
}
