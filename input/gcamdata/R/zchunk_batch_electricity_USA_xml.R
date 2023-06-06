# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_electricity_USA_xml
#'
#' Construct XML data structure for \code{electricity_USA.xml}.
#'()
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electricity_USA.xml}, \code{electricity_USA_allow_new_nuc_ssp5.xml},
#' \code{electricity_USA_allow_new_nuc_ssp3.xml}, \code{electricity_USA_no_moratorium_nuc_ssp5.xml},
#' \code{electricity_USA_no_moratorium_nuc_ssp3.xml}. The corresponding file in the
#' original data system was \code{batch_electricity_USA_xml.R} (gcamusa XML).
module_gcamusa_batch_electricity_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L223.Sector_Investment",
             "L223.SubsectorLogit_Investment_Fuel",
             "L223.SubsectorShrwt_Investment_Fuel",
             "L223.SubsectorInterpTo_Investment_Fuel",
             "L223.SubsectorLogit_Investment",
             "L223.SubsectorShrwt_Investment",
             "L223.StubTech_Investment",
             "L233.GlobalInvestTech_Investment",
             "L223.GlobalTechEff_Investment",
             "L223.StubTechMarket_Investment",
             "L223.GlobalTechCoef_Investment_cool",
             "L223.StubTechCoef_Investment_cool",
             "L223.GlobalTechOMfixed_Investment",
             "L223.GlobalTechOMvar_Investment",
             "L223.GlobalTechCapital_Investment",
             "L223.GlobalTechCapital_Investment_cool",
             "L223.GlobalIntTechEff_Investment",
             "L223.GlobalIntInvTechMaxCapFac_Investment",
             "L223.GlobalTechShrwt_Investment",
             "L223.StubTechInterp_Investment_USA",
             "L223.StubTechShrwt_Investment_USA",
             "L223.GlobalTechCapFac_Investment",
             "L223.TechCapFac_Investment",
             "L223.GlobalTechCapture_Investment",
             "L223.GlobalTechCost_Investment",
             "L223.GlobalTechCost_CapacityCreditCalulator",
             "L223.Sector_Investment_StateShare",
             "L223.Subsector_Investment_StateShare",
             "L223.SubsectorShrwtFllt_Investment_StateShare",
             "L223.TechCoef_Investment_StateShare",
             "L223.TechPmult_Investment_StateShare",
             "L223.TechShrwt_Investment_StateShare",
             "L223.DispatchSector",
             "L223.Sector_Dispatch",
             "L223.SubsectorLogit_Dispatch",
             "L223.SubsectorShrwtFllt_Dispatch",
             "L223.CapacityTech_FutureTechs",
             "L223.CapacityTechSegmentCapFac",
             "L223.CapacityTechMinCapFac",
             "L223.CapacityTech",
             "L223.TechShrwt_Dispatch",
             "L223.TechEff_Dispatch",
             "L223.StubTechEffFlag_Dispatch",
             "L223.TechCoef_Dispatch_cool",
             "L223.TechOMvar_Dispatch",
             "L223.TechOMfixed_Dispatch",
             "L223.TechLifetime_Dispatch",
             "L223.TechProfitShutdown_Dispatch",
             "L223.TechSCurve_Dispatch",
             "L223.TechCapFac_Dispatch",
             "L223.TechCarbonCapture_Dispatch",
             "L223.Production_Dispatch",
             "L223.TechEff_Cal",
             "L223.PrimaryRenewKeyword_Dispatch_USA",
             "L223.AvgFossilEffKeyword_Dispatch_USA",
             "L223.TechTrialMarket_Dispatch",
             "L223.TechTrialMarket_Investment",
             "L223.Sector_Dispatch_Grid",
             "L223.DispatchSectorDispatchSegments",
             "L223.InterestRate_FERC",
             "L223.StubTechCost_offshore_wind_Investment",
             "L223.Pop_FERC",
             "L223.BaseGDP_FERC",
             "L223.LaborForceFillout_FERC",
             "L2232.DeleteSupplysector_USAelec",
             "L2232.Supplysector_USAelec",
             "L2232.SubsectorShrwtFllt_USAelec",
             "L2232.SubsectorInterp_USAelec",
             "L2232.SubsectorInterpTo_USAelec",
             "L2232.SubsectorLogit_USAelec",
             "L2232.TechShrwt_USAelec",
             "L2232.TechCoef_USAelec",
             "L2232.Production_exports_USAelec",
             "L2232.Supplysector_elec_FERC",
             "L2232.ElecReserve_FERC",
             "L2232.SubsectorShrwtFllt_elec_FERC",
             "L2232.SubsectorInterp_elec_FERC",
             "L2232.SubsectorInterpTo_elec_FERC",
             "L2232.SubsectorLogit_elec_FERC",
             "L2232.TechShrwt_elec_FERC",
             "L2232.TechCoef_elec_FERC",
             "L2232.TechCoef_elecownuse_FERC",
             "L2232.Production_imports_FERC",
             "L2232.Production_elec_gen_FERC"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "electricity_USA.xml",
             XML = "electricity_USA_allow_new_nuc_ssp5.xml",
             XML = "electricity_USA_allow_new_nuc_ssp3.xml",
             XML = "electricity_USA_no_moratorium_nuc_ssp5.xml",
             XML = "electricity_USA_no_moratorium_nuc_ssp3.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    stub.technology <- technology <- NULL # silence package check notes

    # Load required inputs

    L223.Sector_Investment <- get_data(all_data, "L223.Sector_Investment")
    L223.SubsectorLogit_Investment_Fuel <- get_data(all_data, "L223.SubsectorLogit_Investment_Fuel")
    L223.SubsectorShrwt_Investment_Fuel <- get_data(all_data, "L223.SubsectorShrwt_Investment_Fuel")
    L223.SubsectorInterpTo_Investment_Fuel <- get_data(all_data, "L223.SubsectorInterpTo_Investment_Fuel")
    L223.SubsectorLogit_Investment <- get_data(all_data, "L223.SubsectorLogit_Investment")
    L223.SubsectorShrwt_Investment <- get_data(all_data, "L223.SubsectorShrwt_Investment")
    L223.StubTech_Investment <- get_data(all_data, "L223.StubTech_Investment")
    L233.GlobalInvestTech_Investment <- get_data(all_data, "L233.GlobalInvestTech_Investment")
    L223.GlobalTechEff_Investment <- get_data(all_data, "L223.GlobalTechEff_Investment")
    L223.StubTechMarket_Investment <- get_data(all_data, "L223.StubTechMarket_Investment")
    L223.GlobalTechCoef_Investment_cool <- get_data(all_data, "L223.GlobalTechCoef_Investment_cool")
    L223.StubTechCoef_Investment_cool <- get_data(all_data, "L223.StubTechCoef_Investment_cool")
    L223.GlobalTechOMfixed_Investment <- get_data(all_data, "L223.GlobalTechOMfixed_Investment")
    L223.GlobalTechOMvar_Investment <- get_data(all_data, "L223.GlobalTechOMvar_Investment")
    L223.GlobalTechCapital_Investment <- get_data(all_data, "L223.GlobalTechCapital_Investment")
    L223.GlobalTechCapital_Investment_cool <- get_data(all_data, "L223.GlobalTechCapital_Investment_cool")
    L223.GlobalIntTechEff_Investment <- get_data(all_data, "L223.GlobalIntTechEff_Investment")
    L223.GlobalIntInvTechMaxCapFac_Investment <- get_data(all_data, "L223.GlobalIntInvTechMaxCapFac_Investment")
    L223.GlobalTechShrwt_Investment <- get_data(all_data, "L223.GlobalTechShrwt_Investment")
    L223.StubTechInterp_Investment_USA <- get_data(all_data, "L223.StubTechInterp_Investment_USA")
    L223.StubTechShrwt_Investment_USA <- get_data(all_data, "L223.StubTechShrwt_Investment_USA")
    L223.GlobalTechCapFac_Investment <- get_data(all_data, "L223.GlobalTechCapFac_Investment")
    L223.TechCapFac_Investment <- get_data(all_data, "L223.TechCapFac_Investment")
    L223.GlobalTechCapture_Investment <- get_data(all_data, "L223.GlobalTechCapture_Investment")
    L223.GlobalTechCost_Investment <- get_data(all_data, "L223.GlobalTechCost_Investment")
    L223.GlobalTechCost_CapacityCreditCalulator <- get_data(all_data, "L223.GlobalTechCost_CapacityCreditCalulator")
    L223.Sector_Investment_StateShare <- get_data(all_data, "L223.Sector_Investment_StateShare")
    L223.Subsector_Investment_StateShare <- get_data(all_data, "L223.Subsector_Investment_StateShare")
    L223.SubsectorShrwtFllt_Investment_StateShare <- get_data(all_data, "L223.SubsectorShrwtFllt_Investment_StateShare")
    L223.TechCoef_Investment_StateShare <- get_data(all_data, "L223.TechCoef_Investment_StateShare")
    L223.TechPmult_Investment_StateShare <- get_data(all_data, "L223.TechPmult_Investment_StateShare")
    L223.TechShrwt_Investment_StateShare <- get_data(all_data, "L223.TechShrwt_Investment_StateShare")
    L223.DispatchSector <- get_data(all_data, "L223.DispatchSector")
    L223.Sector_Dispatch <- get_data(all_data, "L223.Sector_Dispatch")
    L223.SubsectorLogit_Dispatch <- get_data(all_data, "L223.SubsectorLogit_Dispatch")
    L223.SubsectorShrwtFllt_Dispatch <- get_data(all_data, "L223.SubsectorShrwtFllt_Dispatch")
    L223.CapacityTech_FutureTechs <- get_data(all_data, "L223.CapacityTech_FutureTechs")
    L223.CapacityTechSegmentCapFac <- get_data(all_data, "L223.CapacityTechSegmentCapFac")
    L223.CapacityTechMinCapFac <- get_data(all_data, "L223.CapacityTechMinCapFac")
    L223.CapacityTech <- get_data(all_data, "L223.CapacityTech")
    L223.TechShrwt_Dispatch <- get_data(all_data, "L223.TechShrwt_Dispatch")
    L223.TechEff_Dispatch <- get_data(all_data, "L223.TechEff_Dispatch")
    L223.StubTechEffFlag_Dispatch <- get_data(all_data, "L223.StubTechEffFlag_Dispatch")
    L223.TechCoef_Dispatch_cool <- get_data(all_data, "L223.TechCoef_Dispatch_cool")
    L223.TechOMvar_Dispatch <- get_data(all_data, "L223.TechOMvar_Dispatch")
    L223.TechOMfixed_Dispatch <- get_data(all_data, "L223.TechOMfixed_Dispatch")
    L223.TechLifetime_Dispatch <- get_data(all_data, "L223.TechLifetime_Dispatch")
    L223.TechProfitShutdown_Dispatch <- get_data(all_data, "L223.TechProfitShutdown_Dispatch")
    L223.TechSCurve_Dispatch <- get_data(all_data, "L223.TechSCurve_Dispatch")
    L223.TechCapFac_Dispatch <- get_data(all_data, "L223.TechCapFac_Dispatch")
    L223.TechCarbonCapture_Dispatch <- get_data(all_data, "L223.TechCarbonCapture_Dispatch")
    L223.Production_Dispatch <- get_data(all_data, "L223.Production_Dispatch")
    L223.TechEff_Cal <- get_data(all_data, "L223.TechEff_Cal")
    L223.PrimaryRenewKeyword_Dispatch_USA <- get_data(all_data, "L223.PrimaryRenewKeyword_Dispatch_USA")
    L223.AvgFossilEffKeyword_Dispatch_USA <- get_data(all_data, "L223.AvgFossilEffKeyword_Dispatch_USA")
    L223.TechTrialMarket_Dispatch <- get_data(all_data, "L223.TechTrialMarket_Dispatch")
    L223.TechTrialMarket_Investment <- get_data(all_data, "L223.TechTrialMarket_Investment")
    L223.Sector_Dispatch_Grid <- get_data(all_data, "L223.Sector_Dispatch_Grid")
    L223.DispatchSectorDispatchSegments <- get_data(all_data, "L223.DispatchSectorDispatchSegments")
    L223.InterestRate_FERC <- get_data(all_data, "L223.InterestRate_FERC")
    L223.Pop_FERC <- get_data(all_data, "L223.Pop_FERC")
    L223.BaseGDP_FERC <- get_data(all_data, "L223.BaseGDP_FERC")
    L223.LaborForceFillout_FERC <- get_data(all_data, "L223.LaborForceFillout_FERC")
    L223.StubTechCost_offshore_wind_Investment <- get_data(all_data,"L223.StubTechCost_offshore_wind_Investment")

    L2232.DeleteSupplysector_USAelec <- get_data(all_data, "L2232.DeleteSupplysector_USAelec")
    L2232.Supplysector_USAelec <- get_data(all_data, "L2232.Supplysector_USAelec")
    L2232.SubsectorShrwtFllt_USAelec <- get_data(all_data, "L2232.SubsectorShrwtFllt_USAelec")
    L2232.SubsectorInterp_USAelec <- get_data(all_data, "L2232.SubsectorInterp_USAelec")
    L2232.SubsectorInterpTo_USAelec <- get_data(all_data, "L2232.SubsectorInterpTo_USAelec")
    L2232.SubsectorLogit_USAelec <- get_data(all_data, "L2232.SubsectorLogit_USAelec")
    L2232.TechShrwt_USAelec <- get_data(all_data, "L2232.TechShrwt_USAelec")
    L2232.TechCoef_USAelec <- get_data(all_data, "L2232.TechCoef_USAelec")
    L2232.Production_exports_USAelec <- get_data(all_data, "L2232.Production_exports_USAelec")
    L2232.Supplysector_elec_FERC <- get_data(all_data, "L2232.Supplysector_elec_FERC")
    L2232.ElecReserve_FERC <- get_data(all_data, "L2232.ElecReserve_FERC")
    L2232.SubsectorShrwtFllt_elec_FERC <- get_data(all_data, "L2232.SubsectorShrwtFllt_elec_FERC")
    L2232.SubsectorInterp_elec_FERC <- get_data(all_data, "L2232.SubsectorInterp_elec_FERC")
    L2232.SubsectorInterpTo_elec_FERC <- get_data(all_data, "L2232.SubsectorInterpTo_elec_FERC")
    L2232.SubsectorLogit_elec_FERC <- get_data(all_data, "L2232.SubsectorLogit_elec_FERC")
    L2232.TechShrwt_elec_FERC <- get_data(all_data, "L2232.TechShrwt_elec_FERC")
    L2232.TechCoef_elec_FERC <- get_data(all_data, "L2232.TechCoef_elec_FERC")
    L2232.TechCoef_elecownuse_FERC <- get_data(all_data, "L2232.TechCoef_elecownuse_FERC")
    L2232.Production_imports_FERC <- get_data(all_data, "L2232.Production_imports_FERC")
    L2232.Production_elec_gen_FERC <- get_data(all_data, "L2232.Production_elec_gen_FERC")

    # some renames to avoid using add_xml_data_generate_levels
    L223.SubsectorLogit_Investment_Fuel <- rename(L223.SubsectorLogit_Investment_Fuel, subsector = subsector0)
    L223.SubsectorShrwt_Investment_Fuel <- rename(L223.SubsectorShrwt_Investment_Fuel, subsector = subsector0)
    L223.SubsectorInterpTo_Investment_Fuel <- rename(L223.SubsectorInterpTo_Investment_Fuel, subsector = subsector0)

    # ===================================================

    # Produce outputs
    create_xml("electricity_USA.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_logit_tables_xml(L223.Sector_Investment, "Supplysector") %>%
      add_logit_tables_xml_generate_levels(L223.SubsectorLogit_Investment, "SubsectorLogit",
                                           "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data_generate_levels(L223.SubsectorShrwt_Investment, "SubsectorShrwt",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data_generate_levels(L223.StubTech_Investment, "StubTech",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data(L223.GlobalTechCost_CapacityCreditCalulator, "GlobalInvestTechCapacityCredit") %>%
      add_xml_data(rename(L223.GlobalIntTechEff_Investment, intermittent.technology = technology), "GlobalIntTechEff") %>%
      add_xml_data(rename(L223.GlobalIntInvTechMaxCapFac_Investment, int.invest.technology = technology), "GlobalIntInvTechMaxCapFac") %>%
      add_xml_data(L233.GlobalInvestTech_Investment, "GlobalInvestTech") %>%
      add_xml_data(L223.GlobalTechEff_Investment, "GlobalTechEff") %>%
      add_xml_data_generate_levels(L223.StubTechMarket_Investment, "StubTechMarket",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data(L223.GlobalTechCoef_Investment_cool, "GlobalTechCoef") %>%
      add_xml_data_generate_levels(L223.StubTechCoef_Investment_cool, "StubTechCoef",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data(L223.GlobalTechOMfixed_Investment, "GlobalTechOMfixed") %>%
      add_xml_data(L223.GlobalTechOMvar_Investment, "GlobalTechOMvar") %>%
      add_xml_data(L223.GlobalTechCapital_Investment, "GlobalTechCapital") %>%
      add_xml_data(L223.GlobalTechCapital_Investment_cool, "GlobalTechCapital") %>%
      add_xml_data(L223.GlobalTechShrwt_Investment, "GlobalTechShrwt") %>%
      add_xml_data_generate_levels(L223.StubTechShrwt_Investment_USA, "StubTechShrwt",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data_generate_levels(L223.StubTechInterp_Investment_USA, "StubTechInterp",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data(L223.GlobalTechCapFac_Investment, "GlobalTechCapFac") %>%
      add_xml_data_generate_levels(L223.TechCapFac_Investment, "TechCapFac",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data(L223.GlobalTechCapture_Investment, "GlobalTechCapture") %>%
      add_xml_data(L223.GlobalTechCost_Investment, "GlobalTechCapital") %>%
      add_xml_data_generate_levels(L223.TechTrialMarket_Investment, "InvestTechTrialMktName",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data_generate_levels(L223.StubTechCost_offshore_wind_Investment, "StubTechCost",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_node_equiv_xml("subsector") %>%
      add_logit_tables_xml(L223.SubsectorLogit_Investment_Fuel, "SubsectorLogit") %>%
      add_xml_data(L223.SubsectorShrwt_Investment_Fuel, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorInterpTo_Investment_Fuel, "SubsectorInterpTo") %>%
      add_logit_tables_xml(L223.Sector_Investment_StateShare, "Supplysector") %>%
      add_logit_tables_xml(L223.Subsector_Investment_StateShare, "SubsectorLogit") %>%
      add_xml_data(L223.SubsectorShrwtFllt_Investment_StateShare, "SubsectorShrwtFllt") %>%
      add_xml_data(L223.TechCoef_Investment_StateShare, "TechCoef") %>%
      add_xml_data(L223.TechPmult_Investment_StateShare, "TechPmult") %>%
      add_xml_data(L223.TechShrwt_Investment_StateShare, "TechShrwt")  %>%
      add_xml_data(L223.DispatchSector, "DispatchSector")  %>%
      add_logit_tables_xml(L223.Sector_Dispatch, "Supplysector") %>%
      add_logit_tables_xml(L223.SubsectorLogit_Dispatch, "SubsectorLogit") %>%
      add_xml_data(L223.SubsectorShrwtFllt_Dispatch, "SubsectorShrwtFllt") %>%
      add_xml_data(L223.CapacityTechSegmentCapFac, "CapacityTechSegmentCapFac")  %>%
      add_xml_data(L223.TechTrialMarket_Dispatch, "CapacityTechTrialMktName") %>%
      add_xml_data(L223.CapacityTech_FutureTechs, "CapacityTech") %>%
      add_xml_data(L223.CapacityTechMinCapFac, "CapacityTechMinCapFac")  %>%
      add_xml_data(L223.CapacityTech, "CapacityTech") %>%
      add_xml_data(L223.TechShrwt_Dispatch, "TechShrwt")  %>%
      add_xml_data(L223.TechEff_Dispatch, "TechEff") %>%
      add_xml_data(rename(L223.StubTechEffFlag_Dispatch, stub.technology = technology), "StubTechEffFlag") %>%
      add_xml_data(L223.TechCoef_Dispatch_cool, "TechCoef") %>%
      add_xml_data(L223.TechOMvar_Dispatch, "TechOMvar")  %>%
      add_xml_data(L223.TechOMfixed_Dispatch, "TechOMfixed") %>%
      add_xml_data(L223.TechLifetime_Dispatch, "TechLifetime") %>%
      add_xml_data(L223.TechProfitShutdown_Dispatch, "StubTechProfitShutdown") %>%
      add_xml_data(L223.TechSCurve_Dispatch, "TechSCurve") %>%
      add_xml_data(L223.TechCapFac_Dispatch, "TechCapFac") %>%
      add_xml_data(L223.TechCarbonCapture_Dispatch, "CarbonCapture") %>%
      add_xml_data(L223.Production_Dispatch, "Production") %>%
      add_xml_data(L223.TechEff_Cal, "TechEff") %>%
      add_xml_data(L223.PrimaryRenewKeyword_Dispatch_USA, "TechPrimaryRenewKeyword") %>%
      add_xml_data(L223.AvgFossilEffKeyword_Dispatch_USA, "TechAvgFossilEffKeyword") %>%
      add_logit_tables_xml(L223.Sector_Dispatch_Grid, "Supplysector") %>%
      add_xml_data(L223.DispatchSectorDispatchSegments, "DispatchSectorDispatchSegments") %>%
      add_xml_data(L223.InterestRate_FERC, "InterestRate") %>%
      add_xml_data(L223.Pop_FERC, "Pop") %>%
      add_xml_data(L223.BaseGDP_FERC, "BaseGDP") %>%
      add_xml_data(L223.LaborForceFillout_FERC, "LaborForceFillout") %>%
      add_xml_data(L2232.DeleteSupplysector_USAelec, "DeleteSupplysector") %>%
      add_logit_tables_xml(L2232.Supplysector_USAelec, "Supplysector") %>%
      add_xml_data(L2232.SubsectorShrwtFllt_USAelec, "SubsectorShrwtFllt") %>%
      add_xml_data(L2232.SubsectorInterp_USAelec, "SubsectorInterp") %>%
      add_xml_data(L2232.SubsectorInterpTo_USAelec, "SubsectorInterpTo") %>%
      add_logit_tables_xml(L2232.SubsectorLogit_USAelec, "SubsectorLogit") %>%
      add_xml_data(L2232.TechShrwt_USAelec, "TechShrwt") %>%
      add_xml_data(L2232.TechCoef_USAelec, "TechCoef") %>%
      add_xml_data(L2232.Production_exports_USAelec, "Production") %>%
      add_logit_tables_xml(L2232.Supplysector_elec_FERC, "Supplysector") %>%
      add_xml_data(L2232.ElecReserve_FERC, "ElecReserve") %>%
      add_xml_data(L2232.SubsectorShrwtFllt_elec_FERC, "SubsectorShrwtFllt") %>%
      add_xml_data(L2232.SubsectorInterp_elec_FERC, "SubsectorInterp") %>%
      add_xml_data(L2232.SubsectorInterpTo_elec_FERC, "SubsectorInterpTo") %>%
      add_logit_tables_xml(L2232.SubsectorLogit_elec_FERC, "SubsectorLogit") %>%
      add_xml_data(L2232.TechShrwt_elec_FERC, "TechShrwt") %>%
      add_xml_data(L2232.TechCoef_elec_FERC, "TechCoef") %>%
      add_xml_data(L2232.TechCoef_elecownuse_FERC, "TechCoef") %>%
      add_xml_data(L2232.Production_imports_FERC, "Production") %>%
      add_xml_data(L2232.Production_elec_gen_FERC, "Production") %>%
      add_precursors("L223.Sector_Investment",
                     "L223.SubsectorLogit_Investment_Fuel",
                     "L223.SubsectorShrwt_Investment_Fuel",
                     "L223.SubsectorInterpTo_Investment_Fuel",
                     "L223.SubsectorLogit_Investment",
                     "L223.SubsectorShrwt_Investment",
                     "L223.StubTech_Investment",
                     "L233.GlobalInvestTech_Investment",
                     "L223.GlobalTechEff_Investment",
                     "L223.StubTechMarket_Investment",
                     "L223.GlobalTechCoef_Investment_cool",
                     "L223.StubTechCoef_Investment_cool",
                     "L223.GlobalTechOMfixed_Investment",
                     "L223.GlobalTechOMvar_Investment",
                     "L223.GlobalTechCapital_Investment",
                     "L223.GlobalTechCapital_Investment_cool",
                     "L223.GlobalIntTechEff_Investment",
                     "L223.GlobalIntInvTechMaxCapFac_Investment",
                     "L223.GlobalTechShrwt_Investment",
                     "L223.StubTechInterp_Investment_USA",
                     "L223.StubTechShrwt_Investment_USA",
                     "L223.GlobalTechCapFac_Investment",
                     "L223.TechCapFac_Investment",
                     "L223.GlobalTechCapture_Investment",
                     "L223.GlobalTechCost_Investment",
                     "L223.GlobalTechCost_CapacityCreditCalulator",
                     "L223.Sector_Investment_StateShare",
                     "L223.Subsector_Investment_StateShare",
                     "L223.SubsectorShrwtFllt_Investment_StateShare",
                     "L223.TechCoef_Investment_StateShare",
                     "L223.TechPmult_Investment_StateShare",
                     "L223.TechShrwt_Investment_StateShare",
                     "L223.DispatchSector",
                     "L223.Sector_Dispatch",
                     "L223.SubsectorLogit_Dispatch",
                     "L223.SubsectorShrwtFllt_Dispatch",
                     "L223.CapacityTech_FutureTechs",
                     "L223.CapacityTechSegmentCapFac",
                     "L223.CapacityTechMinCapFac",
                     "L223.CapacityTech",
                     "L223.TechShrwt_Dispatch",
                     "L223.TechEff_Dispatch",
                     "L223.StubTechEffFlag_Dispatch",
                     "L223.TechCoef_Dispatch_cool",
                     "L223.TechOMvar_Dispatch",
                     "L223.TechOMfixed_Dispatch",
                     "L223.TechLifetime_Dispatch",
                     "L223.TechProfitShutdown_Dispatch",
                     "L223.TechSCurve_Dispatch",
                     "L223.TechCapFac_Dispatch",
                     "L223.TechCarbonCapture_Dispatch",
                     "L223.Production_Dispatch",
                     "L223.TechEff_Cal",
                     "L223.PrimaryRenewKeyword_Dispatch_USA",
                     "L223.AvgFossilEffKeyword_Dispatch_USA",
                     "L223.TechTrialMarket_Dispatch",
                     "L223.TechTrialMarket_Investment",
                     "L223.Sector_Dispatch_Grid",
                     "L223.DispatchSectorDispatchSegments",
                     "L223.InterestRate_FERC",
                     "L223.Pop_FERC",
                     "L223.BaseGDP_FERC",
                     "L223.StubTechCost_offshore_wind_Investment",
                     "L223.LaborForceFillout_FERC",
                     "L2232.DeleteSupplysector_USAelec",
                     "L2232.Supplysector_USAelec",
                     "L2232.SubsectorShrwtFllt_USAelec",
                     "L2232.SubsectorInterp_USAelec",
                     "L2232.SubsectorInterpTo_USAelec",
                     "L2232.SubsectorLogit_USAelec",
                     "L2232.TechShrwt_USAelec",
                     "L2232.TechCoef_USAelec",
                     "L2232.Production_exports_USAelec",
                     "L2232.Supplysector_elec_FERC",
                     "L2232.ElecReserve_FERC",
                     "L2232.SubsectorShrwtFllt_elec_FERC",
                     "L2232.SubsectorInterp_elec_FERC",
                     "L2232.SubsectorInterpTo_elec_FERC",
                     "L2232.SubsectorLogit_elec_FERC",
                     "L2232.TechShrwt_elec_FERC",
                     "L2232.TechCoef_elec_FERC",
                     "L2232.TechCoef_elecownuse_FERC",
                     "L2232.Production_imports_FERC",
                     "L2232.Production_elec_gen_FERC") ->
      electricity_USA.xml


    # Updates for IM3 Nuclear Scenarios (6 Apr 2023)
    #...................................................
    # -- Scenario 1: CURRENT (No nuclear in states with no nuclear in base year 2015) No modifications needed. This is the current IM3 GCAM-USA runs we have.
    # -- Scenario 2: ALLOW_NEW_NUCLEAR (Allow new GENIII/SMR nuclear in all states that are suitable, even if they have no nuclear in the base year)
    # -- Scenario 3: NUCLEAR_MORATORIUM (No nuclear in 12 states of the US and the unsuitable states, but allow nuclear in all other states, even if they have no nuclear in the base year.)
    # Will modify L223.SubsectorInterpTo_Investment_Fuel & L223.StubTechShrwt_Investment_USA to update to 1's for all suitable states for scenario 2, 0's for unsuitable
    # Will modify L223.SubsectorInterpTo_Investment_Fuel & L223.StubTechShrwt_Investment_USA to update to 1's for all suitable states for scenario 3, 0's for unsuitable + 12 states with declared moratorium

    # Scenario 2 - ssp5
    # electricity_USA_allow_new_nuc_ssp5.xml
    # Unsuitable States for ssp5 (From CERF/Kendall):
    pond_cool <- c("DC", "DE", "NJ", "NV", "RI")
    recirc_cool <- c("DC", "DE", "RI")
    seawater_cool <- c("AL", "AR", "AZ", "CA", "CO", "CT", "DC", "IA", "ID", "IL", "IN", "KS", "KY" ,"MD", "MI", "MN", "MO", "MS", "MT", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SD", "TN", "UT", "VT", "WA", "WI", "WV", "WY")
    once_cool <- c("AZ", "CA", "CO", "DC", "DE", "MD", "MI", "NC", "NH", "NJ", "NM", "NV", "RI", "UT", "VA", "VT", "WY")

    # Identify states that are unsuitable for any nuclear technologies
    unsuitable_nuc <- Reduce(intersect, list(pond_cool, recirc_cool, seawater_cool, once_cool))

    # Produce outputs
    L223.SubsectorShrwt_Investment_Fuel_allow_new_nuc_ssp5 <- L223.SubsectorShrwt_Investment_Fuel
    L223.SubsectorShrwt_Investment_allow_new_nuc_ssp5 <- L223.SubsectorShrwt_Investment
    L223.StubTechShrwt_Investment_USA_allow_new_nuc_ssp5 <- L223.StubTechShrwt_Investment_USA
    L223.TechShrwt_Dispatch_allow_new_nuc_ssp5 <- L223.TechShrwt_Dispatch

    # Redefine the shareweight for nuclear subsector based on scenario 2 restrictions
    L223.SubsectorShrwt_Investment_Fuel_allow_new_nuc_ssp5 <-
      dplyr::mutate(
        L223.SubsectorShrwt_Investment_Fuel_allow_new_nuc_ssp5,
        share.weight = dplyr::case_when(
          (subsector == "nuclear" & region %in% unsuitable_nuc) ~ 0,
          (subsector == "nuclear" & !(region %in% unsuitable_nuc) & year > 2015) ~ 1,
          TRUE ~ share.weight
        )
      )

    # Redefine the share weight subsector technology (nuclear Gen III) based on scenario 2 restrictions
    L223.SubsectorShrwt_Investment_allow_new_nuc_ssp5 <-
      dplyr::mutate(
        L223.SubsectorShrwt_Investment_allow_new_nuc_ssp5,
        share.weight = dplyr::case_when(
          (subsector0 == "nuclear" & region %in% unsuitable_nuc) ~ 0,
          (subsector0 == "nuclear" & !(region %in% unsuitable_nuc) & year > 2015) ~ 1,
          TRUE ~ share.weight
        )
      )

    # Redefine the share weight subsector technology (nuclear Gen III cooling types) based on scenario 2 restrictions
    L223.StubTechShrwt_Investment_USA_allow_new_nuc_ssp5 <-
      dplyr::mutate(
        L223.StubTechShrwt_Investment_USA_allow_new_nuc_ssp5,
        share.weight = dplyr::case_when(
          (subsector0 == "nuclear" & stub.technology == "Gen_III (once through)" & region %in% once_cool) ~ 0,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (once through)" & !(region %in% once_cool) & year > 2015) ~ 1,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (cooling pond)" & region %in% pond_cool) ~ 0,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (cooling pond)" & !(region %in% pond_cool) & year > 2015) ~ 1,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (recirculating)" & region %in% recirc_cool) ~ 0,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (recirculating)" & !(region %in% recirc_cool) & year > 2015) ~ 1,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (seawater)" & region %in% seawater_cool) ~ 0,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (seawater)" & !(region %in% seawater_cool) & year > 2015) ~ 1,
          TRUE ~ share.weight
        )
      )

    # Redefine the share weight subsector technology (nuclear Gen II/III cooling types) based on scenario 2 restrictions
    # Set Gen II share weights to zero
    L223.TechShrwt_Dispatch_allow_new_nuc_ssp5 <-
      dplyr::mutate(
        L223.TechShrwt_Dispatch_allow_new_nuc_ssp5,
        share.weight = dplyr::case_when(
          (subsector == "nuclear" & grepl("Gen_II_LWR", technology)) ~ 0,
          (subsector == "nuclear" & technology == "Gen_III (once through)" & region %in% once_cool) ~ 0,
          (subsector == "nuclear" & technology == "Gen_III (once through)" & !(region %in% once_cool) & year > 2015) ~ 1,
          (subsector == "nuclear" & technology == "Gen_III (cooling pond)" & region %in% pond_cool) ~ 0,
          (subsector == "nuclear" & technology == "Gen_III (cooling pond)" & !(region %in% pond_cool) & year > 2015) ~ 1,
          (subsector == "nuclear" & technology == "Gen_III (recirculating)" & region %in% recirc_cool) ~ 0,
          (subsector == "nuclear" & technology == "Gen_III (recirculating)" & !(region %in% recirc_cool) & year > 2015) ~ 1,
          (subsector == "nuclear" & technology == "Gen_III (seawater)" & region %in% seawater_cool) ~ 0,
          (subsector == "nuclear" & technology == "Gen_III (seawater)" & !(region %in% seawater_cool) & year > 2015) ~ 1,
          TRUE ~ share.weight
        )
      )

    create_xml("electricity_USA_allow_new_nuc_ssp5.xml") %>%
      add_xml_data_generate_levels(L223.SubsectorShrwt_Investment_allow_new_nuc_ssp5, "SubsectorShrwt",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data_generate_levels(L223.StubTechShrwt_Investment_USA_allow_new_nuc_ssp5, "StubTechShrwt",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data(L223.SubsectorShrwt_Investment_Fuel_allow_new_nuc_ssp5, "SubsectorShrwt") %>%
      add_xml_data(L223.TechShrwt_Dispatch_allow_new_nuc_ssp5, "TechShrwt")  %>%
      add_precursors("L223.SubsectorShrwt_Investment_Fuel",
                     "L223.SubsectorShrwt_Investment",
                     "L223.StubTechShrwt_Investment_USA",
                     "L223.TechShrwt_Dispatch") ->
      electricity_USA_allow_new_nuc_ssp5.xml


    # Scenario 3 - ssp 5
    # electricity_USA_no_moratorium_nuc_ssp5.xml
    # Moratorium States: "CA", "CT", "HI", "IL", "ME", "MA", "MN", "NJ", "NY", "OR", "RI", "VT"
    # Unsuitable States (From CERF/Kendall)

    # Define moratorium states
    moratorium <- c("CA", "CT", "HI", "IL", "ME", "MA", "MN", "NJ", "NY", "OR", "RI", "VT")

    # Identify states that are unsuitable for any nuclear technologies including moratorium states
    unsuitable_moratorium <- unique(c(unsuitable_nuc, moratorium))

    # Produce outputs
    L223.SubsectorShrwt_Investment_Fuel_no_moratorium_nuc_ssp5 <- L223.SubsectorShrwt_Investment_Fuel
    L223.SubsectorShrwt_Investment_no_moratorium_nuc_ssp5 <- L223.SubsectorShrwt_Investment
    L223.StubTechShrwt_Investment_USA_no_moratorium_nuc_ssp5 <- L223.StubTechShrwt_Investment_USA
    L223.TechShrwt_Dispatch_no_moratorium_nuc_ssp5 <- L223.TechShrwt_Dispatch

    # Redefine the shareweight for nuclear subsector based on scenario 3 restrictions
    L223.SubsectorShrwt_Investment_Fuel_no_moratorium_nuc_ssp5 <-
      dplyr::mutate(
        L223.SubsectorShrwt_Investment_Fuel_no_moratorium_nuc_ssp5,
        share.weight = dplyr::case_when(
          (subsector == "nuclear" & region %in% unsuitable_moratorium) ~ 0,
          (subsector == "nuclear" & !(region %in% unsuitable_moratorium) & year > 2015) ~ 1,
          TRUE ~ share.weight
        )
      )

    # Redefine the share weight subsector technology (nuclear Gen III) based on scenario 3 restrictions
    L223.SubsectorShrwt_Investment_no_moratorium_nuc_ssp5 <-
      dplyr::mutate(
        L223.SubsectorShrwt_Investment_no_moratorium_nuc_ssp5,
        share.weight = dplyr::case_when(
          (subsector0 == "nuclear" & region %in% unsuitable_moratorium) ~ 0,
          (subsector0 == "nuclear" & !(region %in% unsuitable_moratorium) & year > 2015) ~ 1,
          TRUE ~ share.weight
        )
      )

    # Redefine the share weight subsector technology (nuclear Gen III cooling types) based on scenario 3 restrictions
    L223.StubTechShrwt_Investment_USA_no_moratorium_nuc_ssp5 <-
      dplyr::mutate(
        L223.StubTechShrwt_Investment_USA_no_moratorium_nuc_ssp5,
        share.weight = dplyr::case_when(
          (subsector0 == "nuclear" & subsector == "Gen_III" & region %in% moratorium) ~ 0,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (once through)" & region %in% once_cool) ~ 0,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (once through)" & !(region %in% once_cool) & year > 2015) ~ 1,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (cooling pond)" & region %in% pond_cool) ~ 0,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (cooling pond)" & !(region %in% pond_cool) & year > 2015) ~ 1,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (recirculating)" & region %in% recirc_cool) ~ 0,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (recirculating)" & !(region %in% recirc_cool) & year > 2015) ~ 1,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (seawater)" & region %in% seawater_cool) ~ 0,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (seawater)" & !(region %in% seawater_cool) & year > 2015) ~ 1,
          TRUE ~ share.weight
        )
      )

    # Redefine the share weight subsector technology (nuclear Gen II/III cooling types) based on scenario 3 restrictions
    # Set Gen II share weights to zero
    L223.TechShrwt_Dispatch_no_moratorium_nuc_ssp5 <-
      dplyr::mutate(
        L223.TechShrwt_Dispatch_no_moratorium_nuc_ssp5,
        share.weight = dplyr::case_when(
          (subsector == "nuclear" & grepl("Gen_II_LWR", technology)) ~ 0,
          (subsector == "nuclear" & grepl("Gen_III", technology) & region %in% moratorium) ~ 0,
          (subsector == "nuclear" & technology == "Gen_III (once through)" & region %in% once_cool) ~ 0,
          (subsector == "nuclear" & technology == "Gen_III (once through)" & !(region %in% once_cool) & year > 2015) ~ 1,
          (subsector == "nuclear" & technology == "Gen_III (cooling pond)" & region %in% pond_cool) ~ 0,
          (subsector == "nuclear" & technology == "Gen_III (cooling pond)" & !(region %in% pond_cool) & year > 2015) ~ 1,
          (subsector == "nuclear" & technology == "Gen_III (recirculating)" & region %in% recirc_cool) ~ 0,
          (subsector == "nuclear" & technology == "Gen_III (recirculating)" & !(region %in% recirc_cool) & year > 2015) ~ 1,
          (subsector == "nuclear" & technology == "Gen_III (seawater)" & region %in% seawater_cool) ~ 0,
          (subsector == "nuclear" & technology == "Gen_III (seawater)" & !(region %in% seawater_cool) & year > 2015) ~ 1,
          TRUE ~ share.weight
        )
      )

    create_xml("electricity_USA_no_moratorium_nuc_ssp5.xml") %>%
      add_xml_data_generate_levels(L223.SubsectorShrwt_Investment_no_moratorium_nuc_ssp5, "SubsectorShrwt",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data_generate_levels(L223.StubTechShrwt_Investment_USA_no_moratorium_nuc_ssp5, "StubTechShrwt",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data(L223.SubsectorShrwt_Investment_Fuel_no_moratorium_nuc_ssp5, "SubsectorShrwt") %>%
      add_xml_data(L223.TechShrwt_Dispatch_no_moratorium_nuc_ssp5, "TechShrwt")  %>%
      add_precursors("L223.SubsectorShrwt_Investment_Fuel",
                     "L223.SubsectorShrwt_Investment",
                     "L223.StubTechShrwt_Investment_USA",
                     "L223.TechShrwt_Dispatch") ->
      electricity_USA_no_moratorium_nuc_ssp5.xml


    # Scenario 2 - ssp3
    # electricity_USA_allow_new_nuc_ssp3.xml
    # Unsuitable States for ssp3 (From CERF/Kendall):
    pond_cool <- c("DC", "DE", "NJ", "RI")
    recirc_cool <- c("DC", "DE", "RI")
    seawater_cool <- c("AL", "AR", "AZ", "CA", "CO", "CT", "DC", "IA", "ID", "IL", "IN", "KS", "KY", "MD", "MI", "MN", "MO", "MS", "MT", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SD", "TN", "UT", "VT", "WA", "WI", "WV", "WY")
    once_cool <- c("AZ", "CO", "DC", "DE", "MI", "NC", "NH", "NJ", "NM", "RI", "UT", "VT", "WY")

    # Identify states that are unsuitable for any nuclear technologies
    unsuitable_nuc <- Reduce(intersect, list(pond_cool, recirc_cool, seawater_cool, once_cool))

    # Produce outputs
    L223.SubsectorShrwt_Investment_Fuel_allow_new_nuc_ssp3 <- L223.SubsectorShrwt_Investment_Fuel
    L223.SubsectorShrwt_Investment_allow_new_nuc_ssp3 <- L223.SubsectorShrwt_Investment
    L223.StubTechShrwt_Investment_USA_allow_new_nuc_ssp3 <- L223.StubTechShrwt_Investment_USA
    L223.TechShrwt_Dispatch_allow_new_nuc_ssp3 <- L223.TechShrwt_Dispatch

    # Redefine the shareweight for nuclear subsector based on scenario 2 restrictions
    L223.SubsectorShrwt_Investment_Fuel_allow_new_nuc_ssp3 <-
      dplyr::mutate(
        L223.SubsectorShrwt_Investment_Fuel_allow_new_nuc_ssp3,
        share.weight = dplyr::case_when(
          (subsector == "nuclear" & region %in% unsuitable_nuc) ~ 0,
          (subsector == "nuclear" & !(region %in% unsuitable_nuc) & year > 2015) ~ 1,
          TRUE ~ share.weight
        )
      )

    # Redefine the share weight subsector technology (nuclear Gen III) based on scenario 2 restrictions
    L223.SubsectorShrwt_Investment_allow_new_nuc_ssp3 <-
      dplyr::mutate(
        L223.SubsectorShrwt_Investment_allow_new_nuc_ssp3,
        share.weight = dplyr::case_when(
          (subsector0 == "nuclear" & region %in% unsuitable_nuc) ~ 0,
          (subsector0 == "nuclear" & !(region %in% unsuitable_nuc) & year > 2015) ~ 1,
          TRUE ~ share.weight
        )
      )

    # Redefine the share weight subsector technology (nuclear Gen III cooling types) based on scenario 2 restrictions
    L223.StubTechShrwt_Investment_USA_allow_new_nuc_ssp3 <-
      dplyr::mutate(
        L223.StubTechShrwt_Investment_USA_allow_new_nuc_ssp3,
        share.weight = dplyr::case_when(
          (subsector0 == "nuclear" & stub.technology == "Gen_III (once through)" & region %in% once_cool) ~ 0,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (once through)" & !(region %in% once_cool) & year > 2015) ~ 1,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (cooling pond)" & region %in% pond_cool) ~ 0,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (cooling pond)" & !(region %in% pond_cool) & year > 2015) ~ 1,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (recirculating)" & region %in% recirc_cool) ~ 0,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (recirculating)" & !(region %in% recirc_cool) & year > 2015) ~ 1,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (seawater)" & region %in% seawater_cool) ~ 0,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (seawater)" & !(region %in% seawater_cool) & year > 2015) ~ 1,
          TRUE ~ share.weight
        )
      )

    # Redefine the share weight subsector technology (nuclear Gen II/III cooling types) based on scenario 2 restrictions
    # Set Gen II share weights to zero
    L223.TechShrwt_Dispatch_allow_new_nuc_ssp3 <-
      dplyr::mutate(
        L223.TechShrwt_Dispatch_allow_new_nuc_ssp3,
        share.weight = dplyr::case_when(
          (subsector == "nuclear" & grepl("Gen_II_LWR", technology)) ~ 0,
          (subsector == "nuclear" & technology == "Gen_III (once through)" & region %in% once_cool) ~ 0,
          (subsector == "nuclear" & technology == "Gen_III (once through)" & !(region %in% once_cool) & year > 2015) ~ 1,
          (subsector == "nuclear" & technology == "Gen_III (cooling pond)" & region %in% pond_cool) ~ 0,
          (subsector == "nuclear" & technology == "Gen_III (cooling pond)" & !(region %in% pond_cool) & year > 2015) ~ 1,
          (subsector == "nuclear" & technology == "Gen_III (recirculating)" & region %in% recirc_cool) ~ 0,
          (subsector == "nuclear" & technology == "Gen_III (recirculating)" & !(region %in% recirc_cool) & year > 2015) ~ 1,
          (subsector == "nuclear" & technology == "Gen_III (seawater)" & region %in% seawater_cool) ~ 0,
          (subsector == "nuclear" & technology == "Gen_III (seawater)" & !(region %in% seawater_cool) & year > 2015) ~ 1,
          TRUE ~ share.weight
        )
      )

    create_xml("electricity_USA_allow_new_nuc_ssp3.xml") %>%
      add_xml_data_generate_levels(L223.SubsectorShrwt_Investment_allow_new_nuc_ssp3, "SubsectorShrwt",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data_generate_levels(L223.StubTechShrwt_Investment_USA_allow_new_nuc_ssp3, "StubTechShrwt",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data(L223.SubsectorShrwt_Investment_Fuel_allow_new_nuc_ssp3, "SubsectorShrwt") %>%
      add_xml_data(L223.TechShrwt_Dispatch_allow_new_nuc_ssp3, "TechShrwt")  %>%
      add_precursors("L223.SubsectorShrwt_Investment_Fuel",
                     "L223.SubsectorShrwt_Investment",
                     "L223.StubTechShrwt_Investment_USA",
                     "L223.TechShrwt_Dispatch") ->
      electricity_USA_allow_new_nuc_ssp3.xml


    # Scenario 3 - ssp 3
    # electricity_USA_no_moratorium_nuc_ssp3.xml
    # Moratorium States: "CA", "CT", "HI", "IL", "ME", "MA", "MN", "NJ", "NY", "OR", "RI", "VT"
    # Unsuitable States (From CERF/Kendall)

    # Identify states that are unsuitable for any nuclear technologies including moratorium states
    unsuitable_moratorium <- unique(c(unsuitable_nuc, moratorium))

    # Produce outputs
    L223.SubsectorShrwt_Investment_Fuel_no_moratorium_nuc_ssp3 <- L223.SubsectorShrwt_Investment_Fuel
    L223.SubsectorShrwt_Investment_no_moratorium_nuc_ssp3 <- L223.SubsectorShrwt_Investment
    L223.StubTechShrwt_Investment_USA_no_moratorium_nuc_ssp3 <- L223.StubTechShrwt_Investment_USA
    L223.TechShrwt_Dispatch_no_moratorium_nuc_ssp3 <- L223.TechShrwt_Dispatch

    # Redefine the shareweight for nuclear subsector based on scenario 3 restrictions
    L223.SubsectorShrwt_Investment_Fuel_no_moratorium_nuc_ssp3 <-
      dplyr::mutate(
        L223.SubsectorShrwt_Investment_Fuel_no_moratorium_nuc_ssp3,
        share.weight = dplyr::case_when(
          (subsector == "nuclear" & region %in% unsuitable_moratorium) ~ 0,
          (subsector == "nuclear" & !(region %in% unsuitable_moratorium) & year > 2015) ~ 1,
          TRUE ~ share.weight
        )
      )

    # Redefine the share weight subsector technology (nuclear Gen III) based on scenario 3 restrictions
    L223.SubsectorShrwt_Investment_no_moratorium_nuc_ssp3 <-
      dplyr::mutate(
        L223.SubsectorShrwt_Investment_no_moratorium_nuc_ssp3,
        share.weight = dplyr::case_when(
          (subsector0 == "nuclear" & region %in% unsuitable_moratorium) ~ 0,
          (subsector0 == "nuclear" & !(region %in% unsuitable_moratorium) & year > 2015) ~ 1,
          TRUE ~ share.weight
        )
      )

    # Redefine the share weight subsector technology (nuclear Gen III cooling types) based on scenario 3 restrictions
    L223.StubTechShrwt_Investment_USA_no_moratorium_nuc_ssp3 <-
      dplyr::mutate(
        L223.StubTechShrwt_Investment_USA_no_moratorium_nuc_ssp3,
        share.weight = dplyr::case_when(
          (subsector0 == "nuclear" & subsector == "Gen_III" & region %in% moratorium) ~ 0,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (once through)" & region %in% once_cool) ~ 0,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (once through)" & !(region %in% once_cool) & year > 2015) ~ 1,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (cooling pond)" & region %in% pond_cool) ~ 0,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (cooling pond)" & !(region %in% pond_cool) & year > 2015) ~ 1,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (recirculating)" & region %in% recirc_cool) ~ 0,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (recirculating)" & !(region %in% recirc_cool) & year > 2015) ~ 1,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (seawater)" & region %in% seawater_cool) ~ 0,
          (subsector0 == "nuclear" & stub.technology == "Gen_III (seawater)" & !(region %in% seawater_cool) & year > 2015) ~ 1,
          TRUE ~ share.weight
        )
      )

    # Redefine the share weight subsector technology (nuclear Gen II/III cooling types) based on scenario 3 restrictions
    # Set Gen II share weights to zero
    L223.TechShrwt_Dispatch_no_moratorium_nuc_ssp3 <-
      dplyr::mutate(
        L223.TechShrwt_Dispatch_no_moratorium_nuc_ssp3,
        share.weight = dplyr::case_when(
          (subsector == "nuclear" & grepl("Gen_II_LWR", technology)) ~ 0,
          (subsector == "nuclear" & grepl("Gen_III", technology) & region %in% moratorium) ~ 0,
          (subsector == "nuclear" & technology == "Gen_III (once through)" & region %in% once_cool) ~ 0,
          (subsector == "nuclear" & technology == "Gen_III (once through)" & !(region %in% once_cool) & year > 2015) ~ 1,
          (subsector == "nuclear" & technology == "Gen_III (cooling pond)" & region %in% pond_cool) ~ 0,
          (subsector == "nuclear" & technology == "Gen_III (cooling pond)" & !(region %in% pond_cool) & year > 2015) ~ 1,
          (subsector == "nuclear" & technology == "Gen_III (recirculating)" & region %in% recirc_cool) ~ 0,
          (subsector == "nuclear" & technology == "Gen_III (recirculating)" & !(region %in% recirc_cool) & year > 2015) ~ 1,
          (subsector == "nuclear" & technology == "Gen_III (seawater)" & region %in% seawater_cool) ~ 0,
          (subsector == "nuclear" & technology == "Gen_III (seawater)" & !(region %in% seawater_cool) & year > 2015) ~ 1,
          TRUE ~ share.weight
        )
      )

    create_xml("electricity_USA_no_moratorium_nuc_ssp3.xml") %>%
      add_xml_data_generate_levels(L223.SubsectorShrwt_Investment_no_moratorium_nuc_ssp3, "SubsectorShrwt",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data_generate_levels(L223.StubTechShrwt_Investment_USA_no_moratorium_nuc_ssp3, "StubTechShrwt",
                                   "subsector", "nesting-subsector", 1, FALSE) %>%
      add_xml_data(L223.SubsectorShrwt_Investment_Fuel_no_moratorium_nuc_ssp3, "SubsectorShrwt") %>%
      add_xml_data(L223.TechShrwt_Dispatch_no_moratorium_nuc_ssp3, "TechShrwt")  %>%
      add_precursors("L223.SubsectorShrwt_Investment_Fuel",
                     "L223.SubsectorShrwt_Investment",
                     "L223.StubTechShrwt_Investment_USA",
                     "L223.TechShrwt_Dispatch") ->
      electricity_USA_no_moratorium_nuc_ssp3.xml

    #..................................................


    return_data(electricity_USA.xml,
                electricity_USA_allow_new_nuc_ssp5.xml,
                electricity_USA_allow_new_nuc_ssp3.xml,
                electricity_USA_no_moratorium_nuc_ssp5.xml,
                electricity_USA_no_moratorium_nuc_ssp3.xml)
  } else {
    stop("Unknown command")
  }
}

