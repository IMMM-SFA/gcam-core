# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_socioeconomics_USA_xml
#'
#' Construct XML data structure for \code{socioeconomics_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{socioeconomics_USA.xml}, \code{socioeconomics_USA_SSP1.xml},
#' \code{socioeconomics_USA_SSP2.xml}, \code{socioeconomics_USA_SSP3.xml},
#' \code{socioeconomics_USA_SSP3_rcp85gdp.xml},\code{socioeconomics_USA_SSP4.xml},
#' \code{socioeconomics_USA_SSP5.xml}. The corresponding file in the
#' original data system was \code{batch_socioeconomics_USA.xml} (gcamusa XML).
module_gcamusa_batch_socioeconomics_USA_xml <- function(command, ...) {

  if(command == driver.DECLARE_INPUTS) {
    return(c("L201.Pop_GCAMUSA",
             "L201.Pop_GCAMUSA_SSP",
             "L201.BaseGDP_GCAMUSA",
             "L201.LaborForceFillout_GCAMUSA",
             "L201.LaborProductivity_GCAMUSA",
             "L201.LaborProductivity_gSSP_GCAMUSA",
             "L201.Pop_national_updated_USA",
             "L201.Pop_national_updated_USA_SSP",
             "L201.BaseGDP_national_updated_USA",
             "L201.LaborProductivity_national_updated_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "socioeconomics_USA.xml",
             XML = "socioeconomics_USA_SSP1.xml",
             XML = "socioeconomics_USA_SSP2.xml",
             XML = "socioeconomics_USA_SSP3.xml",
             XML = "socioeconomics_USA_SSP3_rcp85gdp.xml",
             XML = "socioeconomics_USA_SSP4.xml",
             XML = "socioeconomics_USA_SSP5.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L201.Pop_GCAMUSA <- get_data(all_data, "L201.Pop_GCAMUSA")
    L201.Pop_GCAMUSA_SSP <- get_data(all_data, "L201.Pop_GCAMUSA_SSP")
    L201.BaseGDP_GCAMUSA <- get_data(all_data, "L201.BaseGDP_GCAMUSA")
    L201.LaborForceFillout_GCAMUSA <- get_data(all_data, "L201.LaborForceFillout_GCAMUSA")
    L201.LaborProductivity_GCAMUSA <- get_data(all_data, "L201.LaborProductivity_GCAMUSA")
    L201.Pop_national_updated_USA <- get_data(all_data, "L201.Pop_national_updated_USA")
    L201.Pop_national_updated_USA_SSP <- get_data(all_data, "L201.Pop_national_updated_USA_SSP")
    L201.BaseGDP_national_updated_USA <- get_data(all_data, "L201.BaseGDP_national_updated_USA")
    L201.LaborProductivity_national_updated_USA <- get_data(all_data, "L201.LaborProductivity_national_updated_USA")
    L201.LaborProductivity_gSSP_GCAMUSA <- get_data(all_data,"L201.LaborProductivity_gSSP_GCAMUSA")

    # ===================================================

    # Update

    # Produce outputs
    create_xml("socioeconomics_USA.xml") %>%
      add_xml_data(L201.Pop_GCAMUSA, "Pop") %>%
      add_xml_data(L201.BaseGDP_GCAMUSA, "BaseGDP") %>%
      add_xml_data(L201.LaborForceFillout_GCAMUSA, "LaborForceFillout") %>%
      add_xml_data(L201.LaborProductivity_GCAMUSA, "LaborProductivity") %>%
      add_xml_data(L201.Pop_national_updated_USA, "Pop") %>%
      add_xml_data(L201.BaseGDP_national_updated_USA, "BaseGDP") %>%
      add_xml_data(L201.LaborProductivity_national_updated_USA, "LaborProductivity") %>%
      add_precursors("L201.Pop_GCAMUSA",
                     "L201.BaseGDP_GCAMUSA",
                     "L201.LaborForceFillout_GCAMUSA",
                     "L201.LaborProductivity_GCAMUSA",
                     "L201.Pop_national_updated_USA",
                     "L201.BaseGDP_national_updated_USA",
                     "L201.LaborProductivity_national_updated_USA") ->
      socioeconomics_USA.xml

    # Produce outputs SSP1
    create_xml("socioeconomics_USA_SSP1.xml") %>%
      add_xml_data(L201.Pop_GCAMUSA_SSP %>%
                     filter(SSP==1) %>%
                     select(-SSP), "Pop") %>%
      add_xml_data(L201.Pop_national_updated_USA_SSP %>%
                     filter(SSP==1) %>%
                     select(-SSP), "Pop") %>%
      add_xml_data(L201.LaborProductivity_gSSP_GCAMUSA %>%
                     filter(ssp=="ssp1") %>%
                     select(-ssp), "LaborProductivity") %>%
      add_precursors("L201.Pop_GCAMUSA_SSP",
                     "L201.Pop_national_updated_USA_SSP",
                     "L201.LaborProductivity_GCAMUSA",
                     "L201.LaborProductivity_gSSP_GCAMUSA") ->
      socioeconomics_USA_SSP1.xml

    # Produce outputs SSP2
    create_xml("socioeconomics_USA_SSP2.xml") %>%
      add_xml_data(L201.Pop_GCAMUSA_SSP %>%
                     filter(SSP==2) %>%
                     select(-SSP), "Pop") %>%
      add_xml_data(L201.Pop_national_updated_USA_SSP %>%
                     filter(SSP==2) %>%
                     select(-SSP), "Pop")  %>%
      add_xml_data(L201.LaborProductivity_gSSP_GCAMUSA %>%
                     filter(ssp=="ssp2") %>%
                     select(-ssp), "LaborProductivity") %>%
      add_precursors("L201.Pop_GCAMUSA_SSP",
                     "L201.Pop_national_updated_USA_SSP",
                     "L201.LaborProductivity_GCAMUSA",
                     "L201.LaborProductivity_gSSP_GCAMUSA") ->
      socioeconomics_USA_SSP2.xml

    # Produce outputs SSP3
    create_xml("socioeconomics_USA_SSP3.xml") %>%
      add_xml_data(L201.Pop_GCAMUSA_SSP %>%
                     filter(SSP==3) %>%
                     select(-SSP), "Pop") %>%
      add_xml_data(L201.Pop_national_updated_USA_SSP %>%
                     filter(SSP==3) %>%
                     select(-SSP), "Pop")  %>%
      add_xml_data(L201.LaborProductivity_gSSP_GCAMUSA %>%
                     filter(ssp=="ssp3") %>%
                     select(-ssp), "LaborProductivity") %>%
      add_precursors("L201.Pop_GCAMUSA_SSP",
                     "L201.Pop_national_updated_USA_SSP",
                     "L201.LaborProductivity_GCAMUSA",
                     "L201.LaborProductivity_gSSP_GCAMUSA") ->
      socioeconomics_USA_SSP3.xml

    # Produce outputs SSP3_rcp85gdp
    create_xml("socioeconomics_USA_SSP3_rcp85gdp.xml") %>%
      add_xml_data(L201.Pop_GCAMUSA_SSP %>%
                     filter(SSP==3) %>%
                     select(-SSP), "Pop") %>%
      add_xml_data(L201.Pop_national_updated_USA_SSP %>%
                     filter(SSP==3) %>%
                     select(-SSP), "Pop")  %>%
      add_xml_data(L201.LaborProductivity_gSSP_GCAMUSA %>%
                     filter(ssp=="ssp3") %>%
                     dplyr::mutate(laborproductivity = laborproductivity + 0.004) %>%
                     select(-ssp), "LaborProductivity") %>%
      add_precursors("L201.Pop_GCAMUSA_SSP",
                     "L201.Pop_national_updated_USA_SSP",
                     "L201.LaborProductivity_GCAMUSA",
                     "L201.LaborProductivity_gSSP_GCAMUSA") ->
      socioeconomics_USA_SSP3_rcp85gdp.xml

    # Produce outputs SSP4
    create_xml("socioeconomics_USA_SSP4.xml") %>%
      add_xml_data(L201.Pop_GCAMUSA_SSP %>%
                     filter(SSP==4) %>%
                     select(-SSP), "Pop") %>%
      add_xml_data(L201.Pop_national_updated_USA_SSP %>%
                     filter(SSP==4) %>%
                     select(-SSP), "Pop")  %>%
      add_xml_data(L201.LaborProductivity_gSSP_GCAMUSA %>%
                     filter(ssp=="ssp4") %>%
                     select(-ssp), "LaborProductivity") %>%
      add_precursors("L201.Pop_GCAMUSA_SSP",
                     "L201.Pop_national_updated_USA_SSP",
                     "L201.LaborProductivity_GCAMUSA",
                     "L201.LaborProductivity_gSSP_GCAMUSA") ->
      socioeconomics_USA_SSP4.xml

    # Produce outputs SSP5
    create_xml("socioeconomics_USA_SSP5.xml") %>%
      add_xml_data(L201.Pop_GCAMUSA_SSP %>%
                     filter(SSP==5) %>%
                     select(-SSP), "Pop") %>%
      add_xml_data(L201.Pop_national_updated_USA_SSP %>%
                     filter(SSP==4) %>%
                     select(-SSP), "Pop")  %>%
      add_xml_data(L201.LaborProductivity_gSSP_GCAMUSA %>%
                     filter(ssp=="ssp5") %>%
                     select(-ssp), "LaborProductivity") %>%
      add_precursors("L201.Pop_GCAMUSA_SSP",
                     "L201.Pop_national_updated_USA_SSP",
                     "L201.LaborProductivity_GCAMUSA",
                     "L201.LaborProductivity_gSSP_GCAMUSA") ->
      socioeconomics_USA_SSP5.xml


    return_data(socioeconomics_USA.xml,
                socioeconomics_USA_SSP1.xml,
                socioeconomics_USA_SSP2.xml,
                socioeconomics_USA_SSP3.xml,
                socioeconomics_USA_SSP3_rcp85gdp.xml,
                socioeconomics_USA_SSP4.xml,
                socioeconomics_USA_SSP5.xml)
  } else {
    stop("Unknown command")
  }
}
