# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_batch_SSP_xml
#'
#' Construct XML data structure for all the \code{socioeconomics_[g]SSP[1-5].xml} files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{socioeconomics_gSSP1.xml}, \code{socioeconomics_gSSP2.xml}, \code{socioeconomics_gSSP3.xml},
#' \code{socioeconomics_gSSP3_rcp85gdp.xml},\code{socioeconomics_gSSP4.xml}, \code{socioeconomics_gSSP5.xml}, \code{socioeconomics_SSP1.xml},
#' \code{socioeconomics_SSP2.xml}, \code{socioeconomics_SSP3.xml},\code{socioeconomics_SSP3_rcp85gdp.xml}
#' \code{socioeconomics_SSP4.xml}, and \code{socioeconomics_SSP5.xml}.
module_socio_batch_SSP_xml <- function(command, ...) {

  SSP_NUMS <- 1:5

  if(command == driver.DECLARE_INPUTS) {
    return(c("L201.Pop_gSSP1",
             "L201.Pop_gSSP2",
             "L201.Pop_gSSP3",
             "L201.Pop_gSSP4",
             "L201.Pop_gSSP5",
             "L201.Pop_SSP1",
             "L201.Pop_SSP2",
             "L201.Pop_SSP3",
             "L201.Pop_SSP4",
             "L201.Pop_SSP5",
             "L201.BaseGDP_Scen",
             "L201.LaborForceFillout",
             "L201.LaborProductivity_gSSP1",
             "L201.LaborProductivity_gSSP2",
             "L201.LaborProductivity_gSSP3",
             "L201.LaborProductivity_gSSP4",
             "L201.LaborProductivity_gSSP5",
             "L201.LaborProductivity_SSP1",
             "L201.LaborProductivity_SSP2",
             "L201.LaborProductivity_SSP3",
             "L201.LaborProductivity_SSP4",
             "L201.LaborProductivity_SSP5",
             "L201.PPPConvert",
             "L201.BaseGDP_GCAM3",
             "L201.LaborProductivity_GCAM3",
             "L201.Pop_GCAM3"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "socioeconomics_gSSP1.xml",
             XML = "socioeconomics_gSSP2.xml",
             XML = "socioeconomics_gSSP3.xml",
             XML = "socioeconomics_gSSP3_rcp85gdp.xml",
             XML = "socioeconomics_gSSP4.xml",
             XML = "socioeconomics_gSSP5.xml",
             XML = "socioeconomics_SSP1.xml",
             XML = "socioeconomics_SSP2.xml",
             XML = "socioeconomics_SSP3.xml",
             XML = "socioeconomics_SSP3_rcp85gdp.xml",
             XML = "socioeconomics_SSP4.xml",
             XML = "socioeconomics_SSP5.xml",
             XML = "socioeconomics_GCAM3.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    socioeconomics_gSSP1.xml <- socioeconomics_gSSP2.xml <- socioeconomics_gSSP3.xml <-
      socioeconomics_gSSP4.xml <- socioeconomics_gSSP5.xml <- socioeconomics_SSP1.xml <-
      socioeconomics_SSP2.xml <- socioeconomics_SSP3.xml <- socioeconomics_SSP4.xml <-
      socioeconomics_SSP3_rcp85gdp.xml <- socioeconomics_gSSP3_rcp85gdp.xml <-
      socioeconomics_SSP5.xml <- socioeconomics_GCAM3.xml <- NULL  # silence package check notes

    # Load required inputs
    L201.BaseGDP_Scen <- get_data(all_data, "L201.BaseGDP_Scen")
    L201.LaborForceFillout <- get_data(all_data, "L201.LaborForceFillout")
    L201.PPPConvert <- get_data(all_data, "L201.PPPConvert")
    L201.BaseGDP_GCAM3 <- get_data(all_data, "L201.BaseGDP_GCAM3")
    L201.LaborProductivity_GCAM3 <- get_data(all_data, "L201.LaborProductivity_GCAM3")
    L201.Pop_GCAM3 <- get_data(all_data, "L201.Pop_GCAM3")
    L201.Pop_SSP3 <- get_data(all_data, "L201.Pop_SSP3")
    L201.Pop_gSSP3 <- get_data(all_data, "L201.Pop_gSSP3")
    L201.LaborProductivity_SSP3 <- get_data(all_data, "L201.LaborProductivity_SSP3")
    L201.LaborProductivity_gSSP3 <- get_data(all_data, "L201.LaborProductivity_gSSP3")

    for(g in c("g", "")) {
      for(ssp in SSP_NUMS) {
        gssp <- paste0(g, "SSP", ssp)
        xmlfn <- paste0("socioeconomics_", gssp, ".xml")
        popname <- paste0("L201.Pop_", gssp)
        L201.Pop_SSP <- get_data(all_data, popname)
        laborname <- paste0("L201.LaborProductivity_", gssp)
        L201.LaborProductivity_SSP <- get_data(all_data, laborname)

        # Produce output
        create_xml(xmlfn) %>%
          add_xml_data(L201.Pop_SSP, "Pop") %>%
          add_xml_data(L201.BaseGDP_Scen, "BaseGDP") %>%
          add_xml_data(L201.LaborForceFillout, "LaborForceFillout") %>%
          add_xml_data(L201.LaborProductivity_SSP, "LaborProductivity") %>%
          add_xml_data(L201.PPPConvert, "PPPConvert") %>%
          add_precursors(popname, "L201.BaseGDP_Scen", "L201.LaborForceFillout", laborname, "L201.PPPConvert") ->
          x

        # ...and assign into environment
        assign(xmlfn, x)
      }
    }

    # GCAM3 xml
    create_xml("socioeconomics_GCAM3.xml") %>%
      add_xml_data(L201.Pop_GCAM3, "Pop") %>%
      add_xml_data(L201.BaseGDP_GCAM3, "BaseGDP") %>%
      add_xml_data(L201.LaborForceFillout, "LaborForceFillout") %>%
      add_xml_data(L201.LaborProductivity_GCAM3, "LaborProductivity") %>%
      add_xml_data(L201.PPPConvert, "PPPConvert") %>%
      add_precursors("L201.Pop_GCAM3", "L201.BaseGDP_GCAM3", "L201.LaborForceFillout", "L201.LaborProductivity_GCAM3", "L201.PPPConvert") ->
      socioeconomics_GCAM3.xml

    #-------------------------------------------------
    # Modified: 1 Mar 2022, zarrar.khan@pnnl.gov

    # Updating Laborproductivity based on discussion with Brian O'Neil
    # From paper: Renatal et al. 2016, Avoided economic impacts of climate change
    # on agriculture: integrating a land surface model (CLM) with a global economic model (iPETS), Climatic Change.
    # From Brian's email: “The annual GDP growth rate was increased by 0.5 percentage points
    # after 2010 in all regions above the original SSP3 growth rate, leading to a 42% larger global GDP in 2100.”

    L201.LaborProductivity_SSP3_rcp85gdp <- L201.LaborProductivity_SSP3 %>%
      dplyr::mutate(laborproductivity =
                      dplyr::if_else(year > MODEL_FINAL_BASE_YEAR, laborproductivity + 0.005, laborproductivity))

    L201.LaborProductivity_gSSP3_rcp85gdp <- L201.LaborProductivity_gSSP3 %>%
      dplyr::mutate(laborproductivity =
                      dplyr::if_else(year > MODEL_FINAL_BASE_YEAR, laborproductivity + 0.005, laborproductivity))

    # SSP3 Modified with rcp85 GDP
    create_xml("socioeconomics_SSP3_rcp85gdp.xml") %>%
      add_xml_data(L201.Pop_SSP3, "Pop") %>%
      add_xml_data(L201.BaseGDP_Scen, "BaseGDP") %>%
      add_xml_data(L201.LaborForceFillout, "LaborForceFillout") %>%
      add_xml_data(L201.LaborProductivity_SSP3_rcp85gdp, "LaborProductivity") %>%
      add_xml_data(L201.PPPConvert, "PPPConvert") %>%
      add_precursors("L201.Pop_SSP3", "L201.BaseGDP_Scen", "L201.LaborForceFillout", "L201.LaborProductivity_SSP3", "L201.PPPConvert") ->
      socioeconomics_SSP3_rcp85gdp.xml

    # gSSP3 Modified with rcp85 GDP
    create_xml("socioeconomics_gSSP3_rcp85gdp.xml") %>%
      add_xml_data(L201.Pop_gSSP3, "Pop") %>%
      add_xml_data(L201.BaseGDP_Scen, "BaseGDP") %>%
      add_xml_data(L201.LaborForceFillout, "LaborForceFillout") %>%
      add_xml_data(L201.LaborProductivity_gSSP3_rcp85gdp, "LaborProductivity") %>%
      add_xml_data(L201.PPPConvert, "PPPConvert") %>%
      add_precursors("L201.Pop_gSSP3", "L201.BaseGDP_Scen", "L201.LaborForceFillout", "L201.LaborProductivity_gSSP3", "L201.PPPConvert") ->
      socioeconomics_gSSP3_rcp85gdp.xml
    #--------------------------------------------------------


    return_data(socioeconomics_gSSP1.xml, socioeconomics_gSSP2.xml,
                socioeconomics_gSSP3.xml, socioeconomics_gSSP4.xml,
                socioeconomics_gSSP5.xml, socioeconomics_SSP1.xml,
                socioeconomics_SSP2.xml, socioeconomics_SSP3.xml,
                socioeconomics_SSP4.xml, socioeconomics_SSP5.xml,
                socioeconomics_SSP3_rcp85gdp.xml, socioeconomics_gSSP3_rcp85gdp.xml,
                socioeconomics_GCAM3.xml)
  } else {
    stop("Unknown command")
  }
}
