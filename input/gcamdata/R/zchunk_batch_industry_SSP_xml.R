# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_industry_incelas_SSP_xml
#'
#' Construct XML data structures for all the \code{industry_incelas_SSP.xml} files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{industry_incelas_gcam3.xml}, \code{industry_incelas_ssp1.xml}, \code{industry_incelas_ssp2.xml}, \code{industry_incelas_ssp3.xml},
#' \code{industry_incelas_ssp4.xml}, \code{industry_incelas_ssp5.xml}, \code{industry_incelas_gssp1.xml}, \code{industry_incelas_gssp2.xml},
#' \code{industry_incelas_gssp3.xml}, \code{industry_incelas_gssp4.xml}, and \code{industry_incelas_gssp5.xml}.
module_energy_batch_industry_incelas_SSP_xml <- function(command, ...) {

  INCOME_ELASTICITY_INPUTS <- c("GCAM3",
                                paste0("gSSP", 1:5),
                                paste0("SSP", 1:5))

  if(command == driver.DECLARE_INPUTS) {
    return(c(paste("L232.IncomeElasticity_ind", tolower(INCOME_ELASTICITY_INPUTS), sep = "_"),
             "L232.IncomeElasticity_ind_gcam3_USA",
             FILE = "gcam-usa/states_subregions"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "industry_incelas_gcam3.xml",
             XML = "industry_incelas_gssp1.xml",
             XML = "industry_incelas_gssp2.xml",
             XML = "industry_incelas_gssp3.xml",
             XML = "industry_incelas_gssp4.xml",
             XML = "industry_incelas_gssp5.xml",
             XML = "industry_incelas_ssp1.xml",
             XML = "industry_incelas_ssp2.xml",
             XML = "industry_incelas_ssp3.xml",
             XML = "industry_incelas_ssp4.xml",
             XML = "industry_incelas_ssp5.xml",
             XML = "industry_incelas_USA_gcam3.xml",
             XML = "industry_incelas_USA_gssp1.xml",
             XML = "industry_incelas_USA_gssp2.xml",
             XML = "industry_incelas_USA_gssp3.xml",
             XML = "industry_incelas_USA_gssp4.xml",
             XML = "industry_incelas_USA_gssp5.xml",
             XML = "industry_incelas_USA_ssp1.xml",
             XML = "industry_incelas_USA_ssp2.xml",
             XML = "industry_incelas_USA_ssp3.xml",
             XML = "industry_incelas_USA_ssp4.xml",
             XML = "industry_incelas_USA_ssp5.xml"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    industry_incelas_gcam3.xml <- industry_incelas_ssp1.xml <- industry_incelas_ssp2.xml <- industry_incelas_ssp3.xml <-
      industry_incelas_ssp4.xml <- industry_incelas_ssp5.xml<- industry_incelas_gssp1.xml<- industry_incelas_gssp2.xml<-
      industry_incelas_gssp3.xml<- industry_incelas_gssp4.xml <- industry_incelas_gssp5.xml <-
      industry_incelas_USA_gcam3.xml <- industry_incelas_USA_ssp1.xml <- industry_incelas_USA_ssp2.xml <- industry_incelas_USA_ssp3.xml <-
      industry_incelas_USA_ssp4.xml <- industry_incelas_USA_ssp5.xml<- industry_incelas_USA_gssp1.xml<- industry_incelas_USA_gssp2.xml<-
      industry_incelas_USA_gssp3.xml<- industry_incelas_USA_gssp4.xml <- industry_incelas_USA_gssp5.xml <- NULL

    all_data <- list(...)[[1]]

    # Load Inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    L232.IncomeElasticity_ind_gcam3_USA <- get_data(all_data, "L232.IncomeElasticity_ind_gcam3_USA")

    # Loop through all the GCAM3, SSP, and gSSP objects and build the corresponding XML structure
    for(iei in INCOME_ELASTICITY_INPUTS) {
      data_obj <- paste0("L232.IncomeElasticity_ind_", tolower(iei))
      xmlfn <- paste0("industry_incelas_", tolower(iei), '.xml')

      create_xml(xmlfn) %>%
        add_xml_data(get_data(all_data, data_obj), "IncomeElasticity") %>%
        add_precursors(paste0("L232.IncomeElasticity_ind_", tolower(iei))) ->
        xml_obj

      # Assign output to output name
      assign(xmlfn, xml_obj)

      # Copy to US states
      data_obj_usa <-get_data(all_data, data_obj) %>%
        filter(region == "USA") %>%
        left_join(states_subregions %>%
                    select(state) %>%
                    mutate(region="USA"), by = "region") %>%
        select(-region) %>%
        rename(region=state) %>%
        filter(region %in% unique(L232.IncomeElasticity_ind_gcam3_USA$region))

      xmlfn_usa <- paste0("industry_incelas_USA_", tolower(iei), '.xml')

      create_xml(xmlfn_usa) %>%
        add_xml_data(data_obj_usa, "IncomeElasticity") %>%
        add_precursors(paste0("L232.IncomeElasticity_ind_", tolower(iei)),
                       "L232.IncomeElasticity_ind_gcam3_USA",
                       "gcam-usa/states_subregions") ->
        xml_obj_usa

      # Assign output to output name
      assign(xmlfn_usa, xml_obj_usa)

    }

    return_data(industry_incelas_gcam3.xml,
                industry_incelas_ssp1.xml, industry_incelas_ssp2.xml, industry_incelas_ssp3.xml, industry_incelas_ssp4.xml, industry_incelas_ssp5.xml,
                industry_incelas_gssp1.xml, industry_incelas_gssp2.xml, industry_incelas_gssp3.xml, industry_incelas_gssp4.xml, industry_incelas_gssp5.xml,
                industry_incelas_USA_gcam3.xml,
                industry_incelas_USA_ssp1.xml, industry_incelas_USA_ssp2.xml, industry_incelas_USA_ssp3.xml, industry_incelas_USA_ssp4.xml, industry_incelas_USA_ssp5.xml,
                industry_incelas_USA_gssp1.xml, industry_incelas_USA_gssp2.xml, industry_incelas_USA_gssp3.xml, industry_incelas_USA_gssp4.xml, industry_incelas_USA_gssp5.xml)
  } else {
    stop("Unknown command")
  }
}
