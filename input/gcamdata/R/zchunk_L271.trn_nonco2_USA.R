#' module_gcamusa_L271.trn_nonco2_USA
#'
#' U.S. non CO2 emission coeff from the transportation sector
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L271.nonco2_trn_tech_coeff_USA} and \code{L271.nonco2_trn_emiss_control_USA}.
#' @details Share input emissions for the transportation sector to state-LDV/HDV combinations over history
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author NTG January 2021
module_gcamusa_L271.trn_nonco2_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L201.en_pol_emissions",
             "L201.en_ghg_emissions",
             "L201.en_bcoc_emissions",
             "L254.StubTranTechCalInput_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L271.nonco2_trn_tech_coeff_USA",
             "L271.nonco2_trn_emiss_coeff_USA" ))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L201.en_pol_emissions <- get_data(all_data, "L201.en_pol_emissions", strip_attributes=TRUE)
    L201.en_ghg_emissions <- get_data(all_data, "L201.en_ghg_emissions", strip_attributes=TRUE)
    L201.en_bcoc_emissions <- get_data(all_data, "L201.en_bcoc_emissions", strip_attributes=TRUE)
    L254.StubTranTechCalInput_USA     <- get_data(all_data, "L254.StubTranTechCalInput_USA", strip_attributes=TRUE)


    # Silence pacakge checks
      region <-  sector.name <- start.year <-
      stub.technology <- subsector.name <- supplysector <- tranSubsector <-
      tranTechnology <- value <- year <- NULL



    # ===================================================

    ## First we must read in the calibrated values of transportation
    ## sector existence within in state. We then share out emissions
    ## from the USA region based upon state level transportation
    ## existence. This defines our historical emissions for the transporation
    ## sector in GCAM-USA

    L254.StubTranTechCalInput_USA %>%
        left_join(
          rbind(L201.en_pol_emissions,
                L201.en_ghg_emissions) %>% filter(region=="USA") %>% select(-region),
                                 by=c("supplysector","tranSubsector"="subsector","stub.technology","year","minicam.energy.input"="input.name")) %>%
        na.omit->
    L271.StubTranEmissions_USA

    L271.StubTranEmissions_USA %>% group_by(supplysector, tranSubsector, stub.technology, Non.CO2, year) %>%
      mutate(total.cal = sum(calibrated.value),
             input.emissions = input.emissions * (calibrated.value/total.cal)) %>%
      ungroup() %>%
      rename(input.name=minicam.energy.input) %>%
      select(-calibrated.value, -share.weight.year, -subs.share.weight, -tech.share.weight, -total.cal) %>%
      ## Replace values that are 0 divided by 0 which cause NaN's
      replace_na(list(input.emissions = 0))->
      L271.nonco2_trn_tech_coeff_USA

    L254.StubTranTechCalInput_USA %>%
      left_join(
        L201.en_bcoc_emissions %>% filter(region=="USA") %>% select(-region),
        by=c("supplysector","tranSubsector"="subsector","stub.technology","year","minicam.energy.input"="input.name")) %>%
      rename(input.name=minicam.energy.input) %>%
      select(-calibrated.value, -share.weight.year, -subs.share.weight, -tech.share.weight) %>%
      na.omit->
      L271.nonco2_trn_emiss_coeff_USA


    # ===================================================

    # Produce outputs
    L271.nonco2_trn_tech_coeff_USA %>%
      add_title("Pollutant input emissions for transportation technologies in all U.S. states") %>%
      add_units("NA") %>%
      add_comments("U.S. emission coeff based on LDV and HDV technology and population") %>%
      add_legacy_name("L271.nonco2_trn_tech_coeff_USA") %>%
      add_precursors("L201.en_pol_emissions",
                     "L201.en_ghg_emissions",
                     "L254.StubTranTechCalInput_USA") ->
      L271.nonco2_trn_tech_coeff_USA

    L271.nonco2_trn_emiss_coeff_USA %>%
      add_title("BC and OC emissions coeffs for transportation technologies in all U.S. states") %>%
      add_units("NA") %>%
      add_comments("U.S. emission coeff based on LDV and HDV technology and population") %>%
      add_legacy_name("L271.nonco2_trn_emiss_coeff_USA") %>%
      add_precursors("L201.en_bcoc_emissions",
                     "L254.StubTranTechCalInput_USA") ->
      L271.nonco2_trn_emiss_coeff_USA

    return_data(L271.nonco2_trn_tech_coeff_USA,L271.nonco2_trn_emiss_coeff_USA)
  } else {
    stop("Unknown command")
  }
}
