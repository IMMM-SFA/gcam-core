#' module_gcamusa_L2236.elecS_ghg_emissions_USA
#'
#' U.S. electricity non CO2 and GHG emission coefficients by technology sector
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2236.elecS_ghg_tech_coeff_USA} and \code{L2236.elecS_ghg_tech_coeff_USA_hist}.
#' The corresponding file in the original data system was \code{L2236.elecS_ghg_tech_coeff_USA_hist} (gcam-usa level2).
#' @details Write electricity emission coefficients to multiple load segments then compute state
#' shares for each category in the fuel input table.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author KD August 2018
module_gcamusa_L2236.elecS_ghg_emissions_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c('L201.OutputEmissions_elec',
             'L241.OutputEmissCoeff_elec',
             FILE = 'gcam-usa/A23.elec_tech_mapping_cool',
             FILE = "gcam-usa/A23.elecS_coal_emissions",
             FILE = "energy/A23.globaltech_input_driver",
             'L223.Production_Dispatch'))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c('L2236.elecS_ghg_tech_coeff_USA',
             'L2236.elecS_ghg_tech_coeff_USA_hist'))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A23.globaltech_input_driver <- get_data(all_data, "energy/A23.globaltech_input_driver", strip_attributes=TRUE)
    L201.OutputEmissions_elec <- get_data(all_data, 'L201.OutputEmissions_elec', strip_attributes=TRUE)
    L241.OutputEmissCoeff_elec <- get_data(all_data, 'L241.OutputEmissCoeff_elec', strip_attributes=TRUE)

    A23.elec_tech_mapping_cool <- get_data(all_data, 'gcam-usa/A23.elec_tech_mapping_cool', strip_attributes=TRUE)
    A23.elecS_coal_emissions <- get_data(all_data, "gcam-usa/A23.elecS_coal_emissions", strip_attributes=TRUE)
    # ^^^ NOTE: this is a bit of a hack - we should calculate this within the chunk in the future

    L223.Production_Dispatch <- get_data(all_data, 'L223.Production_Dispatch', strip_attributes=TRUE)

    # Silence package checks
    CH4 <- Electric.sector <- Electric.sector.technology <- N2O <- Non.CO2 <-
      calOutputValue <- emiss.coeff <- fuel <- fuel_input <- fuel_input_USA <-
      fuel_input_share <- input.emissions <- palette <- region <- segment_share <-
      state <- technology <- subsector <- subsector_1 <- supplysector <-
      tech_calOuput <- tech_fuel_input <- technology <- value <- year <- NULL


    # ===========================================================================================


    A23.globaltech_input_driver %>%
      left_join(A23.elec_tech_mapping_cool %>%
                  select(technology, to.technology)) %>%
      mutate(technology=to.technology) %>%
      select(-to.technology) ->
    EnTechInputMap

    # Apply the Emiss Coefficient to each region, supplysector, subsector, technology, year
    L241.OutputEmissCoeff_elec %>%
      filter(region == gcam.USA_REGION,
             supplysector == "electricity") %>%
      select(-region, technology=stub.technology) %>%
      left_join(A23.elec_tech_mapping_cool %>%
                  select(technology, to.technology)) %>%
      mutate(technology=to.technology) %>%
      select(-to.technology) %>%
      repeat_add_columns(tibble('region' = gcamusa.STATES)) %>%
      semi_join(L223.Production_Dispatch %>%
                  select(region, supplysector, subsector, technology),
                by = c("supplysector", "subsector", "technology", "region")) %>%
      select(region, supplysector, subsector, technology, year, Non.CO2, emiss.coeff) %>%
      left_join_error_no_match(EnTechInputMap,
                               by = c("supplysector", "subsector", "technology"))  %>%
      select(region, supplysector, subsector, technology, year, Non.CO2, emiss.coeff) ->
      L2236.elecS_ghg_tech_coeff_USA

    A23.elecS_coal_emissions %>%
      mutate(year = max(MODEL_BASE_YEARS)) %>%
               rename(technology=sector) %>%
      repeat_add_columns(tibble('region' = gcamusa.STATES)) %>%
      # join will duplicate rows by a number of cooling systems
      # LJENM throws error, so left_join() is used
      left_join(A23.elec_tech_mapping_cool %>%
                  select(technology, to.technology)) %>%
      mutate(technology=to.technology) %>%
      select(-to.technology) %>%
      mutate(emiss.coeff = emissions/generation) %>%
      select(-emissions, -generation) %>%
      left_join(L223.Production_Dispatch %>%
                  select(region, supplysector, subsector, technology) %>%
                  distinct(),
                by = c("technology", "region")) %>%
      filter(!is.na(supplysector)) %>%
      select(region, supplysector, subsector, technology, year, Non.CO2, emiss.coeff) %>%
      left_join_error_no_match(EnTechInputMap,
                               by = c("supplysector", "subsector", "technology")) %>%
      select(-input.name) %>%
      bind_rows(L2236.elecS_ghg_tech_coeff_USA) ->
      L2236.elecS_ghg_tech_coeff_USA


    # 2b. Historical emissions coefficients
    # Calcuate emissions coefficients for historical vintage
    # We do this calculation by fuel.  It should be done by technology, but this causes issues
    # for natural gas because GCAM-USA has different gas CC / gas steam/CT splits than core GCAM.
    # NOTE: We may be assigning species to technologies, specifically gas CC, which are not appropriate.
    # However, this approach allows us to match USA total emissions by species most accurately.

    # Calculate total USA electricity non-CO2 emissions by species and year from core GCAM
    L201.OutputEmissions_elec %>%
      filter(region == gcam.USA_REGION & grepl("electricity", supplysector)) %>%
      group_by(supplysector, subsector, year, Non.CO2) %>%
      summarise(input.emissions = sum(input.emissions)) %>%
      ungroup() -> L2236.elec_ghg_emissions_USA

    # Calculate total calibrated electricity production by fuel from GCAM-USA
    # Start by creating a table with state and tech detail mapped to core technologies
    # This will be used to map back in national average emissions coefficients from core GCAM
    L223.Production_Dispatch %>%
      filter(subsector %in% unique(L2236.elec_ghg_emissions_USA$subsector)) -> L2236.elec_prod_state

    # Aggregate calibrated electricity production by fuel to USA level
    L2236.elec_prod_state %>%
      filter(year %in% unique(L2236.elec_ghg_emissions_USA$year)) %>%
      group_by(supplysector, subsector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup() -> L2236.elec_prod_USA

    # Map USA emissions by fuel and species (GCAM) to USA electricity production by fuel (GCAM-USA)
    # Calculate emissions coefficients
    L2236.elec_prod_USA %>%
      # join will duplicate rows by # of emissions species
      # LJENM throws error, so left_join() is used
      left_join(L2236.elec_ghg_emissions_USA,
                by = c("supplysector", "subsector", "year")) %>%
      mutate(emiss.coeff = input.emissions / calOutputValue) -> L2236.elec_coef_USA_hist

    # Map emissions factors to state-level technologies
    # Note:  input emissions data from core GCAM only cover 1975-2005
    # Copy emissions coefficients from 2005 forward to 2010 and 2015
    L2236.elec_prod_state %>%
      filter(year %in% unique(L2236.elec_ghg_emissions_USA$year)) %>%
      # join will duplicate rows by # of emissions species
      # LJENM throws error, so left_join() is used
      left_join(L2236.elec_coef_USA_hist %>%
                  select(-input.emissions, -calOutputValue),
                by = c("supplysector", "subsector", "year"))  %>%
      select(region, supplysector, subsector, technology,
             year, Non.CO2, emiss.coeff) %>%
      # Copy emissions coefficients from 2005 forward to 2010 and 2015
      complete(nesting(region, supplysector, subsector, technology, Non.CO2),
               year = MODEL_BASE_YEARS) %>%
      group_by(region, supplysector, subsector, technology, Non.CO2) %>%
      mutate(emiss.coeff = approx_fun(year, emiss.coeff, rule = 2)) %>%
      ungroup() -> L2236.elecS_ghg_tech_coeff_USA_hist

    # Check for missing values
    stopifnot(!any(is.na(L2236.elecS_ghg_tech_coeff_USA)))
    stopifnot(!any(is.na(L2236.elecS_ghg_tech_coeff_USA_hist)))


    # ===========================================================================================

    # Produce outputs
    L2236.elecS_ghg_tech_coeff_USA %>%
      add_title("U.S. electricity GHG emission coefficients by technology sector") %>%
      add_units("NA") %>%
      add_comments("Write electricity emission coefficients to multiple load segments then") %>%
      add_comments("compute state shares for each category in the fuel input table") %>%
      add_legacy_name("L2236.elecS_ghg_tech_coeff_USA") %>%
      add_precursors('energy/A23.globaltech_input_driver',
                     'gcam-usa/A23.elec_tech_mapping_cool',
                     'L201.OutputEmissions_elec',
                     'L241.OutputEmissCoeff_elec',
                     'gcam-usa/A23.elecS_coal_emissions',
                     'L223.Production_Dispatch') ->
      L2236.elecS_ghg_tech_coeff_USA

    L2236.elecS_ghg_tech_coeff_USA_hist %>%
      add_title("Historical U.S. electricity non-CO2 emission coefficients by technology") %>%
      add_units("various - emissions per EJ of electricity generation") %>%
      add_comments("Calcuate emissions coefficients for historical vintage by fuel from") %>%
      add_comments("core GCAM's USA region emissions data and GCAM-USA national aggregate electricity generation.") %>%
      add_comments("NOTE: We do this calculation by fuel.  It should be done by technology, but this causes issues") %>%
      add_comments("for natural gas because GCAM-USA has different gas CC / gas steam/CT splits than core GCAM.") %>%
      add_precursors('L201.OutputEmissions_elec',
                     'L223.Production_Dispatch') ->
      L2236.elecS_ghg_tech_coeff_USA_hist

    return_data(L2236.elecS_ghg_tech_coeff_USA,
                L2236.elecS_ghg_tech_coeff_USA_hist)

  } else {
    stop("Unknown command")
  }
}
