#' module_gcamusa_batch_elecS_ghg_emissions_USA_xml
#'
#' Construct XML data structure for \code{elecS_ghg_emissions_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{elecS_ghg_emissions_USA.xml}. The corresponding file in the
#' original data system was \code{elecS_ghg_emissions_USA.xml} (gcamusa XML).
module_gcamusa_batch_elecS_ghg_emissions_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2236.elecS_ghg_tech_coeff_USA",
             "L2236.elecS_ghg_tech_coeff_USA_hist"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "elecS_ghg_emissions_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2236.elecS_ghg_tech_coeff_USA <- get_data(all_data, 'L2236.elecS_ghg_tech_coeff_USA')
    L2236.elecS_ghg_tech_coeff_USA_hist  <- get_data(all_data, 'L2236.elecS_ghg_tech_coeff_USA_hist')

    # Silence package checks
    emiss.coeff <- NULL

    # ===================================================

    # Rename for XML
    L2236.elecS_ghg_tech_coeff_USA %>%
      dplyr::rename(dispatch.sector=supplysector,
                    capacity.technology=technology,
                    emiss.coef=emiss.coeff) -> L2236.elecS_ghg_tech_coeff_USA

    L2236.elecS_ghg_tech_coeff_USA_hist %>%
      dplyr::rename(dispatch.sector=supplysector,
                    capacity.technology=technology,
                    emiss.coef=emiss.coeff) -> L2236.elecS_ghg_tech_coeff_USA_hist

    # ===================================================

    # Produce outputs

    create_xml("elecS_ghg_emissions_USA.xml") %>%
      add_xml_data(L2236.elecS_ghg_tech_coeff_USA_hist,"OutputEmissCoeffDispatch") %>%
      add_xml_data(L2236.elecS_ghg_tech_coeff_USA,"OutputEmissCoeffDispatch") %>%
      add_precursors("L2236.elecS_ghg_tech_coeff_USA",
                     "L2236.elecS_ghg_tech_coeff_USA_hist") ->
      elecS_ghg_emissions_USA.xml

    return_data(elecS_ghg_emissions_USA.xml)

  } else {
    stop("Unknown command")
  }
}
