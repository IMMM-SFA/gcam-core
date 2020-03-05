#' module_gcamusa_LA114.wind
#'
#' Compute capacity factors for wind by US state.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L114.CapacityFactor_wind_state}, \code{L114.CapacityFactor_wind_state_segment}.
#' The corresponding file in the original data system was \code{LA114.Wind.R} (gcam-usa level1).
#' @details Computes capacity factors for wind by US state.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YO Janurary 2020
module_gcamusa_LA114.wind <- function(command, ...)
  {
  if(command == driver.DECLARE_INPUTS) {
    return(c( FILE = "gcam-usa/dispatch/L114.CapacityFactor_wind_state_segment",
              FILE = "gcam-usa/dispatch/L114.CapacityFactor_wind_state"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L114.CapacityFactor_wind_state", "L114.CapacityFactor_wind_state_segment"))
  } else if(command == driver.MAKE) {

    technology <- year <- state <- sector <- capacity.factor <- fuel <- value <- base_cost <-
      region <- NULL  # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    CapacityFactor_wind_state_segment <- get_data(all_data, "gcam-usa/dispatch/L114.CapacityFactor_wind_state_segment")
    CapacityFactor_wind_state <- get_data(all_data, "gcam-usa/dispatch/L114.CapacityFactor_wind_state")
    # ===================================================

    # Short-cut fix: directly readin post-processed capacity factor data from ReEDS data...
    # ... because the original ReEDS data file is too big (>200MB) to process here.
    CapacityFactor_wind_state %>%
      mutate(sector = "electricity generation") %>%
      mutate(fuel = "wind") %>%
      # add attributes for output...
      add_title("Capacity factor for wind by state", overwrite = T) %>%
      add_units("Unitless") %>%
      add_comments("Direclty read pre-processed data from ReEDS hourly cf data") %>%
      add_legacy_name("L114.CapacityFactor_wind_state (dispatch branch)") %>%
      add_precursors("gcam-usa/dispatch/L114.CapacityFactor_wind_state") ->
      L114.CapacityFactor_wind_state

    CapacityFactor_wind_state_segment %>%
      mutate(sector = "electricity generation") %>%
      mutate(fuel = "wind") %>%
      # add attributes for output...
      add_title("Capacity factor for wind by state and segment", overwrite = T) %>%
      add_units("Unitless") %>%
      add_comments("Direclty read pre-processed data from ReEDS hourly cf data") %>%
      add_legacy_name("L114.CapacityFactor_wind_state_segment (dispatch branch)") %>%
      add_precursors("gcam-usa/dispatch/L114.CapacityFactor_wind_state_segment") ->
      L114.CapacityFactor_wind_state_segment

    return_data(L114.CapacityFactor_wind_state, L114.CapacityFactor_wind_state_segment)
  } else {
    stop("Unknown command")
  }
}
