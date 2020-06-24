# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LB103.load_curve_enduse_USA
#'
#' Process EIA monthly retail electricity sales data by grid regions to get load curves for enduse sectors.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated output: \code{L103.load_segments_sector_gcamusa}. The corresponding file in the
#' original data system was \code{LA103.load_curves_enduse.R} (gcamusa dispatch level1).
#' @details Compute load curve related parameters for enduse sectors
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YO May 2020
module_gcamusa_LB103.load_curve_enduse_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/dispatch/EIA_elec_Mon_enduse",
             "L102.load_segments_gcamusa"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L103.load_segments_sector_gcamusa"))
  } else if(command == driver.MAKE) {

    description <- eia_sector <- state <- units <- source.key <- date <- year <-
      generation <- month <- grid_region <- segmentSplit <- monthHours <-
      segment <- hours <- sector <- genMaxNoBldSuperpeak <- generationSuperpeakBuildings <-
      sumGen <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    EIA_elec_Mon_enduse <- get_data(all_data, "gcam-usa/dispatch/EIA_elec_Mon_enduse")
    L102.load_segments <- get_data(all_data, "L102.load_segments_gcamusa")

    # Perform computations

    # TODO: mapping file?
    EIA_elec_td_map <- tibble(sector = c("elect_td_bld", "elect_td_bld", "elect_td_trn", "elect_td_ind", "elect_td_ind"), eia_sector = c("residential", "commercial", "transportation", "industrial", "other"))

    # Clean up raw EIA data which comes in wide format and state+sector and month+year
    # are combined in column names
    EIA_elec_Mon_enduse %>%
      separate(description, c("state", "eia_sector"), "\\s*:\\s*", fill = "right") %>%
      filter(!is.na(eia_sector), eia_sector != "all sectors") %>%
      gather(-state, -eia_sector, -units, -source.key, key=date, value=generation) %>%
      separate(date, c("month", "year"), "-", fill = "right") %>%
      # years were given with the last two digits, and only contains years in the 2000s
      # so we can pre-pend the decade 20 to get the full year
      mutate(year = paste0("20", year)) %>%
      select(-units, -source.key) %>%
      # here automatically filter MODEL_FINAL_BASE_YEAR to accommodate BYU
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      # clean up missing values
      mutate(generation = as.numeric(if_else(generation == "--", "0", generation))) ->
      EIA_elec_Mon_enduse_tidy

    # map in grid region information and aggregate
    EIA_elec_Mon_enduse_tidy %>%
      # TODO: do we really need as factor for processing or was this just for debugging?
      mutate(month = factor(month, levels = gcamusa.MONTH_ORDER)) %>%
      left_join_error_no_match(EIA_elec_td_map, by = "eia_sector") %>%
      left_join_error_no_match(select(states_subregions, state_name, grid_region),
                               by=c("state" = "state_name")) %>%
      group_by(grid_region, month, sector ) %>%
      summarize(generation = sum(generation)) %>%
      ungroup() ->
      L103.generation_Gr_Mon


    # create a summary of the hourly curves by month
    L102.load_segments %>%
      mutate(segmentSplit = segment) %>%
      separate(segmentSplit, c("month", "dayNight"), gcamusa.SEGMENT_DELIM, fill = "right") %>%
      group_by(grid_region, month) %>%
      mutate(monthHours=sum(hours)) %>%
      ungroup() ->
      L102.load_segments_Mon

    #-------------------------------------------------
    # Create Day, Night and Superpeak segments
    # Split day and night generation by hours in segment
    #-------------------------------------------------
    L103.generation_Gr_Mon %>%
      expand(., ., tibble(segment = gcamusa.DAYNIGHT_ORDER)) %>%
      unite(segment, month, segment, sep=gcamusa.SEGMENT_DELIM, remove = FALSE) %>%
      left_join_error_no_match(L102.load_segments_Mon %>%
                  select(grid_region, segment, month, hours, monthHours) %>%
                  distinct, by = c("grid_region", "month", "segment")) %>%
      mutate (generation = generation * hours/monthHours)->
      L103.generation_Gr_Seg_NoSuperPeak

    #--------------------------------------------------------
    # Replace 0 values with the National Mean for each sector
    #------------------------------------------------------
    L103.generation_Gr_Seg_NoSuperPeak %>%
      group_by(segment, sector) %>%
      summarize(genMean=mean(generation)) %>%
      ungroup()->
      L103.generation_Gr_Seg_NoSuperPeak_MeanBySec

    L103.generation_Gr_Seg_NoSuperPeak %>%
      left_join_error_no_match(L103.generation_Gr_Seg_NoSuperPeak_MeanBySec,
                               by = c("sector", "segment")) %>%
      mutate(generation = if_else(generation == 0, genMean, generation)) %>%
      select(-genMean) ->
      L103.generation_Gr_Seg_NoSuperPeak_RplcZeroNtlMeans

    #-------------------------------------------------------------------------------
    # Superpeak
    # Find segments with maximum generation.
    # Find generation in 10 hours
    # Use relative generation from L102 to increase generation by relative generation factor for all sectors
    # subtract max generation for all sectors (except buildings) from this superpeak to get buildings superpeak
    # Assume other sectors superpeak is just based on their max values
    #-------------------------------------------------------------------------------------

    # Superpeak for all sectors using relative generation from L102
    L103.generation_Gr_Seg_NoSuperPeak_RplcZeroNtlMeans %>%
      select(-sector) %>%
      group_by(grid_region,month,segment,hours,monthHours) %>%
      summarize(generation=sum(generation)) %>%
      ungroup() %>%
      group_by(grid_region) %>%
      filter(generation == max(generation)) %>%
      ungroup() %>%
      left_join_error_no_match(L102.load_segments, by = c("grid_region", "segment", "hours")) %>%
      mutate(generationSuperpeak = generation * gcamusa.ELEC_SUPERPEAK_HRS / (hours * relative.generation),
             segment = gcamusa.ELEC_SEGMENT_SUPERPEAK,
             hours = gcamusa.ELEC_SUPERPEAK_HRS) %>%
      select(-relative.generation, -generation.fraction, -monthHours, -month) ->
      L103.generation_Gr_Seg_SuperPeak_allSectors

    # Sum of max 10 hours for all sectors except buildings
    L103.generation_Gr_Seg_NoSuperPeak_RplcZeroNtlMeans %>%
      filter(sector != "elect_td_bld") %>%
      group_by(grid_region,sector) %>%
      filter(generation == max(generation)) %>%
      mutate(genMaxNoBldSuperpeak = generation*gcamusa.ELEC_SUPERPEAK_HRS/hours) %>%
      ungroup() %>%
      group_by(grid_region) %>%
      summarize(genMaxNoBldSuperpeak = sum(genMaxNoBldSuperpeak)) %>%
      ungroup() %>%
      mutate(segment = gcamusa.ELEC_SEGMENT_SUPERPEAK,
             hours=gcamusa.ELEC_SUPERPEAK_HRS)->
      L103.generation_Gr_Seg_SuperPeak_noBldSectors

    # Subtract noBldSectors superpeak from allSectors superpeak to get superpeak for Buildings
    L103.generation_Gr_Seg_SuperPeak_noBldSectors %>%
      left_join_error_no_match(L103.generation_Gr_Seg_SuperPeak_allSectors,
                               by = c("grid_region", "segment", "hours")) %>%
      mutate(generationSuperpeakBuildings = generationSuperpeak - genMaxNoBldSuperpeak,
             sector = "elect_td_bld") ->
      L103.generation_Gr_Seg_SuperPeak_BldSectors

    # Superpeak by sector for all other sectors
    L103.generation_Gr_Seg_NoSuperPeak_RplcZeroNtlMeans %>%
      filter(sector != "elect_td_bld") %>%
      group_by(grid_region, sector) %>%
      filter(generation == max(generation)) %>%
      filter(hours == min(hours)) %>%
      mutate(genMaxNoBldSuperpeak = generation*gcamusa.ELEC_SUPERPEAK_HRS/hours) %>%
      ungroup() %>%
      mutate(generation = genMaxNoBldSuperpeak,
             hours = gcamusa.ELEC_SUPERPEAK_HRS,
             segment = gcamusa.ELEC_SEGMENT_SUPERPEAK) %>%
      select(-month, -monthHours,-generation)->
      L103.generation_Gr_Seg_SuperPeak_allSectorsNoBlds

    # row_bind to get generation for all sectors
    L103.generation_Gr_Seg_SuperPeak_BldSectors %>%
      select(grid_region, sector, segment, hours, generation=generationSuperpeakBuildings) %>%
      bind_rows(L103.generation_Gr_Seg_SuperPeak_allSectorsNoBlds %>% rename(generation = genMaxNoBldSuperpeak)) %>%
      bind_rows(L103.generation_Gr_Seg_NoSuperPeak_RplcZeroNtlMeans %>% select(-monthHours))->
      L103.load_segments_sector_Comb

    # Calculate generation fraction
    L103.load_segments_sector_Comb %>%
      group_by(grid_region,sector) %>%
      mutate(sumGen=sum(generation),
             generation.fraction = generation/sumGen) %>%
      ungroup() %>%
      select(-generation, -sumGen) %>%
      mutate(month = factor(month, levels = gcamusa.MONTH_ORDER),
             segment = factor(segment, levels = gcamusa.ELEC_LOAD_SEGMENT_ORDER)) %>%
      select(-month) ->
      L103.load_segments_sector

    # Produce outputs
    L103.load_segments_sector %>%
      add_title("Defines the relative shape of each load segment by grid_region by enduse sectors") %>%
      add_units("hours / %") %>%
      add_comments("Defines the relative shape of each load segment by grid_region by enduse sectors") %>%
      add_legacy_name("L103.load_segments_sector") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/dispatch/EIA_elec_Mon_enduse",
                     "L102.load_segments_gcamusa") ->
      L103.load_segments_sector_gcamusa


    return_data(L103.load_segments_sector_gcamusa)
  } else {
    stop("Unknown command")
  }
}
