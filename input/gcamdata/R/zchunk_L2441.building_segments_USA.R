# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2441.building_segments_USA
#'
#' Creates GCAM-USA building dispatch output files for writing to xml.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2441.DeleteThermalService_gcamusa}, \code{L2441.ThermalBaseService_gcamusa}, \code{L2441.ThermalServiceSatiation_gcamusa},
#' \code{L2441.ThermalServiceCalSatiationValue_gcamusa}, \code{L2441.ThermalDefaultCoef_gcamusa}, \code{L2441.Intgains_scalar_gcamusa},
#' \code{L2441.Supplysector_bld_gcamusa}, \code{L2441.FinalEnergyKeyword_bld_gcamusa}, \code{L2441.SubsectorLogit_bld_gcamusa},
#' \code{L2441.SubsectorShrwtFllt_bld_gcamusa}, \code{L2441.SubsectorInterp_bld_gcamusa}, \code{L2441.SubsectorInterpTo_bld_gcamusa},
#' \code{L2441.StubTechFromSector_bld_gcamusa}, \code{L2441.StubTechDeleteInput_gcamusa}, \code{L2441.StubTechEff_segmentinputs_gcamusa},
#' \code{L2441.StubTechCalInput_bld_gcamusa}, \code{L2441.StubTechMarket_bld_gcamusa}, \code{L2441.TechCoef_nonthermal_load_curve_gcamusa},
#' \code{L2441.HDDCDD_Fixed_gcamusa}, \code{L2441.HDDCDD_Fixed_rcp4p5_gcamusa}, \code{L2441.HDDCDD_Fixed_rcp8p5_gcamusa}.
#' The corresponding file in the original data system was \code{L2441.building_segments_USA.R} (dispatch branch gcam-usa level2).
#' @details Creates GCAM-USA building output files for writing to xml.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YO May 2020

module_gcamusa_L2441.building_segments_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/A44.sector",
             FILE = "gcam-usa/A44.subsector_logit",
             "L104.HistoricalDD_S_Segment_gcamusa",
             "L104.DD_S_Segment_all_gcamusa",
             "L103.load_segments_sector_gcamusa",
             "L244.ThermalBaseService_gcamusa",
             "L244.ThermalServiceSatiation_gcamusa",
             "L244.Intgains_scalar_gcamusa",
             "L244.Floorspace_gcamusa",
             "L244.ShellConductance_bld_gcamusa",
             "L244.SubsectorShrwtFllt_bld_gcamusa",
             "L244.SubsectorInterp_bld_gcamusa",
             "L244.SubsectorInterpTo_bld_gcamusa",
             "L244.StubTech_bld_gcamusa",
             "L244.StubTechCalInput_bld_gcamusa",
             "L244.StubTechMarket_bld",
             "L244.GlobalTechEff_bld",
             "L226.TechCoef_electd_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2441.DeleteThermalService_gcamusa",
             "L2441.ThermalBaseService_gcamusa",
             "L2441.ThermalServiceSatiation_gcamusa",
             "L2441.ThermalServiceCalSatiationValue_gcamusa",
             "L2441.ThermalDefaultCoef_gcamusa",
             "L2441.Intgains_scalar_gcamusa",
             "L2441.Supplysector_bld_gcamusa",
             "L2441.FinalEnergyKeyword_bld_gcamusa",
             "L2441.SubsectorLogit_bld_gcamusa",
             "L2441.SubsectorShrwtFllt_bld_gcamusa",
             "L2441.SubsectorInterp_bld_gcamusa",
             "L2441.SubsectorInterpTo_bld_gcamusa",
             "L2441.StubTechFromSector_bld_gcamusa",
             "L2441.StubTechDeleteInput_gcamusa",
             "L2441.StubTechEff_segmentinputs_gcamusa",
             "L2441.StubTechCalInput_bld_gcamusa",
             "L2441.StubTechMarket_bld_gcamusa",
             "L2441.TechCoef_nonthermal_load_curve_gcamusa",
             "L2441.HDDCDD_Fixed_gcamusa",
             "L2441.HDDCDD_Fixed_rcp4p5_gcamusa",
             "L2441.HDDCDD_Fixed_rcp8p5_gcamusa"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    HDD <- CDD <- base.building.size <- base.service <- calibrated.value <- comm <-
      degree.days <- efficiency <- fuel <- rel <- s2 <- s3 <- grid_region <- elec_total <-
      gcam.consumer <- grid_region <- half_life_new <- half_life_stock <- input.cost <-
      input.ratio <- internal.gains.market.name <- internal.gains.output.ratio <-
      internal.gains.scalar <- market.name <- minicam.energy.input <- multiplier <-
      object <- pcFlsp_mm2 <- pcGDP <- pcflsp_mm2cap <- pop <- region <- resid <-
      satiation.adder <- satiation.level <- sector <- sector.name <- service <- share <-
      share.weight <- share_tech1 <- share_tech2 <- share_type <- state <- steepness_new <-
      steepness_stock <- stockavg <- subsector <- subsector.name <- supplysector <-
      tech_type <- technology <- technology1 <- technology2 <-
      thermal.building.service.input <- to.value <- value <- year <- year.fillout <- . <-
      pop_year <- Sector <- pop_share <- growth <- flsp_growth <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    A44.sector <- get_data(all_data, "gcam-usa/A44.sector")
    A44.subsector_logit <- get_data(all_data, "gcam-usa/A44.subsector_logit")
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    L103.load_segments_sector <- get_data(all_data, "L103.load_segments_sector_gcamusa")
    L104.HistoricalDD_S_Segment <- get_data(all_data, "L104.HistoricalDD_S_Segment_gcamusa")
    L104.DD_S_Segment_all <- get_data(all_data, "L104.DD_S_Segment_all_gcamusa")
    L244.ThermalBaseService <- get_data(all_data, "L244.ThermalBaseService_gcamusa")
    L244.ThermalServiceSatiation <- get_data(all_data, "L244.ThermalServiceSatiation_gcamusa")
    L244.Intgains_scalar <- get_data(all_data, "L244.Intgains_scalar_gcamusa")
    L244.Floorspace <- get_data(all_data, "L244.Floorspace_gcamusa")
    L244.ShellConductance_bld <- get_data(all_data, "L244.ShellConductance_bld_gcamusa")
    L244.SubsectorShrwtFllt_bld <- get_data(all_data, "L244.SubsectorShrwtFllt_bld_gcamusa")
    L244.SubsectorInterp_bld <- get_data(all_data, "L244.SubsectorInterp_bld_gcamusa")
    L244.SubsectorInterpTo_bld <- get_data(all_data, "L244.SubsectorInterpTo_bld_gcamusa")
    L244.StubTech_bld <- get_data(all_data, "L244.StubTech_bld_gcamusa")
    L244.StubTechCalInput_bld <- get_data(all_data, "L244.StubTechCalInput_bld_gcamusa")
    L244.StubTechMarket_bld <- get_data(all_data, "L244.StubTechMarket_bld")
    L244.GlobalTechEff_bld <- get_data(all_data, "L244.GlobalTechEff_bld")
    L226.TechCoef_electd_USA <- get_data(all_data, "L226.TechCoef_electd_USA")

    # ===================================================
    # Data Processing

    # get a list of "thermal" services such as comm heating, comm cooling, etc
    thermal_services <- unique(L244.ThermalBaseService$thermal.building.service.input)

    # filter the demand side load curve to just buildings
    L103.load_segments_sector %>%
      filter(sector == "elect_td_bld") %>%
      select(-sector) ->
      L2441.elec_subregion_dist
    # get a list of segments such as superpeak, Jan_day, Jan_night, etc
    # TODO: why factors?
    segment_names <- unique(L2441.elec_subregion_dist$segment)

    # gather "HDD"/"CDD" into heating / cooling service then normalize it
    # so we can use it as the basis to disaggregating the service to the
    # segments
    L104.HistoricalDD_S_Segment %>%
      group_by(state) %>%
      mutate(HDD = HDD/sum(HDD),
             CDD = CDD/sum(CDD)) %>%
      ungroup() %>%
      # note residential and commercial will get the same "shape" to downscale
      # to segments
      expand(., ., sector=c("comm", "resid")) %>%
      gather(service, rel, matches("DD")) %>%
      mutate(service = if_else(service == "HDD", "heating", "cooling"),
             # keep concatenated names which will be useful for joining later
             s2 = paste(sector, service),
             s3 = paste(sector, service, segment)) ->
      L2441.DD_segments_rel

    # Same as the block above except this time for each of the RCP scenarios
    L104.DD_S_Segment_all %>%
      group_by(state) %>%
      mutate(HDD = HDD/sum(HDD),
             CDD = CDD/sum(CDD)) %>%
      ungroup() %>%
      expand(., ., sector=c("comm", "resid")) %>%
      gather(service, rel, matches("DD")) %>%
      mutate(service = if_else(service == "HDD", "heating", "cooling"),
             s2 = paste(sector, service),
             s3 = paste(sector, service, segment)) ->
      L2441.DD_segments_all_rel

    # We need to downscale all buildings electricity use to segments however
    # it only makes sense to use climate data as a proxy for thermal services.
    # So we will calculate it by first calculating how much electricity is used
    # by thermal services at the grid region and segment:
    L244.StubTechCalInput_bld %>%
      filter(minicam.energy.input == "elect_td_bld",
             supplysector %in% thermal_services,
             year == MODEL_FINAL_BASE_YEAR) %>%
      group_by(region, supplysector) %>%
      summarize(calibrated.value = sum(calibrated.value)) %>%
      ungroup() %>%
      left_join_error_no_match(L2441.DD_segments_rel, ., by=c("state" = "region", "s2" = "supplysector")) %>%
      left_join_error_no_match(select(states_subregions, state, grid_region), by = "state") %>%
      mutate(elec_thermal = calibrated.value * rel) %>%
      group_by(grid_region, segment) %>%
      summarize(elec_thermal = sum(elec_thermal)) %>%
      ungroup() ->
      L2441.elec_thermal

    # Now we can calculate the total electricity use by segment using the L103 data
    # and from there take the residual from the thermal services to define the shape
    # of the non-thermal uses of electricity
    L244.StubTechCalInput_bld %>%
      filter(minicam.energy.input == "elect_td_bld",
             year==MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(select(states_subregions, state, grid_region), by=c("region" = "state")) %>%
      group_by(grid_region) %>%
      summarize(calibrated.value = sum(calibrated.value)) %>%
      ungroup() %>%
      left_join_error_no_match(select(L2441.elec_subregion_dist, grid_region, segment, generation.fraction), ., by = "grid_region") %>%
      mutate(elec_total = calibrated.value * generation.fraction) %>%
      group_by(grid_region, segment) %>%
      summarize(elec_total = sum(elec_total)) %>%
      ungroup() %>%
      left_join_error_no_match(L2441.elec_thermal, by = c("grid_region", "segment")) %>%
      mutate(elec_nonthermal = elec_total - elec_thermal) %>%
      group_by(grid_region) %>%
      mutate(nontherm_LC = elec_nonthermal / sum(elec_nonthermal)) %>%
      ungroup() %>%
      select(grid_region, segment, nontherm_LC) ->
      L2441.nonthermal_load_curve

    # Since we will be circumventing the elect_td_bld sector for simplicity we need to adjust
    # for the non-thermal technologies we will replace the one elect_td_bld with one for each
    # segment with the coefficients adjusted by the load curve which we calculate here.
    L2441.nonthermal_load_curve %>%
      mutate(minicam.energy.input = paste0("electricity domestic supply_", segment)) %>%
      left_join_error_no_match(L103.load_segments_sector %>%
                  filter(sector == "elect_td_bld") %>%
                  select(grid_region, segment, generation.fraction), by=c("grid_region", "segment")) %>%
      left_join_error_no_match(filter(L226.TechCoef_electd_USA, supplysector == "elect_td_bld"), ., by=c("market.name" = "grid_region", "minicam.energy.input")) %>%
      mutate(coefficient = coefficient / generation.fraction) ->
      L2441.elect_td_coefs

    # We are going to completely replace the thermal service sectors by
    # segment so start with a table deleting the old ones
    L244.ThermalBaseService %>%
      select(-year, -base.service) %>%
      mutate(supplysector = thermal.building.service.input) ->
      L2441.DeleteThermalService

    # Downscale base service to segment
    L244.ThermalBaseService %>%
      # using left join as we expect the number of rows to change by the
      # number of years in the model calibration years
      left_join(L2441.DD_segments_rel, by=c("region" = "state", "thermal.building.service.input" = "s2")) %>%
      mutate(thermal.building.service.input = s3,
             base.service = base.service * rel) %>%
      select(LEVEL2_DATA_NAMES[["ThermalBaseService"]]) ->
      L2441.ThermalBaseService

    # The service satiation level is a bit different as this function is meant to
    # model the _preference_ for how much to heat or cool which should not be different
    # between segments.  However, it typically also has "scale" mixed into the calibration
    # which we do not want here.  So we use an alternative calibration approach where we
    # have normalized the satiation demand function portion to be beween 0 and 1 where 0
    # indicates to heating/cooling and 1 indicates heat/cool all the wat to the "set point"
    # 100% of the time.  The rest of the scale calibration will be pulled into the coefficient
    # during calibration (i.e. only calibrating one parameter instead of two).

    # The assymptote value is then always 1
    L244.ThermalServiceSatiation %>%
      left_join_error_no_match(L2441.DD_segments_rel, ., by=c("state" = "region", "s2" = "thermal.building.service.input")) %>%
      rename(region = state) %>%
      mutate(thermal.building.service.input = s3,
             satiation.level = satiation.level * rel,
             satiation.level = 1.0) %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceSatiation"]]) ->
      L2441.ThermalServiceSatiation

    # The calibrated satiation value is then setting what the perefernce was
    # in the historical period.
    # TODO: revisit this, we should have values by state + service instead of
    # assuming 80% everywhere.  Although, I have some vague recollection we tried
    # to do some analysis on historical HDD/CDD to back this out didn't find a good
    # correlation
    L2441.ThermalServiceSatiation %>%
      mutate(cal.satiation.value = 0.8) %>%
      select(-satiation.level) ->
      L2441.ThermalServiceCalSatiationValue

    # Some State + Service + Segments will have zero HDD/CDD in the final historical year.
    # However, if we calibrate that in then we could never get service in the future (i.e.
    # it was a cold spring in the final historical year so no need to cool but the next
    # year could be hot).  To handle this we will back out a default coefficient which is
    # the average across all segments (the entire year) and use that instead of the zero
    # which would have been calibrated otherwise.  Note, if there was no service the entire
    # year we would still be left with no possibility for non-zero service in the future.

    # Calculate what the coefficient would be when using the climate across the entire year
    # by back calculating the thermal building service function.
    L244.ThermalBaseService %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L244.Floorspace,
                               by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "year")) %>%
      left_join_error_no_match(L244.ShellConductance_bld,
                               by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "year")) %>%
      left_join_error_no_match(L104.HistoricalDD_S_Segment %>%
                  group_by(state) %>%
                  summarize(CDD=sum(CDD), HDD=sum(HDD)) %>%
                  ungroup(), by=c("region" = "state")) %>%
      mutate(DD = if_else(grepl('cooling', thermal.building.service.input), CDD, HDD)) %>%
      # TODO: use L2441.ThermalServiceCalSatiationValue when we are not just assuming 0.8
      mutate(coefficient = (base.service / base.building.size) / (DD * shell.conductance * floor.to.surface.ratio * 0.8)) %>%
      select(region, thermal.building.service.input, coefficient) ->
      L2441.DefaultCoef

    # Use the default coefficient for any state + service + segment that would have been
    # zero otherwise
    L2441.ThermalBaseService %>%
      filter(year == MODEL_FINAL_BASE_YEAR, base.service == 0.0) %>%
      left_join_error_no_match(L2441.DD_segments_rel,
                               by = c("region" = "state", "thermal.building.service.input" = "s3")) %>%
      left_join_error_no_match(L2441.DefaultCoef, by = c("region", "s2" = "thermal.building.service.input")) %>%
      mutate(base.service = coefficient) %>%
      select(LEVEL2_DATA_NAMES[["ThermalBaseService"]]) %>%
      select(-year) %>%
      rename(coefficient = base.service) ->
      L2441.ThermalDefaultCoef

    # copy internal gains coefficient to new segmented thermal services
    L244.Intgains_scalar %>%
      left_join_error_no_match(L2441.DD_segments_rel, ., by=c("state" = "region", "s2" = "thermal.building.service.input")) %>%
      rename(region = state) %>%
      mutate(thermal.building.service.input = s3,
             # TODO: internal gains when there should be no service?
             internal.gains.scalar = if_else(rel == 0, 0, internal.gains.scalar)) %>%
      select(LEVEL2_DATA_NAMES[["Intgains_scalar"]]) ->
      L2441.Intgains_scalar

    # Make a table of expanded thermal service sectors by segment to create
    # shell for copy the original thermal service supply sectors
    A44.sector %>%
      filter(supplysector %in% thermal_services) %>%
      expand(., ., segment = segment_names) %>%
      mutate(supplysector = paste(supplysector, segment)) ->
      L2441.sector_segment

    # copy sector params
    L2441.sector_segment %>%
      write_to_all_states( c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME )) ->
      L2441.Supplysector_bld

    # Make a table of expanded thermal service sectors by segment to create
    # shell for copy the original thermal service supply subsectors
    A44.subsector_logit %>%
      filter(supplysector %in% thermal_services) %>%
      expand(., ., segment = segment_names) %>%
      mutate(supplysector = paste(supplysector, segment)) ->
      L2441.subsector_logit_segment

    # copy subsector params
    L2441.subsector_logit_segment %>%
      write_to_all_states( c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME )) ->
      L2441.SubsectorLogit_bld

    # copy final energy keywords
    A44.sector %>%
      write_to_all_states( LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]] ) %>%
      filter(supplysector %in% thermal_services) %>%
      expand(., ., segment = segment_names) %>%
      mutate(supplysector = paste(supplysector, segment)) %>%
      select(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]]) ->
      L2441.FinalEnergyKeyword_bld

    # copy subsector share-weight params
    L244.SubsectorShrwtFllt_bld %>%
      filter(supplysector %in% thermal_services) %>%
      expand(., ., segment = segment_names) %>%
      mutate(supplysector = paste(supplysector, segment)) %>%
      select(-segment) ->
      L2441.SubsectorShrwtFllt_bld

    # copy subsector interpolation rules
    L244.SubsectorInterp_bld %>%
      filter(supplysector %in% thermal_services) %>%
      expand(., ., segment = segment_names) %>%
      mutate(supplysector = paste(supplysector, segment)) %>%
      select(-segment) ->
      L2441.SubsectorInterp_bld

    # copy subsector share-weight interpolation to-values.
    L244.SubsectorInterpTo_bld %>%
      filter(supplysector %in% thermal_services) %>%
      expand(., ., segment = segment_names) %>%
      mutate(supplysector = paste(supplysector, segment)) %>%
      select(-segment) ->
      L2441.SubsectorInterpTo_bld

    # Most technology details are in the global technology database and
    # to avoid having to copy all of those even though they will be defined
    # the same but just exist in a different sector.name we will use the
    # feature of the stub-technology to override the sector.name look up
    # to just use the original supplysector name
    L244.StubTech_bld %>%
      filter(supplysector %in% thermal_services) %>%
      expand(., ., segment = segment_names) %>%
      mutate(from.sector = supplysector,
             supplysector = paste(supplysector, segment)) %>%
      select(-segment) ->
      L2441.StubTechFromSector_bld

    # Make a table of expanded thermal service sectors by segment to create
    # shell for copy the original technology params
    L244.GlobalTechEff_bld %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology) %>%
      filter(supplysector %in% thermal_services, subsector == "electricity") %>%
      write_to_all_states( c("region", names(.))) %>%
      left_join_error_no_match(select(states_subregions, state, grid_region), by=c("region" = "state")) %>%
      expand(., ., segment = segment_names) %>%
      mutate(supplysector = paste(supplysector, segment)) ->
      L2441.elec_tech_inputs

    # replace electricity inputs to directly add to electricity demand appropriate
    # for the segment, bypassing the elec_td sector
    L2441.elec_tech_inputs %>%
      select(-segment, -efficiency, -grid_region) ->
      L2441.StubTechDeleteInput

    # set the appropriate names and adjust the efficiency for losses given
    # we are by passing the elec_td sector
    L2441.elec_tech_inputs %>%
      mutate(minicam.energy.input = paste0("electricity domestic supply_", segment),
             market.name = grid_region) %>%
      left_join_error_no_match(select(L2441.elect_td_coefs, region, year, minicam.energy.input, coefficient),
                               by = c("region", "year", "minicam.energy.input")) %>%
      mutate(efficiency = efficiency / coefficient) %>%
      select(LEVEL2_DATA_NAMES[["StubTechEff"]]) ->
      L2441.StubTechEff_segmentinputs

    # downscale all calibrated values to segments
    L244.StubTechCalInput_bld %>%
      filter(supplysector %in% thermal_services) %>%
      # using left join as we are expanding by segments
      left_join(L2441.DD_segments_rel, by=c("region" = "state", "supplysector" = "s2")) %>%
      mutate(supplysector = s3,
             # need to update electricity input names, other fuels should not change
             minicam.energy.input = if_else(minicam.energy.input == "elect_td_bld", paste0("electricity domestic supply_", segment), minicam.energy.input),
             # In principal just scale the value by the curve but the idea is make sure a
             # segment which was zero in the final historical year can have service in the
             # future. The idea here is the sector will be driven by zero demand so if we read
             # calibation values it will still calibrate the right share-weights AND not
             # produce incorrect total demands.
             # TODO: here originally use max, not sure why at some point changed into pmax
             calibrated.value = calibrated.value * max(rel, 1e-10),
             # subs.share.weight = if_else(calibrated.value == 0, 0, 1),
             # tech.share.weight = subs.share.weight) %>%
            subs.share.weight = 1,
            tech.share.weight = if_else(calibrated.value == 0, 0, 1)) %>%
      # Set the appropriate names and adjust the efficiency for losses given we are by passing
      # the elec_td sector.  Note, we use left_join as the non-electricity inputs will
      # be NA but that is ok since we don't need to update those.
      left_join(select(L2441.elect_td_coefs, region, year, minicam.energy.input, coefficient),
                by = c("region", "year", "minicam.energy.input")) %>%
      mutate(calibrated.value = if_else(subsector == "electricity", calibrated.value * coefficient, calibrated.value)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) ->
      L2441.StubTechCalInput_bld

    # delivered biomass has state-level market
    L244.StubTechMarket_bld %>%
      filter(supplysector %in% thermal_services) %>%
      left_join_error_no_match(select(states_subregions, state, grid_region), by=c("region" = "state")) %>%
      expand(., ., segment = segment_names) %>%
      mutate(supplysector = paste(supplysector, segment),
             market.name = if_else(minicam.energy.input == "elect_td_bld", grid_region, market.name),
             minicam.energy.input = if_else(minicam.energy.input == "elect_td_bld", paste0("electricity domestic supply_", segment), minicam.energy.input)) %>%
      #mutate(market.name = if_else(minicam.energy.input == "delivered biomass", region, market.name)) %>%
      select(-segment, -grid_region) ->
      L2441.StubTechMarket_bld

    # The default assumption is to have fixed climate for all years
    L244.ThermalBaseService %>%
      filter(year == MODEL_YEARS[1]) %>%
      select(-year) %>%
      expand(., ., year=MODEL_YEARS) %>%
      # using left_join as we are expanding by segments
      left_join(L104.HistoricalDD_S_Segment, by=c("region" = "state")) %>%
      mutate(thermal.building.service.input = paste(thermal.building.service.input, segment),
             degree.days = if_else(grepl('heating', thermal.building.service.input), HDD, CDD)) %>%
      select(LEVEL2_DATA_NAMES[["HDDCDD"]]) ->
      L2441.HDDCDD_Fixed

    # Split out the RCPs at this point which can then get output into their
    # own add-on files.
    L244.ThermalBaseService %>%
      filter(year == MODEL_YEARS[1]) %>%
      select(-year) %>%
      expand(., ., year=MODEL_FUTURE_YEARS) %>%
      # using left_join as we are expanding by segments
      left_join(L104.DD_S_Segment_all %>% filter(year %in% MODEL_FUTURE_YEARS), by=c("region" = "state","year")) %>%
      mutate(thermal.building.service.input = paste(thermal.building.service.input, segment),
             degree.days = if_else(grepl('heating', thermal.building.service.input), HDD, CDD)) %>%
      select(LEVEL2_DATA_NAMES[["HDDCDD"]], "rcp") ->
      L2441.HDDCDD_Fixed_all

    L2441.HDDCDD_Fixed_all %>%
      filter(rcp=="rcp4.5") %>%
      select(-rcp) -> L2441.HDDCDD_Fixed_rcp4p5

    L2441.HDDCDD_Fixed_all %>%
      filter(rcp=="rcp8.5") %>%
      select(-rcp) -> L2441.HDDCDD_Fixed_rcp8p5

    # For the non-thermal sectors they will still using the elec_td sector
    # and we need to update the load curve there to reflect the non-themal
    # load curve we calculated earlier
    L2441.elect_td_coefs %>%
      mutate(coefficient = coefficient * nontherm_LC) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
      L2441.TechCoef_nonthermal_load_curve


    # ===================================================
    # Produce outputs (and also add _gcamusa here)
    L2441.DeleteThermalService %>%
      add_title("Delete original thermal service names in order to create new ones by segment") %>%
      add_units("NA") %>%
      add_comments("Delete original thermal service names in order to create new ones by segment") %>%
      add_legacy_name("L2441.DeleteThermalService") %>%
      add_precursors("L244.ThermalBaseService_gcamusa") ->
      L2441.DeleteThermalService_gcamusa

    L2441.ThermalBaseService %>%
      add_title("add building service input by segment") %>%
      add_units("EJ") %>%
      add_comments("add building service input by segment") %>%
      add_legacy_name("L2441.ThermalBaseService") %>%
      add_precursors("L244.ThermalBaseService_gcamusa",
                     "L104.HistoricalDD_S_Segment_gcamusa") ->
      L2441.ThermalBaseService_gcamusa

    L2441.ThermalServiceSatiation %>%
      add_title("add building service input by segment") %>%
      add_units("NA") %>%
      add_comments("add building add building service input by segment input by segment") %>%
      add_legacy_name("L2441.ThermalServiceSatiation") %>%
      add_precursors("L244.ThermalServiceSatiation_gcamusa",
                     "L104.HistoricalDD_S_Segment_gcamusa") ->
      L2441.ThermalServiceSatiation_gcamusa

    L2441.ThermalServiceCalSatiationValue %>%
      add_title("add building service input by segment and assign cal.satiation.value as 0.8") %>%
      add_units("NA") %>%
      add_comments("add building service input by segment and assign cal.satiation.value as 0.8") %>%
      add_legacy_name("L2441.ThermalServiceCalSatiationValue") %>%
      same_precursors_as("L2441.ThermalServiceSatiation_usa") ->
      L2441.ThermalServiceCalSatiationValue_gcamusa

    L2441.ThermalDefaultCoef %>%
      add_title("add building service input by segment") %>%
      add_units("NA") %>%
      add_comments("add building service input by segment") %>%
      add_legacy_name("L2441.ThermalDefaultCoef") %>%
      add_precursors("L244.ThermalBaseService_gcamusa",
                     "L104.HistoricalDD_S_Segment_gcamusa",
                     "L244.Floorspace_gcamusa",
                     "L244.ShellConductance_bld_gcamusa") ->
      L2441.ThermalDefaultCoef_gcamusa

    L2441.Intgains_scalar %>%
      add_title("add building service input by segment") %>%
      add_units("NA") %>%
      add_comments("add building service input by segment") %>%
      add_legacy_name("L2441.Intgains_scalar") %>%
      add_precursors("L244.Intgains_scalar_gcamusa",
                     "L104.HistoricalDD_S_Segment_gcamusa") ->
      L2441.Intgains_scalar_gcamusa

    L2441.Supplysector_bld %>%
      add_title("add building supplysector names by segment") %>%
      add_units("NA") %>%
      add_comments("add building supplysector names by segment") %>%
      add_legacy_name("L2441.Supplysector_bld") %>%
      add_precursors("gcam-usa/A44.sector",
                     "L103.load_segments_sector_gcamusa") ->
      L2441.Supplysector_bld_gcamusa

    L2441.FinalEnergyKeyword_bld %>%
      add_title("add building supplysector names by segment") %>%
      add_units("NA") %>%
      add_comments("add building supplysector names by segment") %>%
      add_legacy_name("L2441.FinalEnergyKeyword_bld") %>%
      add_precursors("gcam-usa/A44.sector",
                     "L103.load_segments_sector_gcamusa") ->
      L2441.FinalEnergyKeyword_bld_gcamusa

    L2441.SubsectorLogit_bld %>%
      add_title("add building supplysector names by segment") %>%
      add_units("NA") %>%
      add_comments("add building supplysector names by segment") %>%
      add_legacy_name("L2441.SubsectorLogit_bld") %>%
      add_precursors("gcam-usa/A44.subsector_logit",
                     "L103.load_segments_sector_gcamusa") ->
      L2441.SubsectorLogit_bld_gcamusa

    L2441.SubsectorShrwtFllt_bld %>%
      add_title("add building supplysector names by segment") %>%
      add_units("NA") %>%
      add_comments("add building supplysector names by segment") %>%
      add_legacy_name("L2441.SubsectorShrwtFllt_bld") %>%
      add_precursors("L244.SubsectorShrwtFllt_bld_gcamusa",
                     "L103.load_segments_sector_gcamusa") ->
      L2441.SubsectorShrwtFllt_bld_gcamusa

    L2441.SubsectorInterp_bld %>%
      add_title("add building supplysector names by segment") %>%
      add_units("NA") %>%
      add_comments("add building supplysector names by segment") %>%
      add_legacy_name("L2441.SubsectorInterp_bld") %>%
      add_precursors("L244.SubsectorInterp_bld_gcamusa",
                     "L103.load_segments_sector_gcamusa") ->
      L2441.SubsectorInterp_bld_gcamusa

    L2441.SubsectorInterpTo_bld %>%
      add_title("add building supplysector names by segment") %>%
      add_units("NA") %>%
      add_comments("add building supplysector names by segment") %>%
      add_legacy_name("L2441.SubsectorInterpTo_bld") %>%
      add_precursors("L244.SubsectorInterpTo_bld_gcamusa",
                     "L103.load_segments_sector_gcamusa") ->
      L2441.SubsectorInterpTo_bld_gcamusa

    L2441.StubTechFromSector_bld %>%
      add_title("add building supplysector names by segment") %>%
      add_units("NA") %>%
      add_comments("add building supplysector names by segment") %>%
      add_legacy_name("L2441.StubTechFromSector_bld") %>%
      add_precursors("L244.StubTech_bld_gcamusa",
                     "L103.load_segments_sector_gcamusa") ->
      L2441.StubTechFromSector_bld_gcamusa

    L2441.StubTechDeleteInput %>%
      add_title("delete minicam.energy.input of elect_td_bld") %>%
      add_units("NA") %>%
      add_comments("delete minicam.energy.input of elect_td_bld") %>%
      add_legacy_name("L2441.StubTechDeleteInput") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L103.load_segments_sector_gcamusa",
                     "L244.GlobalTechEff_bld") ->
      L2441.StubTechDeleteInput_gcamusa

    L2441.StubTechEff_segmentinputs %>%
      add_title("define inputs by segment for each corresponding supplysector") %>%
      add_units("NA") %>%
      add_comments("define inputs by segment for each corresponding supplysector") %>%
      add_legacy_name("L2441.StubTechEff_segmentinputs") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L103.load_segments_sector_gcamusa",
                     "L244.GlobalTechEff_bld",
                     "L226.TechCoef_electd_USA",
                     "L244.StubTechCalInput_bld_gcamusa",
                     "L104.HistoricalDD_S_Segment_gcamusa") ->
      L2441.StubTechEff_segmentinputs_gcamusa

    L2441.StubTechCalInput_bld %>%
      add_title("define inputs by segment for stub.technology for each corresponding supplysector") %>%
      add_units("NA") %>%
      add_comments("define inputs by segment for stub.technology for each corresponding supplysector") %>%
      add_legacy_name("L2441.StubTechCalInput_bld") %>%
      add_precursors("L244.StubTechCalInput_bld_gcamusa",
                     "L104.HistoricalDD_S_Segment_gcamusa",
                     "L103.load_segments_sector_gcamusa",
                     "L226.TechCoef_electd_USA",
                     "gcam-usa/states_subregions") ->
      L2441.StubTechCalInput_bld_gcamusa

    L2441.StubTechMarket_bld %>%
      add_title("define market at grid level for stub.technology by segment") %>%
      add_units("NA") %>%
      add_comments("define market at grid level for stub.technology by segment") %>%
      add_legacy_name("L2441.StubTechMarket_bld") %>%
      add_precursors("L244.StubTechMarket_bld",
                     "L103.load_segments_sector_gcamusa",
                     "gcam-usa/states_subregions") ->
      L2441.StubTechMarket_bld_gcamusa

    L2441.TechCoef_nonthermal_load_curve %>%
      add_title("calculate tech coefficients by segment adjusted by non-thermal load curve") %>%
      add_units("NA") %>%
      add_comments("calculate tech coefficients by segment adjusted by non-thermal load curve") %>%
      add_legacy_name("L2441.TechCoef_nonthermal_load_curve") %>%
      add_precursors("L103.load_segments_sector_gcamusa",
                     "L226.TechCoef_electd_USA",
                     "gcam-usa/states_subregions",
                     "L244.StubTechCalInput_bld_gcamusa",
                     "L104.HistoricalDD_S_Segment_gcamusa") ->
      L2441.TechCoef_nonthermal_load_curve_gcamusa

    L2441.HDDCDD_Fixed %>%
      add_title("define DD by building service by segment") %>%
      add_units("NA") %>%
      add_comments("define DD by building service by segment") %>%
      add_legacy_name("L2441.HDDCDD_Fixed") %>%
      add_precursors("L104.HistoricalDD_S_Segment_gcamusa",
                     "L244.ThermalBaseService_gcamusa") ->
      L2441.HDDCDD_Fixed_gcamusa

    L2441.HDDCDD_Fixed_rcp4p5 %>%
      add_title("define DD by building service by segment for rcp45 scenario") %>%
      add_units("NA") %>%
      add_comments("define DD by building service by segment for rcp45 scenario") %>%
      add_legacy_name("L2441.HDDCDD_Fixed_rcp4p5") %>%
      add_precursors("L104.DD_S_Segment_all_gcamusa",
                     "L244.ThermalBaseService_gcamusa") ->
      L2441.HDDCDD_Fixed_rcp4p5_gcamusa

    L2441.HDDCDD_Fixed_rcp8p5 %>%
      add_title("define DD by building service by segment for rcp85 scenario") %>%
      add_units("NA") %>%
      add_comments("define DD by building service by segment for rcp85 scenario") %>%
      add_legacy_name("L2441.HDDCDD_Fixed_rcp8p5") %>%
      add_precursors("L104.DD_S_Segment_all_gcamusa",
                     "L244.ThermalBaseService_gcamusa") ->
      L2441.HDDCDD_Fixed_rcp8p5_gcamusa

    return_data(L2441.DeleteThermalService_gcamusa,
                L2441.ThermalBaseService_gcamusa,
                L2441.ThermalServiceSatiation_gcamusa,
                L2441.ThermalServiceCalSatiationValue_gcamusa,
                L2441.ThermalDefaultCoef_gcamusa,
                L2441.Intgains_scalar_gcamusa,
                L2441.Supplysector_bld_gcamusa,
                L2441.FinalEnergyKeyword_bld_gcamusa,
                L2441.SubsectorLogit_bld_gcamusa,
                L2441.SubsectorShrwtFllt_bld_gcamusa,
                L2441.SubsectorInterp_bld_gcamusa,
                L2441.SubsectorInterpTo_bld_gcamusa,
                L2441.StubTechFromSector_bld_gcamusa,
                L2441.StubTechDeleteInput_gcamusa,
                L2441.StubTechEff_segmentinputs_gcamusa,
                L2441.StubTechCalInput_bld_gcamusa,
                L2441.StubTechMarket_bld_gcamusa,
                L2441.TechCoef_nonthermal_load_curve_gcamusa,
                L2441.HDDCDD_Fixed_gcamusa,
                L2441.HDDCDD_Fixed_rcp4p5_gcamusa,
                L2441.HDDCDD_Fixed_rcp8p5_gcamusa)
  } else {
    stop("Unknown command")
  }
}
