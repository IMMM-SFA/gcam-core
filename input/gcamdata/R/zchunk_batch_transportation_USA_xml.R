# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_transportation_USA_xml
#'
#' Construct XML data structure for \code{transportation_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_USA.xml}. The corresponding file in the
#' original data system was \code{batch_transportation_USA_xml.R} (gcamusa XML).
module_gcamusa_batch_transportation_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L254.Supplysector_trn",
             "L254.FinalEnergyKeyword_trn",
             "L254.tranSubsectorLogit",
             "L254.tranSubsectorShrwtFllt",
             "L254.tranSubsectorInterp",
             "L254.tranSubsectorSpeed",
             "L254.tranSubsectorSpeed_passthru",
             "L254.tranSubsectorSpeed_noVOTT",
             "L254.tranSubsectorSpeed_nonmotor",
             "L254.tranSubsectorVOTT",
             "L254.tranSubsectorFuelPref",
             "L254.StubTranTech",
             "L254.StubTranTechLoadFactor",
             "L254.StubTranTechCost",
             "L254.StubTranTechCoef",
             "L254.PerCapitaBased_trn",
             "L254.PriceElasticity_trn",
             "L254.IncomeElasticity_trn",
             "L254.StubTranTechCalInput",
             "L254.BaseService_trn",
             "L254.DeleteSupplysector_USAtrn",
             "L254.DeleteFinalDemand_USAtrn",
             "L254.Supplysector_trn_USA",
             "L254.FinalEnergyKeyword_trn_USA",
             "L254.tranSubsectorLogit_USA",
             "L254.tranSubsectorShrwtFllt_USA",
             "L254.tranSubsectorInterp_USA",
             "L254.tranSubsectorSpeed_USA",
             "L254.tranSubsectorSpeed_passthru_USA",
             "L254.tranSubsectorSpeed_noVOTT_USA",
             "L254.tranSubsectorSpeed_nonmotor_USA",
             "L254.tranSubsectorVOTT_USA",
             "L254.tranSubsectorFuelPref_USA",
             "L254.StubTranTech_USA",
             "L254.StubTranTech_passthru_USA",
             "L254.StubTranTech_nonmotor_USA",
             "L254.StubTranTechLoadFactor_USA",
             "L254.StubTranTechCost_USA",
             "L254.StubTranTechCoef_USA",
             "L254.PerCapitaBased_trn_USA",
             "L254.PriceElasticity_trn_USA",
             "L254.IncomeElasticity_trn_USA",
             "L254.StubTranTechCalInput_USA",
             "L254.StubTranTechProd_nonmotor_USA",
             "L254.StubTranTechCalInput_passthru_USA",
             "L254.BaseService_trn_USA",
             FILE = "gcam-usa/states_subregions"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "transportation_USA.xml",
             XML = "transportation_USA_SSP1.xml",
             XML = "transportation_USA_SSP3.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L254.DeleteSupplysector_USAtrn <- get_data(all_data, "L254.DeleteSupplysector_USAtrn")
    L254.DeleteFinalDemand_USAtrn <- get_data(all_data, "L254.DeleteFinalDemand_USAtrn")
    L254.Supplysector_trn_USA <- get_data(all_data, "L254.Supplysector_trn_USA")
    L254.FinalEnergyKeyword_trn_USA <- get_data(all_data, "L254.FinalEnergyKeyword_trn_USA")
    L254.tranSubsectorLogit_USA <- get_data(all_data, "L254.tranSubsectorLogit_USA")
    L254.tranSubsectorShrwtFllt_USA <- get_data(all_data, "L254.tranSubsectorShrwtFllt_USA")
    L254.tranSubsectorInterp_USA <- get_data(all_data, "L254.tranSubsectorInterp_USA")
    L254.tranSubsectorSpeed_USA <- get_data(all_data, "L254.tranSubsectorSpeed_USA")
    L254.tranSubsectorSpeed_passthru_USA <- get_data(all_data, "L254.tranSubsectorSpeed_passthru_USA")
    L254.tranSubsectorSpeed_noVOTT_USA <- get_data(all_data, "L254.tranSubsectorSpeed_noVOTT_USA")
    L254.tranSubsectorSpeed_nonmotor_USA <- get_data(all_data, "L254.tranSubsectorSpeed_nonmotor_USA")
    L254.tranSubsectorVOTT_USA <- get_data(all_data, "L254.tranSubsectorVOTT_USA")
    L254.tranSubsectorFuelPref_USA <- get_data(all_data, "L254.tranSubsectorFuelPref_USA")
    L254.StubTranTech_USA <- get_data(all_data, "L254.StubTranTech_USA")
    L254.StubTranTech_passthru_USA <- get_data(all_data, "L254.StubTranTech_passthru_USA")
    L254.StubTranTech_nonmotor_USA <- get_data(all_data, "L254.StubTranTech_nonmotor_USA")
    L254.StubTranTechLoadFactor_USA <- get_data(all_data, "L254.StubTranTechLoadFactor_USA")
    L254.StubTranTechCost_USA <- get_data(all_data, "L254.StubTranTechCost_USA")
    L254.StubTranTechCoef_USA <- get_data(all_data, "L254.StubTranTechCoef_USA")
    L254.PerCapitaBased_trn_USA <- get_data(all_data, "L254.PerCapitaBased_trn_USA")
    L254.PriceElasticity_trn_USA <- get_data(all_data, "L254.PriceElasticity_trn_USA")
    L254.IncomeElasticity_trn_USA <- get_data(all_data, "L254.IncomeElasticity_trn_USA")
    L254.StubTranTechCalInput_USA <- get_data(all_data, "L254.StubTranTechCalInput_USA")
    L254.StubTranTechProd_nonmotor_USA <- get_data(all_data, "L254.StubTranTechProd_nonmotor_USA")
    L254.StubTranTechCalInput_passthru_USA <- get_data(all_data, "L254.StubTranTechCalInput_passthru_USA")
    L254.BaseService_trn_USA <- get_data(all_data, "L254.BaseService_trn_USA")

    # Load related core files which have data for more than one SSP
    # L254.Supplysector_trn <- get_data(all_data, "L254.Supplysector_trn")
    # L254.FinalEnergyKeyword_trn <- get_data(all_data, "L254.FinalEnergyKeyword_trn")
    # L254.tranSubsectorLogit <- get_data(all_data, "L254.tranSubsectorLogit")
    # L254.tranSubsectorShrwtFllt <- get_data(all_data, "L254.tranSubsectorShrwtFllt")
    # L254.tranSubsectorInterp <- get_data(all_data, "L254.tranSubsectorInterp")
    L254.tranSubsectorSpeed <- get_data(all_data, "L254.tranSubsectorSpeed")
    L254.tranSubsectorSpeed_passthru <- get_data(all_data, "L254.tranSubsectorSpeed_passthru")
    #L254.tranSubsectorSpeed_noVOTT <- get_data(all_data, "L254.tranSubsectorSpeed_noVOTT")
    #L254.tranSubsectorSpeed_nonmotor <- get_data(all_data, "L254.tranSubsectorSpeed_nonmotor")
    L254.tranSubsectorVOTT <- get_data(all_data, "L254.tranSubsectorVOTT")
    L254.tranSubsectorFuelPref <- get_data(all_data, "L254.tranSubsectorFuelPref")
    L254.StubTranTech <- get_data(all_data, "L254.StubTranTech")
    L254.StubTranTechLoadFactor <- get_data(all_data, "L254.StubTranTechLoadFactor")
    L254.StubTranTechCost <- get_data(all_data, "L254.StubTranTechCost")
    L254.StubTranTechCoef <- get_data(all_data, "L254.StubTranTechCoef")
    L254.PerCapitaBased_trn <- get_data(all_data, "L254.PerCapitaBased_trn")
    L254.PriceElasticity_trn <- get_data(all_data, "L254.PriceElasticity_trn")
    L254.IncomeElasticity_trn <- get_data(all_data, "L254.IncomeElasticity_trn")
    L254.StubTranTechCalInput <- get_data(all_data, "L254.StubTranTechCalInput")
    #L254.BaseService_trn <- get_data(all_data, "L254.BaseService_trn")

    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")

    # ===================================================

    # Create state level SSP files

    L254.tranSubsectorSpeed_USA_SSP <- L254.tranSubsectorSpeed %>%
      filter(region == "USA") %>%
      left_join(states_subregions %>%
                  select(state) %>%
                  mutate(region="USA"), by = "region") %>%
      select(-region) %>%
      distinct() %>%
      rename(region=state)

    L254.tranSubsectorVOTT_USA_SSP <- L254.tranSubsectorVOTT %>%
      filter(region == "USA") %>%
      left_join(states_subregions %>%
                  select(state) %>%
                  mutate(region="USA"), by = "region") %>%
      select(-region) %>%
      rename(region=state)

    L254.tranSubsectorFuelPref_USA_SSP <-L254.tranSubsectorFuelPref %>%
    filter(region == "USA") %>%
      left_join(states_subregions %>%
                  select(state) %>%
                  mutate(region="USA"), by = "region") %>%
      select(-region) %>%
      distinct() %>%
      rename(region=state)


    L254.StubTranTech_USA_SSP <- L254.StubTranTech %>%
      filter(region == "USA") %>%
      left_join(states_subregions %>%
                  select(state) %>%
                  mutate(region="USA"), by = "region") %>%
      select(-region) %>%
      distinct() %>%
      rename(region=state)

    L254.StubTranTechLoadFactor_USA_SSP <- L254.StubTranTechLoadFactor  %>%
      filter(region == "USA") %>%
      left_join(states_subregions %>%
                  select(state) %>%
                  mutate(region="USA"), by = "region") %>%
      select(-region) %>%
      distinct() %>%
      rename(region=state)

    L254.StubTranTechCost_USA_SSP <- L254.StubTranTechCost  %>%
      filter(region == "USA") %>%
      left_join(states_subregions %>%
                  select(state) %>%
                  mutate(region="USA"), by = "region") %>%
      select(-region) %>%
      distinct() %>%
      rename(region=state)

    L254.StubTranTechCoef_USA_SSP <- L254.StubTranTechCoef  %>%
      filter(region == "USA") %>%
      left_join(states_subregions %>%
                  select(state) %>%
                  mutate(region="USA"), by = "region") %>%
      select(-region, -market.name) %>%
      distinct() %>%
      rename(region=state) %>%
      left_join(L254.StubTranTechCoef_USA %>%
                  select(-year,-coefficient) %>%
                  distinct())

    L254.PerCapitaBased_trn_USA_SSP <- L254.PerCapitaBased_trn  %>%
      filter(region == "USA") %>%
      left_join(states_subregions %>%
                  select(state) %>%
                  mutate(region="USA"), by = "region") %>%
      select(-region) %>%
      distinct() %>%
      rename(region=state)

    L254.PriceElasticity_trn_USA_SSP <- L254.PriceElasticity_trn  %>%
      filter(region == "USA") %>%
      left_join(states_subregions %>%
                  select(state) %>%
                  mutate(region="USA"), by = "region") %>%
      select(-region) %>%
      distinct() %>%
      rename(region=state)

    L254.IncomeElasticity_trn_USA_SSP <- L254.IncomeElasticity_trn  %>%
      filter(region == "USA") %>%
      left_join(states_subregions %>%
                  select(state) %>%
                  mutate(region="USA"), by = "region") %>%
      select(-region) %>%
      distinct() %>%
      rename(region=state)

    # L254.StubTranTechCalInput_USA_SSP
    # Get National Change Ratios in Calibrated value
    # L254.StubTranTechCalInput %>%
    #   select(-subs.share.weight,-tech.share.weight) %>%
    #   tidyr::spread(key="sce", value="calibrated.value") %>%
    #   mutate(SSP1 = SSP1/CORE,
    #          SSP3 = SSP5/CORE,
    #          SSP5 = SSP5/CORE,
    #          check = SSP1*SSP3*SSP5) %>%
    #   filter(check != 1)
    # Appears like there is no change in the calibrated Values for any scenarios so skipping

    # L254.StubTranTechCalInput_USA_SSP <- L254.StubTranTechCalInput_USA  %>%
    #   filter(region == "USA",
    #          sce != "CORE") %>%
    #   left_join(states_subregions %>%
    #               select(state) %>%
    #               mutate(region="USA"), by = "region") %>%
    #   select(-region) %>%
    #   rename(region=state)

    #--------------------------------------------

    # Diagnostics
    if(F){

      diagnosticsOn = F

      L254.tranSubsectorSpeed_USA_SSP -> x
      x1 <- x %>% filter(sce != "CORE")
      x <- x %>% filter(year %in% unique(x1$year),
                        supplysector %in% unique(x1$supplysector))
      ggplot(x, aes(x=year, y=speed, color = sce, fill = sce)) +
        geom_point() + ggtitle("L254.tranSubsectorSpeed_USA_SSP")+
        facet_grid(.~sce) + theme_bw()  +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))-> p; p
      if(diagnosticsOn){ggsave(p)}


      L254.tranSubsectorSpeed_passthru_USA_SSP <- L254.tranSubsectorSpeed_passthru %>%
        filter(region == "USA") %>%
        left_join(states_subregions %>%
                    select(state) %>%
                    mutate(region="USA"), by = "region") %>%
        select(-region) %>%
        distinct() %>%
        rename(region=state)

      L254.tranSubsectorSpeed_passthru_USA_SSP -> x
      x1 <- x %>% filter(sce != "CORE")
      x <- x %>% filter(year %in% unique(x1$year),
                        supplysector %in% unique(x1$supplysector))
      ggplot(x, aes(x=year, y=speed, color = sce, fill = sce)) +
        geom_point() + ggtitle("L254.tranSubsectorSpeed_passthru_USA_SSP")+
        facet_grid(.~sce)+ theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))-> p; p
      if(diagnosticsOn){ggsave(p)}

      L254.tranSubsectorVOTT_USA_SSP -> x
      x1 <- x %>% filter(sce != "CORE")
      x <- x %>% filter(year.fillout %in% unique(x1$year.fillout),
                        supplysector %in% unique(x1$supplysector))
      ggplot(x, aes(x=year.fillout, y=time.value.multiplier, color = sce, fill = sce)) +
        geom_point() + ggtitle("L254.tranSubsectorVOTT_USA_SSP")+
        facet_grid(.~sce)+ theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))-> p; p
      if(diagnosticsOn){ggsave(p)}

      L254.tranSubsectorFuelPref_USA_SSP -> x
      x1 <- x %>% filter(sce != "CORE")
      x <- x %>% filter(year.fillout %in% unique(x1$year.fillout),
                        supplysector %in% unique(x1$supplysector))
      ggplot(x, aes(x=year.fillout, y=fuelprefElasticity, color = sce, fill = sce)) +
        geom_point() + ggtitle("L254.tranSubsectorFuelPref_USA_SSP")+
        facet_grid(.~sce)+ theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))-> p; p
      if(diagnosticsOn){ggsave(p)}

      L254.StubTranTech_USA_SSP -> x
      x1 <- x %>% filter(sce != "CORE")
      x <- x %>% filter(supplysector %in% unique(x1$supplysector))
      ggplot(x, aes(x=supplysector, y=stub.technology, color = sce, fill = sce)) +
        geom_point() + ggtitle("L254.StubTranTech_USA_SSP")+
        facet_grid(.~sce)+ theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))-> p; p
      if(diagnosticsOn){ggsave(p)}

      L254.StubTranTechLoadFactor_USA_SSP -> x
      x1 <- x %>% filter(sce != "CORE")
      x <- x %>% filter(supplysector %in% unique(x1$supplysector),
                        year %in% unique(x1$year))
      ggplot(x, aes(x=year, y=loadFactor, color = sce, fill = sce)) +
        geom_point() + ggtitle("L254.StubTranTechLoadFactor_USA_SSP")+
        facet_grid(.~sce)+ theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))-> p; p
      if(diagnosticsOn){ggsave(p)}

      L254.StubTranTechCost_USA_SSP -> x
      x1 <- x %>% filter(sce != "CORE")
      x <- x %>% filter(supplysector %in% unique(x1$supplysector),
                        year %in% unique(x1$year))
      ggplot(x, aes(x=year, y=input.cost, color = sce, fill = sce)) +
        geom_point() + ggtitle("L254.StubTranTechCost_USA_SSP")+
        facet_grid(.~sce)+ theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))-> p; p
      if(diagnosticsOn){ggsave(p)}

      L254.StubTranTechCoef_USA_SSP -> x
      x1 <- x %>% filter(sce != "CORE")
      x <- x %>% filter(supplysector %in% unique(x1$supplysector),
                        year %in% unique(x1$year))
      ggplot(x, aes(x=year, y=coefficient, color = sce, fill = sce)) +
        geom_point() + ggtitle("L254.StubTranTechCoef_USA_SSP")+
        facet_grid(.~sce)+ theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))-> p; p
      if(diagnosticsOn){ggsave(p)}

      L254.PerCapitaBased_trn_USA_SSP -> x
      ggplot(x, aes(x=energy.final.demand, y=perCapitaBased, color = sce, fill = sce)) +
        geom_point() + ggtitle("L254.PerCapitaBased_trn_USA_SSP")+
        facet_grid(.~sce)+ theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))-> p; p
      if(diagnosticsOn){ggsave(p)}

      L254.PriceElasticity_trn_USA_SSP -> x
      x1 <- x %>% filter(sce != "CORE")
      x <- x %>% filter(energy.final.demand %in% unique(x1$energy.final.demand),
                        year %in% unique(x1$year))
      ggplot(x, aes(x=year, y=price.elasticity, color = sce, fill = sce)) +
        geom_point() + ggtitle("L254.PriceElasticity_trn_USA_SSP")+
        facet_grid(.~sce)+ theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))-> p; p
      if(diagnosticsOn){ggsave(p)}

      L254.IncomeElasticity_trn_USA_SSP -> x
      x1 <- x %>% filter(sce != "CORE")
      x <- x %>% filter(energy.final.demand %in% unique(x1$energy.final.demand),
                        year %in% unique(x1$year))
      ggplot(x, aes(x=year, y=income.elasticity, color = sce, fill = sce)) +
        geom_point() + ggtitle("L254.IncomeElasticity_trn_USA_SSP")+
        facet_grid(.~sce)+ theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))-> p; p
      if(diagnosticsOn){ggsave(p)}
    }


    # ===================================================

    # Produce outputs
    create_xml("transportation_USA.xml") %>%
      add_xml_data(L254.DeleteSupplysector_USAtrn, "DeleteSupplysector") %>%
      add_xml_data(L254.DeleteFinalDemand_USAtrn, "DeleteFinalDemand") %>%
      add_logit_tables_xml(L254.Supplysector_trn_USA, "Supplysector") %>%
      add_xml_data(L254.FinalEnergyKeyword_trn_USA, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L254.tranSubsectorLogit_USA, "tranSubsectorLogit", "tranSubsector") %>%
      add_xml_data(L254.tranSubsectorShrwtFllt_USA, "tranSubsectorShrwtFllt") %>%
      add_xml_data(L254.tranSubsectorInterp_USA, "tranSubsectorInterp") %>%
      add_xml_data(L254.tranSubsectorSpeed_USA, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorSpeed_passthru_USA, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorSpeed_noVOTT_USA, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorSpeed_nonmotor_USA, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorVOTT_USA, "tranSubsectorVOTT") %>%
      add_xml_data(L254.tranSubsectorFuelPref_USA, "tranSubsectorFuelPref") %>%
      add_xml_data(L254.StubTranTech_USA, "StubTranTech") %>%
      add_xml_data(L254.StubTranTech_passthru_USA, "StubTranTech") %>%
      add_xml_data(L254.StubTranTech_nonmotor_USA, "StubTranTech") %>%
      add_xml_data(L254.StubTranTechLoadFactor_USA, "StubTranTechLoadFactor") %>%
      add_xml_data(L254.StubTranTechCost_USA, "StubTranTechCost") %>%
      add_xml_data(L254.StubTranTechCoef_USA, "StubTranTechCoef") %>%
      add_xml_data(L254.PerCapitaBased_trn_USA, "PerCapitaBased") %>%
      add_xml_data(L254.PriceElasticity_trn_USA, "PriceElasticity") %>%
      add_xml_data(L254.IncomeElasticity_trn_USA, "IncomeElasticity") %>%
      add_xml_data(L254.StubTranTechCalInput_USA, "StubTranTechCalInput") %>%
      add_xml_data(L254.StubTranTechProd_nonmotor_USA, "StubTranTechProd") %>%
      add_xml_data(L254.StubTranTechCalInput_passthru_USA, "StubTranTechCalInput") %>%
      add_xml_data(L254.BaseService_trn_USA, "BaseService") %>%
      add_precursors("L254.DeleteSupplysector_USAtrn",
                     "L254.DeleteFinalDemand_USAtrn",
                     "L254.Supplysector_trn_USA",
                     "L254.FinalEnergyKeyword_trn_USA",
                     "L254.tranSubsectorLogit_USA",
                     "L254.tranSubsectorShrwtFllt_USA",
                     "L254.tranSubsectorInterp_USA",
                     "L254.tranSubsectorSpeed_USA",
                     "L254.tranSubsectorSpeed_passthru_USA",
                     "L254.tranSubsectorSpeed_noVOTT_USA",
                     "L254.tranSubsectorSpeed_nonmotor_USA",
                     "L254.tranSubsectorVOTT_USA",
                     "L254.tranSubsectorFuelPref_USA",
                     "L254.StubTranTech_USA",
                     "L254.StubTranTech_passthru_USA",
                     "L254.StubTranTech_nonmotor_USA",
                     "L254.StubTranTechLoadFactor_USA",
                     "L254.StubTranTechCost_USA",
                     "L254.StubTranTechCoef_USA",
                     "L254.PerCapitaBased_trn_USA",
                     "L254.PriceElasticity_trn_USA",
                     "L254.IncomeElasticity_trn_USA",
                     "L254.StubTranTechCalInput_USA",
                     "L254.StubTranTechProd_nonmotor_USA",
                     "L254.StubTranTechCalInput_passthru_USA",
                     "L254.BaseService_trn_USA",
                     "gcam-usa/states_subregions") ->
      transportation_USA.xml

    # Produce outputs SSP1
    create_xml("transportation_USA_SSP1.xml") %>%
      add_xml_data(L254.tranSubsectorSpeed_USA_SSP %>%
                     filter(sce=="SSP1") %>%
                     select(-sce), "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorVOTT_USA_SSP  %>%
                     filter(sce=="SSP1") %>%
                     select(-sce), "tranSubsectorVOTT") %>%
      add_xml_data(L254.tranSubsectorFuelPref_USA_SSP  %>%
                     filter(sce=="SSP1") %>%
                     select(-sce), "tranSubsectorFuelPref") %>%
      add_xml_data(L254.StubTranTech_USA_SSP  %>%
                     filter(sce=="SSP1") %>%
                     select(-sce), "StubTranTech") %>%
      add_xml_data(L254.StubTranTechLoadFactor_USA_SSP  %>%
                     filter(sce=="SSP1") %>%
                     select(-sce), "StubTranTechLoadFactor") %>%
      add_xml_data(L254.StubTranTechCost_USA_SSP  %>%
                     filter(sce=="SSP1") %>%
                     select(-sce), "StubTranTechCost") %>%
      add_xml_data(L254.StubTranTechCoef_USA_SSP  %>%
                     filter(sce=="SSP1") %>%
                     select(-sce), "StubTranTechCoef") %>%
      add_xml_data(L254.PerCapitaBased_trn_USA_SSP  %>%
                     filter(sce=="SSP1") %>%
                     select(-sce), "PerCapitaBased") %>%
      add_xml_data(L254.PriceElasticity_trn_USA_SSP  %>%
                     filter(sce=="SSP1") %>%
                     select(-sce), "PriceElasticity") %>%
      add_xml_data(L254.IncomeElasticity_trn_USA_SSP  %>%
                     filter(sce=="SSP1") %>%
                     select(-sce), "IncomeElasticity") %>%
      add_precursors("L254.tranSubsectorSpeed",
                     "L254.tranSubsectorVOTT",
                     "L254.tranSubsectorFuelPref",
                     "L254.StubTranTech",
                     "L254.StubTranTechLoadFactor",
                     "L254.StubTranTechCost",
                     "L254.StubTranTechCoef",
                     "L254.PerCapitaBased_trn",
                     "L254.PriceElasticity_trn",
                     "L254.IncomeElasticity_trn",
                     "gcam-usa/states_subregions") ->
      transportation_USA_SSP1.xml

    # Produce outputs SSP3
    create_xml("transportation_USA_SSP3.xml") %>%
      add_xml_data(L254.tranSubsectorSpeed_USA_SSP %>%
                     filter(sce=="SSP3") %>%
                     select(-sce), "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorVOTT_USA_SSP  %>%
                     filter(sce=="SSP3") %>%
                     select(-sce), "tranSubsectorVOTT") %>%
      add_xml_data(L254.tranSubsectorFuelPref_USA_SSP  %>%
                     filter(sce=="SSP3") %>%
                     select(-sce), "tranSubsectorFuelPref") %>%
      add_xml_data(L254.StubTranTech_USA_SSP  %>%
                     filter(sce=="SSP3") %>%
                     select(-sce), "StubTranTech") %>%
      add_xml_data(L254.StubTranTechLoadFactor_USA_SSP  %>%
                     filter(sce=="SSP3") %>%
                     select(-sce), "StubTranTechLoadFactor") %>%
      add_xml_data(L254.StubTranTechCost_USA_SSP  %>%
                     filter(sce=="SSP3") %>%
                     select(-sce), "StubTranTechCost") %>%
      add_xml_data(L254.StubTranTechCoef_USA_SSP  %>%
                     filter(sce=="SSP3") %>%
                     select(-sce), "StubTranTechCoef") %>%
      add_xml_data(L254.PerCapitaBased_trn_USA_SSP  %>%
                     filter(sce=="SSP3") %>%
                     select(-sce), "PerCapitaBased") %>%
      add_xml_data(L254.PriceElasticity_trn_USA_SSP  %>%
                     filter(sce=="SSP3") %>%
                     select(-sce), "PriceElasticity") %>%
      add_xml_data(L254.IncomeElasticity_trn_USA_SSP  %>%
                     filter(sce=="SSP3") %>%
                     select(-sce), "IncomeElasticity") %>%
      add_precursors("L254.tranSubsectorSpeed",
                     "L254.tranSubsectorVOTT",
                     "L254.tranSubsectorFuelPref",
                     "L254.StubTranTech",
                     "L254.StubTranTechLoadFactor",
                     "L254.StubTranTechCost",
                     "L254.StubTranTechCoef",
                     "L254.PerCapitaBased_trn",
                     "L254.PriceElasticity_trn",
                     "L254.IncomeElasticity_trn",
                     "gcam-usa/states_subregions") ->
      transportation_USA_SSP3.xml

    return_data(transportation_USA.xml, transportation_USA_SSP1.xml,  transportation_USA_SSP3.xml)
  } else {
    stop("Unknown command")
  }
}
