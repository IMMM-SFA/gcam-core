setwd("../gcamdata")
devtools::load_all()
librayr(dplyr)
library(tidyr)

resources_USA = load_from_cache(inputs_of("module_gcamusa_batch_resources_USA_xml"))


resources_USA[['L210.GrdRenewRsrcCurves_geo_USA']] %>%
    filter(renewresource %in% c("PV_resource", "CSP_resource", "offshore wind resource")) %>%
    group_by(region, renewresource) %>%
    summarize(price=min(extractioncost)) %>%
    ungroup() ->
    fixed_cf_resources

fixed_cf_resources %>%
    select(region, unlimited.resource = renewresource) %>%
    mutate(output.unit = "EJ",
           price.unit = "1-CF",
           market = region) ->
    unlimit_market


fixed_cf_resources %>%
    rename(unlimited.resource = renewresource) %>%
    expand(., ., tibble(year=as.integer(MODEL_YEARS))) ->
    unlimit_price

setwd("../fixes")
create_xml("replace_PV_CSP_offwind_fixedCF.xml") %>%
    add_xml_data(fixed_cf_resources, "DeleteRenewRsrc") %>%
    add_xml_data(unlimit_market, "UnlimitRsrc") %>%
    add_xml_data(unlimit_price, "UnlimitRsrcPrice") %>%
    run_xml_conversion()
