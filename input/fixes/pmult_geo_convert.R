library(gcamdata); library(data.table); library(tibble)


file_to_convert <- data.table::fread("L223.CapacityTechInputPMult_geo.csv") %>%
  tibble::as_tibble(); file_to_convert

filename_xml <- "L223.CapacityTechInputPMult_geo.xml"

gcamdata::create_xml(filename_xml) %>%
  gcamdata::add_xml_data(file_to_convert, "CapacityTechInputPMult")%>%
  gcamdata::run_xml_conversion()
