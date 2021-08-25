# read in original Jung et al. color scheme for mapping

mapColors <- read.delim(here("data-raw/iucn_habitatclassification_composite_1km_ver001/styles/level2.clr"), header = F, 
                        sep = " ") %>% 
  # weird importing (maybe due to spaces) introduced new lines--get rid of them
  dplyr::filter(!is.na(as.numeric(V1))) %>% 
  rename(habType = V1, r = V2, g = V3, b = V4) %>% 
  dplyr::select(-c(V5:V8)) %>% 
  mutate(hex = rgb(r, g, b, maxColorValue = 255)) %>% 
  dplyr::select(habType, hex)

usethis::use_data(mapColors, overwrite = TRUE)
