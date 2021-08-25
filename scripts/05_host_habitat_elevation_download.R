# code to use IUCN API to pull data on habitat suitability and elevation limits
# to replicate, need your own IUCN API token
# https://apiv3.iucnredlist.org/api/v3/token

source(here("R/getRedlistData.R"))

apikey <- Sys.getenv("IUCN_KEY")

# list of host species, from sars_cov_hosts.R script
load(here("data/sars_cov_hosts.rda"))

hosts <- sars_cov_hosts$BAT.SPECIES

# get habitat information for all host species
list.hab <- lapply(1:length(hosts), function(x) f.hab.redlist(hosts[x]))

# combine lists into one data frame, clean up
hostHabitatTypes <- bind_rows(list.hab) %>% 
  filter(suitability == "Suitable") %>% 
  mutate_at(vars(code), as.numeric) %>% 
  # to match up with color scheme codes
  mutate(raster_value = case_when(
    code == 1.4 ~ 104,
    code == 1.5 ~ 105,
    code == 1.6 ~ 106,
    code == 1.7 ~ 107,
    code == 1.8 ~ 108,
    code == 1.9 ~ 109,
    code == 3.4 ~ 304,
    code == 3.5 ~ 305,
    code == 3.7 ~ 307,
    code == 3.8 ~ 308,
    code == 4.4 ~ 404,
    code == 6 ~   600,
    code == 7.1 ~ 701,
    code == 7.2 ~ 702,
    code == 8.2 ~ 802,
    code == 14.1 ~ 1401,
    code == 14.2 ~ 1402,
    code == 14.3 ~ 1403,
    code == 14.4 ~ 1404,
    code == 14.5 ~ 1405,
    code == 14.6 ~ 1406,
    code == 15.8 ~ 1508)) %>% 
  select(-c(season, majorimportance))

# get elevation data for all species
list.elev <- lapply(1:length(hosts), function(x) f.elev.redlist(hosts[x]))

# combine lists into one data frame, pull out elevation data
elevLim <- bind_rows(list.elev) %>% 
  select(scientific_name, elevation_upper, elevation_lower)

# join habitat and elevation data
hostHabElev <- left_join(hostHabitatTypes, elevLim, 
                     by = c("species" = "scientific_name"))

# save habitat and elevation data
usethis::use_data(hostHabElev, overwrite = TRUE)
