# validate AOHs with GBIF point data

source(here("R/validateAOH.R"))

# load GBIF data
# these data should be downloaded in advance using the get_gbif_data.R script
gbif_bat_data <- read.csv(here("data/gbif_bat_data.csv"))

# shapefile of SEA country boundaries
SEA.shp <- readOGR(here("data/SEA.shp"))

# Natural Earth data
NE <-  readOGR(here("data-raw/ne_50m_land.shp"))

AOHfiles <- list.files(path = here("data/AOH"),
                       pattern = "binary", full.names = TRUE)


# comparison of species AOHs to GBIF points (buffered by 5km)
# takes a little while since there's some loops
valResults <- validateAOH(gbifData = gbif_bat_data, AOHfiles = AOHfiles, 
                          countryShapes = SEA.shp, geog = NE, buffSize = 5000)

# calculate percent of occurrence points that overlap with AOH
valResults %<>% 
  mutate(percOcc = round(ptsWithinAOH/numOccPts*100, 1))

# save output
# used for Table S2
usethis::use_data(valResults, overwrite = TRUE)


# calculate some summary stats for main text

# median number occurrence points for spp with at least one occurrence point
filter(valResults, numOccPts > 0) %>% 
  dplyr::summarise(med = median(numOccPts))

# median % of points that are within buffsize of AOH
median(valResults$percOcc, na.rm = T)

# if we only look at species with a decent number of occurrence points
morePts <- filter(valResults, numOccPts > 40)
median(morePts$percOcc, na.rm = T)


