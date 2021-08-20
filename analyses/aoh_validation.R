# validate AOHs with point data-------------------------------------------------
# load GBIF data
# these data were downloaded in advance
# using the get_gbif_data.R script

load(here("data/gbif_bat_data.rda"))

AOHfiles <- list.files(path = here("data/AOH"),
                       pattern = "binary", full.names = TRUE)

# takes a little while since there's some loops
valResults <- validateAOH(gbifData = gbif_bat_data, AOHfiles = AOHfiles, 
                          countryShapes = SEA.shp, geog = NE, buffSize = 5000)

# calculate percent of occurrence points that overlap with AOH
valResults <- valResults %>% 
  mutate(percOcc = round(ptsWithinAOH/numOccPts*100, 1))

# median % of points that are within buffsize of AOH
median(valResults$percOcc, na.rm = T)

# if we only look at species with a decent number of occurrence points
morePts <- filter(valResults, numOccPts > 30)
median(morePts$percOcc, na.rm = T)

#write.csv(valResults, "./dataClean/AOHvalidation.csv", row.names = F)
