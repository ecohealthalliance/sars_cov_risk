# script to assemble several spatial files that will be use in AOH analyses
# also sets up some color details for later plotting
library(here)
library(raster)

#source("./R/libraries.R")
source(here("R/prepFiles.R"))

# habitat types global map
# the 1km res map is easier/faster to work with
# also the authors recommend working at the coarsened resolution
hab.ras <- raster("./dataRaw/iucn_habitatclassification_composite_ver001/iucn_habitatclassification_composite_1km_lvl2_ver001.tif")

# carbonate rock outcrops as sf file
# cave habitats are suitable for many bat species
# Jung et al. doesn't include cave data, so we'll use carbonate rock as a proxy
karst.sf <- st_read("./dataRaw/karst_datasets/karst_wgs/karst_wgs.shp")

# countries of interest
SEAcountries <- c("Bangladesh", "Bhutan", "Brunei", "Cambodia", "China",
                  "Hong Kong", "India", "Indonesia", "Laos", "Macao", 
                  "Malaysia", "Myanmar", "Nepal", "Philippines", "Singapore", 
                  "Sri Lanka", "Taiwan", "Thailand", "East Timor", "Vietnam")

# get necessary habitat, elevation files set up---------------------------------

preppedFiles <- prepFiles(continent = "asia", countryNames = SEAcountries,
                          habitatMap = hab.ras, karst.sf = karst.sf)

# save all the outputs
# 1. shapefile of country boundaries
writeOGR(preppedFiles[[1]], dsn = here("data/"), layer = "SEA",
         driver = "ESRI Shapefile")

# 2a. habitat raster cropped to countries of interest
writeRaster(preppedFiles[[2]], here("data/SEAhabitat.tif"))

# 2b. habitat raster with karst added, cropped to countries of interest
writeRaster(preppedFiles[[3]], here("data/dataClean/SEAhabitatKarst.tif"), datatype = 'INT2U')

# 3. elevation raster
# note that no elevation data were available for Singapore
writeRaster(preppedFiles[[4]], here("data/SEAelevation.tif"))

# map colors--------------------------------------------------------------------

# read in colors for mapping
# https://github.com/Martin-Jung/Habitatmapping/blob/master/styles/level2.clr

mapColors <- read.delim("./dataRaw/habitatTypeColors.txt", header = F, 
                        sep = " ") %>% 
  # weird importing (maybe due to spaces) introduced new lines--get rid of them
  dplyr::filter(!is.na(as.numeric(V1))) %>% 
  rename(habType = V1, r = V2, g = V3, b = V4) %>% 
  dplyr::select(-c(V5:V8)) %>% 
  mutate(hex = rgb(r, g, b, maxColorValue = 255)) %>% 
  dplyr::select(habType, hex)

# write.csv(mapColors, "./dataClean/mapColors.csv", row.names = FALSE)
