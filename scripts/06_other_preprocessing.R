# other pre-processing, creating intermediate files needed for analyses

# Crop, resample and write worldpop raster--------------------------------------
# need to make extent a little larger than study area to aid with resampling

source(here("R/GDALresample.R"))

SEA.ras <- raster(here("data/SEAhabitat.tif"))
newExt <- extent(SEA.ras)*1.1

# load WorldPop data
WP <- raster(here("data-raw/ppp_2020_1km_Aggregated.tif"))

# crop and resample
WPcropped <- crop(WP, newExt)
WPresampled <- gdal_resample(r = WPcropped, r_base = SEA.ras, 
                             method = "bilinear")
names(WPresampled) = 'pop_2020'

# Save raster as integer (INT4U)
writeRaster(WPresampled, here("data/wpop_resampled.tif"), datatype = 'INT4U', 
            overwrite = TRUE)


# read in original Jung et al. color scheme for mapping-------------------------

mapColors <- read.delim(here("data-raw/iucn_habitatclassification_composite_1km_ver001/styles/level2.clr"), 
                        header = F, sep = " ") %>% 
  # weird importing (maybe due to spaces) introduced new lines--get rid of them
  dplyr::filter(!is.na(as.numeric(V1))) %>% 
  rename(habType = V1, r = V2, g = V3, b = V4) %>% 
  dplyr::select(-c(V5:V8)) %>% 
  mutate(hex = rgb(r, g, b, maxColorValue = 255)) %>% 
  dplyr::select(habType, hex)

usethis::use_data(mapColors, overwrite = TRUE)

# select host shapefiles--------------------------------------------------------

source(here("R/getIUCNshapes.R"))     
load(here("data/sars_cov_hosts.rda"))

# trim shapefiles of all terrestrial mammals to only bat species of interest
hostShapes <- getIUCNshapes(specOfInt = sars_cov_hosts$BAT.SPECIES)

writeOGR(hostShapes, dsn = here("data"), layer = "SARSrCoVhostShapefiles",
         driver = "ESRI Shapefile", overwrite_layer = TRUE)
