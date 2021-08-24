# code to create several spatial files that will be use in AOH analyses

source(here("R/prepSEAfiles.R"))

# habitat types global map
# the 1km res map is easier/faster to work with
# also the authors recommend working at the coarsened resolution
hab.ras <- raster(here("data-raw/iucn_habitatclassification_composite_1km_ver001/iucn_habitatclassification_composite_1km_lvl2_ver001.tif"))

# carbonate rock outcrops as sf file
# cave habitats are suitable for many bat species
# Jung et al. doesn't include cave data, so we'll use carbonate rock as a proxy
karst.sf <- st_read(here("data-raw/karst_wgs.shp"))

# countries of interest
SEAcountries <- c("Bangladesh", "Bhutan", "Brunei", "Cambodia", "China",
                  "Hong Kong", "India", "Indonesia", "Laos", "Macao", 
                  "Malaysia", "Myanmar", "Nepal", "Philippines", "Singapore", 
                  "Sri Lanka", "Taiwan", "Thailand", "East Timor", "Vietnam")

# get necessary habitat, elevation files set up---------------------------------

preppedFiles <- prepSEAfiles(continent = "asia", countryNames = SEAcountries,
                          habitatMap = hab.ras, karst.sf = karst.sf)

# save all the outputs
# 1. shapefile of country boundaries
writeOGR(preppedFiles[[1]], dsn = here("data/"), layer = "SEA",
         driver = "ESRI Shapefile")

# 2a. habitat raster cropped to countries of interest
writeRaster(preppedFiles[[2]], here("data/SEAhabitat.tif"), datatype = "INT2U")

# 2b. habitat raster with karst added, cropped to countries of interest
writeRaster(preppedFiles[[3]], here("data/SEAhabitatKarst.tif"), 
            datatype = "INT2U")

# 3. elevation raster
# note that no elevation data were available for Singapore
writeRaster(preppedFiles[[4]], here("data/SEAelevation.tif"), 
            datatype = "INT2S")
