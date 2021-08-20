# main analysis
# make AOHs for SE Asia sarbecovirus bat species
# validate with point data

library(here)
# load functions needed---------------------------------------------------------

source("./R/functions/GDALresample.R")
source("./R/functions/getIUCNshapes.R")                         
source("./R/functions/convertHabitatTypesLong.R")
source("./R/functions/prepRastersCategorical.R")
source("./R/functions/getMapColors.R")
source("./R/functions/getAOH.R")                                   
source("./R/functions/validateAOH.R")

# load all files needed---------------------------------------------------------

# need to have run "1_assembleSarbecoHosts.R" and "2_prepBaseFiles.R" first

# list of species of interest
# this was created with the sars_cov_host.R script
load(here("data/sars_cov_hosts.rda"))

# shapefile of SEA country boundaries
SEA.shp <- readOGR(here("data/SEA.shp"))

# habitat raster (no karst)
habNoKarst.ras <- raster(here("data/SEAhabitat.tif"))

# habitat raster with karst 
habKarst.ras <- raster(here("data/SEAhabitatKarst.tif"))

# elevation raster
# note that no elevation data were available for singapore
SEAelevation <- raster(here("data/SEAelevation.tif"))

# habitat types--wide format data
habTypes <- read.csv("./dataRaw/SARSrCoV_Host_Habitat_Types.csv")

# details on habitat types
habESM <- read.csv("./dataRaw/iucn_habitatclassification_composite_ver001/Jung ESM.csv")

# species elevation limits
# elevation data obtained from IUCN species profiles
elevLim <- read.csv(here("data-raw/elevationLimits.csv"))

# for plotting habitat types
mapColors <- read.csv("./dataClean/mapColors.csv")

# Natural Earth data
NE <-  readOGR("./dataRaw/NaturalEarth/ne_50m_land.shp")

# get host shapefiles-----------------------------------------------------------

hostShapes <- getIUCNshapes(specOfInt = sars_cov_hosts$BAT.SPECIES)

# writeOGR(hostShapes, dsn = "./dataClean", layer = "SARSrCoVhostShapefiles",
#          driver = "ESRI Shapefile")

# reconfigure habitat types to long format--------------------------------------

habitats_long <- convertHabitatsLong(habitatData = habTypes, 
                                     speciesCol = "species_name") %>% 
  dplyr::filter(!is.na(raster_value)) %>% 
  left_join(., elevLim, by = c("species_name" = "species")) %>% 
  dplyr::filter(suitability == "Suitable")

# write.csv(habitats_long, "./dataClean/habitatsLong.csv", row.names = F)

# set up rasters and colors for categorical plotting----------------------------

# here we have two separate rasters
# one raster has karst and will be used for species that find caves suitable
# the other raster doesn't have karst and will be used for the other species

habNoKarstCat <- prepRastersCategorical(rasterQuant = habNoKarst.ras, 
                                        habitatMeta = habESM)
colorsNoKarst <- getMapColors(rasterCat = habNoKarstCat, mapColors = mapColors, 
                              includesKarst = FALSE)

habKarstCat <- prepRastersCategorical(rasterQuant = habKarst.ras, 
                                      habitatMeta = habESM)
colorsKarst <- getMapColors(rasterCat = habKarstCat, mapColors = mapColors, 
                            includesKarst = TRUE)

# produce AOHs for all species--------------------------------------------------

for(i in sort(unique(targetSpec$BAT.SPECIES))){
  getAOH(speciesX = i, 
         habitatSuitability = habitats_long, speciesColName = "species_name",
         elevMaxCol = "upperElevation", elevMinCol = "lowerElevation",
         habKarst = habKarstCat, habNoKarst = habNoKarstCat,
         habColKarst = colorsKarst, habColNoKarst = colorsNoKarst,
         speciesShapes = hostShapes, elevationRas = SEAelevation, 
         makeSubplots = FALSE, subplotDir = "./figures/AOHfigures/",
         countryShapes = SEA.shp,
         outputDir = "./dataClean/AOH/")
}

