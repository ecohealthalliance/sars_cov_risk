# main analysis: make AOHs for SARSr-CoV bat host species

rm(list = ls())

# load functions----------------------------------------------------------------
source(here("R/prepRastersCategorical.R"))
source(here("R/getMapColors.R"))
source(here("R/getAOH.R"))                                   

# load all files needed---------------------------------------------------------

# list of species of interest
# this was created with the sars_cov_host.R script
load(here("data/sars_cov_hosts.rda"))

# Southeast Asia files
# these were created with the aoh_bat_spatial_data.R script
SEA.shp <- readOGR(here("data/SEA.shp")) #shapefile of SEA country boundaries
habNoKarst.ras <- raster(here("data/SEAhabitat.tif")) # habitat raster (no karst)
habKarst.ras <- raster(here("data/SEAhabitatKarst.tif")) # habitat raster with karst 
SEAelevation <- raster(here("data/SEAelevation.tif"))# elevation raster
# note that no elevation data were available for singapore

# IUCN suitable habitat types and elevation limits
# created with host_habitat_elevation_download.R script
load(here("data/hostHabElev.rda"))

# details on habitat types, downloaded from 
# https://static-content.springer.com/esm/art%3A10.1038%2Fs41597-020-00599-8/MediaObjects/41597_2020_599_MOESM2_ESM.xlsx
# and saved as .csv 
habESM <- read.csv(here("data-raw/41597_2020_599_MOESM2_ESM.csv"))

# colors for plotting habitat types
# created with map_colors.R script
load(here("data/mapColors.rda"))

# bat host shapefiles
hostShapes <- readOGR(here("data/SARSrCoVhostShapefiles.shp"))

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

# dir.create(here("data/AOH"))
# dir.create(here("figures"))
dir.create(here("figures/AOHfigures"))

# takes about a min per species if makeSubplots = TRUE
for(i in sort(unique(sars_cov_hosts$BAT.SPECIES))){
  getAOH(speciesX = i, habitatSuitability = hostHabElev,
         habKarst = habKarstCat, habNoKarst = habNoKarstCat,
         habColKarst = colorsKarst, habColNoKarst = colorsNoKarst,
         speciesShapes = hostShapes, elevationRas = SEAelevation, 
         makeSubplots = TRUE, subplotDir = "./figures/AOHfigures/",
         countryShapes = SEA.shp, outputDir = "./data/AOH/")
}

# stack AOHs--------------------------------------------------------------------

# make AOH_heatmap.tif
# used for Fig 3 and other calculations

# get names of all binary AOH files
AOHbinFiles <- list.files(path = "./data/AOH", pattern = "binary",
                          full.names = TRUE)

# stack and sum the species rasters
AOHbinStack <- stack(AOHbinFiles)
AOH_heatmap <- sum(AOHbinStack, na.rm = T)

# Convert 0 to NAs
AOH_heatmap[AOH_heatmap == 0] = NA

writeRaster(AOH_heatmap, here("data/AOH_heatmap.tif"), datatype = "INT2U", 
            overwrite = TRUE)

