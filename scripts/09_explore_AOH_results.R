# code to explore population size and areas of AOHs

source(here("R/calcCounts.R"))

# habitat raster cropped to countries of interest
SEA.ras <- raster(here("data/SEAhabitat.tif"))

# habitat shapefile
SEA.shp <- readOGR(here("data/SEA.shp"))

# human population count data
WP <- raster(here("data/wpop_resampled.tif"))

# names of AOH files with different habitat types
AOHfiles <- list.files(path = here("data/AOH"), full.names = TRUE)
AOHfiles <- AOHfiles[!grepl("binary", AOHfiles)]

# calculate number of people in each AOH, by habitat type-----------------------
countsHT <- calcCounts_byHT(countsRaster = WP, countsType = "people_WP", 
                            AOHfiles = AOHfiles, countryShapes = SEA.shp, 
                            calcArea = TRUE)

usethis::use_data(countsHT, overwrite = TRUE)

# calculate the overall number of people in the consensus area------------------
# ie after combining all the individual species AOHs

consensusArea <- raster(here("data/AOH_heatmap.tif"))

# habitat shapefile
SEA.shp <- readOGR(here("data/SEA.shp"))

countsOverall <- calcCounts_overall(countsRaster = WP, countsType = "people_WP", 
                                    consensusArea = consensusArea,
                                    countryShapes = SEA.shp, calcArea = TRUE)

usethis::use_data(countsOverall, overwrite = TRUE)

# calculate IUCN range areas----------------------------------------------------

# for comparison, want the size of the original IUCN ranges of each species

# if we want to calculate area of each IUCN range,
# need to rasterize species shapefiles individually
# then use raster::area function

IUCNshps <- readOGR(here("data/SARSrCoVhostShapefiles.shp"))
SEA.ras <- raster(here("data/SEAhabitat.tif"))
load(here("data/sars_cov_hosts.rda"))

IUCNareas <- sapply(X = sars_cov_hosts[, "BAT.SPECIES"], FUN = calcIUCNarea, 
                    IUCNshps = IUCNshps, habitatRaster = SEA.ras)

IUCNareas <- as.data.frame(IUCNareas)
IUCNareas$species <- row.names(IUCNareas)
row.names(IUCNareas) <- NULL

usethis::use_data(IUCNareas, overwrite = TRUE)
