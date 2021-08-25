# calculate the overall number of people in the consensus area
# ie after combining all the individual species AOHs

source(here("R/calcCounts.R"))

# human population count data
WP <- raster(here("data/wpop_resampled.tif"))

consensusArea <- raster(here("data/AOH_heatmap.tif"))

# habitat shapefile
SEA.shp <- readOGR(here("data/SEA.shp"))

countsOverall <- calcCounts_overall(countsRaster = WP, countsType = "people_WP", 
                                    consensusArea = consensusArea,
                                    countryShapes = SEA.shp, calcArea = TRUE)

usethis::use_data(countsOverall, overwrite = TRUE)
