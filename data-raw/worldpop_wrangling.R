# Crop, resample and write worldpop raster
# need to make extent a little larger than study area to aid with resampling

source(here("R/GDALresample.R"))

SEA.ras <- raster(here("data/SEAhabitat.tif"))
newExt <- extent(SEA.ras)*1.1

# load WorldPopl data
WP <- raster(here("data-raw/ppp_2020_1km_Aggregated.tif"))

# crop and resample
WPcropped <- crop(WP, newExt)
WPresampled <- gdal_resample(r = WPcropped, r_base = SEA.ras, 
                             method = "bilinear")
names(WPresampled) = 'pop_2020'

# Save raster as integer (INT4S)
writeRaster(WPresampled, here("data/wpop_resampled.tif"), datatype = 'INT4U', 
            overwrite = TRUE)
