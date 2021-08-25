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
            overwrite = T)
