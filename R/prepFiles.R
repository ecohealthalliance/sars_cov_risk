# function to create shapefile of countries of interest, 
# and rasters of habitat types and elevation

prepFiles <- function(continent, countryNames, habitatMap, karst.sf){
  #' @param continentShapes continent region from natural earth
  #' @param countryNames vector of country names, needs to match up with
                        # names in getData("ISO3")
  #' @param habitatMap raster of habitat types
  #' @param karst.sf sf file of carbonate rock outcrops
  #' 
  #' @return return a list with the country shapefiles, habitat rasters, 
  #' and elevation raster
  
  # 1. first want to get the shapefiles for the countries of interest
  continentShp <- ne_countries(continent = continent, scale = "medium")
  
  # get ISO3 country codes given country names
  ISO3codes <- getData("ISO3") %>% 
    dplyr::filter(NAME %in% countryNames) %>% 
    pull(ISO3)
  
  # subset countries of interest from continent
  countryShapes <- continentShp[continentShp@data$iso_a3 %in% ISO3codes, ]
  
  # 2. next crop the global habitat types raster by the countries of interest
  habitatsCropped <- crop(habitatMap, extent(countryShapes))
  
  # 3. incorporate carbonate rock outcrop data as proxy for caves
  karst.sf$value <- 3000
  karst.ras <- fasterize(sf = karst.sf, raster = habitatsCropped, 
                         field = "value")
  
  # add the original habitat type raster and the karst raster
  habs_withKarst <- sum(habitatsCropped, karst.ras, na.rm = TRUE)
  
  # going to classify any land cover covered by karst as habitat 7.1 (caves)
  habs_withKarst <- reclassify(habs_withKarst, cbind(3000, Inf, 701), 
                               include.lowest = T)
  
  # 4. get elevation data
  for(i in ISO3codes){
    tryCatch({ 
      getData("alt", country = i, path = tempdir())
    }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
  }
  
  # get names of all .grd elevation files
  elevFiles <- list.files(path = tempdir(), pattern = "grd", 
                          full.names = TRUE)
  # import as a list of rasters
  elevList <- lapply(elevFiles, raster)
  # combine using mosaic function and do.call
  elevList$fun <- mean
  elevList$na.rm <- TRUE
  elevMosaic <- do.call(mosaic, elevList)
  
  # keeps showing datum warning
  # but based on this documentation https://rdrr.io/cran/raster/src/R/getData.R
  # datum should be WGS84. so set manually
  crs(elevMosaic) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  
  resList <- list(countryShapes, habitatsCropped, habs_withKarst, elevMosaic)
  return(resList)
  
}
