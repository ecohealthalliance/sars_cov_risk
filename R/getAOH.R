# function to get the binary AOH for a species

getAOH <- function(speciesX, habitatSuitability, habKarst, habNoKarst, 
                   habColKarst, habColNoKarst, speciesShapes, elevationRas, 
                   makeSubplots = FALSE, subplotDir, countryShapes, outputDir){
  
  suitable <- habitatSuitability %>%
    dplyr::filter(species == speciesX) %>%
    pull(raster_value)
  
  # pull habitat raster either with or without caves
  # depending on whether caves are suitable for the species
  # habitat colors only needed if you're going to plot habitats for each indiv.
  if(701 %in% suitable){
    hab.ras <- habKarst
    habColors <- habColKarst
    
    myColorKey <- list(labels = 
                         list(labels = c("Not mapped", "1.0", "1.1", "1.4",
                                         "1.5", "1.6", "1.7", "1.8", "1.9",
                                         "2.1", "2.2", "3.0", "3.3", "3.4", 
                                         "3.5", "3.6", "3.7", "4.0", "4.1", 
                                         "4.2", "4.4", "4.5", "4.6", "4.7",
                                         "5.0", "5.2", "5.3", "5.4", "5.5", 
                                         "5.6", "5.7", "5.10", "5.11", "5.13", 
                                         "5.14", "6.0", "7.1", "8.1", "8.2", 
                                         "8.3", "14.1", "14.2", "14.3", "14.5")
                              ))
  } else{
    hab.ras <- habNoKarst
    habColors <- habColNoKarst
    
    myColorKey <- list(labels = 
                         list(labels = c("Not mapped", "1.0", "1.1", "1.4",
                                         "1.5", "1.6", "1.7", "1.8", "1.9",
                                         "2.1", "2.2", "3.0", "3.3", "3.4", 
                                         "3.5", "3.6", "3.7", "4.0", "4.1", 
                                         "4.2", "4.4", "4.5", "4.6", "4.7",
                                         "5.0", "5.2", "5.3", "5.4", "5.5", 
                                         "5.6", "5.7", "5.10", "5.11", "5.13", 
                                         "5.14", "6.0", "8.1", "8.2", "8.3", 
                                         "14.1", "14.2", "14.3", "14.5")
                         ))
  }
  
  # get species range shapefile
  hostRange <- subset(speciesShapes, binomial == speciesX)
  
  # plot of underlying habitat type raster overlaid with outlined host range
  if(makeSubplots == TRUE){
    tiff(filename = paste0(subplotDir, speciesX, "_map1.tif"),
         height = 7, width = 7, units = "in", res = 300)
    print(levelplot(hab.ras, col.regions = habColors,
              colorkey = myColorKey) +
      layer(sp.polygons(countryShapes, lwd = 1),
            data = list(countryShapes = countryShapes)) +
      layer(sp.polygons(hostRange, lwd = 3),
            data = list(hostRange = hostRange)))
    dev.off()
  }
  
  # mask habitat raster by host range
  hab.rasMasked <- mask(hab.ras, hostRange)
  
  # plot habitat raster masked by host range
  if(makeSubplots == TRUE){
    tiff(filename = paste0(subplotDir, speciesX, "_map2.tif"), 
         height = 7, width = 7, units = "in", res = 300)
    print(levelplot(hab.rasMasked, col.regions = habColors, 
                    colorkey = myColorKey) +
      layer(sp.polygons(countryShapes, lwd = 1),
            data = list(countryShapes = countryShapes)) +
      layer(sp.polygons(hostRange, lwd = 3),
            data = list(hostRange = hostRange)))
    dev.off()
  }
  
  # set any values in the raster that aren't "suitable" to NA
  hab.rasMasked[!hab.rasMasked@data@values %in% suitable, ] <- NA
  
  # plot showing suitable habitat within range
  if(makeSubplots == TRUE){
    tiff(filename = paste0(subplotDir, speciesX, "_map3.tif"), 
         height = 7, width = 7, units = "in", res = 300)
    print(levelplot(hab.rasMasked, col.regions = habColors, 
                    colorkey = myColorKey) +
      layer(sp.polygons(countryShapes, lwd = 1),
            data = list(countryShapes = countryShapes)) +
      layer(sp.polygons(hostRange, lwd = 2),
            data = list(hostRange = hostRange)))
    dev.off()
  }
  
  # if no habitat is suitable, skip to the next species
  if(is.infinite(max(values(hab.rasMasked), na.rm = T))){next}
  
  # otherwise, now restrict by host elevation limits
  maxElev <- habitatSuitability %>% 
    dplyr::filter(species == speciesX) %>% 
    distinct(species, .keep_all = TRUE) %>% 
    pull(elevation_upper)
  
  minElev <- habitatSuitability %>% 
    dplyr::filter(species == speciesX) %>% 
    distinct(species, .keep_all = TRUE) %>% 
    pull(elevation_lower)
  
  if(!is.na(maxElev)){
    
    # mask elevation raster by host range
    elev.rasMasked <- mask(elevationRas, hostRange)
    
    # set elevation values outside species elevation limits to NA
    elev.rasMasked[elev.rasMasked > maxElev, ] <- NA
    elev.rasMasked[elev.rasMasked < minElev, ] <- NA
    
    # have to resample the elevation raster 
    # so it can match the extent of the habitat raster
    elevResampled <- resample(elev.rasMasked, hab.rasMasked)
    
    # mask the habitat raster by the elevation raster to get AOH
    AOH <- mask(hab.rasMasked, elevResampled)
    
    # plot showing final AOH
    if(makeSubplots == TRUE & speciesX != "Rhinolophus hipposideros"){
      tiff(filename = paste0(subplotDir, speciesX, "_map4.tif"), 
           height = 7, width = 7, units = "in", res = 300)
      print(levelplot(AOH, col.regions = habColors, colorkey = myColorKey) +
        layer(sp.polygons(countryShapes, lwd = 1),
              data = list(countryShapes = countryShapes)))
      dev.off()
    }
    
    # save 2 versions of AOH so we can combine all species at the end
    
    # the first one will keep the different habitat types
    writeRaster(AOH, paste0(outputDir, speciesX, ".tif"), 
                datatype = "INT2U")
    
    # the second one will have AOH as binary (0/1)
    AOH[!is.na(AOH)] <- 1
    writeRaster(AOH, paste0(outputDir, speciesX, "_binary.tif"), 
                datatype = "INT2U")
    
  } else{
    print(paste("No elevation limits available for", speciesX))
    
    # if there are no elevation limits available, 
    # then AOH consists of any Suitable habitat
    
    # just plot the habitat and country range (map 3 without species range)
    if(makeSubplots == TRUE){
      tiff(filename = paste0(subplotDir, speciesX, "_map4.tif"), 
           height = 7, width = 7, units = "in", res = 300)
      print(levelplot(hab.rasMasked, col.regions = habColors, 
                      colorkey = myColorKey) +
        layer(sp.polygons(countryShapes, lwd = 1)))
      dev.off()
    }
    
    # again save two versions of AOH so we can combine all species at the end
    writeRaster(hab.rasMasked, paste0(outputDir, speciesX, ".tif"), 
                datatype = "INT2U")
    
    hab.rasMasked[!is.na(hab.rasMasked)] <- 1
    writeRaster(hab.rasMasked, paste0(outputDir, speciesX, 
                                      "_binary.tif"), datatype = "INT2U")
  }
  
}

