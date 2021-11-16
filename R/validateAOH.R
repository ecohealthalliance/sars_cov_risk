# testing validation of species AOH with GBIF occurrence data

validateAOH <- function(gbifData, AOHfiles, countryShapes, geog, buffSize){
  
  #' @param gbifData csv of GBIF records, previously downloaded
  #' @param AOHfiles names of AOH rasters
  #' @param countryShapes shapefiles to delineate area of interest
  #' @param geog Natural Earth shapefile
  #' @param buffSize buffer size in m to create around each occurrence point
  #' 
  #' @return returns a dataframe of species, habitat type, counts, and area
  
  library(countrycode)
  
  # clean up GBIF data
  gbifClean <- gbifData %>% 
    # remove old records
    dplyr::filter(year > 1990) %>% 
    # remove unsuitable data sources
    dplyr::filter(basisOfRecord %in% c("HUMAN_OBSERVATION", 
                                       "MACHINE_OBSERVATION", "MATERIAL_SAMPLE",
                                       "OBSERVATION", "PRESERVED_SPECIMEN")) %>% 
    dplyr::filter(occurrenceStatus == "PRESENT") %>% 
    # filter coords where uncertainty is > 35km
    # but a lot of coordinates don't have uncertainty, so keep those
    dplyr::filter(coordinateUncertaintyInMeters < 35000 | 
                    is.na(coordinateUncertaintyInMeters)) %>% 
    # remove non-numeric and NA coords, or non-valid coords
    cc_val(., lon = "decimalLongitude", lat = "decimalLatitude") %>% 
    # remove records in ocean
    # definitely possible that bats have been seen over the ocean
    # but our habitat map is only terrestrial
    cc_sea(., lon = "decimalLongitude", lat = "decimalLatitude", ref = geog) %>% 
    # remove duplicate records for species
    cc_dupl(., lon = "decimalLongitude", lat = "decimalLatitude") %>% 
    # remove records with identical lat/long
    cc_equ(., lon = "decimalLongitude", lat = "decimalLatitude") %>% 
    # remove records with zeros in coordinates
    cc_zero(., lon = "decimalLongitude", lat = "decimalLatitude") %>% 
    # remove records within 5km of country capitals
    cc_cap(., lon = "decimalLongitude", lat = "decimalLatitude", 
           buffer = 5000) %>% 
    # remove records within 1km of country/province centroids
    cc_cen(., lon = "decimalLongitude", lat = "decimalLatitude") %>% 
    # remove records assigned to biodiversity institutions
    cc_inst(., lon = "decimalLongitude", lat = "decimalLatitude") %>% 
    # remove records with coordinate/country mismatch
    # have to convert country code to iso3
    # first get rid of Kosovo--country code not recognized
    filter(!countryCode == "XK") %>% 
    mutate(countryCode = countrycode(countryCode, origin = "iso2c",
                                     destination = "iso3c")) %>% 
    cc_coun(., lon = "decimalLongitude", lat = "decimalLatitude", 
            iso3 = "countryCode")
  
  # create df to hold validation data
  occValidation <- data.frame(matrix(NA, ncol = 3, nrow = length(AOHfiles)))
  colnames(occValidation) <- c("species", "numOccPts", "ptsWithinAOH")
  
  for(i in 1:length(AOHfiles)){
  
    # get species name
    species.i <- regmatches(AOHfiles[[i]], 
                            regexec('AOH/(.*?)_', AOHfiles[[i]]))[[1]][2]
    
    occValidation[i, "species"] <- species.i
    
    # get corresponding species AOH
    AOH.i <- raster(AOHfiles[[i]])
    
    # get species GBIF occurrence points
    pts <- gbifClean %>% 
      dplyr::filter(species == species.i)
  
    
    if(nrow(pts) == 0){
      occValidation[i, "numOccPts"] <- 0
      next
      }
    
    # save points as spatial points obj
    pts.sp <- SpatialPoints(pts[, c("decimalLongitude", "decimalLatitude")],
                            proj4string = CRS("+proj=longlat +datum=WGS84 +datum=WGS84"))
    
    # remove occurrence records outside countries of interest
    pts_incountry <- pts.sp[countryShapes]
    
    if(nrow(pts_incountry@coords) == 0){
      occValidation[i, "numOccPts"] <- 0
      next
    }
    
    occValidation[i, "numOccPts"] <- nrow(pts_incountry@coords)
    
    # levelplot(AOH.i, col.regions = "lightblue", colorkey = FALSE, margin = FALSE) +
    #   layer(sp.polygons(countryShapes, lwd = 1, col = "black")) +
    #   layer(sp.points(pts_incountry, pch = 16, col = "red"))
    
    # create df to hold whether each point overlaps
    speciesPts <- data.frame(matrix(NA, ncol = 2, nrow = nrow(pts_incountry@coords)))
    colnames(speciesPts) <- c("ptNo", "overlap")
    
    # for each occurrence point at a time, create buffer around it
    # extract to see if it overlaps with any AOH cells and record whether it does
    for(j in 1:nrow(pts_incountry@coords)){
      
      speciesPts[j, "ptNo"] <- j
      
      pt.j <- pts_incountry[j, ]
      
      pt.j_buff <- raster::buffer(pt.j, buffSize)
      
      extractedPt <- raster::extract(AOH.i, pt.j_buff)
      
      if(sum(extractedPt[[1]], na.rm = T) >= 1){
        speciesPts[j, "overlap"] <- 1
      }
      else{speciesPts[j, "overlap"] <- 0
      }
    }
    
    ptsWithinAOH <- sum(speciesPts$overlap, na.rm = T)
      
    occValidation[i, "ptsWithinAOH"] <- ptsWithinAOH
  }

  return(occValidation)
}
