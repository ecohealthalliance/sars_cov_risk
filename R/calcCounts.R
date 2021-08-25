# AOH calculations by habitat type----------------------------------------------

# function to calculate number of humans and/or animals
# in the AOH of each species, split by habitat type
# also calculates the area if desired

calcCounts_byHT <- function(countsRaster, countsType, AOHfiles,
                            countryShapes, calcArea = TRUE){
  #' @param countsRaster raster of human counts
  #' @param countsType e.g. people, animals. helps to create a col name
  #' @param AOHfiles names of AOH rasters that include the different habitat types
  #' @param countryShapes shapefiles used for optional plotting
  #' @param calcArea T/F, should the area of each habitat type be calculated?
  #' 
  #' @return returns a dataframe of species, habitat type, counts, and area

  
  # calculate the number of people in each habitat type for each species
  # also the area, if wanted
  
  # set up data frame large enough to hold data 
  # if all habitats were suitable for all species  
  df <- data.frame(matrix(ncol = 4, nrow = 22*length(AOHfiles)))
  countColName <- paste0(countsType, "Count")
  colnames(df) <- c("species", "habType", countColName, "areakm2")
  
  counter <- 1

  for(i in 1:length(AOHfiles)){
    
    # load in AOH for one species
    AOH.i <- raster(AOHfiles[i])
    
    # if there's only one habitat type
    if(min(values(AOH.i), na.rm = T) == max(values(AOH.i), na.rm = T)){
      habType <- max(values(AOH.i), na.rm = T)
      
      # mask the counts raster by the AOH raster
      countsMasked <- mask(countsRaster, AOH.i)
      # then sum the count in all the cells
      countsSum <- ceiling(cellStats(countsMasked, stat = "sum", na.rm = T))
      
      df[counter, "species"] <- gsub("_", " ", names(AOH.i))
      df[counter, "habType"] <- habType
      df[counter, countColName] <- countsSum
      
      if(calcArea == TRUE){
        # calculate the area of all the cells
        areas <- raster::area(AOH.i, na.rm = T)
        # sum the areas
        areaSum <- sum(values(areas), na.rm = T)
        
        df[counter, "areakm2"] <- areaSum
      }
      
      counter <- counter + 1
      
    } else{
      # if there's multiple habitat types
      habTypes <- sort(unique(values(AOH.i)))
      habTypes <- habTypes[!is.na(habTypes)]
      
      for(j in 1:length(habTypes)){
        
        habType.j <- habTypes[j]
        AOH.j <- AOH.i
        
        AOH.j[AOH.j != habType.j] <- NA
        
        # mask the counts raster by the AOH raster
        countsMasked <- mask(countsRaster, AOH.j)
        # then sum the count in all the cells
        countsSum <- ceiling(cellStats(countsMasked, stat = "sum", na.rm = T))
        
        df[counter, "species"] <- gsub("_", " ", names(AOH.j))
        df[counter, "habType"] <- habType.j
        df[counter, countColName] <- countsSum
        
        if(calcArea == TRUE){
          area.ras <- raster::area(AOH.j, na.rm = T)
          areaSum <- sum(values(area.ras), na.rm = T)
          
          df[counter, "areakm2"] <- areaSum
        }
        
        counter <- counter + 1
      }
    }
  }
  
  # remove rows with all NAs
  df <- filter(df, rowSums(is.na(df)) != ncol(df)) 

  return(df)
}

# consensus area calculations---------------------------------------------------

calcCounts_overall <- function(countsRaster, countsType, 
                               consensusArea, countryShapes, calcArea = TRUE){
  #' @param countsRaster raster of human counts
  #' @param countsType e.g. people, pigs, chickens. helps to create a col name
  #' @param consensusArea AOH raster for all species combined
  #' @param countryShapes shapefiles used for optional plotting
  #' @param calcArea T/F, should the area be calculated?
  #' 
  #' @return returns a dataframe of counts and area
  
  # calculate the number of people
  # also the area, if wanted
  
  # set up data frame large enough to hold data 
  # if all habitats were suitable for all species  
  df <- data.frame(matrix(ncol = 2, nrow = 1))
  countColName <- paste0(countsType, "Count")
  colnames(df) <- c(countColName, "areakm2")
  
  # mask the counts raster by the AOH raster
  countsMasked <- mask(countsRaster, consensusArea)
  # then sum the count in all the cells
  countsSum <- ceiling(cellStats(countsMasked, stat = "sum", na.rm = T))
  
  df[1, countColName] <- countsSum
      
  if(calcArea == TRUE){
    # calculate the area of all the cells
    areas <- raster::area(consensusArea, na.rm = T)
    # sum the areas
    areaSum <- sum(values(areas), na.rm = T)
    
    df[1, "areakm2"] <- areaSum
    }
      
  return(df)
}

# calculations of IUCN ranges---------------------------------------------------
  
calcIUCNarea <- function(speciesX, IUCNshps, habitatRaster){
  #' @param speciesX species you want the area of
  #' @param habitatRaster raster to crop everything else to
  #' @param IUCNshp all IUCN species shapefiles
  #' 
  #' @return returns area
  
    # get species range shapefile
    spRange <- subset(IUCNshps, binomial == speciesX)
    
    # mask habitat raster by host range
    habMasked <- mask(habitatRaster, spRange)
    
    # cell values represent area in km2
    spArea <- raster::area(habMasked, na.rm = T)
    
    # sum the cell values to get overall area
    spTotArea <- sum(values(spArea), na.rm = T)
    
    return(spTotArea)
  }