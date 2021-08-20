# function to turn habitat rasters into categorical for plotting

prepRastersCategorical <- function(rasterQuant, habitatMeta){
  
  library(DataCombine)
  # change continuous values of habitat raster to categories
  # by modifying the RAT (raster attribute table)
  
  rasterCat <- as.factor(rasterQuant)
  rat <- levels(rasterCat)[[1]]
  habData <- left_join(rat, habitatMeta, by = c("ID" = "NewCode"))
  #rat[["habitatType"]] <- habData$IUCNLevel
  rat[["habitatType"]] <- habData$ID
  levels(rasterCat) <- rat
  
  return(rasterCat)
}
