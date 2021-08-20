# function that gets the necessary colors to plot different habitat types

getMapColors <- function(rasterCat, mapColors, includesKarst = TRUE){
  rat <- levels(rasterCat)[[1]]
  
  mapColors <- mapColors %>% 
    dplyr::filter(habType %in% rat[["habitatType"]]) %>% 
    # change 0 to white
    mutate(hex = sub("#002DE1", "#FFFFFF", hex)) 
  
  if(includesKarst == TRUE){
    # add gray color for karst/cave
    mapColors <- InsertRow(mapColors, c("701", "#808080"), 37)
    
    habColors <- as.vector(mapColors$hex)
  } else{
    habColors <- as.vector(mapColors$hex)
  }
  
}
