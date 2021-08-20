# https://www.iucnredlist.org/resources/spatial-data-download

# function to pull the IUCN shapefile data for a set of species

getIUCNshapes <- function(shapefileDir = "./dataRaw/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp", 
                          specOfInt){
  #' @param shapefileDir directory where the IUCN shapefiles are
                         # (assumes these are already downloaded)
  #' @param specOfInt scientific names of the species you want shapefiles for
  
  # load terrestrial mammal IUCN shapefiles
  IUCNshapes <- readOGR(shapefileDir)
  
  # check that all species names are listed in Catalogue of Life
  # (tried using IUCN but didn't work)
  # td_create("col")
  # test <- sarbHosts %>% 
  #   dplyr::select(BAT.SPECIES) %>% 
  #   mutate(id = get_ids(BAT.SPECIES, "col")) %>% 
  #   mutate(accepted_name = get_names(id, "col"))
  
  # filter all mammal shapefiles to desired species
  shapesSubset <- IUCNshapes[IUCNshapes$binomial %in% specOfInt, ] 
  
  # check for non overlapping species
  speciesDiff <- setdiff(specOfInt, unique(shapesSubset$binomial))
  
  print(paste("There is no IUCN data for these species: ", speciesDiff))
  
  return(shapesSubset)
  
}


# example
# sarbHosts <- read.csv("./dataClean/bat_sarbecovirus_hosts.csv")
# 
# hostShapes <- getIUCNshapes(specOfInt = sarbHosts$BAT.SPECIES)
# 
# writeOGR(hostShapes, dsn = "./dataClean", layer = "sarbecoHostShapefiles", 
#          driver = "ESRI Shapefile")
