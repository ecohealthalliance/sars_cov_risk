# functions to pull species and elevation data and add species name
# code adapted from:
# https://rstudio-pubs-static.s3.amazonaws.com/704264_4212bab5c9f344a69a20fa5203dcc3cd.html

f.hab.redlist <- function(speciesName){
  hab.spp <- rl_habitats(speciesName, key = apikey)
  
  # save species name as column with habitat suitability
  hab.spp$result$species <- hab.spp$name
  hab.spp$name <- NULL
  
  return(hab.spp$result)
}


f.elev.redlist <- function(speciesName){
  elev.spp <- rl_search(speciesName, key = apikey)
  
  # save species name as column
  elev.spp$result$species <- elev.spp$name
  elev.spp$name <- NULL
  
  return(elev.spp$result)
}