# function to change species habitat types data into long form

convertHabitatsLong <- function(habitatData, speciesCol){
  #' @param habitatData data frame in wide format, providing (at least) species 
                      # name, habitat names, suitability, and importance
  #' @param speciesCol the column name containing the species scientific name
  #' 
  #' @return returns a dataframe of species habitat suitability in long format
  
  pres <- habitatData %>% 
    dplyr::select(-matches("Importance|Suitability")) %>% 
    pivot_longer(starts_with("X"), names_to = "habitat_type", 
                 values_to = "presence") %>% 
    separate(habitat_type, into = c("habitat_type", "habitat_description"),
             sep = "(?<=[0-9])\\.(?=[A-Z])") %>% 
    mutate(habitat_type = gsub("X", "", habitat_type))
  
  if(any(grepl("Suitability", colnames(habitatData)))){
    suit <- habitatData %>% 
      pivot_longer(contains("Suitability"), names_to = "habitat_type", 
                   values_to = "suitability") %>% 
      dplyr::select(speciesCol, habitat_type, suitability) %>% 
      mutate(habitat_type = gsub(".Suitability", "", habitat_type),
             habitat_type = gsub("X", "", habitat_type))
  }
  
  if(any(grepl("Importance", colnames(habitatData)))){
    impt <- habitatData %>% 
      pivot_longer(contains("Importance"), names_to = "habitat_type", 
                   values_to = "importance") %>% 
      dplyr::select(speciesCol, habitat_type, importance) %>% 
      mutate(habitat_type = gsub(".Importance", "", habitat_type),
             habitat_type = gsub("X", "", habitat_type))
  }
  
  if(exists("suit") & exists("impt")){
    habitats_long <- left_join(pres, suit, 
                               by = c(speciesCol, "habitat_type")) %>% 
      left_join(., impt, by = c(speciesCol, "habitat_type")) %>% 
      mutate(raster_value = case_when(
        habitat_type == 1.4 ~ 104,
        habitat_type == 1.5 ~ 105,
        habitat_type == 1.6 ~ 106,
        habitat_type == 1.7 ~ 107,
        habitat_type == 1.8 ~ 108,
        habitat_type == 1.9 ~ 109,
        habitat_type == 3.4 ~ 304,
        habitat_type == 3.5 ~ 305,
        habitat_type == 3.7 ~ 307,
        habitat_type == 3.8 ~ 308,
        habitat_type == 4.4 ~ 404,
        habitat_type == 6 ~   600,
        habitat_type == 7.1 ~ 701,
        habitat_type == 7.2 ~ 702,
        habitat_type == 8.2 ~ 802,
        habitat_type == 14.1 ~ 1401,
        habitat_type == 14.2 ~ 1402,
        habitat_type == 14.3 ~ 1403,
        habitat_type == 14.4 ~ 1404,
        habitat_type == 14.5 ~ 1405,
        habitat_type == 14.6 ~ 1406,
        habitat_type == 15.8 ~ 1508))
    
    # habitats can only be important if they are suitable
    habitats_long <- habitats_long %>% 
      mutate(importance = case_when(
        suitability %in% c("Unknown", "Marginal") ~ NA_integer_,
        TRUE ~ importance))
    
  } else{
    habitats_long <- pres %>% 
      mutate(raster_value = case_when(
        habitat_type == 1.4 ~ 104,
        habitat_type == 1.5 ~ 105,
        habitat_type == 1.6 ~ 106,
        habitat_type == 1.7 ~ 107,
        habitat_type == 1.8 ~ 108,
        habitat_type == 1.9 ~ 109,
        habitat_type == 3.4 ~ 304,
        habitat_type == 3.5 ~ 305,
        habitat_type == 3.7 ~ 307,
        habitat_type == 3.8 ~ 308,
        habitat_type == 4.4 ~ 404,
        habitat_type == 6 ~   600,
        habitat_type == 7.1 ~ 701,
        habitat_type == 7.2 ~ 702,
        habitat_type == 8.2 ~ 802,
        habitat_type == 14.1 ~ 1401,
        habitat_type == 14.2 ~ 1402,
        habitat_type == 14.3 ~ 1403,
        habitat_type == 14.4 ~ 1404,
        habitat_type == 14.5 ~ 1405,
        habitat_type == 14.6 ~ 1406,
        habitat_type == 15.8 ~ 1508))
  }
  
  return(habitats_long)

}

# example
# load habitat presence, importance, suitability data
# sarbecoHostHabitats <- read.csv("./dataRaw/Sarbecovirus_Host_Habitat_Types.csv", 
#                      na.strings = "")
# 
# habitats_long <- convertHabitatsLong(habitatData = sarbHostHabitats, 
#                                      speciesCol = "species_name")
# 
#write.csv(habitats_long, "./dataClean/sarbecoHostHabitatsLong.csv", row.names = F)

