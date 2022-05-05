# code to assess bat CoV research effort 
# modified from code provided by K. Phelps

searchTerms <- read.csv(here("data-raw/searchterms.csv"), header = TRUE)

SEAcountries <- c("Bangladesh", "Bhutan", "Brunei", "Cambodia", "China",
                  "Hong Kong", "India", "Indonesia", "Laos", "Macao", 
                  "Malaysia", "Myanmar", "Nepal", "Philippines", "Singapore", 
                  "Sri Lanka", "Taiwan", "Thailand", "East Timor", "Vietnam")

ISO3codes <- getData("ISO3") %>% 
  dplyr::filter(NAME %in% SEAcountries) %>% 
  pull(ISO3)

# restrict search terms to countries of interest
searchTerms %<>%
  filter(iso_code3 %in% ISO3codes)

# PUBMED LITERATURE SEARCH - RESEARCH EFFORT------------------------------------

# "Chiroptera" AND "Coronavirus" by country
# looping the search to run for all the search terms in the column selected
CoVByCo <- lapply(1:nrow(searchTerms), function(x) 
  entrez_search(db = "pubmed", term = searchTerms$bat_country_coronavirus[x], 
                retmax = 0))
# CoVByCo
# creating the two vectors for dataframe (publication count by country)
counts <- unlist(lapply(seq_along(CoVByCo), function(x) CoVByCo[[x]]$count))
q_trans <- unlist(lapply(seq_along(CoVByCo), 
                          function(x) CoVByCo[[x]]$QueryTranslation))
# creating a database with the combination of the vectors 
PubMedCoVByCountry <- data.frame(query = q_trans, counts = counts, 
                                 searchTerms$iso_code2, searchTerms$iso_code3, 
                                 searchTerms$country) 

# save results
#usethis::use_data(PubMedCoVByCountry, overwrite = TRUE)

# Fig S1------------------------------------------------------------------------

# creating a map on research effort: "Chiroptera" AND "Coronavirus" by country
map_effortCoV <- joinCountryData2Map(PubMedCoVByCountry, joinCode = "ISO3", 
                                     nameJoinColumn = "searchTerms.iso_code3")

SEA_effortCoV <- map_effortCoV[map_effortCoV$ADM0_A3 %in% ISO3codes, ]

breaks <- c(0, 5, 20, 100, 150, 550, 600)
colourPalette <- rev(viridis(6))

png("figures/FigS1.png", width = 6, height = 5, units = "in", res = 300)
mapParams <- mapCountryData(SEA_effortCoV, nameColumnToPlot = "counts",
                            addLegend = FALSE, 
                            mapTitle = "Research Effort (PubMed): \n(Bat OR Bats OR Chiroptera) AND Coronaviruses", 
                            lwd = 1.5,
                            catMethod = breaks, 
                            colourPalette = colourPalette)
do.call(addMapLegend, c(mapParams,legendLabels = "all", legendWidth = 1,
                        legendIntervals = "data"))
dev.off()
