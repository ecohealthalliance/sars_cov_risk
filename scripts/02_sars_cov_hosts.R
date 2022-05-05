# code to prepare`sars_cov_hosts`dataset

# previously compiled table by Olival et al. 2021 that lists bat species 
# in which beta-CoVs were detected
d1 <- read.csv(here("data-raw/SARSrCoVhosts/Olival_Table_S1_data_release_beta-CoVs_bats.csv"))

# part of  Supplementary Table 1 by Latinne et al. 2020
# listing #s of beta CoV RdRp sequences detected from bat species in their study
# https://static-content.springer.com/esm/art%3A10.1038%2Fs41467-020-17687-3/MediaObjects/41467_2020_17687_MOESM1_ESM.pdf
d2 <- read.csv(here("data-raw/SARSrCoVhosts/Latinne_TableS1_betaCoV_hosts.csv"))

# additional SARSr-CoV bat hosts from recent literature
d3 <- read.csv(here("data-raw/SARSrCoVhosts/additional_hosts.csv"))

# some datasets have beta CoVs beyond SARSr-CoVs--remove
sars_cov_hosts <- dplyr::bind_rows(d1, d2, d3) %>% 
  mutate(beta.CoV.SUBGENERA = 
           fct_collapse(beta.CoV.SUBGENERA, Sarbecovirus = 
                          c("Sarbecovirus (Clade 3)", "Sarbecovirus (Clade 2)", 
                            "Sarbecovirus (Clade 1)"))) %>% 
  dplyr::filter(beta.CoV.SUBGENERA == "Sarbecovirus") %>% 
  
  # do some species name cleaning
  replace(., . == "Mops plicatus", "Chaerephon plicatus") %>% # to match IUCN
  replace(., . == "Rhinolophus paradoxolophus", "Rhinolophus rex") %>% # subsp.
  replace(., . == "Hipposideros pomona", "Hipposideros gentilis") %>% # sp split
  distinct(BAT.SPECIES, .keep_all = TRUE) %>% # remove duplicates
  filter(., !grepl(" sp.", BAT.SPECIES)) %>% # rm bats not known to spec. level
  filter(!BAT.SPECIES == "Rhinolophus monoceros") %>% # no IUCN assessment
  rename(REFERENCES = REFERENCE.S.) %>% 
  
  # annotate info on where species are found (from IUCN)
  mutate(SE.ASIA.SPECIES = case_when(
    BAT.SPECIES %in% c("Rhinolophus blasii", "Rhinolophus cornutus", 
                       "Rhinolophus euryale", "Rhinolophus mehelyi", 
                       "Plecotus auritus") ~ 0,
    TRUE ~ 1)) %>% 
  mutate(REGION = case_when(
    BAT.SPECIES %in% c("Hipposideros armiger", "Hipposideros gentilis",
                       "Hipposideros pratti", "Rhinolophus thomasi") ~ "Asia",
    TRUE ~ as.character(REGION))) %>% 
  filter(SE.ASIA.SPECIES == 1)

# Save clean data to data folder
usethis::use_data(sars_cov_hosts, overwrite = TRUE)
