# get species occurrence data from GBIF-----------------------------------------
# GBIF data will be used to validate AOHs

# gbif username, password, and email are set in the .Renviron
user <- Sys.getenv("GBIF_USER")
pwd <- Sys.getenv("GBIF_PWD")
email <- Sys.getenv("GBIF_EMAIL")

# get species usage keys
load(here("data/sars_cov_hosts.rda"))
gbif_taxon_keys <- sars_cov_hosts %>% 
  dplyr::pull("BAT.SPECIES") %>% 
  taxize::get_gbifid_(method = "backbone") %>% 
  imap(~ .x %>% mutate(original_sciname = .y)) %>% 
  bind_rows() %>% 
  dplyr::filter(matchtype == "EXACT" & status == "ACCEPTED") %>% 
  dplyr::pull(usagekey)

# submit GBIF data download request
# occ_download(pred_in("taxonKey", gbif_taxon_keys), format = "SIMPLE_CSV",
#   user = user, pwd = pwd, email = email)

# once the download request has completed, can download
gbif_bat_data_dl <- occ_download_get("0332260-200613084148143")
# and then import
gbif_bat_data <- occ_download_import(gbif_bat_data_dl)
# then save
usethis::use_data(gbif_bat_data, overwrite = TRUE)
