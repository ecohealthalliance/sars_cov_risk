# code to request species occurrence data from GBIF
# GBIF data are used to validate AOHs

# gbif username, password, and email are set in the .Renviron
# need to have your own if you want to make a new data request
user <- Sys.getenv("GBIF_USER")
pwd <- Sys.getenv("GBIF_PWD")
email <- Sys.getenv("GBIF_EMAIL")

# code used to get species usage keys
# load(here("data/sars_cov_hosts.rda"))
# gbif_taxon_keys <- sars_cov_hosts %>% 
#   dplyr::pull("BAT.SPECIES") %>% 
#   taxize::get_gbifid_(method = "backbone") %>% 
#   imap(~ .x %>% mutate(original_sciname = .y)) %>% 
#   bind_rows() %>% 
#   dplyr::filter(matchtype == "EXACT" & status == "ACCEPTED") %>% 
#   dplyr::pull(usagekey)

# submit GBIF data download request
# occ_download(pred_in("taxonKey", gbif_taxon_keys), format = "SIMPLE_CSV",
#   user = user, pwd = pwd, email = email)

# once the download request has completed, can download
gbif_bat_data_dl <- occ_download_get("0039542-210914110416597")
# and then import
gbif_bat_data <- occ_download_import(gbif_bat_data_dl)
# then save
write.csv(gbif_bat_data, here("data/gbif_bat_data.csv"), row.names = FALSE)

# if that's not working, the GBIF search results can also be accessed at
# https://doi.org/10.15468/dl.8w26d8 
