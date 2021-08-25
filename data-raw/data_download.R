# load and unzip data files

library(here)
library(gitcreds)
library(piggyback)

# World Map of Carbonate Rock Outcrops V3.0
# https://crc806db.uni-koeln.de/layer/show/296/
# downloaded manually and saved in data-raw folder
unzip(here("data-raw/karst_wgs.zip"), exdir = here("data-raw"))


# Natural Earth 1:50m Physical Vectors: Land
# version 4.1.0
# https://www.naturalearthdata.com/downloads/50m-physical-vectors/
# downloaded manually and saved in data-raw folder
unzip(here("data-raw/ne_50m_land.zip"), exdir = here("data-raw"))

# IUCN terrestrial mammals shapefiles
# https://www.iucnredlist.org/resources/spatial-data-download
# pb_upload(here("MAMMALS_TERRESTRIAL_ONLY.zip"),
#           repo = "ecohealthalliance/sars_cov_risk",
#           tag = "v1.0.0", .token = gitcreds_get()$password)

# A global map of terrestrial habitat types
# https://zenodo.org/record/3666246#.YSUcm0ApAUE
# pb_upload(here("iucn_habitatclassification_composite_1km_ver001.zip"),
#           repo = "ecohealthalliance/sars_cov_risk",
#           tag = "v1.0.0", .token = gitcreds_get()$password)

# WorldPop Populations Counts
# Population 2020
# https://www.worldpop.org/geodata/summary?id=24777
# pb_upload(here("ppp_2020_1km_Aggregated.tif"),
#           repo = "ecohealthalliance/sars_cov_risk",
#           tag = "v1.0.0", .token = gitcreds_get()$password)


# this *should* download all files using piggyback
# if it doesn't work, can also download manually at:
# https://github.com/ecohealthalliance/sars_cov_risk/releases
# save them into data-raw folder
pb_download(repo = "ecohealthalliance/sars_cov_risk",
            tag = "v1.0.0", .token = gitcreds_get()$password,
            exdir = here("data-raw"))

# unzip all zipped files
unzip(here("data-raw/MAMMALS_TERRESTRIAL_ONLY.zip"), exdir = here("data-raw"))

unzip(here("data-raw/iucn_habitatclassification_composite_1km_ver001.zip"), 
      exdir = here("data-raw"))

