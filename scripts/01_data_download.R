# load and unzip data files that were too big to upload via github

# load packages that will be needed for all further analyses
source(here("R/libraries.R"))

# World Map of Carbonate Rock Outcrops V3.0
# https://crc806db.uni-koeln.de/layer/show/296/
# downloaded manually and saved in data-raw folder
unzip(here("data-raw/karst_wgs.zip"), exdir = here("data-raw"))

# Natural Earth 1:50m Physical Vectors: Land
# version 4.1.0
# https://www.naturalearthdata.com/downloads/50m-physical-vectors/
# downloaded manually and saved in data-raw folder
unzip(here("data-raw/ne_50m_land.zip"), exdir = here("data-raw"))

# this *should* download IUCN shapefiles, the habitat types map,
# and WorldPop data (using piggyback)
pb_download(repo = "ecohealthalliance/sars_cov_risk",
            tag = "v2.0.0", .token = gitcreds_get()$password,
            dest = here("data-raw"))

# if it doesn't work, can also download the files at:
# https://github.com/ecohealthalliance/sars_cov_risk/releases

# or can download from their sources

  # IUCN terrestrial mammals shapefiles
  # https://www.iucnredlist.org/resources/spatial-data-download

  # A global map of terrestrial habitat types
  # https://zenodo.org/record/3666246#.YSUcm0ApAUE
  
  # WorldPop Population Counts: Population 2020
  # https://www.worldpop.org/geodata/summary?id=24777

# if you downloaded the files manually, save them as named below,
# in the data-raw folder
# iucn_habitatclassification_composite_1km_ver001.zip
# MAMMALS_TERRESTRIAL_ONLY.zip
# ppp_2020_1km_Aggregated.tif

# once files are downloaded, unzip all zipped files
unzip(here("data-raw/MAMMALS_TERRESTRIAL_ONLY.zip"), exdir = here("data-raw"))

unzip(here("data-raw/iucn_habitatclassification_composite_1km_ver001.zip"), 
      exdir = here("data-raw"))
