[![DOI](https://zenodo.org/badge/398210464.svg)](https://zenodo.org/badge/latestdoi/398210464)

# sars_cov_risk
Data and code to support: A strategy to assess spillover risk of bat SARS-related coronaviruses in Southeast Asia


### Repo Structure
-  `scripts/` contains all the scripts used to download large data files, conduct pre-processing,
    run the analyses, and generate outputs
-   `R/` contains files with functions used in other scripts
-  `data-raw/` contains data used in these analyses, including
    -   a `SARSrCoVhosts/` directory with lists and citations of SARSr-CoV bat hosts 
    -   a database of IgG positivity over time after infection (`IgGtimeseries`) 
        (see Supplemental Material for further details)
    -   spatial datasets (`karst_wgs.zip` and `ne_50m_land.zip`)
    -   IUCN shapefiles (once downloaded from the release) (`MAMMALS_TERRESTRIAL_ONLY.zip`)
    -   habitat classification files (once downloaded from the release) (`iucn_habitatclassification_composite_1km_ver001.zip`)
    -   WorldPop 2020 population count raster (once downloaded from the release) (`ppp_2020_1km_Aggregated.tif`)
-  `data/` is a holding directory for intermediate data files 
-  `figures/` contains generated figures

---


