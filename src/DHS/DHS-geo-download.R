
#-------------------------------------------------------------------------------
#https://cran.r-project.org/web/packages/surveyPrev/vignettes/vignette-main.html
#-------------------------------------------------------------------------------

library(rdhs)
library(data.table)
library(tidyverse)

library(surveyPrev)

# install.packages("INLA",
#                  repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"),
#                  dep=TRUE,
#                  type = "binary",
#                  install.packages.check.source = "no")
library(INLA)
#remotes::install_github("rspatial/geodata")
library(geodata)
library(sf)
library(SUMMER)
library(rdhs)
library(patchwork)
library(dplyr)
library(tidyr)
library(here)
library(kableExtra)
source(paste0(here::here(),"/src/0-functions.R"))

#Load BRINDA data
mn_df <- readRDS(here("data/clean_brinda_data.rds"))


#built in indicators
data(surveyPrevIndicators)
head(surveyPrevIndicators)
indicator_vec <- surveyPrevIndicators$ID

#built in indicators
data(surveyPrevIndicators)
head(surveyPrevIndicators)
indicator_vec <- surveyPrevIndicators$ID


#set up DHS download
# #sctXX5E87xJbD*
set_rdhs_config(email = "amertens@berkeley.edu",
                project = "MN Prediction",
                data_frame = "data.table::as.data.table",
                config_path = "~/.rdhs.json",
                global = TRUE)




country_vec = c("Cameroon", "Ethiopia", "Kenya",
                "Maldives",  "Malawi", "Nigeria",
                 "Senegal", "South Africa", "Côte d'Ivoire",
                "Ghana", "Gambia",
                "Liberia", "Mozambique",
                "Sierra Leone", "United Republic of Tanzania", "Uganda",
                "Zambia", "Zimbabwe", "Burundi", "Burkina Faso",
                 "Guyana",  "Madagascar","Rwanda",
                "Morocco")


surveyyear_vec=2024:2010



fullres=dhs_compiled_d=dhs_indicators_d=geo_d=NULL
for(i in country_vec){
  for(j in surveyyear_vec){
    geo <- NULL
    try(geo <- getDHSgeo(country = i, year = j))


    if(!is.null(geo)){

      geo$country<-i
      geo$year<-j
      geo_d <- bind_rows(geo_d,geo)
      saveRDS(geo_d, here("data/DHS_compiled_gps_data_interim.rds"))

    }
  }
}

saveRDS(geo_d, here("data/DHS_compiled_gps_data.rds"))
table(geo_d$country, geo_d$DHSYEAR)

