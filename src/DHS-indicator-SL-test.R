
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

#set up DHS download
set_rdhs_config(email = "amertens@berkeley.edu",
                project = "MN Prediction",
                data_frame = "data.table::as.data.table",
                config_path = "~/.rdhs.json",
                global = TRUE)

indicator_vec <- c("ancvisit4+", "RH_ANCN_W_N4P", "womananemia",
                   "AN_ANEM_W_ANY", "unmet_family", "FP_NADA_W_UNT", "FP_CUSA_W_MOD",
                   "AN_NUTS_W_THN","CN_ANMC_C_ANY", "CN_NUTS_C_WH2",
                         "wasting", "CN_NUTS_C_HA2", "stunting", "ML_PMAL_C_RDT",
                         "WS_TLET_H_IMP", "WS_TLET_P_BAS", "WS_SRCE_P_BAS","CH_DIAT_C_ORT", "DPT3", "CH_VACC_C_DP3",
                 "CH_VACC_C_DP1", "CH_VACC_C_BAS", "CH_VACC_C_NON", "CN_BRFS_C_EXB",
                 "CH_VACC_C_MSL", "PCV3", "RotaC1","RH_DELA_C_SKP", "CM_ECMR_C_NNR", "nmr","ML_NETP_H_IT2","HA_HIVP_B_HIV")

year <- 2018
country <- "Zambia" #Check I have access!
# #dhsData <- getDHSdata(country = country,  year = year)
#
#
# dhsData <- getDHSdata(country = country, indicator = indicator, Recode=NULL, year = year)
# #We then use getDHSindicator() to process the raw survey data into a data frame, where the column value is the indicator of interest. The data frame also contains information specifying survey designs in the survey package, including cluster ID, household ID, survey weight and strata information.
# data <- getDHSindicator(dhsData, indicator = indicator)
# head(data)
#
# #spatial information
# geo <- getDHSgeo(country = country, year = year)
# cluster.info <- clusterInfo(geo=geo, poly.adm1=poly.adm1, poly.adm2=poly.adm2,
#                             by.adm1 = "NAME_1",by.adm2 = "NAME_2")
#
#
# #direct estimation
# res_ad1 <- directEST(data = data,
#                      cluster.info = cluster.info,
#                      admin = 1)
# head(res_ad1$res.admin1)
#
# #national and urban-rural estimates
# res_ad0 <- directEST(data=data,
#                      cluster.info= cluster.info,
#                      admin=0,
#                      strata="all")
#
#
# #get all indicators:
# dhsData_full <- getDHSdata(country = "Zambia",
#                       indicator = NULL,
#                       year = 2018)
# names(dhsData_full)
#
# data <- getDHSindicator(dhsData$IRdata, indicator = NULL)
# head(data)



#functionalize indicator estimation
country_vec = c("Afghanistan", "Bangladesh", "Cameroon",
                "Ecuador", "Ethiopia", "Fiji", "Guatemala", "Kenya", "Cambodia",
                "Maldives", "Mexico", "Malawi", "Nigeria", "Nepal", "Pakistan",
                "Philippines", "Senegal", "Vietnam", "Azerbaijan", "Colombia",
                "Costa Rica", "India", "Sri Lanka", "Timor-Leste", "South Africa",
                "Belize", "Brazil", "Côte d'Ivoire",
                "Ghana", "Gambia", "Iraq", "Jordan", "Kyrgyzstan", "Lao People's Democratic Republic",
                "Liberia", "Mozambique", "Nicaragua", "Oman", "Peru", "Papua New Guinea",
                "Sierra Leone", "United Republic of Tanzania", "Uganda",
                "Uzbekistan", "Zambia", "Zimbabwe", "Argentina", "Burundi", "Burkina Faso",
                "Dominican Republic", "Guyana", "Haiti", "Madagascar",
                "Mongolia", "Rwanda",
                "Georgia", "Lebanon", "Morocco", "Tajikistan")
i = "Zambia"
surveyyear_vec=c(2000:2019)
j = 2018
k = indicator_vec[1]

fullres=NULL
for(i in country_vec){
  for(j in surveyyear_vec){
    geo <- NULL
    try(geo <- getDHSgeo(country = i, year = j))

    if(!is.null(geo)){
      poly.adm1 <- geodata::gadm(country=i, level=1, path=tempdir())
      poly.adm1 <- sf::st_as_sf(poly.adm1)
      poly.adm2 <- geodata::gadm(country=i, level=2, path=tempdir())
      poly.adm2 <- sf::st_as_sf(poly.adm2)
      cluster.info=NULL
      try(cluster.info <- clusterInfo(geo=geo, poly.adm1=poly.adm1, poly.adm2=poly.adm2,by.adm1 = "NAME_1",by.adm2 = "NAME_2"))
      if(!is.null(cluster.info)){
        for(k in indicator_vec){

          dhsData=indicator_data=indicator_res=res=NULL
          try(dhsData <- suppressMessages({getDHSdata(country = i, indicator = k, Recode=NULL, year = j) }))

          if(!is.null(dhsData)){

            try(indicator_data <- getDHSindicator(dhsData, indicator = k))
            try(indicator_data <- indicator_data %>% filter(!is.na(value)))
            if(!is.null(indicator_data)){
              if(nrow(indicator_data)>0){
                try(indicator_res <- directEST(data = indicator_data, cluster.info = cluster.info, admin = 0, strata = "all"))
                indicator_res <- indicator_res[[1]]
                if(!is.null(indicator_res)){
                res <- data.frame(country=i, year=j, indicator=k,indicator_res)
                print(res)
                }else{
                  res <- data.frame(country=i, year=j, indicator=k)
                }
              }
            }else{
              res <- data.frame(country=i, year=j, indicator=k)
            }
          }else{
            res <- data.frame(country=i, year=j, indicator=k)
          }
        fullres <- bind_rows(fullres, res)
        }
      }else{
        fullres <- bind_rows(fullres, data.frame(country=i, year=j, indicator=k))
      }
    }
  }
}
fullres

saveRDS(fullres, here("data/DHS_national_indicator_prevalences.rds"))


