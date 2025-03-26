
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
# #sctXX5E87xJbD*
set_rdhs_config(email = "amertens@berkeley.edu",
                project = "MN Prediction",
                data_frame = "data.table::as.data.table",
                config_path = "~/.rdhs.json",
                global = TRUE)

year <- 2018
country <- "Zambia" #Check I have access!
indicator ="womananemia"

# dhsData <- getDHSdata(country = country, indicator = indicator, Recode=NULL, year = year)
# #We then use getDHSindicator() to process the raw survey data into a data frame, where the column value is the indicator of interest. The data frame also contains information specifying survey designs in the survey package, including cluster ID, household ID, survey weight and strata information.
# data <- getDHSindicator(dhsData, indicator = indicator)
# head(data)
#
# #spatial information
# geo <- getDHSgeo(country = country, year = year)
#
# poly.adm1 <- geodata::gadm(country=country, level=1, path=tempdir())
# poly.adm1 <- sf::st_as_sf(poly.adm1)
# poly.adm2 <- geodata::gadm(country=country, level=2, path=tempdir())
# poly.adm2 <- sf::st_as_sf(poly.adm2)
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
# dhsData_full$IRdata
#
# data <- getDHSindicator(dhsData$IRdata, indicator = NULL)
# head(data)



#functionalize indicator estimation
indicator_vec <- c("ancvisit4+", "RH_ANCN_W_N4P", "womananemia",
                   "AN_ANEM_W_ANY", "unmet_family", "FP_NADA_W_UNT", "FP_CUSA_W_MOD",
                   "AN_NUTS_W_THN","CN_ANMC_C_ANY", "CN_NUTS_C_WH2",
                   "wasting", "CN_NUTS_C_HA2", "stunting", "ML_PMAL_C_RDT",
                   "WS_TLET_H_IMP", "WS_TLET_P_BAS", "WS_SRCE_P_BAS","CH_DIAT_C_ORT", "DPT3", "CH_VACC_C_DP3",
                   "CH_VACC_C_DP1", "CH_VACC_C_BAS", "CH_VACC_C_NON", "CN_BRFS_C_EXB",
                   "CH_VACC_C_MSL", "PCV3", "RotaC1","RH_DELA_C_SKP", "CM_ECMR_C_NNR", "nmr","ML_NETP_H_IT2","HA_HIVP_B_HIV")

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


# i = "Kenya"
# i = "Ghana"
# surveyyear_vec=2008
# j = 2024:2010
# k = "womananemia"
# #note: many estimates are missing because the data downloads, but get this error:
# # Error in onestrat(x[index, , drop = FALSE], clusters[index], nPSU[index][1],  :
# #   Stratum (North East urban) has only one PSU at stage 1
#
# surveyyear_vec=2008
# country_vec= c("Kenya", "Ghana")
# indicator_vec <- c( "womananemia")


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

      poly.adm1 <- geodata::gadm(country=i, level=1, path=tempdir())
      poly.adm1 <- sf::st_as_sf(poly.adm1)
      poly.adm2 <- geodata::gadm(country=i, level=2, path=tempdir())
      poly.adm2 <- sf::st_as_sf(poly.adm2)
      cluster.info=NULL
      try(cluster.info <- clusterInfo(geo=geo, poly.adm1=poly.adm1, poly.adm2=poly.adm2,by.adm1 = "NAME_1",by.adm2 = "NAME_2"))
      if(!is.null(cluster.info)){

        d=dhsData=NULL
        try(dhsData <- suppressMessages({getDHSdata(country = i, indicator = NULL, Recode=NULL, year = j) }))
        if(!is.null(dhsData)){
          d=dhsData
          d$geo_data=geo_d
          d$cluster.info=cluster.info
          d$country=i
          d$year=j

          d <- list(d)
          names(d) <- paste0(i,"_",j)

          dhs_compiled_d <- c(dhs_compiled_d, d)
          saveRDS(dhs_compiled_d, here("data/DHS_compiled_raw_data_interim.rds"))
        }

        for(k in indicator_vec){
          dInd=indicator_data=indicator_res=res=NULL


          if(!is.null(dhsData)){
            for(m in 1:length(dhsData)){
              if(!is.null(indicator_data)){
                if(nrow(indicator_data)==0){
                  indicator_data=NULL
                }
              }

              if(is.null(indicator_data)){
                try(indicator_data <- getDHSindicator(dhsData[[m]], indicator = k))
              }
              if(!is.null(indicator_data)){
                try(indicator_data <- indicator_data %>% filter(!is.na(value)))
                if(nrow(indicator_data)>0){
                  dInd=indicator_data
                  dInd$country=i
                  dInd$year=j
                  dInd$indicator=k
                  dhs_indicators_d <- bind_rows(dhs_indicators_d, dInd)
                  saveRDS(dhs_indicators_d, here("data/DHS_compiled_indicator_data_interim.rds"))
                }
              }
            }

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
      saveRDS(fullres, here("data/DHS_national_indicator_prevalences_interim.rds"))

    }
  }
}
head(fullres)

saveRDS(fullres, here("data/DHS_national_indicator_prevalences.rds"))
saveRDS(dhs_compiled_d, here("data/DHS_compiled_raw_data.rds"))
saveRDS(dhs_indicators_d, here("data/DHS_compiled_indicator_data.rds"))
saveRDS(geo_d, here("data/DHS_compiled_gps_data.rds"))



fullres <- readRDS(here("data/DHS_national_indicator_prevalences_interim.rds"))
dim(fullres)
unique(fullres$country)
