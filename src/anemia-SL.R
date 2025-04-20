

rm(list=ls())
library(dplyr)
library(haven)
library(here)
library(purrr)
library(labelled)
library(sl3)
library(origami)
library(tlverse)
library(caret)
library(data.table)
library(ck37r)
source(paste0(here::here(),"/src/0-functions.R"))
source(paste0(here::here(),"/src/0-SL-setup.R"))
source(paste0(here::here(),"/src/DHS/DHS_functions.R"))
source(paste0(here::here(),"/src/DHS/DHS_variable_recode.R"))

DHS_anemia_countries = read.csv(paste0(here(),"/metadata/DHS_countries_with_anemia_data.csv")) %>%
  select(CountryName, SurveyYear) %>% filter(CountryName %in% c("Cameroon", "Ethiopia", "Kenya",
                                                                          "Maldives",  "Malawi", "Nigeria",
                                                                          "Senegal", "South Africa", "Côte d'Ivoire",
                                                                          "Ghana", "Gambia",
                                                                          "Liberia", "Mozambique",
                                                                          "Sierra Leone", "United Republic of Tanzania", "Uganda",
                                                                          "Zambia", "Zimbabwe", "Burundi", "Burkina Faso",
                                                                          "Guyana",  "Madagascar","Rwanda",
                                                                          "Morocco"))



Xvars= c("sex", "age", "rel_to_head", "edu_level.x",
         "edu_years.x", "usual_resident", "slept_last_night", "used_bednet",
         "region.x",
         "residence_type", "hh_size.x", "num_under5", "wealth_quint.x",
         "wealth_score.x", "has_electricity", "has_radio", "has_tv", "has_fridge",
         "has_bicycle", "has_motorcycle", "has_car", "floor_material",
         "wall_material", "roof_material", "water_source", "toilet_type",
         "share_toilet", "cooking_fuel", "has_mosquito_net", "altitude",
         "has_mobile", "has_computer", "age_single", "age_5yrgrp", "edu_level.y",
         "edu_years.y", "religion", "ethnicity", "region.y", "residence",
         "hh_size.y", "marital_st", "wealth_quint.y", "wealth_score.y",
         "partner_edu", "partner_age", "partner_occ", "respond_work",
         "pregnant_now", "months_preg", "time_sincempl", "total_living_ch",
         "living_ch_plus", "months_last_bir", "age_first_union", "age_first_sex",
         "future_fert_pref", "wanted_more_ch", "ideal_num_ch", "bmi_times100",
         "bf_current", "dv_selected", "dv_ever_phys",
         "dv_injury", "hiv_time_test", "hiv_place_test")



filenames <- paste0("dhs_",DHS_anemia_countries$CountryName, " ",DHS_anemia_countries$SurveyYear,".RDS") %>% gsub(" ","_",.)
raw_data_list <- list()
for(i in filenames){
  try(raw_data_list[[i]] <- readRDS(here("data/DHS/",i)))[[1]]
}
names(raw_data_list)


clean_data_list <- list()
for(i in 1:length(raw_data_list)){
  clean_data_list[[i]] <- clean_DHS(raw_data_list[[i]][[1]])
  names(clean_data_list)[i] <- names(raw_data_list)[i]
}
names(clean_data_list) <- gsub("dhs_","",names(clean_data_list))
names(clean_data_list) <- gsub(".RDS","",names(clean_data_list))
names(clean_data_list) <- gsub("_"," ",names(clean_data_list))
names(clean_data_list)

SL_res <- list()
for(i in 1:length(clean_data_list)){
  res=try(DHS_SL(d=clean_data_list[[i]], Xvars=Xvars))
  SL_res[[i]] = res
  names(SL_res)[i] <- names(clean_data_list)[i]

}
names(SL_res)
saveRDS(SL_res, file=here("results/DHS_SL_res.rds"))

SL_cv_MSE <- NULL
for(i in 1:length(SL_res)){
  if(!inherits(SL_res[[i]], "try-error")){
    res=data.frame(SL_res[[i]]$cv_risk_w_sl_revere)
    res$survey=names(SL_res)[i]
    SL_cv_MSE= try(bind_rows(SL_cv_MSE,res))
  }
}

calc_importance <- function(sl_fit){
  set.seed(983)
  varimp <- importance(
    fit = sl_fit,
    #eval_fun = loss_squared_error,
    eval_fun = loss_loglik_binomial,
    type = "permute",
    importance_metric="ratio"
  )

  varimp20=varimp %>% arrange(NLL_ratio) %>% tail(n=20)
  p=importance_plot(x = varimp20)
  return(list(varimp=varimp, varimp20=varimp20, p=p))
}


SL_res <- readRDS(here("results/DHS_SL_res.rds"))


varimp_list <- list()
for(i in 1:length(SL_res)){
  res=NULL
  res=try(calc_importance(SL_res[[i]]$sl_fit))

  if(!inherits(res, "try-error")){
    varimp_list[[i]] <- res
    names(varimp_list)[i] <- names(SL_res)[i]
  }
}











