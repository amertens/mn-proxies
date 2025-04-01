

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


clean_DHS <- function(dfull){
  HR_clean <- clean_HR(dfull$HRdata)
  PR_clean <- clean_PR(dfull$PRdata)

  # Merge household data into the PR data:
  PR_merged <- PR_clean %>%
    left_join(HR_clean, by = c("cluster","hh"))

  IR_clean <- clean_IR(dfull$IRdata)

  # Merge IR (women’s) data into PR_merged for female lines:
  PR_IR_merged <- PR_merged %>%
    left_join(IR_clean, by = c("cluster","hh","line"))
  head(PR_IR_merged)

  table(PR_IR_merged$anemia_cat)

  d <- PR_IR_merged %>%
    mutate(mod_sev_anemia=case_when(anemia_cat==1 | anemia_cat==2 ~ 1,
                                    anemia_cat==3 | anemia_cat==4~ 0,
                                    TRUE ~ NA)) %>%
    filter(!is.na(mod_sev_anemia))

  if(nrow(d)==0){
    cat("No anemia data\n")
  }else{
    return(d)
  }

}



#dfull <- readRDS(here("data/DHS/dhs_Ethiopia_2019.RDS"))
#dfull <- readRDS(here("data/DHS/dhs_Cameroon_2022.RDS"))
dfull <- readRDS(here("data/DHS/dhs_Cameroon_2018.RDS"))
dfull <-dfull[[1]]
temp <- dfull$PRdata
vTemp = makeVlist(temp)
vTemp$name[grepl("anemia", vTemp$label)]
vTemp$label[grepl("anemia", vTemp$label)]
summary(temp$ha57)
summary(temp$ha58)
summary(temp$hc57)
summary(temp$hc58)

d <- clean_DHS(dfull)
head(d)






res=DHS_SL(d=d, Xvars=Xvars)


#NOTE! Takes a long time with a large number of covariates
set.seed(983)
varimp <- importance(
  fit = res$sl_fit,
  eval_fun = loss_squared_error,
  #eval_fun = loss_loglik_binomial,
  type = "permute",
  importance_metric="ratio"
)

varimp20=varimp %>% arrange(MSE_difference) %>% tail(n=20)
importance_plot(x = varimp20)









