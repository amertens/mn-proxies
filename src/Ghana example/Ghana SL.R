

rm(list=ls())
library(dplyr)
library(tidyverse)
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
library(rdhs)
library(maps)


source(paste0(here::here(),"/src/0-functions.R"))
source(paste0(here::here(),"/src/0-SL-setup.R"))
source(paste0(here::here(),"/src/DHS/DHS_functions.R"))
source(paste0(here::here(),"/src/DHS/DHS_variable_recode.R"))

#read clean and merged data
df <- readRDS(here("data/DHS/clean/dhs_Ghana_2019_gee_fp_map_merge.RDS"))
dput(colnames(df))


# # Get Ghana map data
# ghana_map <- map_data("world") %>%
#   filter(region == "Ghana")
#
# unique_locations <- df %>%
#   select(market, latitude, longitude) %>%
#   mutate(latitude=as.numeric(latitude),
#          longitude=as.numeric(longitude)) %>%
#   distinct()
#
#
# # Create the plot
# ggplot() +
#   # Add Ghana map as background
#   geom_polygon(data = ghana_map, aes(x = long, y = lat, group = group),
#                fill = "lightgray", color = "darkgray") +
#   # Add the unique GPS points
#   geom_point(data = geo_df,
#              aes(x = longitude, y = latitude),
#              size = 2, alpha = 0.5) +
#   # Set appropriate theme and labels
#   theme_minimal() +
#   labs(title = "DHS Locations in Ghana",
#        x = "Longitude", y = "Latitude",
#        color = "Market") +
#   # Set appropriate map bounds for Ghana
#   coord_map(xlim = c(-3.5, 1.5), ylim = c(4.5, 11.5))


Xvars = c("sex", "age", "rel_to_head", "edu_level", "edu_years", "usual_resident",
  "slept_last_night", "used_bednet", "region", "residence_type",
  "hh_size", "num_under5", "wealth_quint", "wealth_score", "has_electricity",
  "has_radio", "has_tv", "has_fridge", "has_bicycle", "has_motorcycle",
  "has_car", "floor_material", "wall_material", "roof_material",
  "water_source", "toilet_type", "share_toilet", "cooking_fuel",
  "has_mosquito_net", "altitude", "has_mobile", "has_computer",
  "age_single", "age_5yrgrp", "religion", "ethnicity", "residence",
  "pregnant_now", "months_preg", "total_living_ch", "living_ch_plus",
  "months_last_bir", "dv_selected",  "ALT_GPS", "trmm_mean_10km",
  "pop_mean_10km", "pop_mean_25km", "trmm_mean_25km", "pop_mean_50km",
  "trmm_mean_50km", "nearest_market_distance_km", "nearest_market_id",
  "cassava", "cowpeas", "eggplants", "eggs", "fish_mackerel_fresh",
  "gari", "maize", "maize_yellow", "meat_chicken", "meat_chicken_local",
  "millet", "onions", "peppers_dried", "peppers_fresh", "plantains_apem",
  "plantains_apentu", "rice_imported", "rice_local", "sorghum",
  "soybeans", "tomatoes_local", "tomatoes_navrongo", "yam", "yam_puna",
  "cowpeas_white","Global_Travel_Speed_Friction_Surface", "Global_Pv_Parasite_Rate",
  "Global_Travel_Time_to_Cities", "Global_Motorized_Friction_Surface",
  "Global_Motorized_Travel_Time_to_Healthcare", "Global_Walking_Only_Friction_Surface",
  "Global_Walking_Only_Travel_Time_To_Healthcare", "Africa_HbC_Allele_Frequency",
  "Global_Duffy_Negativity_Phenotype_Frequency", "Global_G6PDd_Allele_Frequency",
  "Global_Sickle_Haemoglobin_HbS_Allele_Frequency", "Africa_Insecticide_Treated_Net_Access",
  "Africa_Insecticide_Treated_Net_Use_Rate", "Africa_IRS_Coverage",
  "Global_Antimalarial_Effective_Treatment", "Global_Pf_Reproductive_Number",
  "Global_Pf_Incidence_Count", "Global_Pf_Incidence_Rate", "Global_Pf_Mortality_Count",
  "Global_Pf_Mortality_Rate", "Global_Pf_Parasite_Rate", "Global_Pv_Incidence_Count",
  "Global_Pv_Incidence_Rate")


table(df$mod_sev_anemia)
table(df$months_preg )
prop.table(table(df$mod_sev_anemia))*100

res=try(DHS_SL(d=df, Xvars=Xvars))


res_cluster = DHS_cluster_SL(
  d            = df,
  Xvars        = Xvars,
  outcome      = "mod_sev_anemia",
  cluster_var  = "cluster",
  weight_var   = "svy_weight",     # comment this out if you’d rather skip weights
  V            = 10
)


res$cv_risk_w_sl_revere
res_cluster$cv_risk_w_sl_revere




# to do: have missing variables together


res_vim=try(calc_importance(res$sl_fit))
res_vim

res_vim_diff=try(calc_importance(res$sl_fit,  eval.fun = loss_squared_error, importance.metric="difference"))
res_vim_diff

res_vim_diff$varimp



#to do: group all missingness variables,
# group all food pricing variables
# all asset variables
#combine wealth variables (wealth score, quintile, and assets)

#"months last bir" = interval since last birth

#use DHS_variable_recode to relable variables




#save models
saveRDS(res, here("results/DHS_Ghana_SL_model.RDS"))
saveRDS(res_vim_diff, here("results/DHS_Ghana_SL_vim.RDS"))





