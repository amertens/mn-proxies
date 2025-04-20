

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


dfull <- readRDS(here("data/DHS/dhs_Ghana_2022.RDS"))[[1]]
head(d)


df <- clean_DHS(dfull)
df <- df %>% select(!ends_with(".y"))
colnames(df) <- gsub("\\.x","",colnames(df))



# #sctXX5E87xJbD*
set_rdhs_config(email = "amertens@berkeley.edu",
                project = "MN Prediction",
                data_frame = "data.table::as.data.table",
                config_path = "~/.rdhs.json",
                global = TRUE)

poly.adm1 <- geodata::gadm(country="Ghana", level=1, path=tempdir())
poly.adm1 <- sf::st_as_sf(poly.adm1)
poly.adm2 <- geodata::gadm(country="Ghana", level=2, path=tempdir())
poly.adm2 <- sf::st_as_sf(poly.adm2)
try(geo <- getDHSgeo(country = "Ghana", year = 2022))

geo_df <- as.data.frame(geo) %>% select(DHSCLUST, LATNUM, LONGNUM) %>%
  rename(
    cluster = DHSCLUST,
    latitude = LATNUM,
    longitude = LONGNUM
  )
head(geo_df)

#merge in gps data
df <- df %>% left_join(geo_df, by = "cluster")

#save clean data
saveRDS(df,here("data/DHS/clean/dhs_Ghana_2022_clean.RDS"))

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


Xvars= c("sex", "age", "rel_to_head", "edu_level",
         "edu_years", "usual_resident", "slept_last_night", "used_bednet",
         "region",
         "residence_type", "hh_size", "num_under5", "wealth_quint",
         "wealth_score", "has_electricity", "has_radio", "has_tv", "has_fridge",
         "has_bicycle", "has_motorcycle", "has_car", "floor_material",
         "wall_material", "roof_material", "water_source", "toilet_type",
         "share_toilet", "cooking_fuel", "has_mosquito_net", "altitude",
         "has_mobile", "has_computer", "age_single", "age_5yrgrp",
         "religion", "ethnicity", "residence","marital_st",
         "partner_edu", "partner_age", "partner_occ", "respond_work",
         "pregnant_now", "months_preg", "time_sincempl", "total_living_ch",
         "living_ch_plus", "months_last_bir", "age_first_union", "age_first_sex",
         "future_fert_pref", "wanted_more_ch", "ideal_num_ch", "bmi_times100",
         "bf_current", "dv_selected", "dv_ever_phys",
         "dv_injury", "hiv_time_test", "hiv_place_test")

table(df$mod_sev_anemia)
table(df$months_preg )
prop.table(table(df$mod_sev_anemia))*100

  #res=try(DHS_SL(d=df, Xvars=Xvars))


  res_cluster = DHS_cluster_SL(
    d            = df,
    Xvars        = Xvars,
    outcome      = "mod_sev_anemia",
    cluster_var  = "cluster",
    weight_var   = "svy_weight",     # comment this out if you’d rather skip weights
    V            = 10
  )


  res$cv_risk_w_sl_revere

calc_importance <- function(sl_fit, eval.fun = loss_loglik_binomial, importance.metric="ratio", n_vars=20){
  set.seed(983)
  varimp <- importance(
    fit = sl_fit,
    eval_fun = eval.fun,
    type = "permute",
    importance_metric=importance.metric
  )

  varimp20=varimp %>% arrange(.[,2]) %>% tail(n=n_vars)
  p=importance_plot(x = varimp20)
  return(list(varimp=varimp, varimp20=varimp20, p=p))
}




# to do: have missing variables together


  res_vim=try(calc_importance(res$sl_fit))
  res_vim

  res_vim_diff=try(calc_importance(res$sl_fit,  eval.fun = loss_squared_error, importance.metric="difference"))
  res_vim_diff













