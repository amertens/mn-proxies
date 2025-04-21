

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

#res=try(DHS_SL(d=df, Xvars=Xvars))


# res_cluster = DHS_cluster_SL(
#   d            = df,
#   Xvars        = Xvars,
#   outcome      = "mod_sev_anemia",
#   cluster_var  = "cluster",
#   weight_var   = "svy_weight",
#   sl           = slmod,
#   V            = 5
# )


# =============================================================
#  DHS_cluster_SL(): SuperLearner with cluster‑indexed CV
#  ------------------------------------------------------------
#  • Uses the same learner stack (object `slmod`) you already set up
#  • Adds survey weights (if available)            –>  representative predictions
#  • Aggregates individual predictions to clusters –>  prevalence
# =============================================================
library(future)
options(future.globals.maxSize = 2 * 1024^3)   # 2 GB

# plan(multisession)
# boot_mat <- future_sapply(seq_len(B), \(b) { ... })
# plan(sequential)

  d            = df
  Xvars        = Xvars
  outcome      = "mod_sev_anemia"
  cluster_var  = "cluster"
  weight_var   = "svy_weight"
  sl           = slmod
  V            = 5

# DHS_cluster_SL <- function(d,
#                            Xvars,
#                            outcome      = "mod_sev_anemia",
#                            cluster_var  = "cluster",
#                            weight_var   = NULL,          # e.g. "wt" once you add it
#                            V            = 10,
#                            sl) {

  # ---------- 1. outcome, covariates, weights ---------------------------------
  Y      <- d[[outcome]]
  X = d %>% select(!!Xvars) %>% as.data.frame()
  cov=unlabelled(X, user_na_to_na = TRUE)

  cov <- cov %>%
    do(impute_missing_values(., type = "standard", add_indicators = T, prefix = "missing_")$data) %>%
    as.data.frame()

  # drop near‑zero‑variance columns
  nzv <- caret::nearZeroVar(cov)
  if (length(nzv) > 0){
    cov <- cov[ , -nzv]
  }

  # weights?
  if (!is.null(weight_var)) {
    wts <- scale(d[[weight_var]]) #might reduce memory
  } else {
    wts <- rep(1, nrow(d))
  }

  # ---------- 2.  build sl3 task ----------------------------------------------
  task <- sl3::sl3_Task$new(
    data        = data.frame(Y = Y,cov,
                             cluster= d[[cluster_var]],
                             wt       = wts),
    covariates  = names(cov),
    outcome     = "Y",
    id          = cluster_var,
    weights     = "wt",
    folds       = origami::make_folds(
      cluster_ids = d[[cluster_var]],
      V           = V
    )
  )

  # ---------- 3.  fit ----------------------------------------------------------
  set.seed(12345)
  sl_fit <- sl$train(task)



  # ---------- 4.  individual predictions & aggregation ------------------------
  d$pred_prob <- sl_fit$predict()

  cluster_prev <- d |>
    dplyr::group_by(.data[[cluster_var]]) |>
    dplyr::summarise(n_women   = dplyr::n(),
                     obs_prev  = mean(.data[[outcome]], na.rm = TRUE),
                     pred_prev = mean(pred_prob,        na.rm = TRUE),
                     .groups   = "drop")

  # return both
  res_cluster= list(sl_fit       = sl_fit,
                    task=task,
       ind_pred     = dplyr::select(d, all_of(cluster_var), pred_prob),
       Xvars=task$column_names,
       cluster_pred = cluster_prev)



#save model
saveRDS(res_cluster, here("results//models/DHS_Ghana_SL_cluster_model.RDS"))


#evaluate performance
cv_risk_w_sl_revere <- res_cluster$sl_fit$cv_risk(
  eval_fun = loss_squared_error, get_sl_revere_risk = TRUE
)


#get outcome predictions
yhat_full <- sl_fit$predict_fold(res_cluster$task,"validation")






# to do: have missing variables together

res_vim=try(calc_importance(res_cluster$sl_fit))
res_vim

res_vim_diff=try(calc_importance(res_cluster$sl_fit,  eval.fun = loss_squared_error, importance.metric="difference"))
res_vim_diff

#save variable importance results
saveRDS(res_vim_diff, here("results//models/DHS_Ghana_SL_cluster_vim_diff.RDS"))
saveRDS(res_vim, here("results//models/DHS_Ghana_SL_cluster_vim.RDS"))

demographics=c("sex", "age","age_single", "age_5yrgrp", "rel_to_head", "usual_resident", "slept_last_night","religion", "ethnicity", "dv_selected")
education =  c( "edu_level", "edu_years")
household = c("residence","residence_type","hh_size", "num_under5","floor_material", "wall_material", "roof_material",
              "water_source", "toilet_type", "share_toilet", "cooking_fuel")
wealth = c("wealth_quint", "wealth_score", "has_electricity",
           "has_radio", "has_tv", "has_fridge", "has_bicycle", "has_motorcycle",
           "has_car","has_mobile", "has_computer")

geography = c("ALT_GPS", "region","altitude", "trmm_mean_10km","trmm_mean_25km", "trmm_mean_50km")

pregnancy_parity = c("pregnant_now", "months_preg", "months_last_bir",
                     "total_living_ch", "living_ch_plus")


food_price=c("cassava", "cowpeas", "eggplants", "eggs", "fish_mackerel_fresh",
             "gari", "maize", "maize_yellow", "meat_chicken", "meat_chicken_local",
             "millet", "onions", "peppers_dried", "peppers_fresh", "plantains_apem",
             "plantains_apentu", "rice_imported", "rice_local", "sorghum",
             "soybeans", "tomatoes_local", "tomatoes_navrongo", "yam", "yam_puna",
             "cowpeas_white")


ruralness = c("Global_Travel_Speed_Friction_Surface", "Global_Pv_Parasite_Rate",
              "Global_Travel_Time_to_Cities", "Global_Motorized_Friction_Surface",
              "Global_Motorized_Travel_Time_to_Healthcare", "Global_Walking_Only_Friction_Surface",
              "Global_Walking_Only_Travel_Time_To_Healthcare",
              "pop_mean_10km", "pop_mean_25km",  "pop_mean_50km",
              "nearest_market_distance_km", "nearest_market_id")

malaria_burden =c("Africa_HbC_Allele_Frequency","Global_Duffy_Negativity_Phenotype_Frequency", "Global_G6PDd_Allele_Frequency",
                  "Global_Sickle_Haemoglobin_HbS_Allele_Frequency", "Global_Pf_Reproductive_Number",
                  "Global_Pf_Incidence_Count", "Global_Pf_Incidence_Rate", "Global_Pf_Mortality_Count",
                  "Global_Pf_Mortality_Rate", "Global_Pf_Parasite_Rate", "Global_Pv_Incidence_Count",
                  "Global_Pv_Incidence_Rate")

malaria_treatment =  c( "has_mosquito_net", "used_bednet","Africa_Insecticide_Treated_Net_Access",
                       "Africa_Insecticide_Treated_Net_Use_Rate", "Africa_IRS_Coverage",
                       "Global_Antimalarial_Effective_Treatment")
missing_vars= res_cluster$Xvars[grepl("missing_",res_cluster$Xvars)]

res_vim_diff_groups=try(calc_importance(res_cluster$sl_fit,  eval.fun = loss_squared_error, importance.metric="difference",
                                        covariate.groups=list("demographics" = demographics[demographics %in% res_cluster$Xvars],
                                                              "education" =education[education %in% res_cluster$Xvars],
                                                              "household" = household[household %in% res_cluster$Xvars],
                                                              "wealth" = wealth[wealth %in% res_cluster$Xvars],
                                                              "geography" = geography[geography %in% res_cluster$Xvars],
                                                              "pregnancy_parity" = pregnancy_parity[pregnancy_parity %in% res_cluster$Xvars],
                                                              "food_price" = food_price[food_price %in% res_cluster$Xvars],
                                                              "ruralness" = ruralness[ruralness %in% res_cluster$Xvars],
                                                              "malaria_burden" = malaria_burden[malaria_burden %in% res_cluster$Xvars],
                                                              "malaria_treatment" = malaria_treatment[malaria_treatment %in% res_cluster$Xvars],
                                                              "missingness" =missing_vars
                                        )))
res_vim_diff_groups




res_vim_groups=try(calc_importance(res_cluster$sl_fit,
                                        covariate.groups=list("demographics" = demographics[demographics %in% res_cluster$Xvars],
                                                              "education" =education[education %in% res_cluster$Xvars],
                                                              "household" = household[household %in% res_cluster$Xvars],
                                                              "wealth" = wealth[wealth %in% res_cluster$Xvars],
                                                              "geography" = geography[geography %in% res_cluster$Xvars],
                                                              "pregnancy_parity" = pregnancy_parity[pregnancy_parity %in% res_cluster$Xvars],
                                                              "food_price" = food_price[food_price %in% res_cluster$Xvars],
                                                              "ruralness" = ruralness[ruralness %in% res_cluster$Xvars],
                                                              "malaria_burden" = malaria_burden[malaria_burden %in% res_cluster$Xvars],
                                                              "malaria_treatment" = malaria_treatment[malaria_treatment %in% res_cluster$Xvars],
                                                              "missingness" =missing_vars
                                        )))
res_vim_groups


saveRDS(res_vim_diff_groups, here("results//models/DHS_Ghana_SL_cluster_vim_diff.RDS"))
saveRDS(res_vim_groups, here("results//models/DHS_Ghana_SL_cluster_vim.RDS"))
