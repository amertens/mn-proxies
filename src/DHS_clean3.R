

rm(list=ls())
library(dplyr)
library(haven)
library(purrr)
source(paste0(here::here(),"/src/0-functions.R"))
source(paste0(here::here(),"/src/DHS/DHS_functions.R"))
source(paste0(here::here(),"/src/DHS/DHS_variable_recode.R"))

dfull <- readRDS(here("data/DHS/dhs_Ghana_2022.RDS"))
dfull <-dfull$Ghana_2022




HR_clean <- clean_HR(dfull$HRdata)
PR_clean <- clean_PR(dfull$PRdata)

# Merge household data into the PR data:
PR_merged <- PR_clean %>%
  left_join(HR_clean, by = c("cluster","hh"))

IR_clean <- clean_IR(dfull$IRdata)
head(IR_clean)

# Merge IR (women’s) data into PR_merged for female lines:
PR_IR_merged <- PR_merged %>%
  left_join(IR_clean, by = c("cluster","hh","line"))
head(PR_IR_merged)

colnames(PR_merged)
colnames(IR_clean)
colnames(PR_IR_merged)

table(PR_IR_merged$man_anemia_cat)
table(PR_IR_merged$anemia_cat)


#-------------------------------------------------------------------------------
# sl3
#-------------------------------------------------------------------------------

d <- PR_IR_merged %>%
  mutate(mod_sev_anemia=case_when(anemia_cat==1 | anemia_cat==2 ~ 1,
                                  anemia_cat==3 | anemia_cat==4~ 0,
                                  TRUE ~ NA)) %>%
  filter(!is.na(mod_sev_anemia))

table(d$mod_sev_anemia)
colnames(PR_IR_merged)

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

Y = d$mod_sev_anemia
#X=d %>% select(v013, v020, v024, v017, v113:v133) %>% as.data.frame()
X = d %>% select(!!Xvars) %>% as.data.frame()

# for(i in 1:ncol(X)){
#   if(!is.null(attributes(X[,i]))){
#     colnames(X)[i] <- attributes(X[,i])$label
#   }
# }
# X = X%>% as_tibble( .name_repair = "unique") %>% as.data.frame()

X=unlabelled(X, user_na_to_na = TRUE)
Y = d$mod_sev_anemia
outcome="Y"
folds=2
CV=F

#Set up SL components
lrnr_mean <- make_learner(Lrnr_mean)
lrnr_glmnet <- Lrnr_glmnet$new()
random_forest <- Lrnr_randomForest$new()
glm_fast <- Lrnr_glm_fast$new()
nnls_lrnr <- Lrnr_nnls$new()
xgboost_lrnr <- Lrnr_xgboost$new()
ranger_lrnr <- Lrnr_ranger$new()
#gbm_lrnr <- Lrnr_gbm$new()
earth_lrnr <- Lrnr_earth$new()
#dbarts_lrnr <- Lrnr_dbarts$new()
hal_lrnr <- Lrnr_hal9001$new()
#gam_lrnr <- Lrnr_gam$new()
polyspline<-Lrnr_polspline$new()
#screen_cor <- Lrnr_pkg_SuperLearner_screener$new("screen.corP")
screen_glmnet <- Lrnr_pkg_SuperLearner_screener$new("screen.glmnet")
#cor_nnls_lrnr <- make_learner(Pipeline, screen_cor, glm_fast)
#cor_glm <- make_learner(Pipeline, screen_cor, glm_fast)
#cor_spline <- make_learner(Pipeline, screen_cor, polyspline)
screened_hal <- make_learner(Pipeline, screen_glmnet, hal_lrnr)



stack <- make_learner(
  Stack,
  lrnr_mean,
  glm_fast,
  #cor_glm,
  lrnr_glmnet,
  ranger_lrnr,
  xgboost_lrnr#,
  #cor_spline#,
  #screened_hal
)

stack <- make_learner(
  Stack,
  lrnr_mean,
  lrnr_glmnet
)

metalearner <- make_learner(Lrnr_nnls)

sl <- make_learner(Lrnr_sl,
                   learners = stack,
                   metalearner = metalearner
)
slmod=sl


cov=X
#Drop near-zero variance predictors

#impute missing
cov <- cov %>%
  do(impute_missing_values(., type = "standard", add_indicators = T, prefix = "missing_")$data) %>%
  as.data.frame()
covars = colnames(cov)
nzv_cols <- nearZeroVar(cov)
dropped_covars <-  colnames(cov)[nzv_cols]
cat("Dropping for low variance: ", dropped_covars)
if(length(nzv_cols) > 0){
  cov <- cov[, -nzv_cols]
}
#not_cov <- data.frame(not_cov)
# for(i in 1:ncol(cov)){
#   if(is.factor(cov[,i])){
#     cov[,i] <- factor_to_indicators(cov[,i])
#   }
# }

cov <- data.frame(cov)
covars=colnames(cov)
dat <-cbind(Y,cov)
dat <- data.table(dat)

# #parallelize
# uname <- system("uname -a", intern = TRUE)
# os <- sub(" .*", "", uname)
# if(os=="Darwin"){
#   cpus_logical <- as.numeric(system("sysctl -n hw.logicalcpu", intern = TRUE))
# } else if(os=="Linux"){
#   cpus_logical <- system("lscpu | grep '^CPU(s)'", intern = TRUE)
#   cpus_logical <- as.numeric(gsub("^.*:[[:blank:]]*","", cpus_logical))
# } else {
#   stop("unsupported OS")
# }
#
# plan(multicore, workers=floor(cpus_logical/2))

# create the sl3 task
set.seed(12345)
SL_task <- make_sl3_Task(
  data = dat,
  covariates = covars,
  outcome = "Y",
  #id="subjid",
  folds = make_folds(dat, fold_fun = folds_vfold, V = folds)
)


##3. Fit the full model


#get cross validated fit
if(CV){
  suppressMessages(cv_sl <- make_learner(Lrnr_cv, slmod, full_fit = TRUE))
  #cv_glm <- make_learner(Lrnr_cv, glm_sl, full_fit = TRUE)
  suppressMessages(sl_fit <- cv_sl$train(SL_task))
  #glm_fit <- cv_glm$train(SL_task)
}else{
  suppressMessages(sl_fit <- slmod$train(SL_task))
  #glm_fit <- glm_sl$train(SL_task)
}

#evaluate performance
cv_risk_w_sl_revere <- sl_fit$cv_risk(
  eval_fun = loss_squared_error, get_sl_revere_risk = TRUE
)
cv_risk_w_sl_revere

#get outcome predictions
yhat_full <- sl_fit$predict_fold(SL_task,"validation")
#yhat_glm <- glm_fit$predict_fold(SL_task,"validation")





#NOTE! Takes a long time with a large number of covariates
set.seed(983)
varimp <- importance(
  fit = sl_fit, eval_fun = loss_squared_error, type = "permute"
)
varimp20=varimp %>% arrange(MSE_difference) %>% tail(n=20)
importance_plot(x = varimp20)


