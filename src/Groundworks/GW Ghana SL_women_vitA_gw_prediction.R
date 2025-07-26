

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
library(SuperLearner)
library(washb)
library(future)

# Optional: speed up with parallel processing
plan(multicore, workers = availableCores() - 1)
options(future.globals.maxSize = 2 * 1024^3)


source(paste0(here::here(),"/src/0-functions.R"))
source(paste0(here::here(),"/src/0-SL-setup.R"))
source(paste0(here::here(),"/src/DHS/DHS_functions.R"))
source(paste0(here::here(),"/src/DHS/DHS_variable_recode.R"))

#read clean and merged data
df <- readRDS(here("data/IPD/Ghana/Ghana_merged_dataset.rds"))
dput(colnames(df))


#vitamin A data
summary(df$wRBP)

gw_vars <- colnames(df)[grepl("gw_", colnames(df))]
dhs_vars <- colnames(df)[grepl("dhs_", colnames(df))]
mics_vars <- colnames(df)[grepl("mics_", colnames(df))]
ihme_vars <- colnames(df)[grepl("ihme_", colnames(df))]
lsms_vars <- colnames(df)[grepl("lsms_", colnames(df))]
map_vars <- colnames(df)[grepl("MAP_", colnames(df))]
wfp_vars <- colnames(df)[grepl("wfp_", colnames(df))]
flunet_vars <- colnames(df)[grepl("flunet_", colnames(df))]



Xvars = c(gw_vars)
#drop time/data variables
Xvars = Xvars[!(Xvars %in% c("gw_wl_wctm","gw_wl_watm","gw_birthymd"))]
Xvars


df_VitA <- df %>% select(gw_wRBP, gw_cnum, !!(Xvars)) %>% as.data.frame() %>% filter(!is.na(gw_wRBP))
summary(df_VitA$gw_wRBP)


#NOTE! Do a super simple model to check prediction of outcomes from outcome related variables


res=try(DHS_SL(d=df_VitA, outcome="gw_wRBP", Xvars=Xvars, id="gw_cnum", folds=5, CV=F, sl=slmod))
saveRDS(res, file = here("results/models/res_GW_Ghana_SL_women_vitA_gwPred.rds"))

#evaluate performance
res$cv_risk_w_sl_revere
sd(df_VitA$gw_wRBP)
sqrt(res$cv_risk_w_sl_revere$MSE[6])
