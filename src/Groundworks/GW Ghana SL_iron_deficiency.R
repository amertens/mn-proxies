

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
options(future.globals.maxSize = 2 * 1024^3)


source(paste0(here::here(),"/src/0-functions.R"))
source(paste0(here::here(),"/src/0-SL-setup.R"))
source(paste0(here::here(),"/src/DHS/DHS_functions.R"))
source(paste0(here::here(),"/src/DHS/DHS_variable_recode.R"))

#read clean and merged data
df <- readRDS(here("data/IPD/Ghana/Ghana_merged_dataset.rds"))
dput(colnames(df))


#vitamin A data
summary(df$cIDAdjBrinda)

dhs_vars <- colnames(df)[grepl("dhs_", colnames(df))]
mics_vars <- colnames(df)[grepl("mics_", colnames(df))]
ihme_vars <- colnames(df)[grepl("ihme_", colnames(df))]
lsms_vars <- colnames(df)[grepl("lsms_", colnames(df))]
map_vars <- colnames(df)[grepl("MAP_", colnames(df))]
wfp_vars <- colnames(df)[grepl("wfp_", colnames(df))]
flunet_vars <- colnames(df)[grepl("flunet_", colnames(df))]



Xvars = c("month",dhs_vars, mics_vars, ihme_vars, lsms_vars, map_vars, wfp_vars, flunet_vars)


df_iron <- df %>% select(cIDAdjBrinda, cnum, !!(Xvars)) %>% as.data.frame() %>% filter(!is.na(cIDAdjBrinda))

res=try(DHS_SL(d=df_iron, outcome="cIDAdjBrinda", Xvars=Xvars, id="cnum", folds=5, CV=F, sl=sl_simple_bin))
saveRDS(res, file = here("results/models/res_GW_Ghana_SL_iron_deficiency.rds"))


#evaluate performance
res$cv_risk_w_sl_revere


#Brier skill score - for binomical outcomes
#*BSS=1→ perfect; 0→ no better than null; <0→ worse than null.*
prev=mean(df$cIDAdjBrinda)
prev
null_brier = prev*(1-prev)
BSS = 1-(res$cv_risk_w_sl_revere$MSE[length(res$cv_risk_w_sl_revere$MSE)] /null_brier)
BSS

