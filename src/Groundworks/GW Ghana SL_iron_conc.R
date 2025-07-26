

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
options(future.globals.maxSize = 4 * 1024^3)


source(paste0(here::here(),"/src/0-functions.R"))
source(paste0(here::here(),"/src/0-SL-setup.R"))
source(paste0(here::here(),"/src/DHS/DHS_functions.R"))
source(paste0(here::here(),"/src/DHS/DHS_variable_recode.R"))

#read clean and merged data
df <- readRDS(here("data/IPD/Ghana/Ghana_merged_dataset.rds"))
dput(colnames(df))


#vitamin A data
summary(df$r_crpagp_sf2)

dhs_vars <- colnames(df)[grepl("dhs_", colnames(df))]
mics_vars <- colnames(df)[grepl("mics_", colnames(df))]
ihme_vars <- colnames(df)[grepl("ihme_", colnames(df))]
lsms_vars <- colnames(df)[grepl("lsms_", colnames(df))]
map_vars <- colnames(df)[grepl("MAP_", colnames(df))]
wfp_vars <- colnames(df)[grepl("wfp_", colnames(df))]
flunet_vars <- colnames(df)[grepl("flunet_", colnames(df))]



Xvars = c("month",dhs_vars, mics_vars, ihme_vars, lsms_vars, map_vars, wfp_vars, flunet_vars)


df_iron <- df %>% select(r_crpagp_sf2, cnum, !!(Xvars)) %>% as.data.frame() %>% filter(!is.na(r_crpagp_sf2))

res=try(DHS_SL(d=df_iron, outcome="r_crpagp_sf2", Xvars=Xvars, id="cnum", folds=5, CV=F, sl=slmod))
saveRDS(res, file = here("results/models/res_GW_Ghana_SL_iron_conc.rds"))


#evaluate performance
res$cv_risk_w_sl_revere
sd(df_iron$r_crpagp_sf2, na.rm = TRUE)
sqrt(550.5974)
