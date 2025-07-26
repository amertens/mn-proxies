

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
library(future)
library(washb)
library(recipes)

plan(multicore, workers = availableCores() - 1)
options(future.globals.maxSize = 2 * 1024^3)


source(paste0(here::here(),"/src/0-functions.R"))
source(paste0(here::here(),"/src/0-SL-setup.R"))
source(paste0(here::here(),"/src/DHS/DHS_functions.R"))
source(paste0(here::here(),"/src/DHS/DHS_variable_recode.R"))

#read clean and merged data
df <- readRDS(here("data/IPD/Ghana/Ghana_merged_dataset.rds"))
#dput(colnames(df))


#vitamin A data
summary(df$gw_cRBPAdjBrinda)

gw_vars <- colnames(df)[grepl("gw_", colnames(df))]



Xvars = c(gw_vars)
#drop time/data variables
Xvars = Xvars[!(Xvars %in% c("gw_wl_wctm","gw_wl_watm","gw_birthymd","gw_fctm","gw_fatm"))]
#drop vars related to the outome
# Xvars[!(Xvars)]
#
#
# #temp
# Xvars=c("gw_cMAD", "gw_cIRF", "gw_cVARF", "gw_haz06", "gw_waz06", "gw_whz06", #"gw_wRBP",
#         "gw_cIDAdjBrinda", "gw_cIDA_Brinda", "gw_cID", "gw_cFerrAdjThurn",
#         "gw_cVADAdjSherryThurn", "gw_cVitASupp",  "gw_month", "gw_week")



df_VitA <- df %>% select(gw_cRBPAdjBrinda, gw_cnum, !!(Xvars)) %>% as.data.frame() %>% filter(!is.na(gw_cRBPAdjBrinda))
summary(df_VitA$gw_cRBPAdjBrinda)


#NOTE! Do a super simple model to check prediction of outcomes from outcome related variables


res=try(DHS_SL(d=df_VitA, outcome="gw_cRBPAdjBrinda", Xvars=Xvars, id="gw_cnum", folds=2, CV=F, sl=slfull))
saveRDS(res, file = here("results/models/res_GW_Ghana_SL_women_vitA_gwPred.rds"))

#evaluate performance
res$cv_risk_w_sl_revere
sd(df_VitA$gw_cRBPAdjBrinda)
sqrt(res$cv_risk_w_sl_revere$MSE[length(res$cv_risk_w_sl_revere$MSE)])
