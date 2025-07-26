

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
summary(df$cRBPAdjBrinda)

dhs_vars <- colnames(df)[grepl("dhs_", colnames(df))]
mics_vars <- colnames(df)[grepl("mics_", colnames(df))]
ihme_vars <- colnames(df)[grepl("ihme_", colnames(df))]
lsms_vars <- colnames(df)[grepl("lsms_", colnames(df))]
map_vars <- colnames(df)[grepl("MAP_", colnames(df))]
wfp_vars <- colnames(df)[grepl("wfp_", colnames(df))]
flunet_vars <- colnames(df)[grepl("flunet_", colnames(df))]



Xvars = c("month",dhs_vars, mics_vars, ihme_vars, lsms_vars, map_vars, wfp_vars, flunet_vars)


df_VitA <- df %>% select(cRBPAdjBrinda, cnum, !!(Xvars)) %>% as.data.frame() %>% filter(!is.na(cRBPAdjBrinda))
summary(df_VitA$cRBPAdjBrinda)


d=df_VitA
outcome="cRBPAdjBrinda"
id="cnum"

X = d %>% select(!!Xvars) %>% as.data.frame()
cov=unlabelled(X, user_na_to_na = TRUE)
Y = d[[outcome]]
id = d[[id]]

#TEMP!!!
#cov<- cov[,1:200]

# Drop columns that are entirely NA
cov <- cov[, !sapply(cov, function(x) all(is.na(x)))]

#drop columns with no variation
cov <- cov %>%
  select(where(~{
    non_na <- .x[!is.na(.x)]
    length(non_na) > 0 && length(unique(non_na)) > 1
  }))


# drop near zero variation columns
nzv_cols <- nzv(cov)
dropped_covars <-  colnames(cov)[nzv_cols]
cat("Dropping for low variance: ", dropped_covars)
if(length(nzv_cols) > 0){
  cov <- cov[, -nzv_cols]
}


#impute missing and Drop near-zero variance predictors
cov <- cov %>%
  do(impute_missing_values(., type = "standard", add_indicators = T, prefix = "missing_")$data) %>%
  as.data.frame()
covars = colnames(cov)
table(is.na(cov))

cov <- data.frame(cov)
covars=colnames(cov)
dat <-cbind(Y,cov)
dat <- data.table(dat)

head(dat)

#X= model.matrix(cov)

X=cov#[,1:10]

listWrappers()
res_glmnet = screen.glmnet(X=as.matrix(X), Y = as.numeric(Y), family = gaussian())
screened_vars_glmnet = colnames(X)[res_glmnet]

res_corP = screen.corP(X=as.matrix(design_matrix(X)), Y = as.numeric(Y), family = gaussian())
screened_vars_corP = colnames(X)[res_corP]

res_randomForest= screen.randomForest(X=as.matrix(X), Y = as.numeric(Y), family = gaussian())
screened_vars_RF = colnames(X)[res_randomForest]


length(screened_vars_glmnet)
length(screened_vars_corP)
length(screened_vars_RF)
length(colnames(X))


