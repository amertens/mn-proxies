

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
library(terra)
library(caret)

#load data
# "C:\Users\andre\OneDrive\Documents\mn-proxies\data\IPD\Ghana\Ghana GMS GPS points_170403.xlsx"
# "C:\Users\andre\OneDrive\Documents\mn-proxies\data\IPD\Ghana\GMS_WomenAllData_DavisBerkeley.dta"
# "C:\Users\andre\OneDrive\Documents\mn-proxies\data\IPD\Ghana\GMS_ChildrenAllData_DavisBerkeley.dta"

#note lat and long is flipped in the raw data
gps <- readxl::read_xlsx(here("data", "IPD", "Ghana", "Ghana GMS GPS points_170403.xlsx")) %>%
  mutate(
    latitude = as.numeric(lon),
    longitude = as.numeric(lat)
  ) %>% subset(., select = -c(lon, lat, `...12`, Team)) %>%
  rename(cnum=SR)
head(gps)

#save cleaned gps data
write.csv(gps, file=here("data", "IPD", "Ghana", "Ghana_GMS_GPS_cleaned.csv"))


# df_women <- read_dta(here("data", "IPD", "Ghana", "GMS_WomenAllData_DavisBerkeley.dta"))
# df_children <- read_dta(here("data", "IPD", "Ghana", "GMS_ChildrenAllData_DavisBerkeley.dta"))

df <- read_dta(here("data", "IPD", "Ghana", "GMS_Women+Children_DavisBerkeley.dta"))
table(df$cnum)

df <- left_join(df, gps, by = "cnum")

head(df)

#check and drop near zero variance columns
colnames(df[, nzv(df)])
dput(colnames(df[, nzv(df)]))


#drop unneeded variables
df <- df %>% subset(., select = -c(l010, l011, clnc, cl003, cbalbl001, cbacfa,
                                   cbbcl006, cbbcr_o, cl007, cdzlbl002, cdzcl008, cdzcors,
                                   cegenerated_table_list_label_78, celbl003, cel009, cepwt,
                                   cecol, coln, cglbl004, cglbl006, cglbl007, cglbl008,
                                   cglbl009, cglbl010, cglbl011, cglbl012, cglbl013, cglbl014,
                                   cfdggenerated_table_list_label_1, cfdgchg7, cfdgchg16,
                                   cfdgchg18, ofds, csf, l015, dnp, cil016, cirutf,
                                   ciifi, cimnp, cjlbl005, cjvta_dt, ccom, bcic, cd,
                                   ecepf, cres, res_o, fl022, gcl023, gcl024, gctbf,
                                   hcanref, hcsamref, cl025, cl026, cmob, wFoodGrp1,
                                   wAnemiaSev, wVAD, wVADAdjThurn, diffage_bioform_quest,
                                   cDiarrheaBlood, cLRI, cBFContinued, cSemiSolid, cOverweightYN,
                                   cAnemiaSev, `_logcrpcoeffRBP`, `_logagpcoeffRBP`, `_logcrpcoeffRBPMRDR`,
                                   `_logagpcoeffRBPMRDR`, cVADAdjBrinda05, VAS_m, INT_y, cVAS_valid,
                                   cWasteSev, cUnderSev, a_yr, wlnc, wl003, pgci, lbl001,
                                   wkg_yr, wrl, woj, wd_wsk, wd_wcs, wd_pgl, wd_va_dt,
                                   we_wl004, we_wl005, we_wl006, we_wl007, we_wl008, we_wl009,
                                   we_wl010, we_wl011, we_wl012, we_wl013, wfdg_generated_table_list_label_,
                                   wfdg_reserved_name_for_field_lis, wfdg_fg1, wfdg_fg15,
                                   wfdg_fg18, wfdg_fg23, wf, wg_generated_table_list_label_10,
                                   wg_lbl003, wg_reserved_name_for_field_list_, wg_fo2, fo_o,
                                   wh_generated_table_list_label_11, wh_lbl004, wh_reserved_name_for_field_list_,
                                   wh_ff7, ff_o, wi_lbl002, lbl005, wfr, wfro, wcom,
                                   hTreatDrinkWaterYN, b_icf, wrcs, whr_o, wl_wl012, wo_wran,
                                   wl013, wl014, wl015, wmob, wFvoMort, wFFOther, wSmoke,
                                   wSevUnderNut, wModUnderNut, wMUACUnderNut, intyear ))


head(df)

df  <- df %>% subset(., select = -c(start, end, deviceid, tn))


#rename variables
df <- df %>% rename(month=amon, year=ayr, hhid=cachnc, hh_label=cachln, childid=caclnr, child_label=cacln, agem=caam1, child_weight=cbacwb, diar14d=cbbcdo, fever14d=cbbcfv)


saveRDS(df, file=here("data", "IPD", "Ghana", "Ghana_GMS_cleaned.rds"))

