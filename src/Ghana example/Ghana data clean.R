

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

# #sctXX5E87xJbD*
set_rdhs_config(email = "amertens@berkeley.edu",
                project = "MN Prediction",
                data_frame = "data.table::as.data.table",
                config_path = "~/.rdhs.json",
                global = TRUE)


source(paste0(here::here(),"/src/0-functions.R"))
source(paste0(here::here(),"/src/0-SL-setup.R"))
source(paste0(here::here(),"/src/DHS/DHS_functions.R"))
source(paste0(here::here(),"/src/DHS/DHS_variable_recode.R"))

dfull <- readRDS(here("data/DHS/dhs_Ghana_2019.RDS"))[[1]]


GEE_df <- read.csv(here("data/DHS/clean/DHS_GEE_merge_GH_2019_mkts.csv")) #%>% filter(DHSYEAR==2019)
head(GEE_df)
table(GEE_df$nearest_market_id)


# GEE_df <- read.csv(here("data/DHS/clean/DHS_GEE_merge_GH.csv")) %>% filter(DHSYEAR==2019)
# head(GEE_df)

GEE_df <- GEE_df %>% select(DHSCLUST, LATNUM,   LONGNUM, ALT_GPS, trmm_mean_10km,
                            pop_mean_10km, pop_mean_25km,
                            trmm_mean_25km, pop_mean_50km,
                            trmm_mean_50km, nearest_market_distance_km, nearest_market_id)



df <- clean_DHS(dfull)
df <- df %>% select(!ends_with(".y"))
colnames(df) <- gsub("\\.x","",colnames(df))




poly.adm1 <- geodata::gadm(country="Ghana", level=1, path=tempdir())
poly.adm1 <- sf::st_as_sf(poly.adm1)
poly.adm2 <- geodata::gadm(country="Ghana", level=2, path=tempdir())
poly.adm2 <- sf::st_as_sf(poly.adm2)
try(geo <- getDHSgeo(country = "Ghana", year = 2019))

geo_df <- as.data.frame(geo) %>% select(DHSCLUST, LATNUM, LONGNUM) %>%
  rename(
    cluster = DHSCLUST,
    latitude = LATNUM,
    longitude = LONGNUM
  )
head(geo_df)

#merge in gps data
df <- df %>% left_join(geo_df, by = "cluster")


#-------------------------------------------------------------------------------
# GEE data
#-------------------------------------------------------------------------------

#merge in GEE data
head(GEE_df)

df <- df %>% left_join(GEE_df, by = c("cluster"="DHSCLUST"))

#-------------------------------------------------------------------------------
# Food price
#-------------------------------------------------------------------------------

#merge in food pricing data
wfp <- read.csv(here("data/food_price/wfp_food_prices_gha.csv"))
head(wfp)
wfp <- wfp[-1,]
head(df)

#get the average food prices the year of sampling (should this be the year before?)
table(wfp$priceflag)
table(wfp$pricetype )
wfp <- wfp %>%
  mutate(year=year(wfp$date),
         usdprice=as.numeric(usdprice),
         priceflag=factor(priceflag, levels=c("actual","actual,aggregate","aggregate")),
         pricetype = factor(pricetype, levels=c("Retail","Wholesale")),
         unit=factor(unit)) #%>%
  #filter(year==2019|year==2018)

levels(wfp$unit) <- c("KG", setdiff(levels(wfp$unit), "KG"))

ave_price <- wfp %>%
  group_by(market, commodity , currency,priceflag, pricetype, unit) %>%
  summarise(price = mean(usdprice, na.rm=T),
            sd_price=sd(usdprice, na.rm=T)) %>%
  group_by(market, commodity , currency) %>%
  arrange(market, commodity , currency, priceflag, pricetype, unit) %>% slice(1) %>%
  ungroup()
head(ave_price)


#get the price df to merge
price_df <- ave_price %>% select(market, commodity, price ) %>%
  # transform commodity to wide
  pivot_wider(names_from = commodity, values_from = price) %>%
  janitor::clean_names() %>%
  rename(nearest_market_id=market)

head(price_df)

head(df)
df <- left_join(df, price_df, by = "nearest_market_id")
table(is.na(price_df$cassava ))
table(is.na(df$cassava ))


#-------------------------------------------------------------------------------
# Malaria Atlas
#-------------------------------------------------------------------------------

pts    <- vect(geo_df,
               geom = c("longitude", "latitude"),
               crs  = "EPSG:4326")                   # WGS‑84 (same as MAP rasters)

pfpr20  <- rast("2020_GH_pfpr_mean.tif")             # replace name as needed

#202406

GHA_Accessibility_201501_Global_Travel_Speed_Friction_Surface <- rast(here("data/Malaria Atlas/GHA_Accessibility_201501_Global_Travel_Speed_Friction_Surface.tif"))
geo_df$Global_Travel_Speed_Friction_Surface <- extract(GHA_Accessibility_201501_Global_Travel_Speed_Friction_Surface, pts)[, 2]           # [,1] is the row ID terra adds

GHA_Malaria_202406_Global_Pv_Parasite_Rate  <-rast(here("data/Malaria Atlas/GHA_Malaria_202406_Global_Pv_Parasite_Rate.tif"))
geo_df$Global_Pv_Parasite_Rate <- extract(GHA_Malaria_202406_Global_Pv_Parasite_Rate, pts)[, 2]           # [,1] is the row ID terra adds

GHA_Accessibility_201501_Global_Travel_Time_to_Cities <- rast(here("data/Malaria Atlas/GHA_Accessibility_201501_Global_Travel_Time_to_Cities.tif"))
geo_df$Global_Travel_Time_to_Cities <- extract(GHA_Accessibility_201501_Global_Travel_Time_to_Cities, pts)[, 2]           # [,1] is the row ID terra adds

GHA_Accessibility_202001_Global_Motorized_Friction_Surface <- rast(here("data/Malaria Atlas/GHA_Accessibility_202001_Global_Motorized_Friction_Surface.tif"))
geo_df$Global_Motorized_Friction_Surface <- extract(GHA_Accessibility_202001_Global_Motorized_Friction_Surface, pts)[, 2]           # [,1] is the row ID terra adds

GHA_Accessibility_202001_Global_Motorized_Travel_Time_to_Healthcare <- rast(here("data/Malaria Atlas/GHA_Accessibility_202001_Global_Motorized_Travel_Time_to_Healthcare.tif"))
geo_df$Global_Motorized_Travel_Time_to_Healthcare <- extract(GHA_Accessibility_202001_Global_Motorized_Travel_Time_to_Healthcare, pts)[, 2]           # [,1] is the row ID terra adds

GHA_Accessibility_202001_Global_Walking_Only_Friction_Surface <- rast(here("data/Malaria Atlas/GHA_Accessibility_202001_Global_Walking_Only_Friction_Surface.tif"))
geo_df$Global_Walking_Only_Friction_Surface <- extract(GHA_Accessibility_202001_Global_Walking_Only_Friction_Surface, pts)[, 2]           # [,1] is the row ID terra adds

GHA_Accessibility_202001_Global_Walking_Only_Travel_Time_To_Healthcare <- rast(here("data/Malaria Atlas/GHA_Accessibility_202001_Global_Walking_Only_Travel_Time_To_Healthcare.tif"))
geo_df$Global_Walking_Only_Travel_Time_To_Healthcare <- extract(GHA_Accessibility_202001_Global_Walking_Only_Travel_Time_To_Healthcare, pts)[, 2]           # [,1] is the row ID terra adds

GHA_Blood_Disorders_201201_Africa_HbC_Allele_Frequency <- rast(here("data/Malaria Atlas/GHA_Blood_Disorders_201201_Africa_HbC_Allele_Frequency.tif"))
geo_df$Africa_HbC_Allele_Frequency <- extract(GHA_Blood_Disorders_201201_Africa_HbC_Allele_Frequency, pts)[, 2]           # [,1] is the row ID terra adds

GHA_Blood_Disorders_201201_Global_Duffy_Negativity_Phenotype_Frequency.tif <- rast(here("data/Malaria Atlas/GHA_Blood_Disorders_201201_Global_Duffy_Negativity_Phenotype_Frequency.tif"))
geo_df$Global_Duffy_Negativity_Phenotype_Frequency <- extract(GHA_Blood_Disorders_201201_Global_Duffy_Negativity_Phenotype_Frequency.tif, pts)[, 2]           # [,1] is the row ID terra adds

GHA_Blood_Disorders_201201_Global_G6PDd_Allele_Frequency <- rast(here("data/Malaria Atlas/GHA_Blood_Disorders_201201_Global_G6PDd_Allele_Frequency.tif"))
geo_df$Global_G6PDd_Allele_Frequency <- extract(GHA_Blood_Disorders_201201_Global_G6PDd_Allele_Frequency, pts)[, 2]           # [,1] is the row ID terra adds

GHA_Blood_Disorders_201201_Global_Sickle_Haemoglobin_HbS_Allele_Frequency  <- rast(here("data/Malaria Atlas/GHA_Blood_Disorders_201201_Global_Sickle_Haemoglobin_HbS_Allele_Frequency.tif"))
geo_df$Global_Sickle_Haemoglobin_HbS_Allele_Frequency <- extract(GHA_Blood_Disorders_201201_Global_Sickle_Haemoglobin_HbS_Allele_Frequency, pts)[, 2]           # [,1] is the row ID terra adds

GHA_Interventions_202106_Africa_Insecticide_Treated_Net_Access <- rast(here("data/Malaria Atlas/GHA_Interventions_202406_Africa_Insecticide_Treated_Net_Access.tif"))
geo_df$Africa_Insecticide_Treated_Net_Access <- extract(GHA_Interventions_202106_Africa_Insecticide_Treated_Net_Access, pts)[, 2]           # [,1] is the row ID terra adds

GHA_Interventions_202106_Africa_Insecticide_Treated_Net_Use_Rate <- rast(here("data/Malaria Atlas/GHA_Interventions_202406_Africa_Insecticide_Treated_Net_Use_Rate.tif"))
geo_df$Africa_Insecticide_Treated_Net_Use_Rate <- extract(GHA_Interventions_202106_Africa_Insecticide_Treated_Net_Use_Rate, pts)[, 2]           # [,1] is the row ID terra adds

GHA_Interventions_202106_Africa_IRS_Coverage  <- rast(here("data/Malaria Atlas/GHA_Interventions_202106_Africa_IRS_Coverage.tif"))
geo_df$Africa_IRS_Coverage <- extract(GHA_Interventions_202106_Africa_IRS_Coverage, pts)[, 2]           # [,1] is the row ID terra adds

GHA_Interventions_202106_Global_Antimalarial_Effective_Treatment  <- rast(here("data/Malaria Atlas/GHA_Interventions_202406_Global_Antimalarial_Effective_Treatment.tif"))
geo_df$Global_Antimalarial_Effective_Treatment <- extract(GHA_Interventions_202106_Global_Antimalarial_Effective_Treatment, pts)[, 2]           # [,1] is the row ID terra adds

GHA_Malaria_202202_Global_Pf_Reproductive_Number <- rast(here("data/Malaria Atlas/GHA_Malaria_202202_Global_Pf_Reproductive_Number.tif"))
geo_df$Global_Pf_Reproductive_Number <- extract(GHA_Malaria_202202_Global_Pf_Reproductive_Number, pts)[, 2]           # [,1] is the row ID terra adds

GHA_Malaria_202406_Global_Pf_Incidence_Count <- rast(here("data/Malaria Atlas/GHA_Malaria_202406_Global_Pf_Incidence_Count.tif"))
geo_df$Global_Pf_Incidence_Count <- extract(GHA_Malaria_202406_Global_Pf_Incidence_Count, pts)[, 2]           # [,1] is the row ID terra adds

GHA_Malaria_202406_Global_Pf_Incidence_Rate <- rast(here("data/Malaria Atlas/GHA_Malaria_202406_Global_Pf_Incidence_Rate.tif"))
geo_df$Global_Pf_Incidence_Rate <- extract(GHA_Malaria_202406_Global_Pf_Incidence_Rate, pts)[, 2]           # [,1] is the row ID terra adds

GHA_Malaria_202406_Global_Pf_Mortality_Count <- rast(here("data/Malaria Atlas/GHA_Malaria_202406_Global_Pf_Mortality_Count.tif"))
geo_df$Global_Pf_Mortality_Count <- extract(GHA_Malaria_202406_Global_Pf_Mortality_Count, pts)[, 2]           # [,1] is the row ID terra adds

GHA_Malaria_202406_Global_Pf_Mortality_Rate <- rast(here("data/Malaria Atlas/GHA_Malaria_202406_Global_Pf_Mortality_Rate.tif"))
geo_df$Global_Pf_Mortality_Rate <- extract(GHA_Malaria_202406_Global_Pf_Mortality_Rate, pts)[, 2]           # [,1] is the row ID terra adds

GHA_Malaria_202406_Global_Pf_Parasite_Rate. <- rast(here("data/Malaria Atlas/GHA_Malaria_202406_Global_Pf_Parasite_Rate.tif"))
geo_df$Global_Pf_Parasite_Rate <- extract(GHA_Malaria_202406_Global_Pf_Parasite_Rate., pts)[, 2]           # [,1] is the row ID terra adds

GHA_Malaria_202406_Global_Pv_Incidence_Count <- rast(here("data/Malaria Atlas/GHA_Malaria_202406_Global_Pv_Incidence_Count.tif"))
geo_df$Global_Pv_Incidence_Count <- extract(GHA_Malaria_202406_Global_Pv_Incidence_Count, pts)[, 2]           # [,1] is the row ID terra adds

GHA_Malaria_202406_Global_Pv_Incidence_Rate <- rast(here("data/Malaria Atlas/GHA_Malaria_202406_Global_Pv_Incidence_Rate.tif"))
geo_df$Global_Pv_Incidence_Rate <- extract(GHA_Malaria_202406_Global_Pv_Incidence_Rate, pts)[, 2]           # [,1] is the row ID terra adds


#merge back in with the main df

df <- df %>% left_join(geo_df, by = c("cluster","latitude","longitude"))


saveRDS(df, here::here("data/DHS/clean/dhs_Ghana_2019_gee_fp_map_merge.RDS"))
