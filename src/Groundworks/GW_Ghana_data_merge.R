

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
library(sf)
library(terra)
library(caret)



d <- readRDS(here("data", "IPD", "Ghana", "Ghana_GMS_cleaned.rds"))

table(d$year)

summary(d$cRBPAdjBrinda)
table(is.na(d$cRBPAdjBrinda))


#merged_proxies <- readRDS(here::here("data/DHS/clean/dhs_Ghana_2019_gee_fp_map_merge.RDS"))

#-------------------------------------------------------------------------------
#  Admin 1 and 2 membership
#-------------------------------------------------------------------------------

summary(d$latitude)
summary(d$longitude)
d$lat= as.numeric(d$latitude)
d$lon= as.numeric(d$longitude)

poly.adm <- geodata::gadm(country="GH", level=2, path=tempdir())
poly.adm <- sf::st_as_sf(poly.adm) %>% select(NAME_1, NAME_2) %>% rename(Admin1 = NAME_1, Admin2 = NAME_2)
d_sf <- st_as_sf(d, coords = c("longitude","latitude"), crs = 4326)
poly.adm <- st_transform(poly.adm, crs = 4326)
#df <- as.data.frame(st_join(d_sf, poly.adm, join = st_within))
df <- (st_join(d_sf, poly.adm, join = st_within))

table(df$Admin1)
table(df$Admin2)


#-------------------------------------------------------------------------------
# Food price
#-------------------------------------------------------------------------------

wfp <- read.csv(here("data/food_price/wfp_food_prices_gha.csv"))
head(wfp)
wfp <- wfp[-1,]


#subset to keep: market name, lat, long. Also only keep one row from long format
markets_subset <- wfp %>% #drop top row, variable description
  group_by(market) %>%
  slice(1) %>%
  ungroup() %>%
  select(market, latitude, longitude)

# Convert to sf
#DHS_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
markets_sf <- st_as_sf(markets_subset, coords = c("longitude", "latitude"), crs = 4326)
st_geometry(df)

# Project to meters for accurate distance, drop z coordinate first
df <- st_zm(df, drop = TRUE, what = "ZM")
DHS_proj <- st_transform(df, 3857)  # Web Mercator
markets_proj <- st_transform(markets_sf, 3857)

# Distance matrix: rows = original points, columns = markets
dist_matrix <- st_distance(DHS_proj, markets_proj)

# Get the minimum distance and index of nearest market
nearest_dist <- apply(dist_matrix, 1, min)
nearest_market_index <- apply(dist_matrix, 1, which.min)

# Add the results to your original data
df$nearest_market_distance_km <- nearest_dist / 1000  # convert to km
df$nearest_market_id <- markets_subset$market[nearest_market_index]


#merge in food pricing data

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
# GEE data
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Malaria Atlas
#-------------------------------------------------------------------------------


pts = data.frame(lon=df$lon, lat=df$lat)

Interventions__202106_Africa_Insecticide_Treated_Net_Access <- rast(here("data/Malaria Atlas/Ghana/Interventions__202106_Africa_Insecticide_Treated_Net_Access.tif"))
df$MAP_Insecticide_Treated_Net_Access <- terra::extract(Interventions__202106_Africa_Insecticide_Treated_Net_Access, pts, method="bilinear")[, 2]

Malaria__202206_Global_Pf_Mortality_Count.tif <- rast(here("data/Malaria Atlas/Ghana/Malaria__202206_Global_Pf_Mortality_Count.tif"))
df$MAP_Pf_Mortality_Count <- terra::extract(Malaria__202206_Global_Pf_Mortality_Count.tif, pts, method="bilinear")[, 2]

Malaria__202206_Global_Pf_Mortality_Rate.tif <- rast(here("data/Malaria Atlas/Ghana/Malaria__202206_Global_Pf_Mortality_Rate.tif"))
df$MAP_Pf_Mortality_Rate <- terra::extract(Malaria__202206_Global_Pf_Mortality_Rate.tif, pts, method="bilinear")[, 2]

Malaria__202206_Global_Pf_Parasite_Rate.tif <- rast(here("data/Malaria Atlas/Ghana/Malaria__202206_Global_Pf_Parasite_Rate.tif"))
df$MAP_Pf_Parasite_Rate <- terra::extract(Malaria__202206_Global_Pf_Parasite_Rate.tif, pts, method="bilinear")[, 2]

Malaria__202206_Global_Pv_Incidence_Count.tif <- rast(here("data/Malaria Atlas/Ghana/Malaria__202206_Global_Pv_Incidence_Count.tif"))
df$MAP_Pv_Incidence_Count <- terra::extract(Malaria__202206_Global_Pv_Incidence_Count.tif, pts, method="bilinear")[, 2]

Malaria__202206_Global_Pv_Incidence_Rate.tif <- rast(here("data/Malaria Atlas/Ghana/Malaria__202206_Global_Pv_Incidence_Rate.tif"))
df$MAP_Pv_Incidence_Rate <- terra::extract(Malaria__202206_Global_Pv_Incidence_Rate.tif, pts, method="bilinear")[, 2]

Malaria__202206_Global_Pv_Parasite_Rate.tif <- rast(here("data/Malaria Atlas/Ghana/Malaria__202206_Global_Pv_Parasite_Rate.tif"))
df$MAP_Pv_Parasite_Rate <- terra::extract(Malaria__202206_Global_Pv_Parasite_Rate.tif, pts, method="bilinear")[, 2]

Malaria__202406_Global_Pf_Incidence_Count.tif <- rast(here("data/Malaria Atlas/Ghana/Malaria__202406_Global_Pf_Incidence_Count.tif"))
df$MAP_Pf_Incidence_Count <- terra::extract(Malaria__202406_Global_Pf_Incidence_Count.tif, pts, method="bilinear")[, 2]

Malaria__202406_Global_Pf_Incidence_Rate.tif <- rast(here("data/Malaria Atlas/Ghana/Malaria__202406_Global_Pf_Incidence_Rate.tif"))
df$MAP_Pf_Incidence_Rate <- terra::extract(Malaria__202406_Global_Pf_Incidence_Rate.tif, pts, method="bilinear")[, 2]

Malaria__202406_Global_Pf_Mortality_Count.tif <- rast(here("data/Malaria Atlas/Ghana/Malaria__202406_Global_Pf_Mortality_Count.tif"))
df$MAP_Pf_Mortality_Count <- terra::extract(Malaria__202406_Global_Pf_Mortality_Count.tif, pts, method="bilinear")[, 2]

Malaria__202406_Global_Pf_Mortality_Rate.tif <- rast(here("data/Malaria Atlas/Ghana/Malaria__202406_Global_Pf_Mortality_Rate.tif"))
df$MAP_Pf_Mortality_Rate <- terra::extract(Malaria__202406_Global_Pf_Mortality_Rate.tif, pts, method="bilinear")[, 2]

Malaria__202406_Global_Pf_Parasite_Rate.tif <- rast(here("data/Malaria Atlas/Ghana/Malaria__202406_Global_Pf_Parasite_Rate.tif"))
df$MAP_Pf_Parasite_Rate <- terra::extract(Malaria__202406_Global_Pf_Parasite_Rate.tif, pts, method="bilinear")[, 2]

Malaria__202406_Global_Pv_Incidence_Count.tif <- rast(here("data/Malaria Atlas/Ghana/Malaria__202406_Global_Pv_Incidence_Count.tif"))
df$MAP_Pv_Incidence_Count <- terra::extract(Malaria__202406_Global_Pv_Incidence_Count.tif, pts, method="bilinear")[, 2]

Malaria__202406_Global_Pv_Incidence_Rate.tif <- rast(here("data/Malaria Atlas/Ghana/Malaria__202406_Global_Pv_Incidence_Rate.tif"))
df$MAP_Pv_Incidence_Rate <- terra::extract(Malaria__202406_Global_Pv_Incidence_Rate.tif, pts, method="bilinear")[, 2]

Malaria__202406_Global_Pv_Parasite_Rate.tif <- rast(here("data/Malaria Atlas/Ghana/Malaria__202406_Global_Pv_Parasite_Rate.tif"))
df$MAP_Pv_Parasite_Rate <- terra::extract(Malaria__202406_Global_Pv_Parasite_Rate.tif, pts, method="bilinear")[, 2]

Interventions__202106_Africa_Insecticide_Treated_Net_Use.tif <- rast(here("data/Malaria Atlas/Ghana/Interventions__202106_Africa_Insecticide_Treated_Net_Use.tif"))
df$MAP_Insecticide_Treated_Net_Use <- terra::extract(Interventions__202106_Africa_Insecticide_Treated_Net_Use.tif, pts, method="bilinear")[, 2]

Interventions__202106_Africa_Insecticide_Treated_Net_Use_Rate.tif <- rast(here("data/Malaria Atlas/Ghana/Interventions__202106_Africa_Insecticide_Treated_Net_Use_Rate.tif"))
df$MAP_Insecticide_Treated_Net_Use_Rate <- terra::extract(Interventions__202106_Africa_Insecticide_Treated_Net_Use_Rate.tif, pts, method="bilinear")[, 2]

Interventions__202106_Africa_IRS_Coverage.tif <- rast(here("data/Malaria Atlas/Ghana/Interventions__202106_Africa_IRS_Coverage.tif"))
df$MAP_IRS_Coverage <- terra::extract(Interventions__202106_Africa_IRS_Coverage.tif, pts, method="bilinear")[, 2]

Interventions__202106_Global_Antimalarial_Effective_Treatment.tif <- rast(here("data/Malaria Atlas/Ghana/Interventions__202106_Global_Antimalarial_Effective_Treatment.tif"))
df$MAP_Antimalarial_Effective_Treatment <- terra::extract(Interventions__202106_Global_Antimalarial_Effective_Treatment.tif, pts, method="bilinear")[, 2]

Interventions__202406_Africa_Insecticide_Treated_Net_Access.tif <- rast(here("data/Malaria Atlas/Ghana/Interventions__202406_Africa_Insecticide_Treated_Net_Access.tif"))
df$MAP_Insecticide_Treated_Net_Access <- terra::extract(Interventions__202406_Africa_Insecticide_Treated_Net_Access.tif, pts, method="bilinear")[, 2]

Interventions__202406_Africa_Insecticide_Treated_Net_Use.tif <- rast(here("data/Malaria Atlas/Ghana/Interventions__202406_Africa_Insecticide_Treated_Net_Use.tif"))
df$MAP_Insecticide_Treated_Net_Use <- terra::extract(Interventions__202406_Africa_Insecticide_Treated_Net_Use.tif, pts, method="bilinear")[, 2]

Interventions__202406_Africa_Insecticide_Treated_Net_Use_Rate.tif <- rast(here("data/Malaria Atlas/Ghana/Interventions__202406_Africa_Insecticide_Treated_Net_Use_Rate.tif"))
df$MAP_Insecticide_Treated_Net_Use_Rate <- terra::extract(Interventions__202406_Africa_Insecticide_Treated_Net_Use_Rate.tif, pts, method="bilinear")[, 2]

Interventions__202406_Global_Antimalarial_Effective_Treatment.tif <- rast(here("data/Malaria Atlas/Ghana/Interventions__202406_Global_Antimalarial_Effective_Treatment.tif"))
df$MAP_Antimalarial_Effective_Treatment <- terra::extract(Interventions__202406_Global_Antimalarial_Effective_Treatment.tif, pts, method="bilinear")[, 2]

Malaria__202202_Global_Pf_Reproductive_Number.tif <- rast(here("data/Malaria Atlas/Ghana/Malaria__202202_Global_Pf_Reproductive_Number.tif"))
df$MAP_Pf_Reproductive_Number <- terra::extract(Malaria__202202_Global_Pf_Reproductive_Number.tif, pts, method="bilinear")[, 2]

Malaria__202206_Global_Pf_Incidence_Count.tif <- rast(here("data/Malaria Atlas/Ghana/Malaria__202206_Global_Pf_Incidence_Count.tif"))
df$MAP_Pf_Incidence_Count <- terra::extract(Malaria__202206_Global_Pf_Incidence_Count.tif, pts, method="bilinear")[, 2]

Malaria__202206_Global_Pf_Incidence_Rate.tif <- rast(here("data/Malaria Atlas/Ghana/Malaria__202206_Global_Pf_Incidence_Rate.tif"))
df$MAP_Pf_Incidence_Rate <- terra::extract(Malaria__202206_Global_Pf_Incidence_Rate.tif, pts, method="bilinear")[, 2]


#-------------------------------------------------------------------------------
# Conflict Data
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# FluNet Data
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Food security Data
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Cadre Harmonise Data
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# DHS Admin 1 indicators
#-------------------------------------------------------------------------------

dhs <- readRDS(here("data/DHS/clean/Ghana_2016_dhs_aggregation.rds"))
head(dhs)

colnames(dhs)

table(dhs$DHSREGEN)
table(df$Admin1)
df <- left_join(as.data.frame(df), dhs, by = c("Admin1" = "DHSREGEN"))


#-------------------------------------------------------------------------------
# Save data
#-------------------------------------------------------------------------------

saveRDS(df, file=here("data", "IPD", "Ghana", "Ghana_merged_dataset.rds"))
