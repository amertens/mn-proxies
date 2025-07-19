

# ---- 0.  Packages ----
rm(list=ls())
library(haven)      # read .dta files
library(dplyr)      # data manipulation
library(janitor)    # clean variable names
library(survey)     # weighted estimates (if you keep HH‑level weights)
library(sf)         # optional: mapping shapefiles
library(here)

# ---- 1.  Read data ----
hh   <- read_dta(here("data/LSMS/g7aggregates_hhlevel.dta"))  %>% clean_names()
ind  <- read_dta(here("data/LSMS/g7aggregates_indlevel.dta")) %>% clean_names()
)
table(ind$region)


# Code	Region
# 1	Western
# 2	Central
# 3	Gt. Accra
# 4	Volta
# 5	Eastern
# 6	Ashanti
# 7	B. Ahafo
# 8	Northern
# 9	Upper East
# 10	Upper West

table(hh$region)
hh <- hh %>% mutate(admin1 = factor(region, levels=c(1:10),
             labels=c("Western", "Central", "Greater Accra", "Volta", "Eastern", "Ashanti", "Bono Ahafo", "Northern", "Upper East", "Upper West")))
table(hh$admin1)


# # (optional) cluster–GPS centroids
# gps  <- read_dta("data/glss7_gps_ea.dta") %>%
#   select(clust, lat_deg = latitude, lon_deg = longitude)

# # ---- 2.  Attach GPS & basic geography ----
# hh <- hh %>%
#   left_join(gps, by = "clust") %>%          # adds lat/lon
#   mutate(region  = as_factor(region),        # labelled >> factor
#          district = substr(loc5, 1, 5))      # keep as character

# ---- 3.  Select predictors likely linked to MND ----
predictor_vars <- c(
  "pcinc_net",          # per‑capita net income
  "padq_hh",            # daily per‑capita exp (kcal proxy)
  "mfdshare",           # share non‑food expenditure
  "fd_p_cer",           # % expenditure on cereals
  "fd_p_meat", "fd_p_veg", "fd_p_fruit", "fd_p_puls",
  "fdhhds"              # dietary diversity score
)

hh_small <- hh %>%
  select(clust, admin1, rururb, all_of(predictor_vars), wta_s)  # wta_s = survey weight

# ---- 4‑A.  Cluster‑level (EA) aggregates ----
ea_summary <- hh_small %>%
  group_by(admin1) %>%
  summarise(across(all_of(predictor_vars),
                   ~ weighted.mean(.x, wta_s, na.rm = TRUE),
                   .names = "m_{.col}"),
            households  = n(),
            admin1    = first(admin1),
            rururb      = first(rururb))


# ---- 5.  Merge into micronutrient‑survey data set ----
mnd <- read_dta("data/ghana_mnd_survey_2017.dta") %>% clean_names()

# If MND sample is at cluster level:
mnd <- mnd %>% left_join(ea_summary,  by = c("cluster_id" = "clust"))

# If only district identifiers are available:
mnd <- mnd %>% left_join(dist_summary, by = c("district_code" = "loc5"))

# ---- 6.  Quick check ----
summary(mnd[, grep("^m_", names(mnd))])
# ---- 0.  Packages ----
library(haven)      # read .dta files
library(dplyr)      # data manipulation
library(janitor)    # clean variable names
library(survey)     # weighted estimates (if you keep HH‑level weights)
library(sf)         # optional: mapping shapefiles

# ---- 1.  Read data ----
hh   <- read_dta("data/g7aggregates_hhlevel.dta")  %>% clean_names()
ind  <- read_dta("data/g7aggregates_indlevel.dta") %>% clean_names()

# (optional) cluster–GPS centroids
gps  <- read_dta("data/glss7_gps_ea.dta") %>%
  select(clust, lat_deg = latitude, lon_deg = longitude)

# ---- 2.  Attach GPS & basic geography ----
hh <- hh %>%
  left_join(gps, by = "clust") %>%          # adds lat/lon
  mutate(region  = as_factor(region),        # labelled >> factor
         district = substr(loc5, 1, 5))      # keep as character

# ---- 3.  Select predictors likely linked to MND ----
predictor_vars <- c(
  "pcinc_net",          # per‑capita net income
  "padq_hh",            # daily per‑capita exp (kcal proxy)
  "mfdshare",           # share non‑food expenditure
  "fd_p_cer",           # % expenditure on cereals
  "fd_p_meat", "fd_p_veg", "fd_p_fruit", "fd_p_puls",
  "fdhhds"              # dietary diversity score
)

hh_small <- hh %>%
  select(clust, loc5, rururb, all_of(predictor_vars), wta_s)  # wta_s = survey weight

# ---- 4‑A.  Cluster‑level (EA) aggregates ----
ea_summary <- hh_small %>%
  group_by(clust) %>%
  summarise(across(all_of(predictor_vars),
                   ~ weighted.mean(.x, wta_s, na.rm = TRUE),
                   .names = "m_{.col}"),
            households  = n(),
            district    = first(loc5),
            rururb      = first(rururb))

# ---- 4‑B.  District (Admin‑2) aggregates ----
dist_summary <- hh_small %>%
  group_by(loc5) %>%
  summarise(across(all_of(predictor_vars),
                   ~ weighted.mean(.x, wta_s, na.rm = TRUE),
                   .names = "m_{.col}"),
            households = n())

# ---- 5.  Merge into micronutrient‑survey data set ----
mnd <- read_dta("data/ghana_mnd_survey_2017.dta") %>% clean_names()

# If MND sample is at cluster level:
mnd <- mnd %>% left_join(ea_summary,  by = c("cluster_id" = "clust"))

# If only district identifiers are available:
mnd <- mnd %>% left_join(dist_summary, by = c("district_code" = "loc5"))

# ---- 6.  Quick check ----
summary(mnd[, grep("^m_", names(mnd))])
