
# Load necessary libraries
rm(list=ls())
library(tidyverse)
library(readr)
library(fuzzyjoin)
library(here)

# Set file


# Load datasets
oncho <- read_csv(here("data/IHME/Africa and Yemen Onchocerciasis Prevalence Geospatial Estimates 2000-2018/Data [CSV]/IHME_AFRICA_ONCHO_2000_2018_PREV_ADMIN_2_Y2020M08D18.CSV"))
hiv <- read_csv(here("data/IHME/Africa HIV Prevalence Geospatial Estimates 2000-2017/Data [CSV]/IHME_AFRICA_HIV_2000_2017_HIV_PREV_ADMIN_2_Y2019M03D15.CSV"))
gps <- read.csv(here("data", "IPD", "Ghana", "Ghana_GMS_GPS_cleaned.csv"))

# Filter for Ghana 2017
oncho_ghana <- oncho %>% filter(ADM0_NAME == "Ghana", year == 2017)
hiv_ghana <- hiv %>% filter(ADM0_NAME == "Ghana", year == 2017)

# Merge IHME datasets
merged_ihme <- inner_join(
  oncho_ghana,
  hiv_ghana,
  by = c("ADM0_CODE", "ADM0_NAME", "ADM1_CODE", "ADM1_NAME", "ADM2_CODE",
         "ADM2_NAME", "year", "age_group_id", "age_group_name",
         "sex_id", "sex", "measure", "metric"),
  suffix = c("_oncho", "_hiv"),
)
colnames(merged_ihme) <- paste0("IHME_", colnames(merged_ihme))

# Prepare GPS data for join
head(gps)
gps_clean <- gps %>%
  rename(District_Name = District.Name) %>%
  mutate(District_Name = str_to_lower(str_trim(District_Name)))

# Fuzzy join on district names (ADM2_NAME vs District_Name)
merged_ihme <- merged_ihme %>%
  mutate(ADM2_NAME_lower = str_to_lower(str_trim(IHME_ADM2_NAME)))

fuzzy_merged <- stringdist_left_join(
  gps_clean,
  merged_ihme,
  by = c("District_Name" = "ADM2_NAME_lower"),
  method = "jw",  # Jaro-Winkler distance works well for names
  max_dist = 0.2, # Adjust threshold as needed
  distance_col = "dist"
)

# pick best match per GPS record
final_merged <- fuzzy_merged %>%
  group_by(cnum) %>%
  slice_min(order_by = dist, with_ties = FALSE) %>%
  ungroup()

# Preview result
print(head(final_merged))

# Save result
write_csv(final_merged, here("data/IHME/ghana_2017_merged_IHME_data.csv"))


