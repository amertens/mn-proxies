
#-------------------------------------------------------------------------------
# Calculate DHS Indicators at Admin-2 Level for Ghana
#-------------------------------------------------------------------------------

library(rdhs)
library(surveyPrev)
library(geodata)
library(sf)
library(dplyr)
library(tidyr)
library(here)
library(purrr)

# Set up DHS configuration (use your existing credentials)
#        #sctXX5E87xJbD*
set_rdhs_config(email = "amertens@berkeley.edu",
                project = "MN Prediction",
                data_frame = "data.table::as.data.table",
                config_path = "~/.rdhs.json",
                global = TRUE)

#-------------------------------------------------------------------------------
# 1. Load your existing Ghana data or download fresh
#-------------------------------------------------------------------------------

source(paste0(here::here(),"/src/0-functions.R"))
source(paste0(here::here(),"/src/DHS/DHS_functions.R"))
source(paste0(here::here(),"/src/DHS/DHS_variable_recode.R"))

dfull <- readRDS(here("data/DHS/dhs_Ghana_2016.RDS"))
dfull <-dfull$Ghana_2016




HR_clean <- clean_HR(dfull$HRdata)
PR_clean <- clean_PR(dfull$PRdata)

# Merge household data into the PR data:
PR_merged <- PR_clean %>%
  left_join(HR_clean, by = c("cluster","hh"))

IR_clean <- clean_IR(dfull$IRdata)
head(IR_clean)

# Merge IR (women’s) data into PR_merged for female lines:
PR_IR_merged <- PR_merged %>%
  left_join(IR_clean, by = c("cluster","hh","line"))
head(PR_IR_merged)

colnames(PR_merged)
colnames(IR_clean)
colnames(PR_IR_merged)

table(PR_IR_merged$man_anemia_cat)
table(PR_IR_merged$anemia_cat)

#-------------------------------------------------------------------------------
# 2. Get GPS coordinates and administrative boundaries
#-------------------------------------------------------------------------------

# Get GPS coordinates for Ghana 2022
geo<-readRDS(here("data/DHS_compiled_gps_data.rds")) %>% filter(country == "Ghana" & year == 2017)

#geo <- getDHSgeo(country = "Ghana", year = 2017)

# Download administrative boundaries
poly.adm1 <- geodata::gadm(country = "Ghana", level = 1, path = tempdir())
poly.adm1 <- sf::st_as_sf(poly.adm1)

poly.adm2 <- geodata::gadm(country = "Ghana", level = 2, path = tempdir())
poly.adm2 <- sf::st_as_sf(poly.adm2)

# Create cluster information linking GPS to admin boundaries
cluster.info <- clusterInfo(geo = geo,
                            poly.adm1 = poly.adm1,
                            poly.adm2 = poly.adm2,
                            by.adm1 = "NAME_1",
                            by.adm2 = "NAME_2")


#test
admin2_results <- directEST(data = as.numeric(PR_IR_merged$has_bicycle),
                            cluster.info = cluster_info,
                            admin = 2,  # Admin-2 level
                            strata = "all")

#-------------------------------------------------------------------------------
# 3. Define indicators to calculate
#-------------------------------------------------------------------------------

# Key indicators (you can modify this list based on your needs)
indicators_to_calculate <- c(
  "womananemia",          # Women's anemia
  "AN_ANEM_W_ANY",       # Any anemia in women
  "CN_ANMC_C_ANY",       # Any anemia in children
  "stunting",            # Child stunting
  "CN_NUTS_C_HA2",       # Stunting (height-for-age)
  "wasting",             # Child wasting
  "CN_NUTS_C_WH2",       # Wasting (weight-for-height)
  "ancvisit4+",          # 4+ ANC visits
  "RH_ANCN_W_N4P",       # 4+ ANC visits
  "DPT3",                # DPT3 vaccination
  "CH_VACC_C_DP3",       # DPT3 vaccination
  "CH_VACC_C_BAS",       # Basic vaccination
  "unmet_family",        # Unmet family planning need
  "FP_NADA_W_UNT",       # Unmet need for family planning
  "WS_SRCE_P_BAS",       # Basic water source
  "WS_TLET_P_BAS",       # Basic sanitation
  "ML_NETP_H_IT2"        # ITN ownership
)

#-------------------------------------------------------------------------------
# 4. Function to calculate admin-2 estimates for a single indicator
#-------------------------------------------------------------------------------

calculate_admin2_indicator <- function(dhsData, indicator_name, cluster_info) {

  cat("Processing indicator:", indicator_name, "\n")

  # Try to extract indicator data from different data files
  indicator_data <- NULL

  # Try each data file in the DHS dataset
  for (data_name in names(dhsData)) {
    if (is.null(indicator_data) || nrow(indicator_data) == 0) {
      try({
        temp_data <- getDHSindicator(dhsData[[data_name]], indicator = indicator_name)
        if (!is.null(temp_data) && nrow(temp_data) > 0) {
          indicator_data <- temp_data %>% filter(!is.na(value))
        }
      }, silent = TRUE)
    }
  }

  # If we have valid data, calculate admin-2 estimates
  if (!is.null(indicator_data) && nrow(indicator_data) > 0) {

    # Calculate direct estimates at admin-2 level
    admin2_results <- NULL
    try({
      admin2_results <- directEST(data = indicator_data,
                                  cluster.info = cluster_info,
                                  admin = 2,  # Admin-2 level
                                  strata = "all")
    }, silent = TRUE)

    if (!is.null(admin2_results) && !is.null(admin2_results$res.admin2)) {
      result <- admin2_results$res.admin2 %>%
        mutate(
          indicator = indicator_name,
          country = "Ghana",
          year = 2022
        ) %>%
        select(country, year, indicator, admin2.name, admin2.char,
               mean, var, sd, median, lower, upper, logit.est, logit.prec)

      return(result)
    }
  }

  # Return empty result if calculation failed
  return(data.frame(
    country = "Ghana",
    year = 2022,
    indicator = indicator_name,
    admin2.name = NA,
    admin2.char = NA,
    mean = NA, var = NA, sd = NA, median = NA,
    lower = NA, upper = NA, logit.est = NA, logit.prec = NA
  ))
}

#-------------------------------------------------------------------------------
# 5. Calculate all indicators at admin-2 level
#-------------------------------------------------------------------------------

all_admin2_results <- list()
indicator_name="ancvisit4+"
dhsData=ghana_data

for (indicator in indicators_to_calculate) {
  result <- calculate_admin2_indicator(ghana_data, indicator, cluster.info)
  all_admin2_results[[indicator]] <- result

  # Print progress
  cat("Completed:", indicator, "- Got", nrow(result), "admin-2 units\n")
}

# Combine all results
admin2_estimates <- bind_rows(all_admin2_results)

#-------------------------------------------------------------------------------
# 6. Also calculate admin-1 (regional) estimates for comparison
#-------------------------------------------------------------------------------

calculate_admin1_indicator <- function(dhsData, indicator_name, cluster_info) {

  cat("Processing admin-1 indicator:", indicator_name, "\n")

  indicator_data <- NULL

  for (data_name in names(dhsData)) {
    if (is.null(indicator_data) || nrow(indicator_data) == 0) {
      try({
        temp_data <- getDHSindicator(dhsData[[data_name]], indicator = indicator_name)
        if (!is.null(temp_data) && nrow(temp_data) > 0) {
          indicator_data <- temp_data %>% filter(!is.na(value))
        }
      }, silent = TRUE)
    }
  }

  if (!is.null(indicator_data) && nrow(indicator_data) > 0) {
    admin1_results <- NULL
    try({
      admin1_results <- directEST(data = indicator_data,
                                  cluster.info = cluster_info,
                                  admin = 1,  # Admin-1 level
                                  strata = "all")
    }, silent = TRUE)

    if (!is.null(admin1_results) && !is.null(admin1_results$res.admin1)) {
      result <- admin1_results$res.admin1 %>%
        mutate(
          indicator = indicator_name,
          country = "Ghana",
          year = 2022
        )
      return(result)
    }
  }

  return(data.frame(
    country = "Ghana", year = 2022, indicator = indicator_name,
    admin1.name = NA, admin1.char = NA,
    mean = NA, var = NA, sd = NA, median = NA,
    lower = NA, upper = NA, logit.est = NA, logit.prec = NA
  ))
}

# Calculate admin-1 estimates
all_admin1_results <- list()
for (indicator in indicators_to_calculate) {
  result <- calculate_admin1_indicator(ghana_data, indicator, cluster.info)
  all_admin1_results[[indicator]] <- result
}

admin1_estimates <- bind_rows(all_admin1_results)

#-------------------------------------------------------------------------------
# 7. Save results
#-------------------------------------------------------------------------------

# Save admin-2 results
saveRDS(admin2_estimates, here("data/DHS_Ghana_2022_admin2_estimates.rds"))
write.csv(admin2_estimates, here("data/DHS_Ghana_2022_admin2_estimates.csv"), row.names = FALSE)

# Save admin-1 results
saveRDS(admin1_estimates, here("data/DHS_Ghana_2022_admin1_estimates.rds"))
write.csv(admin1_estimates, here("data/DHS_Ghana_2022_admin1_estimates.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# 8. Summary and visualization
#-------------------------------------------------------------------------------

# Summary of successful calculations
summary_admin2 <- admin2_estimates %>%
  filter(!is.na(mean)) %>%
  group_by(indicator) %>%
  summarise(
    n_admin2_units = n(),
    mean_prevalence = round(mean(mean, na.rm = TRUE), 3),
    min_prevalence = round(min(mean, na.rm = TRUE), 3),
    max_prevalence = round(max(mean, na.rm = TRUE), 3),
    .groups = 'drop'
  )

print("Summary of Admin-2 calculations:")
print(summary_admin2)

# Show which admin-2 units we have data for
admin2_units <- admin2_estimates %>%
  filter(!is.na(mean) & !is.na(admin2.name)) %>%
  distinct(admin2.name, admin2.char) %>%
  arrange(admin2.name)

cat("\nAdmin-2 units with data (", nrow(admin2_units), "total):\n")
print(admin2_units)

#-------------------------------------------------------------------------------
# 9. Optional: Create maps
#-------------------------------------------------------------------------------

# Function to create a simple map for one indicator
create_admin2_map <- function(indicator_name, estimates_data, admin_boundaries) {

  # Filter data for the specific indicator
  indicator_data <- estimates_data %>%
    filter(indicator == indicator_name & !is.na(mean))

  if (nrow(indicator_data) == 0) {
    cat("No data available for", indicator_name, "\n")
    return(NULL)
  }

  # Merge with spatial boundaries
  map_data <- admin_boundaries %>%
    left_join(indicator_data, by = c("NAME_2" = "admin2.name"))

  # Create simple plot
  library(ggplot2)

  p <- ggplot(map_data) +
    geom_sf(aes(fill = mean), color = "white", size = 0.1) +
    scale_fill_viridis_c(name = "Prevalence", na.value = "grey90") +
    theme_void() +
    labs(title = paste("Ghana Admin-2:", indicator_name),
         subtitle = "DHS 2022") +
    theme(legend.position = "bottom")

  return(p)
}

# Example: Create map for women's anemia
if ("womananemia" %in% indicators_to_calculate) {
  library(ggplot2)
  anemia_map <- create_admin2_map("womananemia", admin2_estimates, poly.adm2)
  if (!is.null(anemia_map)) {
    ggsave(here("plots/ghana_admin2_womens_anemia.png"),
           anemia_map, width = 10, height = 8, dpi = 300)
  }
}

cat("\nAdmin-2 indicator calculation completed!\n")
cat("Results saved to:", here("data/DHS_Ghana_2022_admin2_estimates.rds"), "\n")
