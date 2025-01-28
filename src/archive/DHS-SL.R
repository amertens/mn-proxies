
library(here)
library(VIM)  # For KNN imputation
library(mice) # For creating missingness indicators
library(caret)
library(tidyverse)
library(fuzzyjoin)
fullres <- readRDS(here("data/DHS_national_indicator_prevalences.rds"))

table(fullres$country)

brinda <- readRDS(here("data/clean_brinda_data.rds"))

head(fullres)
wide_data <- fullres %>% select(country, year, indicator, direct.est) %>%
  pivot_wider(
    id_cols = c(country, year),
    names_from = indicator,
    values_from = direct.est
  )
head(wide_data)

#drop near zero variance columns
near_zero_variance_columns <- nearZeroVar(wide_data)
wide_data <- wide_data[,-near_zero_variance_columns]


# Step 1: Prepare the data
# Separate numeric columns (for imputation) from non-numeric columns
numeric_cols <- wide_data %>% select_if(is.numeric) %>% names()
non_numeric_cols <- setdiff(names(wide_data), numeric_cols)

#Perform KNN imputation on numeric columns and add numeric columns
imputed_data <- kNN(wide_data[, numeric_cols], k = 5)  # Adjust k as needed
imputed_data[,grepl("_imp",colnames(imputed_data))] <- 1*imputed_data[,grepl("_imp",colnames(imputed_data))]
wide_data <- data.frame(country=wide_data$country, imputed_data)

# Function to find the closest year
closest_year <- function(year1, year2) {
  year2[which.min(abs(year1 - year2))]
}

#function to clean brinda data
extract_first_number <- function(strings) {
  as.numeric(sub("^(\\d+\\.?\\d*).*", "\\1", strings))
}
clean_brinda_data <- function(brinda_data, zinc=F){

  if(zinc){
    brinda_data <- brinda_data %>% filter(.[,4]!="–")
    brinda_data[,4] <- as.numeric(extract_first_number(brinda_data[,4]))
  }else{
    brinda_data <- brinda_data %>% filter(.[,4]!="–"|.[,5]!="–")
    brinda_data[,4] <- as.numeric(extract_first_number(brinda_data[,4]))
    brinda_data[,5] <- as.numeric(extract_first_number(brinda_data[,5]))
  }

  return(brinda_data)
}

iron_child <- clean_brinda_data(brinda$iron_child)
iron_women <- clean_brinda_data(brinda$iron_women)
vita_child <- clean_brinda_data(brinda$vita_child)
vita_women <- clean_brinda_data(brinda$vita_women)
zinc_child <- clean_brinda_data(brinda$zinc_child, zinc=TRUE)
zinc_women <- clean_brinda_data(brinda$zinc_women, zinc=TRUE)

brinda_iron_women <- clean_brinda_data(brinda$iron_women)
brinda_iron_women <- clean_brinda_data(brinda$iron_women)


# Perform the fuzzy join


joined_data <- full_join(brinda_iron_women, wide_data, by = c("country"), relationship = "many-to-many") %>%
  filter(!is.na(year.x), !is.na(year.y)) %>%
  group_by(country, year.x) %>%
  mutate(
    closest_survey_year = closest_year(year.x, year.y),
    year_difference = abs(year.y - closest_survey_year)
  ) %>%
  filter(year.y == closest_survey_year) %>%
  ungroup()


joined_data

# Step 4: Combine imputed numeric data with non-numeric data
final_data <- bind_cols(
  wide_data[, non_numeric_cols],
  imputed_data[, numeric_cols]
)

# Step 5: Add missingness indicators to the final dataset
final_data_with_indicators <- bind_cols(
  final_data,
  missingness_data %>% select(starts_with("miss_"))
)

# View the first few rows and summary of the final dataset
print(head(final_data_with_indicators))
print(summary(final_data_with_indicators))

# If you want to save the final dataset to a CSV file, uncomment and modify the following line:
# write_csv(final_data_with_indicators, "path_to_save_imputed_data_with_indicators.csv")

# Optional: Calculate and print the percentage of missingness for each variable
missingness_percentage <- colMeans(is.na(wide_data)) * 100
print(sort(missingness_percentage, decreasing = TRUE))
