
library(here)
library(VIM)  # For KNN imputation
library(mice) # For creating missingness indicators
library(caret)
library(tidyverse)
library(fuzzyjoin)
fullres <- readRDS(here("data/DHS_national_indicator_prevalences.rds"))
head(fullres)
table(fullres$country)

head(fullres)
wide_data <- fullres %>% select(country, year, indicator, direct.est) %>%
  pivot_wider(
    id_cols = c(country, year),
    names_from = indicator,
    values_from = direct.est
  )

#temp subset to one outcome
wide_data <- wide_data %>% filter(!is.na(womananemia ))
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

wide_data$womananemia


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
