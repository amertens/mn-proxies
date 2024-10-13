
rm(list=ls())
library(tidyverse)
library(here)
library(readxl)

indicator_df_wide <- readRDS(here::here("data/GHO_indicator_data_wide.rds"))

anemia <- read.csv(here::here("data/MN portal data/anemia.csv"))
anemia <- anemia %>% select(!starts_with("Reference")) %>%
  filter(!is.na(Prevalence)) %>%
  select(Micronutrient, Indicator,WHO.region, ISOCODE, Country, Representativeness.name,
         Area.covered,  Survey.Year, Population.group, Gender, Sample.size, Value.type,
         Data.adjusted.for, Indicator.comments)
colnames(anemia)
head(anemia)


head(indicator_df_wide)
table(is.na(anemia$Prevalence))
table(is.na(anemia$Mean))


table(indicator_df_wide$SpatialDim)
table(anemia$ISOCODE)

indicator_df_wide <- indicator_df_wide %>% rename("country_code" = "SpatialDim")
anemia <- anemia %>% rename("country_code" = "ISOCODE")


#NOTE! The Dim1 variable seems to be duplicating surveys other than values of this variables
# Check if this needs to be used in the transformation
indicator_df_wide2 <- indicator_df_wide %>% ungroup() %>% distinct(country_code, TimeDimensionValue, .keep_all = TRUE)

#temp do exact match
anemia <- anemia %>% mutate(Survey.Year = as.numeric(Survey.Year))
indicator_df_wide2 <- indicator_df_wide2 %>% mutate(Survey.Year = as.numeric(TimeDimensionValue))

#merge by country and the subset to the closest matching years
dim(anemia)
dim(indicator_df_wide)
indicator_df_wide2$gho_data <- 1
d <- left_join(anemia, indicator_df_wide2, by = c("country_code", "Survey.Year")) %>%
  filter(!is.na(gho_data))
dim(d)


summary(d$AMR_INFECT_MRSA)

d2 <- d %>% filter(!is.na(AMR_INFECT_MRSA))



#visualize missingness
library(tidyverse)
library(naniar)
library(visdat)

# Function to analyze missing data patterns
analyze_missing_data <- function(df) {
  # Basic summary of missing data
  cat("Summary of missing data:\n")
  print(sum(is.na(df)))
  print(colSums(is.na(df)))

  # Percentage of missing data
  missing_percentages <- colMeans(is.na(df)) * 100
  missing_df <- data.frame(
    variable = names(missing_percentages),
    percent_missing = missing_percentages
  )

  # Sort by percentage missing (descending)
  missing_df <- missing_df[order(-missing_df$percent_missing), ]

  # Print table of missing percentages
  cat("\nPercentage of missing data by variable:\n")
  print(missing_df)

  # Visualize missing data patterns
  cat("\nVisualizing missing data patterns:\n")

  # Overall view of missingness
  print(vis_miss(df))

  # Missingness correlation
  print(gg_miss_upset(df))

  # Missingness by variable
  print(gg_miss_var(df))

  # Potential correlation between missingness
  print(gg_miss_fct(df))
}


analyze_missing_data(indicator_df_wide)
analyze_missing_data(indicator_df_wide2)



#----------------------------------
# Note:
#----------------------------------

# Possible approach would be to use the closest year to the survey year for the GHO data for each indicator
# (because each may come from a different year) - make it merge within a window, and impute missingness
