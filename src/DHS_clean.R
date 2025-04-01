

library(dplyr)
library(haven)  # if reading Stata (.dta) or SPSS files

#----------------------
# 1. Load IR dataset
#----------------------
df <- readRDS(here("data/DHS/dhs_Ghana_2022.RDS"))
df <-df$Ghana_2022
# Suppose we downloaded "GHA_IR_2022.DTA" (the name here is hypothetical)
IR_data <- df$IRdata

# Minimal cleaning: rename key variables
IR_clean <- IR_data %>%
  transmute(
    cluster     = v001,
    household   = v002,
    line        = v003,
    age         = v012,            # Age in years
    region      = v024,            # Region code
    residence   = v025,            # Urban/rural
    education   = v106,            # Education level
    wealth_q    = v190,            # Wealth quintile (often 1=lowest, 5=highest)
    hemoglobin  = v453 / 10,       # If v453 is stored as g/dL*10 (check Ghana codebook)
    anemia_cat  = v457,            # Categorical anemia level
    # DHS typically codes: 1=Mild, 2=Moderate, 3=Severe, 0/4=Not anemic (varies by survey)
    weight      = v005 / 1000000   # Sample weight (for complex survey design)
  ) %>%
  # Optionally recode anemia category
  mutate(
    anemia_cat = case_when(
      anemia_cat == 0 ~ "Not Anemic",
      anemia_cat == 1 ~ "Mild",
      anemia_cat == 2 ~ "Moderate",
      anemia_cat == 3 ~ "Severe",
      TRUE            ~ NA_character_
    )
  ) %>%
  # (Optionally) keep only women with valid anemia measures
  filter(!is.na(hemoglobin))

#----------------------
# 2. Load PR dataset
#----------------------
PR_data <- df$PRdata

# If you only want household-level attributes from PR, you can reduce duplication
# by grouping by cluster-household and keeping distinct rows:
PR_household <- PR_data %>%
  group_by(hv001, hv002) %>%
  # Summarize household-level attributes or keep the first row as needed
  summarise(
    # For example, overall household size:
    hh_size = max(hv009, na.rm=TRUE),
    # The "de jure" or "de facto" type of place, hv025 = urban/rural, etc.
    # If you want the standard DHS wealth index for the HH:
    hh_wealth_index = max(hv270, na.rm=TRUE),
    # Or you might choose a more direct method: pick the row for the household head
    # ...
  ) %>%
  ungroup() %>%
  rename(cluster = hv001,
         household = hv002)

# If instead you want *person-level* merges (like the woman’s line-level data),
# you’d rename hv001 -> cluster, hv002 -> household, hvidx -> line, then keep that.

#----------------------
# 3. Merge IR with household-level data
#----------------------
analysis_data <- IR_clean %>%
  left_join(PR_household, by = c("cluster", "household"))

# After merging, you have women's anemia outcomes and relevant household attributes
# in one data frame. You can now do further cleaning or recoding as needed.

# Example: Subset to plausible predictors
analysis_data <- analysis_data %>%
  select(
    cluster, household, line,
    weight,                # Survey weight
    age,
    region,
    residence,
    education,
    wealth_q,             # Wealth quintile from IR
    hh_size,              # Household size from PR
    hh_wealth_index,      # (If you wanted the PR-coded wealth index instead)
    hemoglobin,
    anemia_cat
    # ... add more if desired
  )
