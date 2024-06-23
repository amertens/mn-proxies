

library(tidyverse)
library(readxl)
library(here)

# Specify the path to your Excel file
excel_file <- "C:/Users/andre/Documents/mn-proxies/data/BRINDA/MN_prevalence_20240529.xlsx"

# Get the names of all the sheets in the Excel file
sheet_names <- excel_sheets(excel_file)

# Read each sheet into a separate data frame and store them in a list
data_list <- lapply(sheet_names[1:6], function(sheet) read_excel(excel_file, sheet = sheet))
names(data_list) <- sheet_names[1:6]

#survey name cleaning function
split_year <- function(df){
  # Remove the parentheses and extract the year
  df$year <- sapply(strsplit(df$country_year, " \\(|\\)"), function(x) {
    year_range <- strsplit(x[2], "–")[[1]]
    as.integer(year_range[1])
  })

  # Extract the country name
  df$country <- sapply(strsplit(df$country_year, " \\("), function(x) x[1])

  # Remove the original 'country_year' variable
  df$country_year <- NULL

  return(df)
}


data_list[[1]] <- data_list[[1]] %>%
  rename(country_year = Survey,
         age = `age (mo)`,
         prev_ferr = ferrbr_adj,
         prev_stfr = stfrbr_adj) %>%
  split_year(.) %>%
  select(country, year, age, prev_ferr, prev_stfr)
data_list[[2]] <- data_list[[2]] %>%
  rename(country_year = Survey,
         age = `age (mo)`,
         prev_ferr = ferrbr_adj,
         prev_stfr = stfrbr_adj) %>%
  split_year(.) %>%
  select(country, year, age, prev_ferr, prev_stfr)
data_list[[3]] <- data_list[[3]] %>%
  rename(country_year = Survey,
         age = `age (mo)`,
         prev_rol = rolbr_adj,
         prev_rbp = rbpbr_adj) %>%
  split_year(.) %>%
  select(country, year, age, prev_rol, prev_rbp)
data_list[[4]] <- data_list[[4]] %>%
  rename(country_year = Survey,
         age = `age (mo)`,
         prev_rol = rolbr_unadj,
         prev_rbp = rbpbr_unadj) %>%
  split_year(.) %>%
  select(country, year, age, prev_rol, prev_rbp)
data_list[[5]] <- data_list[[5]] %>%
  rename(country_year = Survey,
         age = `age (mo)`,
         prev_zinc = zincbr_adj) %>%
  split_year(.) %>%
  select(country, year, age, prev_zinc)
data_list[[6]] <- data_list[[6]] %>%
  rename(country_year = Survey,
         age = `age (yr)`,
         prev_zinc = zincvmn_prev ) %>%
  split_year(.) %>%
  select(country, year, age, prev_zinc)


saveRDS(data_list, file = here("data/clean_brinda_data.rds"))

#get list of unique countries and years
country_years <- bind_rows(
  data_list$zinc_women %>% select(country, year),
  data_list$zinc_child %>% select(country, year),
  data_list$vita_women %>% select(country, year),
  data_list$vita_child %>% select(country, year),
  data_list$iron_women %>% select(country, year),
  data_list$iron_child %>% select(country, year)
) %>% distinct()

country_years %>% distinct(country) %>% as.vector() %>% dput()
summary(country_years$year)
