

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


data_list[[1]] <- data_list[[1]] %>%
  rename(country_year = Survey,
         age = `age (mo)`,
         prev_ferr = ferrbr_adj,
         prev_stfr = stfrbr_adj) %>%
  select(country_year, age, prev_ferr, prev_stfr)
data_list[[2]] <- data_list[[2]] %>%
  rename(country_year = Survey,
         age = `age (mo)`,
         prev_ferr = ferrbr_adj,
         prev_stfr = stfrbr_adj) %>%
  select(country_year, age, prev_ferr, prev_stfr)
data_list[[3]] <- data_list[[3]] %>%
  rename(country_year = Survey,
         age = `age (mo)`,
         prev_rol = rolbr_adj,
         prev_rbp = rbpbr_adj) %>%
  select(country_year, age, prev_rol, prev_rbp)
data_list[[4]] <- data_list[[4]] %>%
  rename(country_year = Survey,
         age = `age (mo)`,
         prev_rol = rolbr_unadj,
         prev_rbp = rbpbr_unadj) %>%
  select(country_year, age, prev_rol, prev_rbp)
data_list[[5]] <- data_list[[5]] %>%
  rename(country_year = Survey,
         age = `age (mo)`,
         prev_zinc = zincbr_adj) %>%
  select(country_year, age, prev_zinc)
data_list[[6]] <- data_list[[6]] %>%
  rename(country_year = Survey,
         age = `age (yr)`,
         prev_zinc = zincvmn_prev ) %>%
  select(country_year, age, prev_zinc)




# colnames(data_list[[1]])
# colnames(data_list[[2]])

# # Bind all the data frames together into a single data frame
# d_iron <- do.call(bind_rows, data_list[1:2])
# d_vita <- do.call(bind_rows, data_list[3:4])
# d_zinc <- do.call(bind_rows, data_list[5:6])
#
# head(d)
# d <- full_join(d, d_iron, by = "country_year")

saveRDS(data_list, file = here("data/clean_brinda_data.rds"))
