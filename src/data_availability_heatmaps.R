

# Load required libraries
rm(list=ls())
library(tidyverse)
library(ggplot2)
library(readxl)
library(here)
library(patchwork)
here()

# Function to read and process data
file_path="Landscape data sources/2. Hunger and Nutrition Commitment Index Africa.xlsx"
sheet_name = "Data (2)"
read_and_process <- function(file_path, sheet_name = NULL, year_col = "Year", available_cols = NULL) {

  data <- read_excel(here(file_path), sheet = sheet_name)
  try(  data <- data %>% rename(Country = Economy))
  try(  data <- data %>% rename(Country = country ))
  data <- data %>% mutate(Year = (!!sym(year_col)))
  data <- data %>% filter(!is.na(Year), Year != "X")

  #if year is a range aka 1999-2000, just select the first one
  data$Year <- as.numeric(str_extract(data$Year, "\\d{4}"))

  if (!is.null(available_cols)) {
    data <- data %>%
      pivot_longer(cols = all_of(available_cols), names_to = "DataType", values_to = "Available") %>%
      mutate(Available = ifelse(Available == "Yes", 1, 0))
  } else {
    data <- data %>% mutate(Available = 1)
  }

  return(data %>% select(Country, Year, Available))
}

# Read and process data
hanci_data <- read_and_process("Landscape data sources/2. Hunger and Nutrition Commitment Index Africa.xlsx")
fews_data <- read_and_process("Landscape data sources/Famine Early Warning Systems Network.xlsx",
                              year_col = "Years of available studies")
ch_data <- read_and_process("Landscape data sources/Cadre Harmonise 2.xlsx")
faostat_data <- read_and_process("Landscape data sources/FAOSTAT-FBS-SUA-5-21-24.xlsx",
                                 sheet_name = "Landscape sheet",
                                 available_cols = c("Food Balance Sheet (FBS)- new methodology", "Supply and Utilization Accounts (SUA)"))
ghed_data <- read_and_process("Landscape data sources/Landscape_data source_GHED_2024_08_30.xlsx",
                              sheet_name = "Data (2)", year_col = "year",
                              available_cols = NULL)
flunet_data <- read_and_process("Landscape data sources/Landscape_data source-FluNet_2024-8-30.xlsx",
                              sheet_name = "Sheet1", year_col = "Year",
                              available_cols = NULL)
wfp_data <- read_and_process("Landscape data sources/Landscape_data source-WFP_FoodPrices_2024-7-9.xlsx",
                                sheet_name = "Sheet1", year_col = "Year",
                                available_cols = NULL)
hces_data <- read_excel(here("Landscape data sources/Landscape_data source-HCES_2024-2-14.xlsx"), sheet= "HCES")  %>%
  rename(Country = Economy) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(`HCES (Y/N)` =="Y") %>%
  mutate(Available = 1)  %>% select(Country, Year, Available)




# Read DHS data
dhs_data <- read_excel(here("Landscape data sources/Landscape_data source_DHS_2024-04-09.xlsx"))
dhs_data <- dhs_data %>%
  rename(Country = Economy) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(Year)) %>%
  mutate(Available = 1)


mics_data <- read_excel(here("Landscape data sources/Landscape_data source_MICS_2024-04-01.xlsx"))
mics_data <- mics_data %>%
  rename(Country = Economy) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(Year), Available=="yes") %>%
  mutate(Available = 1)


vmnis_data <- read_excel(here("Landscape data sources/Landscape_data source-VMNIS_2024-02-15.xlsx"), sheet="List of economies")
vmnis_data <- vmnis_data %>%
  rename(Country = Economy) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(Available = 1)






# Combine all datasets for the first heatmap

# Combine all datasets including global health expenditure data
combined_data <- bind_rows(
  mutate(hanci_data, Source = "HANCI"),
  mutate(hces_data, Source = "HCES"),
  mutate(fews_data, Source = "FEWS"),
  mutate(ch_data, Source = "Cadre\nHarmonise"),
  mutate(faostat_data, Source = "FAOSTAT"),
  mutate(flunet_data, Source = "FluNet"),
  mutate(wfp_data, Source = "WFP"),
  mutate(dhs_data, Source = "DHS"),
  mutate(mics_data, Source = "MICS"),
  mutate(ghed_data, Source = "GHED"),
  mutate(vmnis_data, Source = "VMNIS")
) %>% filter(!is.na(Year)) %>%
  select(Source, Country, Year, Available)



# Create complete grid
all_countries <- unique(combined_data$Country)
all_years <- seq(min(combined_data$Year), max(combined_data$Year), by = 1)
all_sources <- unique(combined_data$Source)

complete_grid <- expand_grid(Country = all_countries, Year = all_years, Source = all_sources)

# Merge complete grid with data
final_data <- complete_grid %>%
  left_join(combined_data, by = c("Country", "Year", "Source")) %>%
  mutate(Available = ifelse(is.na(Available), 0, Available)) %>%
  #subset to 2010-2024
  filter(Year >= 2010, Year <= 2024)

#NOTE! Need to standardize the country names to match each other
unique(final_data$Country)

final_data$Country[final_data$Country=="Cote D'Ivoire"] <- "Côte d'Ivoire"
final_data$Country[final_data$Country=="Côte d’Ivoire"] <- "Côte d'Ivoire"
final_data$Country[final_data$Country=="Côte d'Ivoire"] <- "Côte d'Ivoire"
final_data$Country[final_data$Country=="Sierra-Leone"] <- "Sierra Leone"
final_data$Country[final_data$Country=="Gambia, The"] <- "Gambia"
final_data$Country[final_data$Country=="Guinea-Bissau"] <- "Guinea Bissau"

final_data <- final_data %>% mutate(Country = as.character(Country)) %>% arrange(Country)
final_data <- final_data %>% mutate(Country = factor(Country, levels=rev(unique(final_data$Country))))

# Create multipanel plot
p1 <- ggplot(final_data, aes(x = Year, y = Country, fill = factor(Available))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "lightgrey", "1" = "darkgreen"),
                    name = "Data Available",
                    labels = c("No", "Yes")) +
  facet_wrap(. ~ Source, nrow=2) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 4)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 8)) +
  labs(title = "Comparison of Data Availability by Year and Country Across Select Databases",
       x = "Year",
       y = "Country")
p1

ggsave(p1, file="figures/combined_data_availability_heatmap.jpeg", width = 8, height = 6)


dhs_indicators_wide <- read_excel(here("Landscape data sources/Landscape_data source_DHS_2024-04-09.xlsx"), sheet=2) %>%
  rename(Country = Economy)
unique(dhs_indicators_wide$Year)

# Process DHS indicator data
dhs_indicators <- dhs_indicators_wide %>%
  select(-c(Code, Region, Year, Survey, `Datasets Available`)) %>%
  pivot_longer(cols = -Country, names_to = "Indicator", values_to = "Available") %>%
  mutate(Available = ifelse(Available == "X", 1, 0))

#dont drop year and region, but keep as identifiers in the pivot
dhs_indicators <- dhs_indicators_wide %>%
  filter( Survey=="DHS") %>% mutate(country_year = paste(Country, Year, sep="_")) %>%
  select(-c(Region, Year, Code, Survey, `Datasets Available`)) %>%
  pivot_longer(cols = -c(Country,country_year), names_to = "Indicator", values_to = "Available") %>%
  mutate(Available = ifelse(Available == "X", 1, 0))

#drop indicators not available in many countries
dhs_indicators$Available[is.na(dhs_indicators$Available)] <- 0
dhs_indicators <- dhs_indicators %>% group_by(Indicator) %>% filter(sum(Available)>2) %>% ungroup()

#sort and reorder indicators based on frequency of availability
dhs_indicator_levels <- dhs_indicators %>% group_by(Indicator) %>% summarise(sum=sum(Available)) %>% arrange(desc(sum)) %>% pull(Indicator)
dhs_indicators <- dhs_indicators %>% mutate(Indicator = factor(Indicator, levels=rev(dhs_indicator_levels)))


# Create heatmap for DHS indicators
p2 <- ggplot(dhs_indicators, aes(x = country_year , y = Indicator, fill = factor(Available))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "lightgrey", "1" = "darkgreen"),
                    name = "Data Available",
                    labels = c("No", "Yes")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.y = element_text(size = 6)) +
  labs(title = "DHS Indicator Availability by country and survey year",
       y = "Indicator",
       x = "Country")

ggsave(p2, file="figures/dhs_indicator_availability_heatmap_by_year.jpeg", width = 10, height = 6)

#now, collapse the data to show availability of indicators by country for any year
head(dhs_indicators)
dhs_indicators_country <- dhs_indicators %>% group_by(Country, Indicator) %>% summarise(Available=1*(sum(Available)>0)) %>% ungroup()
dhs_indicator_levels <- dhs_indicators_country %>% group_by(Indicator) %>% summarise(sum=sum(Available)) %>% arrange(desc(sum)) %>% pull(Indicator)
dhs_indicators_country <- dhs_indicators_country %>% mutate(Indicator = factor(Indicator, levels=rev(dhs_indicator_levels)))

p3 <- ggplot(dhs_indicators_country, aes(y = Indicator, x = Country, fill = factor(Available))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "lightgrey", "1" = "darkgreen"),
                    name = "Data Available",
                    labels = c("No", "Yes")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.y = element_text(size = 8)) +
  labs(title = "DHS Indicator\nAvailability by Country",
       y = "Indicator",
       x = "Country")
p3

ggsave(p3, file=here("figures/dhs_indicator_availability_heatmap.jpeg"), width = 5, height = 8)




#updated format
# Summarize the data by Country and Source over the selected period
# Summarize the data by Country and Source over the selected period
survey_summary <- final_data %>%
  filter(Year >= 2010) %>%
  group_by(Country, Source) %>%
  summarize(n_surveys = sum(Available),
            recent_year = ifelse(n_surveys > 0, max(Year[Available == 1]), NA)) %>%
  ungroup()

# Create the bubble chart with text labels for the most recent year
ggplot(survey_summary, aes(x = Source, y = Country)) +
  geom_point(aes(size = n_surveys, fill = recent_year),
             shape = 21, color = "black") +
  geom_text(aes(label = ifelse(n_surveys > 0, recent_year, "")),
            color = "grey20", size = 3, fontface = "bold") +
  scale_size_continuous(range = c(3, 12)) +
  scale_fill_gradient(low = "blue", high = "lightblue",
                      na.value = "grey", name = "Most Recent Survey") +
  theme_minimal() +
  labs(title = "Survey Count and Recency by Data Source and Country",
       x = "Data Source", y = "Country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
