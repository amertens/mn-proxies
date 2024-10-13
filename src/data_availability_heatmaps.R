

# Load required libraries
library(tidyverse)
library(ggplot2)
library(readxl)
library(here)
library(patchwork)

# Function to read and process data
read_and_process <- function(file_path, sheet_name = NULL, year_col = "Year", available_cols = NULL) {
  data <- read_excel(here(file_path), sheet = sheet_name)
  data <- data %>%
    rename(Country = Economy) %>%
    mutate(Year = as.numeric(!!sym(year_col))) %>%
    filter(!is.na(Year), Year != "X")

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
hanci_data <- read_and_process("landscape data/Landscape data sources/2. Hunger and Nutrition Commitment Index Africa.xlsx")
fews_data <- read_and_process("landscape data/Landscape data sources/Famine Early Warning Systems Network.xlsx",
                              year_col = "Years of available studies")
ch_data <- read_and_process("landscape data/Landscape data sources/Cadre Harmonise 2.xlsx")
faostat_data <- read_and_process("landscape data/Landscape data sources/FAOSTAT-FBS-SUA-5-21-24.xlsx",
                                 sheet_name = "Landscape sheet",
                                 available_cols = c("Food Balance Sheet (FBS)- new methodology", "Supply and Utilization Accounts (SUA)"))

# Read DHS data
dhs_data <- read_excel(here("landscape data/Landscape data sources/Landscape_data source_DHS_2024-04-09.xlsx"))
dhs_data <- dhs_data %>%
  rename(Country = Economy) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(Year)) %>%
  mutate(Available = 1)

# Combine all datasets for the first heatmap
combined_data <- bind_rows(
  mutate(hanci_data, Source = "HANCI"),
  mutate(fews_data, Source = "FEWS"),
  mutate(ch_data, Source = "Cadre Harmonise"),
  mutate(faostat_data, Source = "FAOSTAT"),
  mutate(dhs_data, Source = "DHS")
)

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

# Create multipanel plot
p1 <- ggplot(final_data, aes(x = Year, y = Country, fill = factor(Available))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "lightgrey", "1" = "darkgreen"),
                    name = "Data Available",
                    labels = c("No", "Yes")) +
  facet_grid(. ~ Source) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 6)) +
  labs(title = "Data Availability Comparison",
       x = "Year",
       y = "Country")

ggsave(p1, file="figures/combined_data_availability_heatmap.png", width = 20, height = 12, units = "in", dpi = 300)


dhs_indicators_wide <- read_excel(here("landscape data/Landscape data sources/Landscape_data source_DHS_2024-04-09.xlsx"), sheet=2) %>%
  rename(Country = Economy)

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

ggsave(p2, file="figures/dhs_indicator_availability_heatmap_by_year.png", width = 20, height = 12, units = "in", dpi = 300)

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
        axis.text.y = element_text(size = 6)) +
  labs(title = "DHS Indicator Availability by Country",
       y = "Indicator",
       x = "Country")
p3

ggsave(p3, file="figures/dhs_indicator_availability_heatmap.png", width = 20, height = 12, units = "in", dpi = 300)

