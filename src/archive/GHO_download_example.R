

#https://medium.com/@kirstin.lyon/connect-to-global-health-observatory-who-data-api-with-r-fef4315693a1
library(glue)
library(httr)
library(jsonlite)
library(tidyverse)


## Global Variables ----

URL_BASE <-  "https://ghoapi.azureedge.net/api/"

## Global Variables ----
SADC <- c("AGO", "NAM", "ZAF", "LSO", "SWZ",
          "BWA", "ZWE", "ZMB", "MOZ", "MWI",
          "MDG", "COM", "SYC", "MUS")

EAC <- c("BDI", "KEN", "RWA", "SSD", "TZA", "UGA", "COD")

ECOWAS  <- c("BEN", "BFA", "CPV", "CIV", "GMB", "GHA", "GIN",
             "GNB", "LBR", "MLI", "NER", "NGA", "SEN", "SLE", "TGO")

COUNTRY <- c(SADC, EAC, ECOWAS)

INDICATOR_TEXT <- "tuberculosis"
INDICATOR_TEXT_TB <- "tb"
INDICATOR <- "TB_"

convert_JSON_to_tbl <- function(url){
  data <- GET(url)
  data_df <- fromJSON(content(data, as = "text", encoding = "utf-8"))
  data_tbl <-  map_if(data_df, is.data.frame, list) %>%
    as_tibble %>%
    unnest(cols = c(value)) %>%
    select(-'@odata.context')
}


# Find what you need

all_dimension <-  convert_JSON_to_tbl("https://ghoapi.azureedge.net/api/Dimension")
all_countries <-  convert_JSON_to_tbl(glue(URL_BASE,"Dimension/COUNTRY/DimensionValues")) %>%
  select(Code, Title)
all_indicators <-  convert_JSON_to_tbl(glue(URL_BASE,"Indicator")) %>%
  select(-Language)


#Select a few based on globals
# indicators_from_code <- all_indicators %>%
#   filter(grepl(INDICATOR, IndicatorCode, ignore.case = TRUE))
#
# indicators_from_text <- all_indicators %>%
#   filter(grepl(INDICATOR_TEXT, IndicatorName, ignore.case = TRUE))
#
# indicators_from_text_tb <- all_indicators %>%
#   filter(grepl(INDICATOR_TEXT_TB, IndicatorName, ignore.case = TRUE))

indicators_from_code <- all_indicators
indicators_from_text <- all_indicators
indicators_from_text_tb <- all_indicators

indicators <- indicators_from_code %>%
  union(indicators_from_text) %>%
  union(indicators_from_text_tb)

indicator_codes <- indicators %>%
  pull(IndicatorCode)


indicator_data <- map(indicator_codes, function(id) {
  response <- GET(
    glue(URL_BASE, id))
  content(response, "text")
})


indicator_data_tbl <- map_dfr(indicator_data, ~ fromJSON(.x,
                                                         simplifyVector = TRUE)
                              %>% as_tibble()
                              %>% unnest(cols = everything()))

dim(indicator_data_tbl)

saveRDS(indicator_data_tbl, here::here("data/GHO_indicator_data_raw.rds"))

# head(indicator_data_tbl)
#
# "HIV_0000000012" %in% indicator_data_tbl$IndicatorCode
# "HIV_0000000012" %in% indicator_codes

indicator_df <- indicator_data_tbl %>% mutate(NumericValue=as.numeric(NumericValue)) %>%
  filter(SpatialDimType == "COUNTRY", !is.na(NumericValue)) %>%
  subset(., select= -c(value, NumericValue, `@odata.context`, Id, SpatialDimType,
                       TimeDimType,DataSourceDim,DataSourceDimType,
                       Low, High, Comments, Date, TimeDimensionBegin, TimeDimensionEnd))

#subset indicators
indicator_df <- indicator_df %>%
  filter(!grepl("_ARCHIVED", IndicatorCode, ignore.case = TRUE),
         !(indicator_df$Dim1Type %in% c("AGEGROUP", "RADONSUBQUESTIONS", "TOBACCO_NICOTINE_PRODUCT",
                                        "ALCOHOLTYPE", "ASSISTIVETECHPRODUCT", "SUBSTANCETYPEDISORDER",
                                        "SUBSTANCETYPE","CAREPATIENT","BEVERAGETYPE","ROADUSERTYPE",
                                        "SOCIALCOSTTYPE","UNCERTAINTY_INTERVAL","AGEGROUP",
                                        "TB_TREATMENTTYPE","TB_CASETYPE","TB_BENCHMARK","TB_TREATHISTORY"  )))

unique(indicator_df$IndicatorCode)

unique(indicator_df$Dim1Type)
unique(indicator_df$IndicatorCode[indicator_df$Dim1Type=="TB_TREATHISTORY" & !is.na(indicator_df$Dim1Type)])


saveRDS(indicator_df, here::here("data/GHO_indicator_data.rds"))

indicator_df <- readRDS(here::here("data/GHO_indicator_data.rds"))

#Transform to wide
head(indicator_df)
colnames(indicator_df)
gc()


indicator_df_wide <- indicator_df %>%
  filter(#IndicatorCode %in% c("HIV_0000000011", "HIV_0000000012"),
         !is.na(NumericValue)) %>%
  group_by(IndicatorCode, SpatialDim, ParentLocationCode, ParentLocation, Dim1Type, Dim1, TimeDim, Dim2Type, Dim2, Dim3Type, Dim3, TimeDimensionValue) %>%
  summarise(NumericValue = mean(NumericValue)) %>%
  pivot_wider(
    id_cols = c(SpatialDim, ParentLocationCode, ParentLocation, Dim1Type, Dim1, TimeDim, Dim2Type, Dim2, Dim3Type, Dim3, TimeDimensionValue),
    names_from = IndicatorCode,
    values_from = NumericValue
  )

saveRDS(indicator_df_wide, here::here("data/GHO_indicator_data_wide.rds"))
head(indicator_df_wide)

