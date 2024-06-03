
#Introduction vignette
#https://docs.ropensci.org/rdhs/articles/introduction.html
#https://cran.r-project.org/web/packages/rdhs/vignettes/introduction.html

#------------------------------------------------------------------------------
# Anemia example
# https://docs.ropensci.org/rdhs/articles/anemia.html
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# Merging in precipitation with CHIRPS
# https://tech.popdata.org/dhs-research-hub/posts/2024-02-04-dhs-chirps/
#------------------------------------------------------------------------------

rm(list=ls())
library(rdhs)
library(data.table)
library(tidyverse)
library(survey)
library(haven)

#Set up account
set_rdhs_config(email = "amertens@berkeley.edu",
                project = "MN Prediction",
                data_frame = "data.table::as.data.table",
                config_path = "~/.rdhs.json",
                global = TRUE)
#sctXX5E87xJbD*

#Check account
config <-get_rdhs_config()
rdhs:::authenticate_dhs(config)

avail_data=rdhs:::available_datasets(config)
avail_data


indicators <- dhs_indicators()
tail(indicators[grepl("anemia", Label), .(IndicatorId, ShortName, Label)])
write.csv(indicators,paste0(here::here(),"/metadata/dhs_indicators.csv"))

unique(indicators$Level2)

indicator_types <- indicators %>% distinct(Level2, .keep_all=T) %>%
  select(Level2, Label,Level1, Level3) %>%
  rename(`Variable type`=Level2,
         `Example variable`=Label,
         `Module`=Level1,
         `Survey`=Level3)
write.csv(indicator_types,paste0(here::here(),"/metadata/dhs_indicator_types.csv"))



countries <- dhs_countries() %>% as.data.table()
# dhscc <- countries[CountryName %in% c("Armenia", "Cambodia", "Lesotho"), DHS_CountryCode]
# dhscc

dhscc <- countries[CountryName %in% c("Bangladesh"), DHS_CountryCode]
dhscc <- countries[CountryName %in% c("Zambia"), DHS_CountryCode]
dhscc


statcomp <- dhs_data(indicatorIds = "AN_ANEM_W_ANY", countryIds = dhscc) %>% as.data.table()
statcomp[,.(Indicator, CountryName, SurveyYear, Value, DenominatorWeighted)]

ggplot(statcomp, aes(SurveyYear, Value, col=CountryName)) +
  geom_point() + geom_line()

surveychar <- dhs_survey_characteristics()
write.csv(surveychar,paste0(here::here(),"/metadata/dhs_survey_characteristics.csv"))


surveys_all <- dhs_surveys()
write.csv(surveys_all, paste0(here::here(),"/metadata/dhs_survey_all.csv"))

surveychar[grepl("Vitamin A questions", SurveyCharacteristicName, ignore.case=TRUE)]
surveychar[grepl("Micron", SurveyCharacteristicName, ignore.case=TRUE)]
surveys <- dhs_surveys(surveyCharacteristicIds = 20, countryIds = dhscc) %>% as.data.table()

datasets_all <- dhs_datasets()
write.csv(datasets_all, paste0(here::here(),"/metadata/dhs_datasets_all.csv"))



surveys <- dhs_surveys(surveyCharacteristicIds = 41, countryIds = dhscc) %>% as.data.table()
surveys[,.(SurveyId, CountryName, SurveyYear, NumberOfWomen, SurveyNum, FieldworkEnd)]

datasets <- dhs_datasets(surveyIds = surveys$SurveyId, fileType = "IR", fileFormat="flat") %>% as.data.table()
datasets[, .(SurveyId, SurveyNum, FileDateLastModified, FileName)]

#Download datasets
## set up your credentials
# set_rdhs_config(email = "amertens@berkeley.edu",
#                 project = "MN Prediction",
#                 config_path = "~/.rdhs.json",
#                 global = TRUE)
datasets$path <- unlist(get_datasets(datasets$FileName))


d <- readRDS(datasets$path[1])
head(d)
labels(d)

labels <- sapply(d, function(x) attr(x, "label"))
labels[grepl('nutr',labels)]
# multiple micronutrient powder

print_labels(colnames(d))
d$v001

sapply(dat, is.labelled)
dat$v456 <- zap_labels(dat$v456)
dat <- as_factor(dat)

with(dat, table(SurveyId, v025, useNA="ifany"))
with(dat, table(SurveyId, v106, useNA="ifany"))
with(dat, table(SurveyId, v454, useNA="ifany"))
with(dat, table(SurveyId, v455, useNA="ifany"))
with(dat, table(v042, v454, useNA="ifany"))
