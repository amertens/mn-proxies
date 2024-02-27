
#Introduction vignette
#https://docs.ropensci.org/rdhs/articles/introduction.html

#------------------------------------------------------------------------------
# Anemia example
# https://docs.ropensci.org/rdhs/articles/anemia.html
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# Merging in precipitation with CHIRPS
# https://tech.popdata.org/dhs-research-hub/posts/2024-02-04-dhs-chirps/
#------------------------------------------------------------------------------





library(rdhs)
library(data.table)
library(ggplot2)
library(survey)
library(haven)


set_rdhs_config(data_frame = "data.table::as.data.table")

indicators <- dhs_indicators()
tail(indicators[grepl("anemia", Label), .(IndicatorId, ShortName, Label)])

countries <- dhs_countries() %>% as.data.table()
# dhscc <- countries[CountryName %in% c("Armenia", "Cambodia", "Lesotho"), DHS_CountryCode]
# dhscc

dhscc <- countries[CountryName %in% c("Bangladesh"), DHS_CountryCode]
dhscc


statcomp <- dhs_data(indicatorIds = "AN_ANEM_W_ANY", countryIds = dhscc) %>% as.data.table()
statcomp[,.(Indicator, CountryName, SurveyYear, Value, DenominatorWeighted)]

ggplot(statcomp, aes(SurveyYear, Value, col=CountryName)) +
  geom_point() + geom_line()

surveychar <- dhs_survey_characteristics()
surveychar[grepl("Vitamin A questions", SurveyCharacteristicName, ignore.case=TRUE)]

surveys <- dhs_surveys(surveyCharacteristicIds = 41, countryIds = dhscc) %>% as.data.table()
surveys[,.(SurveyId, CountryName, SurveyYear, NumberOfWomen, SurveyNum, FieldworkEnd)]

datasets <- dhs_datasets(surveyIds = surveys$SurveyId, fileType = "IR", fileFormat="flat") %>% as.data.table()
datasets[, .(SurveyId, SurveyNum, FileDateLastModified, FileName)]

#Download datasets
## set up your credentials
set_rdhs_config(email = "amertens@berkeley.edu", project = "MN Prediction")
datasets$path <- unlist(get_datasets(datasets$FileName))




