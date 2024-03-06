
#------------------------------------------------------------------------------
# download data:
# https://tech.popdata.org/dhs-research-hub/posts/2024-02-02-download-dhs-data/
# Merging in precipitation with CHIRPS
# https://tech.popdata.org/dhs-research-hub/posts/2024-02-04-dhs-chirps/

# Could also use malaria atlas project:
# https://www.rdocumentation.org/packages/malariaAtlas/versions/1.5.1/topics/fillDHSCoordinates
#------------------------------------------------------------------------------

library(rdhs)
library(data.table)
library(tidyverse)
library(survey)
library(haven)
library(ipumsr)
library(sf)


#Set up account
set_rdhs_config(email = "amertens@berkeley.edu",
                project = "MN Prediction",
                data_frame = "data.table::as.data.table",
                config_path = "~/.rdhs.json",
                global = TRUE)


#maybe check if my password can't have special characters

indicators <- dhs_indicators()
unique(indicators$Label)
tail(indicators[grepl("gps", Label), .(IndicatorId, ShortName, Label)])
tail(indicators[grepl("anemia", Label), .(IndicatorId, ShortName, Label)])

countries <- dhs_countries() %>% as.data.table()
dhscc <- countries[CountryName %in% c("Burkina Faso"), DHS_CountryCode]
dhscc

#NEED to figure out how to download gps
#BFGE61FL.ZIP is BF gps data

surveychar <- dhs_survey_characteristics()
surveychar[grepl("gps", SurveyCharacteristicName, ignore.case=TRUE)]
surveychar[grepl("anthro", SurveyCharacteristicName, ignore.case=TRUE)]
#26 for GPS, 10 for anthropometry

#dhs_surveys(returnFields=c("SurveyCharacteristicIds","SurveyId", "SurveyYearLabel","SurveyType","CountryName"))

#Figure out what the 41 references
anthro_survey <- dhs_surveys(surveyCharacteristicIds = 10, surveyYear=2010, countryIds = dhscc) %>% as.data.table()
gps_survey <- dhs_surveys(surveyCharacteristicIds = 26, surveyYear=2010, countryIds = dhscc) %>% as.data.table()
gps_survey[,.(SurveyId, CountryName, SurveyYear, NumberOfWomen, SurveyNum, FieldworkEnd)]
#looks like in the same file


datasets <- dhs_datasets(surveyIds = gps_survey$SurveyId, fileType = "IR", fileFormat="flat") %>% as.data.table()
datasets[, .(SurveyId, SurveyNum, FileDateLastModified, FileName)]

datasets$path <- unlist(get_datasets(datasets$FileName, clear_cache=F)) #Clear cache if you are getting a data access error and you do have data access
datasets

d <- readRDS(datasets$path[1])
head(d)

#To recode Z-scores
d$v440[d$v440>9990] <- NA
d$v440 <- d$v440/100
summary(d$v440)

#search through variables
search_variable_labels(datasets$FileName[1], "height")


# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).


library(ipumsr)

dhs <- read_ipums_micro(
  ddi = "data/dhs/idhs_00015.xml",
  data_file = "data/dhs/idhs_00015.dat.gz",
  verbose = TRUE
)

# ddi <- read_ipums_ddi("idhs_00001.xml")
# data <- read_ipums_micro(ddi)


# # Load IPUMS DHS extract
# dhs <- read_ipums_micro(
#   ddi = "data/dhs/idhs_00018.xml",
#   data_file = "data/dhs/idhs_00018.dat.gz",
#   verbose = FALSE
# )

# Select a subset of variables
dhs <- dhs |>
  select(SAMPLE, YEAR, IDHSPID, IDHSHID, DHSID, URBAN, HWHAZWHO)

dhs
