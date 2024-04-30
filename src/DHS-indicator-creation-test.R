
#https://docs.ropensci.org/rdhs/

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


## call with no arguments to return all characterstics
sc <- dhs_survey_characteristics()
sc[grepl("Malaria", sc$SurveyCharacteristicName), ]
ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode"))
survs <- dhs_surveys(surveyCharacteristicIds = 89, countryIds = c("CD","TZ"), surveyYearStart = 2013)
datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "FL", fileType = "PR")
downloads <- get_datasets(datasets$FileName)


#The get_datasets() function returns a vector with a file path to the saved location of the downloaded datasets. These are read using readRDS():
  cdpr <- readRDS(downloads$CDPR61FL)
#Value labels are stored as attributes to each of the columns of the data frame using the labelled class (see haven::labelled o).


# rapid diagnostic test search (I think this could all be used for)
vars <- search_variable_labels(datasets$FileName, search_terms = "malaria rapid test")

# and now extract the data
extract <- extract_dhs(vars, add_geo = TRUE)

# and grab the questions from this now utilising the survey variables
 vars <- search_variables(datasets$FileName, variables = c("hv024","hml35"))

# and now extract the data
extract <- extract_dhs(vars, add_geo = FALSE)

# now let's try our second extraction
extract <- rbind_labelled(extract,
                          labels = list("hv024" = "concatenate",
                                        "hml35" = c("Neg"=0, "Pos"=1)))

# identify questions but specifying the reformat argument
questions <- search_variables(datasets$FileName, variables = c("hv024", "hml35"),reformat=TRUE)

# and now extract the data
extract <- extract_dhs(questions, add_geo = FALSE)

# group our results
extract <- rbind_labelled(extract)

# our hv024 variable is now just character strings, so you can decide when/how to factor/label it later
str(extract)


#Try this with FE_FRTR_W_A10 (Age-specific fertility rate for the three years preceding the survey for age group 10-14 expressed per 1,000 women)
 indicators <- dhs_indicators()
 indicators[1]
 indicators$IndicatorId[1]

 # sc <- dhs_survey_characteristics()
 # sc[grepl(indicators$Level1[1], sc$SurveyCharacteristicName), ]
 # sc[grepl(indicators$TagIds[1], sc$SurveyCharacteristicID), ]



 ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode"))
 survs <- dhs_surveys(indicatorIds  = indicators$IndicatorId[1], countryIds = c("CM","BD"), surveyYearStart = 2013)
 datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "FL", fileType = "PR")

#TO DO! How to link indicators to actual survey values
 #  search_variable_labels(datasets$FileName, regex = "ertility")
 #
 # vars <- search_variables(datasets$FileName, variables = indicators$SDRID[1])
 # vars

 datasets$path <- unlist(get_datasets(datasets$FileName))


 d <- readRDS(datasets$path[1])
 head(d)

 #TO DO! How to get the haven labels?
