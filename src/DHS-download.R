
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





library(rdhs)
library(data.table)
library(ggplot2)
library(survey)
library(haven)

#Set up account
set_rdhs_config(email = "amertens@berkeley.edu",
                project = "MN Prediction",
                data_frame = "data.table::as.data.table",
                config_path = "~/.rdhs.json",
                global = TRUE)
#Check account
config <-get_rdhs_config()
rdhs:::authenticate_dhs(config)

avail_data=rdhs:::available_datasets(config)
avail_data

#maybe check if my password can't have special characters

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
# set_rdhs_config(email = "amertens@berkeley.edu",
#                 project = "MN Prediction",
#                 config_path = "~/.rdhs.json",
#                 global = TRUE)
datasets$path <- unlist(get_datasets(datasets$FileName))


survs <- dhs_surveys(surveyCharacteristicIds = 89,
                     countryIds = c("CD","TZ"),
                     surveyType = "DHS",
                     surveyYearStart = 2013)

# and lastly use this to find the datasets we will want to download and let's download the flat files (.dat) datasets (have a look in the dhs_datasets documentation for all argument options, and fileformat abbreviations etc.)
datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "flat")
str(datasets)

#microbenchmark::microbenchmark(dhs_surveys(surveyYear = 1992),times = 1)

# download datasets
downloads <- get_datasets(datasets$FileName, clear_cache=T)

downloads <- get_datasets('AFBR71DT.ZIP')

dataset_filenames='AFBR71DT.ZIP'

 download_option = "rds"
 reformat = FALSE
         all_lower = TRUE
         output_dir_root = NULL
         clear_cache = FALSE

  client <- check_for_client()
  if (is.null(output_dir_root)) {
    output_dir_root <- file.path(client$get_root(), "datasets")
  }
  client$get_datasets(dataset_filenames, download_option = download_option,
                      reformat = reformat, all_lower = all_lower, output_dir_root = output_dir_root,
                      clear_cache = clear_cache)



  datasets <- dhs_datasets(surveyIds = 'AF2015DHS', fileFormat = "flat")
  datasets

  downloads <- get_datasets(datasets$FileName, clear_cache=T)

 df <- readRDS(downloads$AFBR71FL)
head(df)
