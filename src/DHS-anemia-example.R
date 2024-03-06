
#------------------------------------------------------------------------------
# Anemia example
# https://docs.ropensci.org/rdhs/articles/anemia.html
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


indicators <- dhs_indicators()
tail(indicators[grepl("anemia", Label), .(IndicatorId, ShortName, Label)])

countries <- dhs_countries() %>% as.data.table()
# dhscc <- countries[CountryName %in% c("Armenia", "Cambodia", "Lesotho"), DHS_CountryCode]
# dhscc

dhscc <- countries[CountryName %in% c("Cambodia", "Lesotho"), DHS_CountryCode]
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

datasets$path <- unlist(get_datasets(datasets$FileName, clear_cache=F)) #Clear cache if you are getting a data access error and you do have data access
head(search_variable_labels(datasets$FileName[10], "hemoglobin")[,1:2])


survs <- dhs_surveys(surveyCharacteristicIds = 89,
                     countryIds = c("CD","TZ"),
                     surveyType = "DHS",
                     surveyYearStart = 2013)

# and lastly use this to find the datasets we will want to download and let's download the flat files (.dat) datasets (have a look in the dhs_datasets documentation for all argument options, and fileformat abbreviations etc.)
datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "flat")
str(datasets)

#microbenchmark::microbenchmark(dhs_surveys(surveyYear = 1992),times = 1)

# download datasets
downloads <- get_datasets(datasets$FileName, clear_cache=F) #Clear cache if having access errors

ir <- readRDS(datasets$path[7])
table(as_factor(ir$v042))

table(as_factor(ir$v455))

summary(ir$v456)

search_variable_labels(datasets$FileName[1], "currently.*pregnant")[,1:2]

table(as_factor(ir$v454))

#Extract survey data
vars <- c("SurveyId", "CountryName", "SurveyYear", "v000", "v001", "v005",
          "v012", "v024", "v025", "v106", "v042", "v454", "v455", "v456")
datlst <- list()

for(i in 1:nrow(datasets)){

  if(file.exists(datasets$path[i])){

    print(paste(i, datasets$SurveyId[i]))
    ir <- readRDS(datasets$path[i])

    ir$SurveyId <- datasets$SurveyId[i]
    ir$CountryName <- datasets$CountryName[i]
    ir$SurveyYear <- datasets$SurveyYear[i]

    datlst[[datasets$SurveyId[i]]] <- ir[vars]
  }
}

dat <- rbind_labelled(datlst,
                      labels = list(v024 = "concatenate",
                                    v454 = c("no/don't know" = 0L,
                                             "yes" = 1L, "missing" = 9L)))
sapply(dat, is.labelled)
dat$v456 <- zap_labels(dat$v456)
dat <- as_factor(dat)

with(dat, table(SurveyId, v025, useNA="ifany"))
with(dat, table(SurveyId, v106, useNA="ifany"))
with(dat, table(SurveyId, v454, useNA="ifany"))
with(dat, table(SurveyId, v455, useNA="ifany"))
with(dat, table(v042, v454, useNA="ifany"))

# Calculate anemia prevalence
# Create indicator variable for ‘any anemia’. The threshold depends on pregnancy status.

dat$v456[dat$v456 == 999] <- NA
with(dat, table(v455, is.na(v456)))

dat$anemia <- as.integer(dat$v456  < ifelse(dat$v454 == "yes", 110, 120))
dat$anemia_denom <- as.integer(!is.na(dat$anemia))

#Specify survey design using the survey package.

dat$w <- dat$v005/1e6
des <- svydesign(~v001+SurveyId, data=dat, weights=~w)

anemia_prev <- svyby(~anemia, ~SurveyId, des, svyciprop, na.rm=TRUE, vartype="ci")
anemia_denom <- svyby(~anemia_denom, ~SurveyId, des, svytotal, na.rm=TRUE)

anemia_prev <- merge(anemia_prev, anemia_denom[c("SurveyId", "anemia_denom")])
res <- statcomp[,.(SurveyId, CountryName, SurveyYear, Value, DenominatorUnweighted, DenominatorWeighted)][anemia_prev, on="SurveyId"]

res$anemia <- 100*res$anemia
res$ci_l <- 100*res$ci_l
res$ci_u <- 100*res$ci_u
res$anemia_denom0 <- round(res$anemia_denom)

knitr::kable(res[,.(CountryName, SurveyYear, Value, anemia, ci_l, ci_u, DenominatorWeighted, anemia_denom0)], digits=1)

#Regression analysis: relationship between education and anemia
des <- update(des, v106 = relevel(v106, "primary"))
summary(svyglm(anemia ~ SurveyId + v025 + v106, des, family="binomial"))
