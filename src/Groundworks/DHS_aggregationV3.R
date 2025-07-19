# load our package
library(rdhs)
library(ggplot2)


svy_year=2016

#get survey ID
d <- dhs_data(countryIds = "GH",
              indicatorIds = "FE_FRTR_W_A15",
              surveyYearStart = svy_year,
              breakdown = "subnational")

# get our related spatial data frame object
sp <- download_boundaries(surveyId = d$SurveyId[1])


dhs_indicators()
dhs_indicators_df <- dhs_indicators(returnFields=c("IndicatorId","ShortName","Label","Definition"," Level1", "Level2", "Level3", "MeasurementType", "Denominator"))
write.csv(dhs_indicators_df, file = here("data/DHS/clean/dhs_indicators_metadata.csv"), row.names = FALSE)

unique(dhs_indicators_df$Level1)
unique(dhs_indicators_df$Level2)
unique(dhs_indicators_df$Level3)

#for(i in 1:2)
for(i in 1:nrow(dhs_indicators_df)) {
  cat(paste0(dhs_indicators_df$IndicatorId[i], ": ", dhs_indicators_df$Label[i]), "\n")
  # make request
  d <- NULL
  try(
  d <- dhs_data(countryIds = "GH",
                indicatorIds = dhs_indicators_df$IndicatorId[i],
                surveyYearStart = svy_year,
                breakdown = "subnational"))
  if(!is.null(d) & length(unique(d$Value)) > 1) {
    # match our values to the regions
    m <- d$Value[match(sp$sdr_subnational_boundaries$REG_ID, d$RegionId)]
    sp$sdr_subnational_boundaries$Value <- m
    colnames(sp$sdr_subnational_boundaries)[colnames(sp$sdr_subnational_boundaries) == "Value"] <- dhs_indicators_df$IndicatorId[i]

  }


}

head(sp$sdr_subnational_boundaries)
head(sp$sdr_subnational_boundaries)[,1:30]



# Use ggplot and geom_sf to plot

sp$sdr_subnational_boundaries$SV_BACK_M_UNW

indicator="SV_BACK_M_UNW"
ggplot(sp$sdr_subnational_boundaries) +
  #geom_sf(aes(fill = !!(indicator)   )) +
  geom_sf(aes(fill =  SV_BACK_M_UNW  )) +
  ggtitle(dhs_indicators_df$Definition[dhs_indicators_df$IndicatorId == indicator])


#save the spatial data frame
saveRDS(sp$sdr_subnational_boundaries, file = here("data/DHS/clean/Ghana_2016_dhs_aggregation.rds"))

