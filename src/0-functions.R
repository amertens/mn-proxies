

makeVlist <- function(dta) {
  labels <- sapply(dta, function(x) attr(x, "label"))
  tibble(name = names(labels),
         label = as.character(labels))
}


clean_DHS <- function(dfull){
  HR_clean <- clean_HR(dfull$HRdata)
  PR_clean <- clean_PR(dfull$PRdata)

  # Merge household data into the PR data:
  PR_merged <- PR_clean %>%
    left_join(HR_clean, by = c("cluster","hh"))

  IR_clean <- clean_IR(dfull$IRdata)

  # Merge IR (women’s) data into PR_merged for female lines:
  PR_IR_merged <- PR_merged %>%
    left_join(IR_clean, by = c("cluster","hh","line"))
  head(PR_IR_merged)

  table(PR_IR_merged$anemia_cat)

  d <- PR_IR_merged %>%
    mutate(mod_sev_anemia=case_when(anemia_cat==1 | anemia_cat==2 ~ 1,
                                    anemia_cat==3 | anemia_cat==4~ 0,
                                    TRUE ~ NA)) %>%
    filter(!is.na(mod_sev_anemia))

  if(nrow(d)==0){
    cat("No anemia data\n")
  }else{
    return(d)
  }

}



#fix getDHSdata functions to correctly select the country instead of all countries
getDHSdata <- function(country, indicator = NULL, Recode = NULL, year){
  IR_Individual <- c("ancvisit4+", "RH_ANCN_W_N4P", "womananemia",
                     "AN_ANEM_W_ANY", "unmet_family", "FP_NADA_W_UNT", "FP_CUSA_W_MOD",
                     "AN_NUTS_W_THN")
  PR_Household_Member <- c("CN_ANMC_C_ANY", "CN_NUTS_C_WH2",
                           "wasting", "CN_NUTS_C_HA2", "stunting", "ML_PMAL_C_RDT",
                           "WS_TLET_H_IMP", "WS_TLET_P_BAS", "WS_SRCE_P_BAS")
  KR_Children <- c("CH_DIAT_C_ORT", "DPT3", "CH_VACC_C_DP3",
                   "CH_VACC_C_DP1", "CH_VACC_C_BAS", "CH_VACC_C_NON", "CN_BRFS_C_EXB",
                   "CH_VACC_C_MSL", "PCV3", "RotaC1")
  BRdata_Birth <- c("RH_DELA_C_SKP", "CM_ECMR_C_NNR", "nmr")
  HRdata_Household <- c("ML_NETP_H_IT2")
  HIV <- c("HA_HIVP_B_HIV")
  indicator <- indicator
  if (is.null(indicator) & is.null(Recode)) {
    Type <- NULL
  }
  else if (!is.null(Recode)) {
    Type = Recode
  }
  else if (indicator %in% IR_Individual) {
    Type <- c("Individual Recode")
  }
  else if (indicator %in% PR_Household_Member) {
    Type <- c("Household Member Recode")
  }
  else if (indicator %in% KR_Children) {
    Type <- c("Children's Recode")
  }
  else if (indicator %in% BRdata_Birth) {
    Type <- c("Births Recode")
  }
  else if (indicator %in% HRdata_Household) {
    Type <- c("Household Recode")
  }
  else if (indicator %in% HIV) {
    Type <-
      c("Individual Recode", "Men's Recode", "HIV Test Results Recode")
  }
  else {
    Type <- NULL
  }
  if (!is.null(Type)) {
    message(paste(Type, "is used.\n\n"))
  }
  else {
    message("All DHS files are downloaded.\n\n")
  }
  CountryName <- stringr::str_to_title(country)
  country_vec <- rdhs::dhs_countries()$CountryName == CountryName
  dhs_countries_data <- dhs_countries()
  countryId <- dhs_countries_data[country_vec, ]
  potential_surveys <-
    rdhs::dhs_datasets(countryIds = countryId$DHS_CountryCode, surveyYear = year) %>% dplyr::filter(FileFormat == "Stata dataset (.dta)")
  if (length(Type) == 1) {
    surveys <- potential_surveys %>% dplyr::filter(FileType ==
                                                     c(Type))
    data.paths.tmp <- get_datasets(surveys[surveys$SurveyYear ==
                                             year,]$FileName, clear_cache = T)
    Rdata <- readRDS(paste0(data.paths.tmp))
    return(Rdata)
  }
  else if (length(Type) > 1) {
    surveys <- potential_surveys %>% dplyr::filter(FileType %in%
                                                     c(Type))
    data.paths.tmp <- get_datasets(surveys[surveys$SurveyYear ==
                                             year,]$FileName, clear_cache = T)
    all = list()
    listname = surveys$FileType
    for (i in 1:length(Type)) {
      all[[listname[i]]] <- readRDS(paste0(data.paths.tmp[i]))
    }
    return(all)
  }
  else if (is.null(Type)) {
    all <- NULL
    list <- c(
      "Men's Recode",
      "Household Member Recode",
      "Children's Recode",
      "Births Recode",
      "Couples' Recode",
      "Household Recode",
      "Individual Recode"
    )
    listname <- c("MRdata",
                  "PRdata",
                  "KRdata",
                  "BRdata",
                  "CRdata",
                  "HRdata",
                  "IRdata")
    for (i in 1:length(list)) {
      Type <- list[i]
      surveys <- potential_surveys %>% dplyr::filter(FileType ==
                                                       c(Type))
      if (dim(surveys)[1] == 0) {

      }
      else {
        data.paths.tmp <- get_datasets(surveys[surveys$SurveyYear ==
                                                 year,]$FileName, clear_cache = T)
        Rdata <- readRDS(paste0(data.paths.tmp))
        all[[listname[i]]] <- Rdata
      }
    }
    return(all)
  }
}

getDHSgeo <- function(country, year){
  CountryName <- stringr::str_to_title(country)
  country_vec <- rdhs::dhs_countries()$CountryName == CountryName
  dhs_countries_data <- dhs_countries()
  countryId <- dhs_countries_data[country_vec, ]
  surveys <- rdhs::dhs_datasets(countryIds = countryId$DHS_CountryCode,
                                surveyYear = year) %>% dplyr::filter(FileType == "Geographic Data")
  data.paths.tmp <- get_datasets(surveys[surveys$SurveyYear == year, ]$FileName, clear_cache = T)
  geo <- readRDS(paste0(data.paths.tmp))
  return(geo)
}

