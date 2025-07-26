

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


# data=cov
# type = "standard"
# add_indicators = TRUE
# prefix = "miss_"
# skip_vars = NULL
# all_vars = FALSE
# remove_constant = TRUE
# remove_collinear = TRUE
# values = NULL
# h2o_glrm = NULL
# glrm_k = 10L
# verbose = FALSE


# impute_missing_values <- function (data, type = "standard", add_indicators = TRUE, prefix = "miss_",
#                                    skip_vars = NULL, all_vars = FALSE, remove_constant = TRUE,
#                                    remove_collinear = TRUE, values = NULL, h2o_glrm = NULL,
#                                    glrm_k = 10L, verbose = FALSE){
#   missing_indicators = NULL
#   new_data = data
#   non_skipped_vars = !colnames(data) %in% skip_vars
#   results = list(type = type, add_indicators = add_indicators,
#                  skip_vars = skip_vars, prefix = prefix)
#   any_nas = which(sapply(colnames(data), function(col) !col %in%
#                            skip_vars && anyNA(data[[col]])))
#   if (verbose) {
#     cat("Found", length(any_nas), "variables with NAs.\n")
#   }
#   if (type == "standard") {
#     if (verbose) {
#       cat("Running standard imputation.\n")
#     }
#     impute_values = vector("list", sum(non_skipped_vars))
#     names(impute_values) = colnames(data[non_skipped_vars])
#     if (all_vars) {
#       loop_over = which(non_skipped_vars)
#       names(loop_over) = colnames(data)[non_skipped_vars]
#     }else{
#       loop_over = any_nas
#     }
#     sum_nas = sapply(loop_over, function(col_i) sum(is.na(data[[col_i]])))
#     col_classes = sapply(loop_over, function(col_i) class(data[[col_i]]))
#     for(i in loop_over){
#       colname = names(loop_over)[loop_over == i]
#       nas = sum_nas[colname]
#       col_class = col_classes[colname]
#       if (verbose) {
#         cat("Imputing", colname, paste0("(", i, " ",
#                                         col_class, ")"), "with", prettyNum(nas, big.mark = ","),
#             "NAs.")
#       }
#       if (colname %in% names(values)){
#         impute_value = values[[colname]]
#         if (verbose) {cat(" Pre-filled.")}
#       }else{
#           if (col_class %in% c("factor")){
#             impute_value = Mode(data[[i]], exclude_na = TRUE)[1]
#           }else{
#             if (col_class %in% c("integer", "numeric", "logical",
#                                  "labelled", "integer64")) {
#               impute_value = median(data[[i]], na.rm = TRUE)
#             }else{
#               warning(paste(colname, "should be numeric or factor type. But its class is",
#                             col_class))
#             }}}
#
#
#       if (verbose) {
#         cat(" Impute value:", impute_value, "\n")
#       }
#       impute_values[[colname]] = impute_value
#       if (nas == nrow(data)) {
#         if (verbose) {
#           cat("Note: cannot impute", colname, "because all values are NA.\n")
#         }
#         next
#       }
#       else if (nas == 0) {
#         next
#       }
#       else {
#         new_data[is.na(data[[i]]), i] = impute_value
#       }
#     }
#
#
#     if (!all_vars) {
#       impute_values = impute_values[names(any_nas)]
#     }
#     results$impute_values = impute_values
#   }
#   else if (type == "knn") {
#     if (verbose) {
#       cat("Running knn imputation. NOTE: this will standardize your data!\n")
#     }
#     if (!"RANN" %in% installed.packages()) {
#       stop("knn imputation requires the RANN package. Please run install.packages(\"RANN\")")
#     }
#     impute_info = caret::preProcess(new_data, method = c("knnImpute"))
#     new_data = predict(impute_info, new_data)
#     results$impute_info = impute_info
#   }
#   else if (type == "glrm") {
#     if (verbose) {
#       cat("Running glrm imputation via h2o.\n")
#     }
#     capture.output({
#       h2o::h2o.init(nthreads = -1)
#     }, split = verbose)
#     capture.output({
#       df_h2o = h2o::as.h2o(new_data[, !names(new_data) %in%
#                                       skip_vars])
#     }, split = verbose)
#     if (is.null(h2o_glrm)) {
#       capture.output({
#         model_glrm = h2o::h2o.glrm(training_frame = df_h2o,
#                                    k = min(ncol(df_h2o), glrm_k), loss = "Quadratic",
#                                    init = "SVD", svd_method = "GramSVD", regularization_x = "None",
#                                    regularization_y = "None", min_step_size = 1e-06,
#                                    max_iterations = 1000)
#       }, split = verbose)
#     }
#     else {
#       model_glrm = h2o_glrm
#     }
#     capture.output({
#       imp_h2o = predict(model_glrm, df_h2o)
#     }, split = verbose)
#     results$h2o_glrm = model_glrm
#     capture.output({
#       glrm_data = as.data.frame(imp_h2o)
#     }, split = verbose)
#     names(glrm_data) = setdiff(names(data), skip_vars)
#     for (colname_i in names(any_nas)) {
#       missing_val = is.na(new_data[[colname_i]])
#       new_data[missing_val, colname_i] = glrm_data[missing_val,
#                                                    colname_i]
#     }
#   }
#   if (add_indicators) {
#     if (length(any_nas) > 0L) {
#       if (verbose) {
#         cat("Generating missingness indicators.\n")
#       }
#       missing_indicators = missingness_indicators(data[,
#                                                        names(any_nas), drop = FALSE], prefix = prefix,
#                                                   remove_constant = remove_constant, remove_collinear = remove_collinear,
#                                                   verbose = verbose)
#       if (verbose) {
#         cat(paste0("Indicators added (", ncol(missing_indicators),
#                    "):"), paste(colnames(missing_indicators),
#                                 collapse = ", "), "\n")
#       }
#       results$indicators_added = colnames(missing_indicators)
#       new_data = cbind(new_data, missing_indicators)
#     }
#   }
#   results$data = new_data
#   results
#
#   #custom
# }
