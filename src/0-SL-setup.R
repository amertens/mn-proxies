

#Set up SL components
lrnr_mean <- make_learner(Lrnr_mean)
lrnr_glmnet <- Lrnr_glmnet$new()
random_forest <- Lrnr_randomForest$new()
glm_fast <- Lrnr_glm_fast$new()
nnls_lrnr <- Lrnr_nnls$new()
xgboost_lrnr <- Lrnr_xgboost$new()
ranger_lrnr <- Lrnr_ranger$new()
#gbm_lrnr <- Lrnr_gbm$new()
earth_lrnr <- Lrnr_earth$new()
#dbarts_lrnr <- Lrnr_dbarts$new()
hal_lrnr <- Lrnr_hal9001$new()
#gam_lrnr <- Lrnr_gam$new()
polyspline<-Lrnr_polspline$new()
#screen_cor <- Lrnr_pkg_SuperLearner_screener$new("screen.corP")
#screen_glmnet <- Lrnr_pkg_SuperLearner_screener$new("screen.glmnet")
#cor_nnls_lrnr <- make_learner(Pipeline, screen_cor, glm_fast)
#cor_glm <- make_learner(Pipeline, screen_cor, glm_fast)
#cor_spline <- make_learner(Pipeline, screen_cor, polyspline)
#screened_hal <- make_learner(Pipeline, screen_glmnet, hal_lrnr)



stack <- make_learner(
  Stack,
  lrnr_mean,
  glm_fast,
  #cor_glm,
  lrnr_glmnet,
  ranger_lrnr,
  xgboost_lrnr#,
  #cor_spline#,
  #screened_hal
)

stack <- make_learner(
  Stack,
  lrnr_mean,
  lrnr_glmnet
)

metalearner <- make_learner(Lrnr_nnls)

sl <- make_learner(Lrnr_sl,
                   learners = stack,
                   #loss_function = loss_loglik_binomial,
                   metalearner = metalearner
)
slmod=sl



DHS_SL <- function(d, Xvars, outcome="mod_sev_anemia", folds=2, CV=F){
  X = d %>% select(!!Xvars) %>% as.data.frame()
  cov=unlabelled(X, user_na_to_na = TRUE)
  Y = d[[outcome]]

  #impute missing and Drop near-zero variance predictors
  cov <- cov %>%
    do(impute_missing_values(., type = "standard", add_indicators = T, prefix = "missing_")$data) %>%
    as.data.frame()
  covars = colnames(cov)
  nzv_cols <- nearZeroVar(cov)
  dropped_covars <-  colnames(cov)[nzv_cols]
  cat("Dropping for low variance: ", dropped_covars)
  if(length(nzv_cols) > 0){
    cov <- cov[, -nzv_cols]
  }

  cov <- data.frame(cov)
  covars=colnames(cov)
  dat <-cbind(Y,cov)
  dat <- data.table(dat)


  # create the sl3 task
  set.seed(12345)
  SL_task <- make_sl3_Task(
    data = dat,
    covariates = covars,
    outcome = "Y",
    #id="subjid",
    folds = make_folds(dat, fold_fun = folds_vfold, V = folds)
  )

  #get cross validated fit
  if(CV){
    suppressMessages(cv_sl <- make_learner(Lrnr_cv, slmod, full_fit = TRUE))
    #cv_glm <- make_learner(Lrnr_cv, glm_sl, full_fit = TRUE)
    suppressMessages(sl_fit <- cv_sl$train(SL_task))
    #glm_fit <- cv_glm$train(SL_task)
  }else{
    suppressMessages(sl_fit <- slmod$train(SL_task))
    #glm_fit <- glm_sl$train(SL_task)
  }

  #evaluate performance
  cv_risk_w_sl_revere <- sl_fit$cv_risk(
    eval_fun = loss_squared_error, get_sl_revere_risk = TRUE
  )
  cv_risk_w_sl_revere

  #get outcome predictions
  yhat_full <- sl_fit$predict_fold(SL_task,"validation")
  #yhat_glm <- glm_fit$predict_fold(SL_task,"validation")


  return(list(sl_fit=sl_fit, yhat_full=yhat_full,cv_risk_w_sl_revere=cv_risk_w_sl_revere))
}
