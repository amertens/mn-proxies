



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

# stack <- make_learner(
#   Stack,
#   lrnr_mean,
#   lrnr_glmnet
# )

metalearner <- make_learner(Lrnr_nnls)

sl <- make_learner(Lrnr_sl,
                   learners = stack,
                   loss_function = loss_loglik_binomial,
                   metalearner = metalearner
)
slmod=sl



DHS_SL <- function(d, Xvars, outcome="mod_sev_anemia", id="cluster", folds=5, CV=F, sl){
  X = d %>% select(!!Xvars) %>% as.data.frame()
  cov=unlabelled(X, user_na_to_na = TRUE)
  Y = d[[outcome]]
  id = d[[id]]

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
  dat <-cbind(Y,id,cov)
  dat <- data.table(dat)


  # create the sl3 task
  set.seed(12345)
  SL_task <- make_sl3_Task(
    data = dat,
    covariates = covars,
    outcome = "Y",
    id="id"#,
    #folds = make_folds(dat, fold_fun = folds_vfold, V = folds)
  )

  #get cross validated fit
  if(CV){
    suppressMessages(cv_sl <- make_learner(Lrnr_cv, sl, full_fit = TRUE))
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


  return(list(sl_fit=sl_fit, yhat_full=yhat_full,cv_risk_w_sl_revere=cv_risk_w_sl_revere,
              Xvars=SL_task$column_names))
}











# =============================================================
#  DHS_cluster_SL(): SuperLearner with cluster‑indexed CV
#  ------------------------------------------------------------
#  • Uses the same learner stack (object `slmod`) you already set up
#  • Adds survey weights (if available)            –>  representative predictions
#  • Aggregates individual predictions to clusters –>  prevalence
# =============================================================
DHS_cluster_SL <- function(d,
                           Xvars,
                           outcome      = "mod_sev_anemia",
                           cluster_var  = "cluster",
                           weight_var   = NULL,          # e.g. "wt" once you add it
                           V            = 10,
                           sl) {

  # ---------- 1. outcome, covariates, weights ---------------------------------
  Y      <- d[[outcome]]
  X = d %>% select(!!Xvars) %>% as.data.frame()
  cov=unlabelled(X, user_na_to_na = TRUE)

  cov <- cov %>%
    do(impute_missing_values(., type = "standard", add_indicators = T, prefix = "missing_")$data) %>%
    as.data.frame()

  # drop near‑zero‑variance columns
  nzv <- caret::nearZeroVar(cov)
  if (length(nzv) > 0){
    cov <- cov[ , -nzv]
  }

  # weights?
  if (!is.null(weight_var)) {
    wts <- d[[weight_var]]
  } else {
    wts <- rep(1, nrow(d))
  }

  # ---------- 2.  build sl3 task ----------------------------------------------
  task <- sl3::sl3_Task$new(
    data        = data.frame(Y = Y,cov,
                                         cluster= d[[cluster_var]],
                                         wt       = wts),
    covariates  = names(cov),
    outcome     = "Y",
    id          = cluster_var,
    weights     = "wt",
    folds       = origami::make_folds(
      cluster_ids = d[[cluster_var]],
      V           = V
    )
  )

  # ---------- 3.  fit ----------------------------------------------------------
  set.seed(12345)
  sl_fit <- sl$train(task)


  # ---------- 4.  individual predictions & aggregation ------------------------
  d$pred_prob <- sl_fit$predict()

  cluster_prev <- d |>
    dplyr::group_by(.data[[cluster_var]]) |>
    dplyr::summarise(n_women   = dplyr::n(),
                     obs_prev  = mean(.data[[outcome]], na.rm = TRUE),
                     pred_prev = mean(pred_prob,        na.rm = TRUE),
                     .groups   = "drop")

  # return both
  list(sl_fit       = sl_fit,
       ind_pred     = dplyr::select(d, all_of(cluster_var), pred_prob),
       Xvars=task$column_names,
       cluster_pred = cluster_prev)
}


calc_importance <- function(sl_fit, eval.fun = loss_loglik_binomial,
                            importance.metric="ratio", n_vars=20,
                            covariate.groups=NULL){
  set.seed(983)
  varimp <- importance(
    fit = sl_fit,
    eval_fun = eval.fun,
    type = "permute",
    importance_metric=importance.metric,
    covariate_groups = covariate.groups,
  )

  varimp20=varimp %>% arrange(.[,2]) %>% tail(n=n_vars)
  p=importance_plot(x = varimp20)
  return(list(varimp=varimp, varimp20=varimp20, p=p))
}


# =============================================================
#  SL_surface(): map a SuperLearner over a 5‑km grid
#  -------------------------------------------------------------
#  • sl_fit  = object returned by DHS_cluster_SL()
#  • task    = the sl3_Task used to train the ensemble
#  • cov_stack = terra SpatRaster of gridded predictors
#  • ghana_admin0 = sf polygon mask for Ghana
#  • res_km  = grid resolution
#  • B       = # bootstrap replicates   (for SEs)
# =============================================================
sl_surface <- function(sl_fit, task, cov_stack,
                       ghana_admin0, res_km = 5,
                       B = 300, seed = 1) {

  library(sf); library(terra); library(dplyr); library(future.apply)

  ## 1.  5‑km grid -----------------------------------------------------------
  grid_pts <- st_make_grid(ghana_admin0,
                           cellsize = res_km / 111,
                           what     = "centers") |>
    st_as_sf() |>
    st_intersection(ghana_admin0)

  grid_df <- data.frame(st_coordinates(grid_pts))
  names(grid_df) <- c("lon", "lat")

  ## 2.  Extract raster covariates ------------------------------------------
  if (!is.null(cov_stack)) {
    cov_mat <- terra::extract(cov_stack, grid_df[, c("lon","lat")],
                              bind = TRUE) |>
      dplyr::select(-ID)
  } else cov_mat <- data.frame()

  # predictor names must match those in the training task
  Xvars <- task$nodes$covariates
  grid_cov <- cov_mat |>
    dplyr::select(any_of(Xvars)) |>
    mutate(across(where(is.factor), as.character))  # keep types

  ## 3.  Base prediction -----------------------------------------------------
  grid_task <- sl3::sl3_Task$new(data       = grid_cov,
                                 covariates = Xvars,
                                 outcome    = NULL)

  grid_pred <- sl_fit$predict(grid_task)  # point estimate (vector)

  ## 4.  Cluster‑bootstrap SE -----------------------------------------------
  set.seed(seed)
  clust_ids <- unique(task$data[[task$nodes$id]])
  boot_mat  <- matrix(NA_real_, nrow = nrow(grid_cov), ncol = B)

  plan(multisession)          # parallel if you like
  boot_mat <- future_sapply(seq_len(B), \(b) {
    samp <- sample(clust_ids, replace = TRUE)
    d_b  <- task$data %>% dplyr::filter(.data[[task$nodes$id]] %in% samp)

    task_b <- task$clone(deep = TRUE)
    task_b$data <- d_b

    fit_b  <- sl_fit$learner$train(task_b)        # re‑fit ensemble
    fit_b$predict(grid_task)
  })
  plan(sequential)

  grid_sd <- apply(boot_mat, 1, sd)

  ## 5.  Rasterise -----------------------------------------------------------
  r_template <- rast(extent = ext(range(grid_df$lon),
                                  range(grid_df$lat)),
                     resolution = res_km / 111,
                     crs = "EPSG:4326")

  r_mu <- rasterize(vect(cbind(grid_df, pred = grid_pred), crs = "EPSG:4326"),
                    r_template, field = "pred", fun = "mean")

  r_sd <- rasterize(vect(cbind(grid_df, sd = grid_sd),   crs = "EPSG:4326"),
                    r_template, field = "sd", fun = "mean")

  list(mean = r_mu, sd = r_sd, draws = boot_mat)
}


