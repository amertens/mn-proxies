# 1.  Fit the ensemble on individual rows ---------------------
res_sl <- DHS_cluster_SL(
  d           = d,                 # your cleaned women‑level df
  Xvars       = Xvars,             # covariate names
  outcome     = "mod_sev_anemia",
  cluster_var = "cluster",
  weight_var  = "wt",
  V           = 10)

sl_fit <- res_sl$sl_fit
task    <- res_sl$sl_fit$training_task      # keep for bootstrap

# 2.  Covariate rasters (optional) ----------------------------
cov_stack <- rast(list(
  elev = "srtm_gh.tif",
  ntl  = "viirs_2022.tif"
))   # predictors must match Xvars

# 3.  Surface (mean + bootstrap SD) ---------------------------
surf_sl <- sl_surface(
  sl_fit, task,
  cov_stack    = cov_stack,
  ghana_admin0 = gha_sf,
  res_km       = 5,
  B            = 300)

plot(surf_sl$mean, main = "SuperLearner mean – 5 km")
plot(surf_sl$sd,   main = "SuperLearner SE – 5 km")
