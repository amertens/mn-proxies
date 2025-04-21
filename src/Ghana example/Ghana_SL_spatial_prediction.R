
# ----------------------------------------------------------
# 0.  Build a 5‑km prediction grid for Ghana
# ----------------------------------------------------------
library(sf); library(raster); library(exactextractr); library(here)

#load DHS data
df <- readRDS(here("data/DHS/clean/dhs_Ghana_2019_gee_fp_map_merge.RDS"))


gha_shape <- rnaturalearth::ne_countries(country = "Ghana", returnclass = "sf")
grid5 <- st_make_grid(gha_shape, cellsize = 0.05, what = "centers") |>    # ≈5‑km
  st_as_sf() |> st_crop(gha_shape)

# ----------------------------------------------------------
# 1.  Attach gridded covariates
# ----------------------------------------------------------

# -------- pick a template ----------
rs_wealth <- rast(here(
  "data/DHS/predicted_surfaces/ghana_wealth_score_surface_5km.tif"))

# -------- bring in the second layer  ----------
rs_hbc <- rast(here(
  "data/Malaria Atlas/GHA_Blood_Disorders_201201_Africa_HbC_Allele_Frequency.tif"))

# -------- make them identical -----------------
## 1. make sure the CRS matches the template
if (!same.crs(rs_hbc, rs_wealth)) {
  rs_hbc <- project(rs_hbc, rs_wealth, method = "bilinear")   # bilinear = safe for continuous values
}

## 2. match resolution, origin & extent
rs_hbc <- resample(rs_hbc, rs_wealth, method = "bilinear")

#combine rasters
cov_stack <- c(rs_wealth, rs_hbc)
names(cov_stack) <- c("wealth", "hbc")


# grid5 is an sf POINT layer → convert to SpatVector
grid5_v  <- terra::vect(grid5)

grid_cov <- terra::extract(cov_stack, grid5_v,
                           method = "bilinear",   # bilinear = smooth for continuous covariates
                           bind   = FALSE)[ , -1] # drop ID column


grid5     <- cbind(grid5, grid_cov)

# ----------------------------------------------------------
# 2.  Make sure the *same* covariates exist on DHS rows
# ----------------------------------------------------------
df <- df |>
  dplyr::mutate(lon = as.numeric(longitude),
                lat = as.numeric(latitude)) |>
  sf::st_as_sf(coords = c("lon","lat"), crs = st_crs(cov_stack))

# convert sf POINTS → SpatVector and extract
df_cov <- terra::extract(
  cov_stack,
  terra::vect(df),
  method = "bilinear",   # smooth interpolation for continuous covariates
  bind   = FALSE)[ , -1] # drop the ID column that terra adds

names(df_cov) <- names(cov_stack)   # c("wealth", "hbc")
df <- cbind(sf::st_drop_geometry(df), df_cov)   # back to data.frame


# ----------------------------------------------------------
# 3.  Fit SuperLearner with an *additional* spatial learner
# ----------------------------------------------------------
## ----------------------------------------------------------
## 1.  Build a spatial CV scheme  (Davies & vdL, Sec. 4)
## ----------------------------------------------------------
library(blockCV)    # for spatial blocking
set.seed(2025)

folds <- spatialBlock(
  speciesData = df |>             # the DHS cluster points (sf or data.frame)
    st_as_sf(coords = c("longitude", "latitude"),
             crs = st_crs(cov_stack)),
  species     = "mod_sev_anemia", # outcome column
  theRange    = 50000,            # 50‑km buffer ≈ “out of sight – out of sample”
  k           = 5,                # 5‑fold CV as in the paper
  selection   = "random",
  showBlocks  = FALSE,  progress = FALSE
)

df$fold_id <- folds$foldID        # integer 1…5 per row

## ----------------------------------------------------------
## 2.  Define the sl3 Task  (lon/lat are covariates)
## ----------------------------------------------------------
library(sl3);  library(data.table)

Xvars_spatial <- c(names(cov_stack),          # gridded layers
                   "longitude", "latitude")   # raw coordinates  <- new

df$Y <- df$mod_sev_anemia

task <- sl3_Task$new(
  data        = as.data.table(df),
  covariates  = Xvars_spatial,
  outcome     = "Y",
  id          = "cluster",
  weights     = "svy_weight",           # DHS sampling weights
  folds       = df$fold_id      # user‑supplied spatial folds
)

## ----------------------------------------------------------
## 3.  Base learners  (similar to Davies & vdL library)
## ----------------------------------------------------------
lrnr_mean     <- Lrnr_mean$new()
glm_fast      <- Lrnr_glm_fast$new()
ranger_lrnr   <- Lrnr_ranger$new(min.node.size = 5)
xgboost_lrnr  <- Lrnr_xgboost$new()

# thin‑plate spline over coords (fields::Tps style)
tps_formula   <- as.formula("Y ~ s(longitude, latitude)")
lrnr_tps      <- Lrnr_gam$new(formula = tps_formula)


learners= c("lrnr_mean", "glm_fast",
                        "ranger_lrnr", "xgboost_lrnr",
                        "lrnr_tps")

base_stack <- Stack$new(lrnr_mean, glm_fast,
                        ranger_lrnr, xgboost_lrnr,
                        lrnr_tps)

## ----------------------------------------------------------
## 4. Spatially adaptive **meta‑learner**
##     (GAM of base‑preds + smooth s(lon,lat))
## ----------------------------------------------------------

library(sl3); library(data.table)

## ------------------------------------------------------------
## 0.  a helper that builds the meta‑task WITH coords
## ------------------------------------------------------------
meta_with_coords <- function(task, preds, fold_number) {

  # 1. base‑learner predictions come in as a matrix → data.table
  base_dt <- as.data.table(preds)

  # 2. pull coords from the ORIGINAL task
  coords  <- task$get_data(cols = c("longitude", "latitude"))

  # 3. outcome & optional weights / ids
  y       <- task$get_outcome()
  ids     <- task$get_ids()         # keeps cluster IDs if you set them
  wts     <- task$get_weights()

  # 4. stitch everything together
  meta_dt <- data.table(
    Y         = y,
    longitude = coords$longitude,
    latitude  = coords$latitude,
    base_dt
  )

  sl3_Task$new(
    data        = meta_dt,
    covariates  = setdiff(names(meta_dt), c("Y", "id", "weights")),
    outcome     = "Y",
    id          = ids,
    weights     = wts
  )
}

## ------------------------------------------------------------
## 1.  meta‑learner that USES the coords
## ------------------------------------------------------------
# meta_spatial <- Lrnr_gam$new(
#   formula = Y ~ s(longitude, latitude) + . - longitude - latitude
# )

meta_spatial <- Lrnr_gam$new(formula = NULL)

## ------------------------------------------------------------
## 2.  SuperLearner with custom meta‑task generator
## ------------------------------------------------------------
sl_spatial <- Lrnr_sl$new(
  learners                    = base_stack,      # your TPS, RF, XGB, …
  metalearner                 = meta_spatial,
  metalearner_task_generator  = meta_with_coords,   # <‑‑ key line
  loss_function               = loss_loglik_binomial
)

## ------------------------------------------------------------
## 3.  Train – no “longitude not found” error now
## ------------------------------------------------------------

## ----------------------------------------------------------
## 5.  Fit & evaluate with spatial CV
## ----------------------------------------------------------

sl_fit <- sl_spatial$train(task)

#Need to update to get the CV risk
#cv_risk <- sl_fit$cv_risk # cross‑validated log‑likelihood risk

## ----------------------------------------------------------
## 6.  Predict on the 5‑km grid (needs lon/lat columns)
## ----------------------------------------------------------
grid_mat <- grid5 |>
  st_drop_geometry() |>
  cbind(st_coordinates(grid5)) |>       # lon, lat
  setNames(c(names(cov_stack), "longitude", "latitude"))

grid_task <- sl3_Task$new(
  data        = as.data.table(grid_mat),
  covariates  = Xvars_spatial,
  outcome     = NULL
)

grid5$pred_mean <- sl_fit$predict(grid_task)

saveRDS(grid5, here("data/DHS/clean/ghana_5km_predictions.RDS"))

## ----------------------------------------------------------
## 6‑bis.  Parametric‑bootstrap uncertainty (200 replicates)
##          – cluster‑level resampling –
## ----------------------------------------------------------
library(data.table)

n_clust <- length(unique(df$cluster))

# helper: build a fresh sl3 Task from any bootstrap data.frame
make_task <- function(dat) {
  sl3_Task$new(
    data        = as.data.table(dat),
    covariates  = Xvars_spatial,
    outcome     = "Y",
    id          = "cluster",
    weights     = "svy_weight",
    folds       = NULL               # no CV inside each bootstrap fit
  )
}


boot_reps=20

set.seed(2025)                       # reproducibility
boot_preds <- replicate(boot_reps, {

  ## 1. sample clusters WITH replacement and keep duplicates
  samp_ids <- sample(unique(df$cluster),
                     size       = n_clust,
                     replace    = TRUE)

  df_b <- rbindlist(
    lapply(samp_ids, function(cl) df[df$cluster == cl, ]),
    use.names = TRUE)

  ## 2. rebuild task and refit the whole SuperLearner
  task_b <- make_task(df_b)
  fit_b  <- sl_spatial$train(task_b)

  ## 3. predict on the fixed 5‑km grid
  fit_b$predict(grid_task)

}, simplify = "matrix")              # result: n_grid × 200 matrix

## 4. summarise posterior‑like distribution
grid5$pred_sd   <- apply(boot_preds, 1, sd)
grid5$pred_low  <- grid5$pred_mean - 1.96 * grid5$pred_sd
grid5$pred_high <- grid5$pred_mean + 1.96 * grid5$pred_sd



# ----------------------------------------------------------
# 7.  Visualise point estimate *and* uncertainty
# ----------------------------------------------------------
library(sf); library(ggplot2); library(patchwork)

## ----------------------------------------------------------
## 0.  Keep only points whose centroids fall INSIDE Ghana
## ----------------------------------------------------------
grid5_clip <- grid5[                   # same columns, fewer rows
  st_within(grid5, gha_shape, sparse = FALSE)[, 1], ]

## ----------------------------------------------------------
## 1.  Posterior mean
## ----------------------------------------------------------
p_mean <- ggplot() +
  geom_sf(data = gha_shape, fill = NA, colour = "grey40") +
  geom_sf(data = grid5_clip, aes(colour = pred_mean), size = 0.7) +
  scale_colour_viridis_c(name = "Predicted\nanaemia") +
  theme_void() +
  labs(title = "A. Posterior mean (SuperLearner)",
       subtitle = "5 km resolution, Ghana 2019 DHS")

## ----------------------------------------------------------
## 2.  Bootstrap SD
## ----------------------------------------------------------
p_sd <- ggplot() +
  geom_sf(data = gha_shape, fill = NA, colour = "grey40") +
  geom_sf(data = grid5_clip, aes(colour = pred_sd), size = 0.7) +
  scale_colour_viridis_c(name = "SD\n(200 cluster bootstraps)") +
  theme_void() +
  labs(title = "B. Posterior SD (uncertainty)",
       subtitle = "Higher values = less certain predictions")

## ----------------------------------------------------------
## 3.  Side‑by‑side display
## ----------------------------------------------------------
p_mean + p_sd + plot_layout(ncol = 2)

#save plot
ggsave(here("figures/ghana_anemia_prediction.png"),
       width = 12, height = 6, dpi = 300)
