
rm(list=ls())


dhs_surface <- function(dat,                # cluster‑level tibble
                        var_value,          # column with continuous outcome
                        ghana_admin0,       # sf or SpatVector
                        cov_stack  = NULL,  # terra SpatRaster (optional)
                        res_km      = 5,    # grid resolution (km)
                        nsim_draws  = 500   # posterior draws for uncertainty
) {
  library(sf);  library(terra);  library(dplyr);  library(spaMM)

  ## 0. Boundary to sf -------------------------------------------------------
  if (inherits(ghana_admin0, "SpatVector"))
    ghana_admin0 <- st_as_sf(ghana_admin0)

  ## 1. Data prep -----------------------------------------------------------
  dat <- dat |>
    rename(lon = longitude, lat = latitude) |>
    mutate(value = .data[[var_value]])

  ## 2. Gaussian Matérn spatial model --------------------------------------
  fmla <- value ~ 1 + Matern(1 | lon + lat)

  fit <- fitme(fmla,
               data    = dat,
               family  = gaussian(),
               method  = "REML")      # REML usual for Gaussian MBG

  ## 3. 5‑km prediction grid ----------------------------------------------
  grid_pts <- st_make_grid(ghana_admin0,
                           cellsize = res_km / 111,
                           what = "centers") |>
    st_as_sf() |>
    st_intersection(ghana_admin0)

  grid_df  <- as.data.frame(st_coordinates(grid_pts))
  names(grid_df) <- c("lon","lat")

  ## 4. Predict mean & variance --------------------------------------------
  pp <- predict(fit,
                newdata   = grid_df,
                type      = "response",
                variances = list(predVar = TRUE))

  grid_pts$pred_mean <- as.numeric(pp)
  grid_pts$pred_sd   <- sqrt(attr(pp, "predVar"))
  grid_pts$pred_low  <- grid_pts$pred_mean - 1.96*grid_pts$pred_sd
  grid_pts$pred_high <- grid_pts$pred_mean + 1.96*grid_pts$pred_sd

  ## 5. Rasterise mean & SD -------------------------------------------------
  # 5. Raster template  ------------------------------------------------
  e <- terra::ext( min(grid_df$lon), max(grid_df$lon),
                   min(grid_df$lat), max(grid_df$lat) )   # SpatExtent

  r_template <- terra::rast(
    e,
    resolution = res_km / 111,          # degrees per km
    crs        = "EPSG:4326"
  )


  r_mu <- rasterize(vect(cbind(grid_df, m = grid_pts$pred_mean),
                         crs = "EPSG:4326"),
                    r_template, field = "m", fun = "mean")

  r_sd <- rasterize(vect(cbind(grid_df, s = grid_pts$pred_sd),
                         crs = "EPSG:4326"),
                    r_template, field = "s", fun = "mean")

  ## 6. Parametric bootstrap draws -----------------------------------------
  sim_mat <- predict(fit,
                     newdata = grid_df,
                     type    = "response",
                     nsim    = nsim_draws)

  if (nrow(sim_mat) == nsim_draws) sim_mat <- t(sim_mat)

  list(mean  = r_mu,
       sd    = r_sd,
       draws = sim_mat,
       model = fit)
}

dhs_prev_surface <- function(dat,                # cluster‑level tibble
                        var_success,        # column with successes
                        var_trials,         # column with trials
                        ghana_admin0,       # sf or SpatVector boundary
                        cov_stack   = NULL, # terra SpatRaster (optional)
                        res_km      = 5,    # grid resolution (km)
                        nsim_draws  = 500   # posterior draws for uncertainty
) {

  library(sf);    library(terra)
  library(dplyr); library(spaMM)

  ## ------------------------------------------------------------------
  ## 0.  Inputs / conversions
  ## ------------------------------------------------------------------
  if (inherits(ghana_admin0, "SpatVector"))
    ghana_admin0 <- sf::st_as_sf(ghana_admin0)

  # rename for convenience
  dat <- dat %>%
    rename(lon = longitude, lat = latitude) %>%              # if needed
    mutate(success = .data[[var_success]],
           trials  = .data[[var_trials]])


  dat <- dat %>%                       # tibble/data.frame
    mutate(success = .data[[var_success]],
           trials  = .data[[var_trials]])


  ## ------------------------------------------------------------------
  ## 1.  Fit binomial Matérn GLMM
  ## ------------------------------------------------------------------
  fmla <- as.formula("cbind(success, trials - success) ~ 1 +
                      Matern(1 | lon + lat)")

  fit <- spaMM::fitme(fmla,
                      data    = dat,
                      family  = binomial(),
                      method  = "ML")

  ## ------------------------------------------------------------------
  ## 2.  Prediction grid (≈ res_km)
  ## ------------------------------------------------------------------

  grid_pts <- st_make_grid(ghana_admin0,
                           cellsize = res_km / 111,  # deg per km
                           what     = "centers") %>%
    st_as_sf() %>%
    st_intersection(ghana_admin0)

  grid_df <- data.frame(st_coordinates(grid_pts))
  names(grid_df) <- c("lon","lat")

  ## ------------------------------------------------------------------
  ## 3.  Predict mean & SE
  ## ------------------------------------------------------------------

  pp <- predict(fit,
                newdata = grid_df,
                type    = "response",
                variances = list(predVar = TRUE))

  grid_pts$prev_mean <- as.numeric(pp)        # fitted prevalence
  grid_pts$prev_sd <- sqrt(attr(pp, "predVar"))

  grid_pts$prev_low  <- plogis(qnorm(0.025, qlogis(grid_pts$prev_mean),
                                     grid_pts$prev_sd / (grid_pts$prev_mean*(1-grid_pts$prev_mean))))
  grid_pts$prev_high <- plogis(qnorm(0.975, qlogis(grid_pts$prev_mean),
                                     grid_pts$prev_sd / (grid_pts$prev_mean*(1-grid_pts$prev_mean))))

  ## ------------------------------------------------------------------
  ## 4.  Rasterise mean & SD
  ## ------------------------------------------------------------------



  # 1. template: use resolution = res_km/111 degrees
  r_template <- rast(extent = ext(range(grid_df$lon),
                                  range(grid_df$lat)),
                     resolution = res_km / 111,
                     crs = "EPSG:4326")

  # 2. make points a SpatVector with the prediction attached
  pts <- vect(cbind(grid_df, pred = grid_pts$prev_mean), crs = "EPSG:4326")

  # 3. burn into raster (mean of duplicates not needed—one per cell)
  r_mu <- rasterize(pts, r_template, field = "pred", fun = "mean")

  #same for SD layer
  pts_sd <- vect(cbind(grid_df, sd = grid_pts$prev_sd), crs = "EPSG:4326")
  r_sd   <- rasterize(pts_sd, r_template, field = "sd", fun = "mean")

  ## ------------------------------------------------------------------
  ## 5·A  Posterior draws  (parametric bootstrap)  --------------------
  ## ------------------------------------------------------------------
  boot_mat <- predict(
    fit,
    newdata = grid_df,
    type    = "response",
    nsim    = nsim_draws)

  # spaMM returns a matrix  (nsim × N) or (N × nsim) depending on version
  if (nrow(boot_mat) == nsim_draws) boot_mat <- t(boot_mat)


  list(mean  = r_mu,
       sd    = r_sd,
       draws = boot_mat,
       model = fit)
}



gha_v <- geodata::gadm(country="Ghana", level=0, path=tempdir())
gha_sf <- st_as_sf(gha_v)
d = readRDS(here::here("data/DHS/clean/dhs_Ghana_2019_gee_fp_map_merge.RDS"))

#-------------------------------------------------------------------------------
# Anemia
#-------------------------------------------------------------------------------

cluster_dat <- d %>%
  group_by(cluster, latitude, longitude) %>%
  summarise(success = sum(mod_sev_anemia == 1),
            trials  = n(),
            .groups = "drop") %>%
  as.data.frame()

dat=cluster_dat
var_success  = "success"
var_trials   = "trials"
ghana_admin0 = gha_sf
surf <- dhs_prev_surface(cluster_dat,
                    var_success  = "success",
                    var_trials   = "trials",
                    ghana_admin0 = gha_sf)

terra::plot(surf$mean, main = "Anaemia prevalence – spaMM 5km")
terra::plot(surf$sd, main = "SE of Anaemia prevalence\npredictions – spaMM 5km")


#-------------------------------------------------------------------------------
# HH wealth (for predictions later)
#-------------------------------------------------------------------------------

# cluster‑aggregate wealth
cluster_wi <- d %>%                                  # women‑level frame
  group_by(cluster, latitude, longitude) %>%
  summarise(value = mean(wealth_score),
            .groups = "drop")

surf_wi <- dhs_surface(cluster_wi,
                       var_value     = "value",
                       ghana_admin0  = gha_sf)

plot(surf_wi$mean, main = "Predicted mean wealth index – 5 km")
plot(surf_wi$sd,   main = "Prediction SD")

out_stack <- c(surf_wi$mean)         # layer 1 = mean, 2 = sd
names(out_stack) <- c("wealth_mean")

terra::writeRaster(
  out_stack,
  filename  = here::here("data/DHS/predicted_surfaces/ghana_wealth_score_surface_5km.tif"),
  overwrite = TRUE                        # allow overwrite if rerun
)
