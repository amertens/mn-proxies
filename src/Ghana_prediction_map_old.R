
#  IF install.packages("INLA") fails, use:
#  install.packages("INLA", repos = "https://inla.r-inla-download.org/R/stable")


# =============================================================
# 0.  DATA: cluster‑level counts  ------------------------------
# =============================================================
library(dplyr)
library(sf)
library(raster)
library(INLA)
library(Matrix)

# df must have: cluster, longitude, latitude, mod_sev_anemia (0/1)
df = readRDS(here::here("data/DHS/clean/dhs_Ghana_2022_clean.RDS"))


# -------------------------------------------------------------
cluster_dat <- df %>%                               # your cleaned frame
  group_by(cluster, longitude, latitude) %>%
  summarise(y = sum(mod_sev_anemia, na.rm = TRUE),  # # anaemic women
            n = n(),                                # # tested
            .groups = "drop")

coords <- as.matrix(cluster_dat[, c("longitude", "latitude")])

# =============================================================
# 1.  BUILD SPDE MESH  ----------------------------------------
# =============================================================
gha_sf   <- rnaturalearth::ne_countries(country = "Ghana",
                                        returnclass = "sf") |>
  st_transform(4326)

# Buffer 50 km around Ghana to avoid edge artefacts
mesh <- inla.mesh.2d(boundary = as(gha_sf, "Spatial"),
                     max.edge = c(0.25, 3),             # ≈25–300 km
                     cutoff   = 0.05)                   # no duplicate points

plot(mesh); points(coords, col = "red", pch = 16, cex = .6)

# SPDE object
spde <- inla.spde2.matern(mesh = mesh, alpha = 2)       # Matérn ν = 1

# Index for the spatial random effect
s.index <- inla.spde.make.index("spatial", n.spde = spde$n.spde)

# =============================================================
# 2.  STACK FOR INLA  -----------------------------------------
# =============================================================
A.obs <- inla.spde.make.A(mesh    = mesh,
                          loc     = coords)
A.int.obs  <- Matrix(1, nrow = nrow(cluster_dat), ncol = 1, sparse = TRUE)

stk.obs <- inla.stack(
  data   = list(y = cluster_dat$y,
                Ntrials = cluster_dat$n),
  A      = list(A.obs, A.int.obs),
  effects= list(s.index,
                data.frame(intercept = 1)),
  tag    = "obs")

# =============================================================
# 3.  PREDICTION GRID (≈5 km)  --------------------------------
# =============================================================

# 3·1  Grid centroids inside Ghana -----------------------------
grid5 <- st_make_grid(gha_sf, cellsize = 0.05, what = "centers") |>  # 0.05° ≈ 5 km
  st_as_sf() |> st_intersection(gha_sf)

grid_xy <- st_coordinates(grid5)

# 3·2  Projection matrix from GP field to grid locations -------
A.pred <- inla.spde.make.A(mesh, loc = grid_xy)

# 3·3  Intercept column as an explicit matrix (nrow × 1) -------
A.int.pred <- Matrix(1, nrow = nrow(grid_xy), ncol = 1, sparse = TRUE)

# 3·4  Prediction stack ----------------------------------------
stk.pred <- inla.stack(
  data    = list(y = NA, Ntrials = NA),
  A       = list(A.pred, A.int.pred),   # <- both have nrow = nrow(grid_xy)
  effects = list(s.index,
                 data.frame(intercept = 1)),
  tag     = "pred"
)

# 3·5  Combine with the observation stack ----------------------
stk.full <- inla.stack(stk.obs, stk.pred)


# =============================================================
# 4.  FIT MODEL  ----------------------------------------------
# =============================================================
formula <- y ~ 0 + intercept + f(spatial, model = spde)

result <- inla(formula,
               family       = "binomial",
               Ntrials      = stk.full$data$Ntrials,
               data         = inla.stack.data(stk.full),
               control.predictor = list(A = inla.stack.A(stk.full),
                                        compute = TRUE),
               control.fixed     = list(mean = 0, prec = 1e-6),
               control.compute   = list(config = TRUE),   # for posterior draws
               verbose = FALSE)

summary(result)

# =============================================================
# 5.  EXTRACT POSTERIOR PREDICTIONS  ---------------------------
# =============================================================
idx.pred  <- inla.stack.index(stk.full, "pred")$data
lp.mean   <- result$summary.linear.predictor[idx.pred, "mean"]
lp.sd     <- result$summary.linear.predictor[idx.pred, "sd"]

grid5$prev_mean <- plogis(lp.mean)
grid5$prev_sd   <- (plogis(lp.mean + lp.sd) - plogis(lp.mean - lp.sd))/2
grid5$prev_low  <- plogis(lp.mean - 1.96*lp.sd)
grid5$prev_high <- plogis(lp.mean + 1.96*lp.sd)

# =============================================================
# 6.  RASTERISE & SAVE  ---------------------------------------
# =============================================================
r_template <- raster(extent(gha_sf), res = 0.05)
r_pred  <- rasterize(grid_xy, r_template, grid5$prev_mean, fun = mean)
r_sd    <- rasterize(grid_xy, r_template, grid5$prev_sd,   fun = mean)

writeRaster(stack(r_pred, r_sd),
            filename = "ghana_anemia_GP_2022.tif",
            overwrite = TRUE)

# =============================================================
# 7.  QUICK VISUALS  ------------------------------------------
# =============================================================
library(ggplot2); library(viridis)

ggplot() +
  geom_raster(data = as.data.frame(r_pred, xy = TRUE),
              aes(x = x, y = y, fill = layer)) +
  geom_sf(data = gha_sf, fill = NA, col = "grey20", size = .4) +
  scale_fill_viridis(name = "Anaemia\nprevalence", option = "B",
                     limits = c(0,0.8), oob = scales::squish) +
  coord_sf(expand = FALSE) +
  labs(title = "Bayesian GP – predicted prevalence of anaemia, Ghana 2022") +
  theme_void()

ggplot() +
  geom_raster(data = as.data.frame(r_sd, xy = TRUE),
              aes(x = x, y = y, fill = layer)) +
  geom_sf(data = gha_sf, fill = NA, col = "grey20", size = .4) +
  scale_fill_viridis(name = "Posterior SD", option = "C") +
  coord_sf(expand = FALSE) +
  labs(title = "Posterior uncertainty (SD) – Ghana 2022") +
  theme_void()
