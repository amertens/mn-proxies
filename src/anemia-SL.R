library(rdhs)
library(data.table)
library(tidyverse)

library(surveyPrev)

# install.packages("INLA",
#                  repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"),
#                  dep=TRUE,
#                  type = "binary",
#                  install.packages.check.source = "no")
library(INLA)
#remotes::install_github("rspatial/geodata")
library(geodata)
library(sf)
library(SUMMER)
library(rdhs)
library(patchwork)
library(dplyr)
library(tidyr)
library(here)
library(kableExtra)
source(paste0(here::here(),"/src/0-functions.R"))


dhs_compiled_d <- readRDS(here("data/DHS_compiled_raw_data_interim.rds"))
length(dhs_compiled_d)
names(dhs_compiled_d)
dhs_indicators_d <- readRDS(here("data/DHS_compiled_indicator_data_interim.rds"))
table(dhs_indicators_d$indicator)

head(dhs_compiled_d)
length(dhs_compiled_d)
names(dhs_compiled_d)

head(dhs_indicators_d)

table(dhs_indicators_d$indicator)
