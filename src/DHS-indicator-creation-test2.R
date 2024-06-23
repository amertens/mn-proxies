
#-------------------------------------------------------------------------------
#https://cran.r-project.org/web/packages/surveyPrev/vignettes/vignette-main.html
#-------------------------------------------------------------------------------

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
library(kableExtra)
source(paste0(here::here(),"/src/0-functions.R"))

#Load BRINDA data



#built in indicators
data(surveyPrevIndicators)
head(surveyPrevIndicators)

#set up DHS download
set_rdhs_config(email = "amertens@berkeley.edu",
                project = "MN Prediction",
                data_frame = "data.table::as.data.table",
                config_path = "~/.rdhs.json",
                global = TRUE)


indicator <- "ancvisit4+"

year <- 2018
country <- "Zambia" #Check I have access!
#dhsData <- getDHSdata(country = country,  year = year)

dhsData <- getDHSdata(country = country, indicator = indicator, year = year)
#We then use getDHSindicator() to process the raw survey data into a data frame, where the column value is the indicator of interest. The data frame also contains information specifying survey designs in the survey package, including cluster ID, household ID, survey weight and strata information.
data <- getDHSindicator(dhsData, indicator = indicator)
head(data)

#spatial information
geo <- getDHSgeo(country = country, year = year)

poly.adm1 <- geodata::gadm(country="ZMB", level=1, path=tempdir())
poly.adm1 <- sf::st_as_sf(poly.adm1)
poly.adm2 <- geodata::gadm(country="ZMB", level=2, path=tempdir())
poly.adm2 <- sf::st_as_sf(poly.adm2)

cluster.info <- clusterInfo(geo=geo, poly.adm1=poly.adm1, poly.adm2=poly.adm2,
                            by.adm1 = "NAME_1",by.adm2 = "NAME_2")
head(cluster.info$data)
admin.info1 <- adminInfo(poly.adm = poly.adm1, admin = 1,by.adm = "NAME_1")
admin.info2 <- adminInfo(poly.adm = poly.adm2, admin = 2,by.adm = "NAME_2",by.adm.upper = "NAME_1")
head(admin.info2$data)

#direct estimation
res_ad1 <- directEST(data = data,
                     cluster.info = cluster.info,
                     admin = 1)
head(res_ad1$res.admin1)

#national and urban-rural estimates
res_ad0 <- directEST(data=data,
                     cluster.info= cluster.info,
                     admin=0,
                     strata="all")
res_ad0_urban <- directEST(data=data,
                           cluster.info= cluster.info,
                           admin=0,
                           strata="urban")
res_ad0_rural <- directEST(data=data,
                           cluster.info= cluster.info,
                           admin=0,
                           strata="rural")
list(all = res_ad0$res.admin0,
     urban = res_ad0_urban$res.admin0,
     rural = res_ad0_rural$res.admin0)

#admin 2 direct estimates
res_ad2 <- directEST(data = data,
                     cluster.info = cluster.info,
                     admin = 2,
                     aggregation = FALSE)

#smoothing direct estimates
#Admin 1 Fay-Herriot estimates
smth_res_ad1_bym2 <- fhModel(data,
                             cluster.info = cluster.info,
                             admin.info = admin.info1,
                             admin = 1,
                             model = "bym2",
                             aggregation =FALSE)

smth_res_ad1_iid <- fhModel(data,
                            cluster.info = cluster.info,
                            admin.info = admin.info1,
                            admin = 1,
                            model = "iid",
                            aggregation =FALSE)
head(smth_res_ad1_bym2$res.admin1)


#Cluster-level model
cl_res_ad1 <- clusterModel(data=data,
                           cluster.info=cluster.info,
                           admin.info  = admin.info1,
                           stratification = FALSE,
                           model = "bym2",
                           admin = 1,
                           aggregation =FALSE,
                           CI = 0.95)

cl_res_ad2 <- clusterModel(data=data,
                           cluster.info= cluster.info,
                           admin.info = admin.info2,
                           model = "bym2",
                           stratification =FALSE,
                           admin = 2,
                           aggregation =FALSE,
                           CI = 0.95)
head(cl_res_ad2$res.admin2)

#6 Visualizing prevalence estimates
# Arrange all estimates into a long-format data frame
out1 <- res_ad1$res.admin1[, c("admin1.name", "direct.est", "cv")]
colnames(out1)[2] <- "mean"
out1$model <- "Direct Estimates"
out2 <- smth_res_ad1_bym2$res.admin1[, c("admin1.name", "mean", "cv")]
out2$model <- "Fay-Herriot Model"
out3 <- cl_res_ad1$res.admin1[, c("admin1.name", "mean", "cv")]
out3$model <- "Unstratified Cluster-level Model"

g1 <- mapPlot(data = rbind(out1, out2, out3), geo = poly.adm1,
              by.data = "admin1.name",  by.geo = "NAME_1", is.long = TRUE,
              variable = "model", value = "mean", legend.label = "Mean")

g2 <- mapPlot(data = rbind(out1, out2, out3), geo = poly.adm1,
              by.data = "admin1.name",  by.geo = "NAME_1", is.long = TRUE,
              variable = "model", value = "cv", legend.label = "CV")
g1 / g2


#7 Aggregation to higher admin levels
#7.1 Computing population size from WorldPop raster

library(raster)
pop.abbrev <- "ZMB"
year <- 2018
pop.dir <- "../data/Zambia/worldpop"

#7.2 Estimating population size by survey weight
agg.survey1 <- aggSurveyWeight(data = data, cluster.info = cluster.info, admin = 1)
agg.survey2 <- aggSurveyWeight(data = data, cluster.info = cluster.info, admin = 2,
                               poly.adm = poly.adm2, by.adm = "NAME_2", by.adm.upper = "NAME_1")

admin.info1 <- adminInfo(poly.adm = poly.adm1,
                         admin = 1,
                         by.adm="NAME_1",
                         agg.pop  =ZambiaPopWomen$admin1_pop,
                         proportion = ZambiaPopWomen$admin1_urban )

admin.info2 <- adminInfo(poly.adm = poly.adm2,
                         admin = 2,by.adm="NAME_2",by.adm.upper = "NAME_1",
                         agg.pop =ZambiaPopWomen$admin2_pop,
                         proportion = ZambiaPopWomen$admin2_urban)

admin.info1.survey <- adminInfo(poly.adm = poly.adm1,
                                admin = 1,
                                by.adm="NAME_1",
                                agg.pop  =agg.survey1,
                                proportion = ZambiaPopWomen$admin1_urban )

admin.info2.survey  <- adminInfo(poly.adm = poly.adm2,
                                 admin = 2,by.adm="NAME_2",by.adm.upper = "NAME_1",
                                 agg.pop =agg.survey2,
                                 proportion = ZambiaPopWomen$admin2_urban)

head(admin.info2$data)

out1 <- rbind(cbind(admin.info1$data, type = "WorldPop"),
              cbind(admin.info1.survey$data, type = "SurveyWeight"))
out1 <- out1 %>% group_by(type) %>% mutate(pop.frac = population / sum(population))
g1 <- mapPlot(data = out1, geo = poly.adm1, size = 0.1,
              by.data = "admin1.name", by.geo = "NAME_1",
              variable = "type",  value = "pop.frac", is.long = TRUE,
              legend.label = "Population Fraction")

out2 <- rbind(cbind(admin.info2$data, type = "WorldPop"),
              cbind(admin.info2.survey$data, type = "SurveyWeight"))
out2 <- out2 %>% group_by(type) %>% mutate(pop.frac = population / sum(population))
poly.adm2$admin2.name.full=paste0(poly.adm2$NAME_1,"_",poly.adm2$NAME_2)
g2 <- mapPlot(data = out2, geo = poly.adm2, size = 0.1,
              by.data = "admin2.name.full", by.geo = "admin2.name.full",
              variable = "type",  value = "pop.frac", is.long = TRUE,
              legend.label = "Population Fraction")
g1 / g2


out2$pop.frac.within <- out2$population / out2$population.admin1
out3 <- tidyr::spread(out2[, c("admin2.name.full", "pop.frac.within", "type")],
                      type, pop.frac.within)
out3 <- left_join(out3, admin.info2$data[, c("admin2.name.full", "population")])
ggplot(out3, aes(x = SurveyWeight, y = WorldPop, size = population)) +
  geom_point(alpha = 0.5, color = "red")+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")

#7.3 Aggregating direct estimates
res_ad1agg <- directEST(data = data,
                        cluster.info = cluster.info,
                        admin = 1,
                        weight = "population",
                        admin.info = admin.info1,
                        aggregation = TRUE)
head(res_ad1agg$res.admin1)

