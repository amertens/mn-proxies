
# install.packages("malariaAtlas")
# install.packages("terra")
library(terra)
library(malariaAtlas)



#all countries and their ISO codes
listPRPointCountries(version = "202406")


#all rasters to get
listRaster()
MDG_pr_data <- getPR(country = "Ghana", species = "both")

#getting a country specific shapefile
MDG_shp <- getShp(ISO = "GHA", admin_level = "admin0")
autoplot(MDG_shp)

# Get the raster data for the specified dataset and shapefile (sometimes you may need to specify year)
MDG_PfPR2_10 <- getRaster(dataset_id = "Malaria__202206_Global_Pf_Mortality_Count", shp = MDG_shp, year =2017)

#plot the raster
p <- autoplot(MDG_PfPR2_10, shp_df = MDG_shp)
p

# Save the raster to a file with a name denotine the country and data file (and year if applicable)
# Common formats include GeoTIFF (.tif), ESRI ASCII (.asc), and others
writeRaster(my_raster, "path/to/output_file.tif", overwrite=TRUE)
