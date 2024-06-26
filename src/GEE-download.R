
#https://cran.r-project.org/web/packages/rgee/vignettes/rgee01.html

#install.packages("rgee")
library(rgee)

ee_check()
# ee_clean_pyenv()
# ee_install(py_env = "rgee")
#1
# ee_install_set_pyenv(py_path="C:/Users/andre/AppData/Local/r-miniconda/envs/rgee/python.exe",
#                      py_env = "YOUR_ENV")

# ee_Initialize(user = 'amertens@berkeley.edu')
# ee_Authenticate()
# ee_clean_user_credentials()

srtm <- ee$Image("USGS/SRTMGL1_003")
viz <- list(
  max = 4000,
  min = 0,
  palette = c("#000000","#5AAD5A","#A9AD84","#FFFFFF")
)

Map$addLayer(
  eeObject = srtm,
  visParams =  viz,
  name = 'SRTM'
)



test <- ee$Image("BIOPAMA/GlobalOilPalm/v1")
test$bandNames()$getInfo()

srtm$bandNames()$getInfo()



# Example dataset ID for soil Zinc (replace with the actual dataset ID)
dataset_id <- "your_soil_zinc_dataset"

# Load the dataset
soilZincData <- ee$ImageCollection(dataset_id)$first()


#https://developers.google.com/earth-engine/datasets/catalog/ISDASOIL_Africa_v1_zinc_extractable


library(rgee)
ee_Initialize()
db <- 'CGIAR/SRTM90_V4'
db <- 'Oxford/MAP/IGBP_Fractional_Landcover_5km_Annual'


image <- ee$Image(db)
image$bandNames()$getInfo()
#> [1] "elevation"
