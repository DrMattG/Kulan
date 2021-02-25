### Upgrade rgee!
#devtools::install_github("r-spatial/rgee")

library(rgee)

ee_install()
ee_check()
rgee::ee_install_upgrade()
#rgee::ee_clean_pyenv()

earthengine_python <- Sys.getenv("EARTHENGINE_PYTHON", unset = NA)
print(earthengine_python)
Sys.setenv(RETICULATE_PYTHON = earthengine_python)
ee_current_version <- system.file("python/ee_utils.py", package = "rgee")
ee_utils <- rgee:::ee_source_python(ee_current_version)
 print(ee_utils$ee$'__version__')
#
ee_Initialize() # Follow the instructions and repeat the process alot!!!!!


#TEST TO SEE IF IT WORKS - If you get a map it worked!!
createTimeBand <-function(img) {
  year <- ee$Date(img$get('system:time_start'))$get('year')$subtract(1991L)
  ee$Image(year)$byte()$addBands(img)
}

collection <- ee$
  ImageCollection('NOAA/DMSP-OLS/NIGHTTIME_LIGHTS')$
  select('stable_lights')$
  map(createTimeBand)

col_reduce <- collection$reduce(ee$Reducer$linearFit())
col_reduce <- col_reduce$addBands(
  col_reduce$select('scale'))
ee_print(col_reduce)
Map$setCenter(9.08203, 47.39835, 3)
Map$addLayer(
  eeObject = col_reduce,
  visParams = list(
    bands = c("scale", "offset", "scale"),
    min = 0,
    max = c(0.18, 20, -0.18)
  ),
  name = "stable lights trend"
)

#####################################################

image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")

# Display the image.
Map$centerObject(image)
Map$addLayer(image, name = "Landsat 8 original image")

# Define visualization parameters in an object literal.
vizParams <- list(
  bands = c("B5", "B4", "B3"),
  min = 5000, max = 15000, gamma = 1.3
)

Map$addLayer(image, vizParams, "Landsat 8 False color")

# Use Map to add features and feature collections to the map. For example,
counties <- ee$FeatureCollection("TIGER/2016/Counties")

Map$addLayer(
  eeObject = counties,
  visParams = vizParams,
  name = "counties"
)

