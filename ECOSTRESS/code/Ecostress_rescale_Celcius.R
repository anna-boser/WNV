library(raster)

files <- paste0(here::here("ECOSTRESS", "data", "June-Sept_2018-2020", "renamed"), "/",
                list.files(here::here("ECOSTRESS", "data", "June-Sept_2018-2020", "renamed")))

celcius <- function(file){
  raster <- raster(file)
  raster <- raster*.02 - 273.15
  writeRaster(raster, file, overwrite = TRUE)
}

lapply(files, celcius)

