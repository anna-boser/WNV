library(raster)

files <- list.files(here::here("ECOSTRESS", "data", "June-Sept_2018-2020", "renamed"))

files <- files[c(1-3,5-10, 12-17, 19-28, 30-40, 42-52, 54-68, 70-73)]
# y2020 <- files[c(2-3,5-9,12,15,21-22,24,26,34,35,36,37,39,43,46,48,51,52,55,57-58,60-61,63,67,70-72)]
# y2019 <- files[c(1,13-14,19,23,25,27,31-32,38,44,49-50,54,56,59,62,66,73)]
# y2018 <- files[c(10,16:17,20,28,30,33,40,42,45,64:65,68)]

#coeficient and intercept for linear correction for air tamperatures found in eval_cimis
intercept = 11.5374
coef = 0.5032

apply_equ <- function(file, kind, equation){
  print(file)
  T <- raster(paste0(here::here("ECOSTRESS", "data", "June-Sept_2018-2020", "renamed"), "/",
                     file))
  raster <- equation(T)
  raster[is.na(raster[])] <- 0 
  
  dir.create(here::here("ECOSTRESS", "data", "June-Sept_2018-2020", kind))
  dir.create(here::here("ECOSTRESS", "data", "June-Sept_2018-2020", kind, "not_corrected"))
  writeRaster(raster, paste0(here::here("ECOSTRESS", "data", "June-Sept_2018-2020", kind, "not_corrected"), "/",
                             file))
  
  #corrected
  T <- T*coef + intercept
  raster <- equation(T)
  raster[is.na(raster[])] <- 0 
  
  dir.create(here::here("ECOSTRESS", "data", "June-Sept_2018-2020", kind, "corrected"))
  writeRaster(raster, paste0(here::here("ECOSTRESS", "data", "June-Sept_2018-2020", kind, "corrected"), "/",
                             file))
}

tarsalis_transmission <- function(file){
  apply_equ(file, "tarsalis_transmission", function(T){-(2.94*10^-3) * T * (T - 11.3) * (T - 41.9)})
}

pipiens_infection <- function(file){
  apply_equ(file, "pipiens_infection", function(T){-(2.56*10^-3) * T * (T - 15.6) * (T - 52.2)})
}

tarsalis_biting_rate <- function(file){
  apply_equ(file, "tarsalis_biting_rate", function(T){(1.67*10^-4) * T * (T- 2.3) * (32.0 - T)^(1/2)})
}

pipiens_biting_rate <- function(file){
  apply_equ(file, "pipiens_biting_rate", function(T){(1.70*10^-4) * T * (T- 9.4) * (39.6 - T)^(1/2)})
}

temperature <- function(file){
  apply_equ(file, "temperature", function(T){T})
}

lapply(files, tarsalis_transmission)
lapply(files, pipiens_infection)
lapply(files, tarsalis_biting_rate)
lapply(files, pipiens_biting_rate)
lapply(files, temperature)

