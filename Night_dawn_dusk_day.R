library(raster)
library(stringr)
library(ggplot2)

#Comp_temp comes from the old cimis stuff...
night <- Comp_temp$dt[Comp_temp$dayness == min(Comp_temp$dayness)][1]
day <- Comp_temp$dt[Comp_temp$dayness == max(Comp_temp$dayness)][1]
dawn <- filter(Comp_temp, morning == TRUE)$dt[
  abs(filter(Comp_temp, morning == TRUE)$dayness) == min(abs(filter(Comp_temp, morning == TRUE)$dayness))][1]
dusk <- filter(Comp_temp, morning == FALSE)$dt[
  abs(filter(Comp_temp, morning == FALSE)$dayness) == min(abs(filter(Comp_temp, morning == FALSE)$dayness))][1]

chosen <- c(night, dawn, day, dusk)

four_panel <- function(type, correction){
  files <- list.files(here::here("ECOSTRESS", "data", "June-Sept_2018-2020", type, correction))
  
  date <- ymd(substr(files, 14, 23))
  hhmmss <- str_extract(files, regex('[0-9]{2}:{1}[0-9]{2}:{1}[0-9]{2}'))
  dt <- ymd_hms(paste(date, hhmmss), tz = "America/Los_Angeles")
  
  files <- files[dt %in% chosen]
  
  night <- raster(paste0(here::here("ECOSTRESS", "data", "June-Sept_2018-2020", type, correction), "/",
                            files[1]))
  dawn <- raster(paste0(here::here("ECOSTRESS", "data", "June-Sept_2018-2020", type, correction), "/",
                         files[2]))
  day <- raster(paste0(here::here("ECOSTRESS", "data", "June-Sept_2018-2020", type, correction), "/",
                         files[3]))
  dusk <- raster(paste0(here::here("ECOSTRESS", "data", "June-Sept_2018-2020", type, correction), "/",
                         files[4]))
  
  pal <- colorRampPalette(c("white", "red"))
  
  return(c(
    plot(night, col = pal(50)),
    plot(dawn, col = pal(50)),
    plot(day, col = pal(50)),
    plot(dusk, col = pal(50))
  ))
  
}

four_panel("pipiens_biting_rate", "not_corrected")
