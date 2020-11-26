library(tmap)
library(lubridate)
library(stringr)
library(raster)
library(ggplot2)
library(dplyr)
library(sf)

time_series <- function(type, correction){
  files <- list.files(here::here("ECOSTRESS", "data", "June-Sept_2018-2020", type, correction))
  
  date <- ymd(substr(files, 14, 23))
  hhmmss <- str_extract(files, regex('[0-9]{2}:{1}[0-9]{2}:{1}[0-9]{2}'))
  dt <- ymd_hms(paste(date, hhmmss), tz = "America/Los_Angeles")
  
  # df <- data.frame(dt, Temperature = NA, ag_temp = NA, non_ag_temp = NA)
  
  df <- c()
  for (i in 1:length(files)){
    raster <- raster(paste0(here::here("ECOSTRESS", "data", "June-Sept_2018-2020", type, correction), "/",
                            files[i]))
    ag_poly <- st_read(here::here("ECOSTRESS", "data", "Kern_ag", paste0("kern", year(dt[i])), paste0("kern", year(dt[i]), ".shp"))) %>%
      st_transform(4326)

    ag <- mask(raster,
               ag_poly,
               inverse = FALSE)
    agdf <- data.frame(dt = dt[i], type = "Agrigulture", Temperature = values(ag)[!is.na(values(ag))])

    nag <- mask(raster,
                ag_poly,
                inverse = TRUE)
    nagdf <- data.frame(dt = dt[i], type = "Not Agrigulture", Temperature = values(nag)[!is.na(values(nag))])

    df <- rbind(df, agdf, nagdf)
    
    # df <- rbind(df, data.frame(dt = dt[i], Temperature = values(raster)))
  }
  
}

df <- time_series("temperature", "not_corrected")


tm_shape(raster) + 
  tm_raster() + tm_shape(ag_poly) + tm_polygons()

df1 <- df %>% group_by(dt) %>% summarize(Temperature = mean(Temperature))
ggplot(df1, aes(x = hour(dt) + minute(dt)/60, y = Temperature, alpha = .02)) +
  geom_point() + 
  geom_smooth(method = lm)
