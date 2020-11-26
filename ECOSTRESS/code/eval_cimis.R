library(here)
library(data.table)
library(lubridate)
library(stringr)
library(dplyr)
library(sp)
library(raster)
library(ggplot2)
library(suncalc)
library(tidyr)
library(purrr)
library(sf)

#get the lost of points for the appears database
# latlon <- data.table(t(read.csv(here::here("ECOSTRESS", "data", "cimis", "LatLonfromsheets.csv"), skip = 4, row.names = 1)))
# latlon$ID <- latlon$ID %>% as.numeric()
# rownames(latlon) <- latlon$ID
# write.csv(latlon, here::here("ECOSTRESS", "data", "cimis", "LatLon.csv"))
#Then use this to get the appears points

#get the ecostress data
get_eco <- function(year){#https://cimis.water.ca.gov/Stations.aspx
  read.csv(here::here("ECOSTRESS", "data", "cimis", paste0("sj-points", year), paste0("SJ-points", year, "-ECO2LSTE-001-results.csv")))
}
eco <- rbindlist(lapply(2018:2020, get_eco))

#remove poor quality pixels
eco <- filter(eco, ECO2LSTE_001_SDS_QC_Mandatory_QA_flags_Description == "Pixel produced, best quality")
eco$ECOSTRESS <- eco$ECO2LSTE_001_SDS_LST - 273.15 #in celcius not kelvin
eco <- dplyr::select(eco, Category, ID, Latitude, Longitude, Date, ECOSTRESS)
eco$dt <- ymd_hms(eco$Date, tz = "UTC") %>% with_tz("America/Los_Angeles")
eco$date <- date(eco$dt)
eco$Date <- NULL

#get the cimis data
get_cimis <- function(year){#https://cimis.water.ca.gov/Stations.aspx
  rbind(read.csv(here::here("ECOSTRESS", "data", "cimis", paste0("SJ_", year, "_6.csv"))), 
        read.csv(here::here("ECOSTRESS", "data", "cimis", paste0("SJ_", year, "_8.csv"))) )
}
cimis <- rbindlist(lapply(2018:2020, get_cimis))

#add date time to cimis
cimis$mid_dt <- ymd_hms(paste(mdy(cimis$Date), 
                              paste0(as.numeric(str_extract(cimis$Hour..PST., regex("[1-9]+"))) - 1, ":30:00")), 
                        tz = "America/Los_Angeles")

cimis <- cimis[!is.na(mid_dt),] # remove NAs

#using the renamed file names, get the list of date times that are closest to the ecostress date times
get_mid_dts <- function(dt){
  dt <- cimis$mid_dt[abs(cimis$mid_dt - dt) == min(abs(cimis$mid_dt - dt))][1]
  dt
}
eco$mid_dt <- lapply(eco$dt, get_mid_dts) %>% purrr::reduce(c) #the closest dts to the times the cimis sensors give us
eco$Stn.Id <- eco$ID
eco$ID <- NULL

#merge ecostress and cimis data by location and date time
Comp_temp <- base::merge(x = eco, 
                         y = dplyr::select(cimis, Stn.Id, mid_dt, Air_Temp = Air.Temp..C.), 
                         by = c("Stn.Id", "mid_dt"), 
                         all.x = TRUE, all.y = FALSE) #sometimes there is more than 1 cimis measurement which is what makes the dataset grow. 

# add sunrise and sunset times to Comp_temp in order to get a day flag
sunrise_sunset <- function(row){
  getSunlightTimes(date = ymd(row[[8]]),
                   lat = row[[4]], 
                   lon = row[[5]],
                   # data = Comp_temp,
                   keep = c("sunrise", "sunset"),
                   tz = "America/Los_Angeles")
}

sunrise_set <- c()
for(i in 1:nrow(Comp_temp)){
  sunrise_set <- rbind(sunrise_set, sunrise_sunset(Comp_temp[i,]))
}

Comp_temp <- merge(Comp_temp, sunrise_set, by.x = c("date", "Latitude", "Longitude"), by.y = c("date", "lat", "lon")) #merge sunrise/sunset data 
Comp_temp$daytime <- Comp_temp$dt > Comp_temp$sunrise & Comp_temp$dt < Comp_temp$sunset #if the measurement is between sunrise and sunset
Comp_temp$dawn <- (Comp_temp$dt > (Comp_temp$sunrise - 3*3600) & Comp_temp$dt < (Comp_temp$sunrise + 3*3600)) #if the measurement is within an hour of dawn
Comp_temp$dusk <- (Comp_temp$dt > (Comp_temp$sunrise - 3*3600) & Comp_temp$dt < (Comp_temp$sunrise + 3*3600)) #if the measurement is within an hour of dusk
Comp_temp$dawn.dusk <- Comp_temp$dawn|Comp_temp$dusk

Comp_temp$Location <- Comp_temp$Category
Comp_temp$Category <- NULL

#how far into the day or night a time is relative to sunset or sunrise; whichever is closer. Negative values = night. 
dayness <- c()
for (i in 1:nrow(Comp_temp)){
  dt = hour(Comp_temp$dt[i]) + minute(Comp_temp$dt[i])/60
  sunset = hour(Comp_temp$sunset[i]) + minute(Comp_temp$sunset[i])/60
  sunrise = hour(Comp_temp$sunrise[i]) + minute(Comp_temp$sunrise[i])/60
  if (abs(dt - sunrise) < abs(dt - sunset)){ #if the time is closer to sunrise than to sunset
    dayn = (dt - sunrise) #how many hours after the sunrise is it (negative values mean night)
  } else {
    dayn = (sunset - dt) #how many hours before sunset is it (negative values mean night)
  }
  dayness <- c(dayness, dayn)
}
Comp_temp$dayness <- dayness

#whether the time is closer to sunrise (TRUE) or sunset (FALSE)
Comp_temp$morning <- abs(Comp_temp$dt - Comp_temp$sunrise) < abs(Comp_temp$dt - Comp_temp$sunset)

Comp_temp$time_dif <- as.numeric(abs(Comp_temp$dt - Comp_temp$mid_dt)) # difference between ecostress time and sensor time in seconds

#####################################################################################
# CHOSEN PLOT # CHOSEN PLOT # CHOSEN PLOT # CHOSEN PLOT # CHOSEN PLOT # CHOSEN PLOT #
#####################################################################################

#time series of temperatures on an hourly and monthly timescale. Still need to add smoothing line
ggplot(pivot_longer(Comp_temp, cols = c("Air_Temp", "ECOSTRESS"), names_to = "Sensor", values_to = "Temperature"), 
       aes(x = hour(dt) + minute(dt)/60, y = Temperature, color = Sensor)) + 
  geom_point(aes(alpha = .5)) +
  geom_smooth() + 
  xlab("Hour of day") +
  ylab("Temperature (C)") + 
  labs(title = "Temperature by hour of day in the San Joaquin Valley", 
       subtitle = "June - September, 2018 - 2020") + 
  guides(alpha = FALSE) + 
  scale_color_discrete(name = "", labels = c("Cimis Air Temperature", "Ecostress Surface Temperature")) # + 
  # facet_wrap(~year(date)) #looks like the years got hotter on average




# look at dayness and split based on whether dayness represents closeness to sunset or sunrise
ggplot(pivot_longer(Comp_temp, cols = c("Air_Temp", "ECOSTRESS"), names_to = "Sensor", values_to = "Temperature"),
       aes(x = dayness, y = Temperature, color = Sensor)) +
  geom_point(aes(alpha = .5)) +
  geom_smooth() +
  xlab("Hour of day") + 
  facet_wrap(~morning)
#year it's right around sunrise and sunset that the crossover happens. 

#ecostress vs air temperature
Comp_temp$time_of_day <- ifelse(Comp_temp$daytime, "Day", "Night")
Comp_temp$time_of_day <- ifelse(abs(Comp_temp$dayness)<2, "Dawn/Dusk", Comp_temp$time_of_day)

#See if latitude makes a difference
ggplot(Comp_temp, aes(x = ECOSTRESS, y = Air_Temp)) + 
  geom_point(aes(color = Latitude, shape = time_of_day, alpha = .2)) +
  stat_function(fun = function(x){x}) + 
  labs(title = "ECOSTRESS vs. air temperature in the San Joaquin Valley", 
       subtitle = "Selected images from June - September, 2018 - 2020") + 
  xlab("ECOSTRESS (C)") + 
  ylab("Air Temperature (C)")

#see if year makes a difference
ggplot(Comp_temp, aes(x = ECOSTRESS, y = Air_Temp)) + 
  geom_point(aes(color = as.factor(year(date)), alpha = .2)) +
  stat_function(fun = function(x){x}) + 
  labs(title = "ECOSTRESS vs. air temperature in the San Joaquin Valley", 
       subtitle = "Selected images from June - September, 2018 - 2020") + 
  xlab("ECOSTRESS (C)") + 
  ylab("Air Temperature (C)")

#####################################################################################
# CHOSEN PLOT # CHOSEN PLOT # CHOSEN PLOT # CHOSEN PLOT # CHOSEN PLOT # CHOSEN PLOT #
#####################################################################################

ggplot(Comp_temp, aes(x =  Air_Temp, y =ECOSTRESS)) + 
  geom_point(aes(color = dayness, alpha = .2)) +
  stat_function(fun = function(x){x}, show.legend = TRUE) + 
  labs(title = "ECOSTRESS vs. air temperature in the San Joaquin Valley", 
       subtitle = "June - September, 2018 - 2020", 
       caption = "Air temperature values from CIMIS weather stations.
       Hours into the day calculated as the number of hours from sunrise or sunset, whichever is closest. 
       Values near zero represent times near sunrise or sunset. 
       Positive values represent daytime while negative values represent night.") + 
  ylab("ECOSTRESS (C)") + 
  xlab("Air Temperature (C)") + 
  ylim(min = 5, max = 60) + 
  xlim(min = 5, max = 45) + 
  scale_colour_gradient2(low = "blue", high = "red") + 
  theme_dark() + 
  geom_smooth(method = lm, show.legend = TRUE) + 
  guides(alpha = FALSE) + 
  labs(color = "Hours into the day")

#####################################################################################
# CHOSEN PLOT # CHOSEN PLOT # CHOSEN PLOT # CHOSEN PLOT # CHOSEN PLOT # CHOSEN PLOT #
#####################################################################################

#split by time of day

ggplot(Comp_temp, aes(x =  Air_Temp, y =ECOSTRESS)) + 
  geom_point(aes(color = dayness, alpha = .2)) +
  stat_function(fun = function(x){x}, show.legend = TRUE) + 
  labs(title = "ECOSTRESS vs. air temperature in the San Joaquin Valley", 
       subtitle = "June - September, 2018 - 2020", 
       caption = "Air temperature values from CIMIS weather stations.
       Hours into the day calculated as the number of hours from sunrise or sunset, whichever is closest. 
       Values near zero represent times near sunrise or sunset. 
       Positive values represent daytime while negative values represent night.
       Dawn/Dusk include any values between -2 and 2.") + 
  ylab("ECOSTRESS (C)") + 
  xlab("Air Temperature (C)") + 
  ylim(min = 5, max = 60) + 
  xlim(min = 5, max = 45) + 
  scale_colour_gradient2(low = "blue", high = "red") + 
  theme_dark() + 
  geom_smooth(method = lm, show.legend = TRUE) + 
  guides(alpha = FALSE) + 
  labs(color = "Hours into the day") + 
  facet_wrap(~time_of_day)


#find lm for correction to air temperature rather than surface temperature
lm(formula = Air.Temp..C.~eco_vals, data = Comp_temp)


# ag data from http://www.kernag.com/gis/gis-data.asp
# Note that 2020 might still be updated!

Comp_temp_latlon <- unique(dplyr::select(Comp_temp, Latitude, Longitude, Stn.Id))
Comp_temp_latlon <- st_as_sf(Comp_temp_latlon, coords = c("Longitude", "Latitude"), crs = 4326) %>% st_transform(st_crs(kern2018))

ag_stns <- function(year){
  df <- st_join(Comp_temp_latlon, 
          st_read(here::here("ECOSTRESS", "data", "Kern_ag", paste0("kern", year), paste0("kern", year, ".shp"))), 
          join = st_within)
  stns <- filter(df, !is.na(DT_ACT))
  stns <- stns$Stn.Id %>% unique()
}
for(year in 2018:2020){
  assign(paste0("ag_stn", year), ag_stns(year))
}

# every year, the only station within an ag region is station 5. That's not really enough to do analysis on, but here's two plots. 

ggplot(Comp_temp, aes(x = ECOSTRESS, y = Air_Temp)) + 
  geom_point(aes(color = (Stn.Id == 5), alpha = .2)) +
  stat_function(fun = function(x){x}) + 
  labs(title = "ECOSTRESS vs. air temperature in the San Joaquin Valley", 
       subtitle = "Selected images from June - September, 2018 - 2020") + 
  ylab("ECOSTRESS (C)") + 
  xlab("Air Temperature (C)") + 
  ylim(min = 10, max = 50) + 
  xlim(min = 10, max = 50) + 
  # scale_colour_gradient2(low = "blue", high = "red") + 
  theme_dark() + 
  geom_smooth(method = lm)

ggplot(pivot_longer(Comp_temp, cols = c("Air_Temp", "ECOSTRESS"), names_to = "Sensor", values_to = "Temperature"), 
       aes(x = hour(dt) + minute(dt)/60, y = Temperature, color = Sensor)) + 
  geom_point(aes(alpha = .5)) +
  geom_smooth() + 
  xlab("Hour of day") + 
  facet_grid(~(Stn.Id == 5))

#It doesn't look out of the ordinary at all. 


# Old code from when I only looked at the locations within the study region. 

# #clear ecostress images
# files <- list.files(here::here("ECOSTRESS", "data", "June-Sept_2018-2020", "renamed"))
# files <- files[c(1-3,5-10, 12-17, 19-28, 30-40, 42-52, 54-68, 70-73)] #good images in celcius
# 
# get_cimis <- function(year){
#   read.csv(here::here("ECOSTRESS", "data", "cimis", paste0("cimis_", year, ".csv"))) #https://cimis.water.ca.gov/Stations.aspx
# }
# 
# cimis <- rbindlist(lapply(2018:2020, get_cimis))
# 
# cimis$mid_dt <- ymd_hms(paste(mdy(cimis$Date),
#                               paste0(as.numeric(str_extract(cimis$Hour..PST., regex("[1-9]+"))) - 1, ":30:00")),
#                         tz = "America/Los_Angeles") #add date time
# 
# cimis$date <- date(cimis$mid_dt) #date in date format
# 
# cimis <- cimis[!is.na(mid_dt),] #3 mid_dt nas...
# 
# get_mid_dts <- function(file){
#   date <- ymd(substr(file, 14, 23))
#   hhmmss <- substr(file, 5, 12)
#   dt <- ymd_hms(paste(date, hhmmss), tz = "America/Los_Angeles")
#   dt <- unique(cimis$mid_dt[abs(cimis$mid_dt - dt) == min(abs(cimis$mid_dt - dt))])
#   dt
# }
# 
# eco_dts <- lapply(files, get_mid_dts) #the closest dts to the times the cimis sensors give us
# 
# # cimis <- cimis[mid_dt %in% eco_dts, ]
# 
# #originally tried these four sensors, but only 39 and 80 ended up being in the study area.
# latslons <- data.frame(Stn.Id = c(2, 39, 142, 80),
#                        lat = c(36.336222, 36.597444, 36.721083, 36.820833),
#                        lon = c(-120.11291, -119.50404, -119.38903, -119.74231))
# 
# # cimis <- merge(cimis, latslons, by = "Stn.Id")
# 
# # 2
# # latitude: 36.336222
# # longitude: -120.11291
# #
# # 39
# # latitude: 36.597444
# # longitude: -119.50404
# #
# # 142
# # latitude: 36.721083
# # longitude: -119.38903
# #
# # 80
# # latitude: 36.820833
# # longitude: -119.74231
# 
# 
# #pull the value from the closest ecostress pixel to the sensor
# get_eco_points <- function(file){
# 
#   date <- ymd(substr(file, 14, 23))
#   hhmmss <- substr(file, 5, 12)
#   dt <- ymd_hms(paste(date, hhmmss), tz = "America/Los_Angeles")
# 
#   df <- latslons
#   df$dt <- dt
#   df$mid_dt <- unique(cimis$mid_dt[abs(cimis$mid_dt - dt) == min(abs(cimis$mid_dt - dt))])
#   coordinates(latslons) <- ~lon+lat
#   latslons = SpatialPoints(latslons, proj4string = CRS("+init=epsg:4326"))
# 
#   raster <- raster(paste0(here::here("ECOSTRESS", "data", "June-Sept_2018-2020", "renamed"), "/",
#                           file))
# 
#   df$eco_vals <- raster::extract(raster, latslons)
#   df
# }
# 
# ecostress <- rbindlist(lapply(files, get_eco_points))
# ecostress <- ecostress[!is.na(eco_vals),] # get rid of sensors outside of study area.
# 
# Comp_temp <- merge(ecostress, cimis, by = c("Stn.Id", "mid_dt"))
# 
# #three hour lag on soil temperature
# three_h_lag <- cimis[,.(Stn.Id = Stn.Id, mid_dt = mid_dt - 3*(60*60), Soil.Temp..C._3h = Soil.Temp..C.),]
# Comp_temp <- merge(Comp_temp, three_h_lag, by = c("Stn.Id", "mid_dt"))
# 
# # add sunrise and sunset times to Comp_temp in order to get a day flag
# sunrise_sunset <- function(row){
#   getSunlightTimes(date = lubridate::date(row[[2]]),
#                    lat = row[[3]],
#                    lon = row[[4]],
#                    # data = Comp_temp,
#                    keep = c("sunrise", "sunset"),
#                    tz = "America/Los_Angeles")
# }
# 
# # apply(Comp_temp, 1, sunrise_sunset) #no clue why this doesn't work... back to the loops :'()
# sunrise_set <- c()
# for(i in 1:nrow(Comp_temp)){
#   sunrise_set <- rbind(sunrise_set, sunrise_sunset(Comp_temp[i,]))
# }
# 
# Comp_temp <- merge(Comp_temp, sunrise_set, by = c("date", "lat", "lon"))
# Comp_temp$daytime <- Comp_temp$dt > Comp_temp$sunrise & Comp_temp$dt < Comp_temp$sunset
# Comp_temp$dawn <- (Comp_temp$dt > (Comp_temp$sunrise - 3600) & Comp_temp$dt < (Comp_temp$sunrise + 3600)) |
#   (Comp_temp$dt > (Comp_temp$sunset - 3600) & Comp_temp$dt < (Comp_temp$sunset + 3600))
# 
# dayness <- c()
# for (i in 1:nrow(Comp_temp)){
#   dt = hour(Comp_temp$dt[i]) + minute(Comp_temp$dt[i])/60
#   sunset = hour(Comp_temp$sunset[i]) + minute(Comp_temp$sunset[i])/60
#   sunrise = hour(Comp_temp$sunrise[i]) + minute(Comp_temp$sunrise[i])/60
#   if (abs(dt - sunrise) < abs(dt - sunset)){ #if the time is closer to sunrise than to sunset
#     dayn = (dt - sunrise) #how many hours after the sunrise is it (negative values mean night)
#   } else {
#     dayn = (sunset - dt) #how many hours before sunset is it (negative values mean night)
#   }
#   dayness <- c(dayness, dayn)
# }
# 
# 
# Comp_temp$dayness <- dayness
# 
# Comp_temp$time_dif <- as.numeric(abs(Comp_temp$dt - Comp_temp$mid_dt)) # difference between ecostress time and sensor time in seconds
#
# #comparing ecostress and soil temp values, with and without 3 hour lag. 
# ggplot(Comp_temp, aes(x = eco_vals, y = Soil.Temp..C.)) + 
#   geom_point(aes(color = daytime, shape = as.factor(Stn.Id))) + 
#   geom_smooth(aes(color = daytime), method = "lm")
# 
# ggplot(Comp_temp, aes(x = eco_vals, y = Soil.Temp..C._3h)) + 
#   geom_point(aes(color = daytime, shape = as.factor(Stn.Id))) + 
#   geom_smooth(aes(color = daytime), method = "lm")
# 
# #ecostress and air temperature values
# ggplot(Comp_temp, aes(x = eco_vals, y = Air.Temp..C.)) + 
#   geom_point(aes(color = dawn, shape = as.factor(Stn.Id))) +
#   stat_function(fun = function(x){x})
# 
# ggplot(Comp_temp, aes(x = eco_vals, y = Air.Temp..C.)) + 
#   geom_point(aes(color = daytime, shape = as.factor(Stn.Id))) +
#   stat_function(fun = function(x){x})
# 
# #air temp and soil temp...
# ggplot(Comp_temp, aes(x = Soil.Temp..C., y = Air.Temp..C.)) + 
#   geom_point(aes(color = daytime, shape = as.factor(Stn.Id)))
# 
# #time series of temperatures on an hourly and monthly timescale. 
# ggplot(Comp_temp) + 
#   geom_line(aes(x = hour(dt) + minute(dt)/60, y = eco_vals), color = "red") + 
#   geom_line(aes(x = hour(dt) + minute(dt)/60, y = Soil.Temp..C.), color = "green") + 
#   geom_line(aes(x = hour(dt) + minute(dt)/60, y = Soil.Temp..C._3h), color = "darkgreen") + 
#   geom_line(aes(x = hour(dt) + minute(dt)/60, y = Air.Temp..C.), color = "blue")
# 
# ggplot(Comp_temp) + 
#   geom_line(aes(x = month(dt) + day(dt)/31, y = eco_vals), color = "red") + 
#   geom_line(aes(x = month(dt) + day(dt)/31, y = Soil.Temp..C.), color = "green") + 
#   geom_line(aes(x = month(dt) + day(dt)/31, y = Soil.Temp..C._3h), color = "darkgreen") + 
#   geom_line(aes(x = month(dt) + day(dt)/31, y = Air.Temp..C.), color = "blue") + 
#   facet_wrap(~as.factor(year(dt)))
# 
# 
# 
# #pretty plot 
# Comp_temp$time_of_day <- ifelse(Comp_temp$daytime, "Day", "Night")
# Comp_temp$time_of_day <- ifelse(Comp_temp$dawn, "Dawn/Dusk", Comp_temp$time_of_day)
# 
# ggplot(Comp_temp, aes(x = eco_vals, y = Air.Temp..C.)) + 
#   geom_point(aes(color = time_of_day, shape = as.factor(Stn.Name))) +
#   stat_function(fun = function(x){x}) + 
#   labs(title = "ECOSTRESS vs. air temperature in the San Joaquin Valley", 
#        subtitle = "Selected images from June - September, 2018 - 2020") + 
#   xlab("ECOSTRESS (C)") + 
#   ylab("Air Temperature (C)")
# 
# ggplot(Comp_temp, aes(y = eco_vals, x = Air.Temp..C.)) + 
#   geom_point(aes(color = dayness, shape = as.factor(Stn.Name))) +
#   stat_function(fun = function(x){x}) + 
#   labs(title = "ECOSTRESS vs. air temperature in the San Joaquin Valley", 
#        subtitle = "Selected images from June - September, 2018 - 2020") + 
#   ylab("ECOSTRESS (C)") + 
#   xlab("Air Temperature (C)") + 
#   ylim(min = 10, max = 50) + 
#   xlim(min = 10, max = 50) + 
#   scale_colour_gradient2(low = "blue", high = "red") + 
#   theme_dark() + 
#   geom_smooth(method = lm)
# 
# #split by night or day
# 
# ggplot(Comp_temp, aes(y = eco_vals, x = Air.Temp..C.)) + 
#   geom_point(aes(color = dayness, shape = as.factor(Stn.Name))) +
#   stat_function(fun = function(x){x}) + 
#   labs(title = "ECOSTRESS vs. air temperature in the San Joaquin Valley", 
#        subtitle = "Selected images from June - September, 2018 - 2020") + 
#   ylab("ECOSTRESS (C)") + 
#   xlab("Air Temperature (C)") + 
#   ylim(min = 10, max = 50) + 
#   xlim(min = 10, max = 50) + 
#   scale_colour_gradient2(low = "blue", high = "red") + 
#   theme_dark() +
#   facet_wrap(~daytime) + 
#   geom_smooth(method = lm)
# 
# 
# #find lm for correction to air temperature rather than surface temperature
# lm(formula = Air.Temp..C.~eco_vals, data = Comp_temp)


#identify the date times that are closest and furthest to sunset and sunrise for Night_dawn_dusk_day.R
