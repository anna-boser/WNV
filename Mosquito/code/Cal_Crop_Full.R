library(here)
library(sf)
library(data.table)
library(conflicted)
library(dplyr)

DF <- fread(here("data", "WNV_full.csv"))

DF$Date <- as.Date(DF$Date)

DF <- DF[,.(Count = sum(Count)), by = .(Lat, Lon, City, Type, Year, Month, Date)] #add up identical time/type/locations

CA_State <- st_read(dsn = here("data", "ca-state-boundary", "CA_State_TIGER2016.shp"), layer = "CA_State_TIGER2016") %>% st_transform(4326)

DF = st_as_sf(cbind(DF, DF$Lat, DF$Lon), coords = c("V3", "V2"), crs = 4326, agr = "constant")

DF <- st_intersection(DF, CA_State$geometry)

DF$geometry <- NULL

fwrite(DF, here("data", "WNV_Cropped.csv"))



#County version: 

DF <- fread(here("data", "WNV_full.csv"))

DF$Date <- as.Date(DF$Date)

DF <- DF[,.(Count = sum(Count)), by = .(Lat, Lon, City, Type, Year, Month, Date)] #add up identical time/type/locations

CA_Counties <- st_read(dsn = here("data", "CA_Counties", "CA_Counties_TIGER2016.shp"), layer = "CA_Counties_TIGER2016") %>% st_transform(4326)
CA_Counties <- CA_Counties %>% select("County" = NAMELSAD)

DF = st_as_sf(cbind(DF, DF$Lat, DF$Lon), coords = c("V3", "V2"), crs = 4326, agr = "constant")

DF <- st_intersection(DF, CA_Counties)

DF$geometry <- NULL

fwrite(DF, here("data", "WNV_Counties.csv"))


# County shapefile

DF <- merge(DF, CA_Counties, by = "County")

st_write(DF, here("data", "WNV_Counties.shp"), layer = "WNV_Counties")
