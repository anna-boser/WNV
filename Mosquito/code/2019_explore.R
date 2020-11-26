library(here)
library(data.table)
library(sf)
library(dplyr)
library(lubridate)
library(xml2)
library(rvest)
# library(XML)
# library(magrittr)
library(stringr)

CA_State <- st_read(dsn = "~/Downloads/tl_2017_us_state", layer = "tl_2017_us_state") %>% filter(STUSPS == "CA")
# CA_County <- st_read(dsn = "~/Downloads/tl_2018_us_county", layer = "tl_2018_us_county")

scrape <- function(start = ymd(20030101), end = "month"){
  
  if (is.character(end)){ #Get the end date in the format required
    end <- (start %m+% months(1) - 1) %>% as.character() 
  } #Note that the dates are not inclusive 
  
  start <- paste0(substr(start, 1,4), substr(start, 6, 7), substr(start, 9, 10)) #Format required
  end <- paste0(substr(end, 1,4), substr(end, 6, 7), substr(end, 9, 10))
  
  URL <- paste0("https://mathew.vectorsurv.org/v2/arbo/surv/", start, "-", end, "?targets=WNVPools")
  html <- read_html(URL)
  txt <- html_text(html_nodes(html,"p")) #Get the text with the data embedded
  
  Lon <- str_extract_all(txt, regex('[0-9.-]+(?=,)'))[[1]] %>% as.numeric() #extract all longitudes
  Lat <- str_extract_all(txt, regex('[0-9.-]+(?=])'))[[1]] %>% as.numeric()#extract all latitudes
  Mosquitos <- str_extract_all(txt, regex('(?<=:)[0-9]'))[[1]] %>% as.numeric()#extract all values
  Year <- year(ymd(start))
  Month <- month(ymd(start))
  
  DF <- data.frame(Lat, Lon, Mosquitos, Year, Month)
  
  return(DF)
}

DF <- scrape(start = ymd(20190101), end = ymd(20191206))


DF_sf = st_as_sf(DF, coords = c("Lon", "Lat"), 
                 crs = 4269, agr = "constant")

DF_sf <- st_intersection(DF_sf, CA_State)

Locations <- DF_sf$geometry %>% unique() %>% length()




