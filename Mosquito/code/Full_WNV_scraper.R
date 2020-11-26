library(xml2)
library(rvest)
library(stringr)
# library(lubridate)
library(data.table)
library(here)
library(tidyr)
library(conflicted)


URL <- paste0("https://mathew.vectorsurv.org/v2/arbo/")
html <- read_html(URL)
txt <- html_text(html_nodes(html,"p")) #Get the text with the data embedded

Lon <- str_extract_all(txt, regex('(?<="coordinates":\\[)[0-9.-]+(?=,)'))[[1]] %>% as.numeric() #extract all longitudes
Lat <- str_extract_all(txt, regex('(?<=,)[0-9.-]+(?=\\])'))[[1]] %>% as.numeric() #extract all latitudes
City <- str_extract_all(txt, regex('(?<="city":")[^"]*(?=")'))[[1]]
Type <- str_extract_all(txt, regex('(?<="type":")[^"]*(?=","collections")'))[[1]]
Collections <- str_extract_all(txt, regex('(?<="collections":\\[)[^\\]]*(?=\\])'))[[1]] 

DF <- data.frame(Lat, Lon, City, Type, Collections)
DF = separate_rows(DF,5,sep = ",")
DF$Month <- substr(DF$Collections, 3, 5)
DF$Year <- substr(DF$Collections, 7, 10) %>% as.numeric()
DF$Count <- str_extract_all(DF$Collections, regex('(?<=:)[0-9]+')) %>% as.numeric()
  
month_list <- list("Sep" = "09", "Oct" = "10", "Jul" = "07", "Jun" = "06", "Aug" = "08", "Apr" = "04", "May" = "05", "Nov" = "11", "Mar" = "03", "Dec" = "12", "Jan" = "01", "Feb" = "02")
DF$Date <- as.Date(paste0(DF$Year, "/", month_list[DF$Month], "/01"), "%Y/%m/%d")
DF$Month <- month_list[DF$Month] %>% as.numeric

DF$Collections <- NULL

fwrite(DF, here("data", "WNV_full.csv"))

