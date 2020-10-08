
library(xml2)
library(rvest)
# library(XML)
# library(magrittr)
library(stringr)
library(lubridate)
library(data.table)
library(here)

scrape <- function(start = ymd(20060101), end = "month"){
  
  if (end == "month"){ #Get the end date in the format required
    end <- start %m+% months(1) %>% as.character() 
    end <- paste0(substr(end, 1,4), substr(end, 6, 7), substr(end, 9, 10))
  } #Note that the dates are not inclusive 
  
  start <- paste0(substr(start, 1,4), substr(start, 6, 7), substr(start, 9, 10)) #Format required
  
  URL <- paste0("https://mathew.vectorsurv.org/v2/arbo/surv/", start, "-", end, "?targets=WNVPools")
  html <- read_html(URL)
  txt <- html_text(html_nodes(html,"p")) #Get the text with the data embedded
  
  Lon <- str_extract_all(txt, regex('[0-9.-]+(?=,)'))[[1]] #extract all longitudes
  Lat <- str_extract_all(txt, regex('[0-9.-]+(?=])'))[[1]] #extract all latitudes
  Mosquitos <- str_extract_all(txt, regex('(?<=:)[0-9]'))[[1]] #extract all values
  start <- ymd(start)
  end <- ymd(end)
  
  DF <- data.frame(Lat, Lon, Mosquitos, "Start" = start, "End" = end)
  
  return(DF)
}

list <- ymd(20060101) %m+% months(1:(12*13+9))

DF <- rbindlist(lapply(list, scrape))

fwrite(DF, here("WNV.csv"))

