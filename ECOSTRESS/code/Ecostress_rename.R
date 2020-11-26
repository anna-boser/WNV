library(lubridate)
library(here)
library(stringr)

files <- list.files(path = here::here("ECOSTRESS", "data", "June-Sept_2018-2020", "raw", "LST"), pattern = "tif", recursive = TRUE)[202:1]

#imgaes sorted based on LST_Stack under data/raw/LST

perfect <- files[c(1,6,7,9,10,12,13,15,19,21,22,25,27,31,34,35,38,40,41,43,44,45,46,47,48,49,52,54,56,60,63,64,67,69,
             70,72,78,80,83,94,95,96,97,98,102,103,104,111,112,118,124,128,132,133,134,135,137,142,146,158,159,160,165,170,
             172,176,181,184,189,190,191,199,202)]
chopped <- files[c(3,4,8,11,14,16,17,18,20,26,28,50,58,65,66,71,86,87,106,108,113,120,123,125,126,127,136,138,139,140,
             141,156,157,161,162,164,168,177,179,180,193,194)]
issues <- files[c(2,5,23,24,29,30,32,33,36,37,39,42,51,53,55,57,59,61,62,68,73,74,75,76,77,79,81,82,84,85,88,89,90,91,92,
            93,99,100,101,105,107,109,110,114,115,116,117,119,121,122,129,130,131,143,144,145,147,148,149,150,151,152,153,
            154,155,163,166,167,169,171,173,174,175,178,182,183,185,186,187,188,192,195,196,197,198,200,201)]

my.file.copy <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.copy(from = from,  to = to)
}

move.file <- function(file, type){
  my.file.copy(from = here::here("ECOSTRESS", "data", "June-Sept_2018-2020", "raw", "LST", file),
              to = here::here("ECOSTRESS", "data", "June-Sept_2018-2020", "raw", "LST", type, file))
}

lapply(perfect, move.file, "perfect")
lapply(chopped, move.file, "chopped")
lapply(issues, move.file, "issues")

rename <- function(file){
  
  nums <- str_extract(file, regex('(?<=doy)[0-9]+'))
  year <- substr(nums, 1, 4)
  doy <- substr(nums, 5, 7) %>% as.numeric()
  hhmmss <- substr(nums, 8, 13)
  date <- as.Date(doy - 1, origin = paste0(year, "-01-01"))
  dt <- ymd_hms(paste(date, hhmmss), tz = "UTC") %>% with_tz("America/Los_Angeles")
  month <- format(dt,"%m")
  hour <- hour(dt)
  day <- format(dt,"%d")
  time <- substr(dt, 12, 19)
  
  name <- paste0("eco", "_", time, "_", year, "_", month, "_", day, ".tif")
  
  my.file.copy(from = here::here("ECOSTRESS", "data", "June-Sept_2018-2020", "raw", "LST", "perfect", file), 
               to = here::here("ECOSTRESS", "data", "June-Sept_2018-2020", "renamed", name))
  #   my.file.copy(from = here::here("ECOSTRESS", "data", "June-Sept_2018-2020", "raw", "LST", "perfect", file), 
  #             to = here::here("ECOSTRESS", "data", "June-Sept_2018-2020", "organized", year, month, hour, name))
}

lapply(perfect, rename)


# a couple diagnostics of what we've got and what we threw out

df <- data.frame(file = files, class = NA)
df$class[df$file %in% perfect] <- "perfect"
df$class[df$file %in% issues] <- "issues"
df$class[df$file %in% chopped] <- "chopped"

df$nums <- str_extract(df$file, regex('(?<=doy)[0-9]+'))
df$year <- substr(df$nums, 1, 4)
df$doy <- substr(df$nums, 5, 7) %>% as.numeric()
df$hhmmss <- substr(df$nums, 8, 13)
df$date <- as.Date(df$doy - 1, origin = paste0(df$year, "-01-01"))
df$dt <- ymd_hms(paste(df$date, df$hhmmss), tz = "UTC") %>% with_tz("America/Los_Angeles")
df$month <- month(df$dt, label = TRUE, abbr = TRUE)
df$hour <- hour(df$dt)
df$day <- day(df$dt)
df$time <- substr(df$dt, 12, 19)


ggplot(filter(df, class == "perfect")) + geom_bar(aes(x = hour, fill = class), position = "dodge") #+ facet_grid(.~month)
# doesn't look like we're getting rid of images disproportionately by month. I do worry that we're biasing temperature because we don't have cloudy days. 
ggplot(filter(df, class != "issues")) + geom_bar(aes(x = hour, fill = class)) #+ facet_grid(.~month)

ggplot(filter(df, class != "issues")) + geom_bar(aes(x = hour, fill = class)) + facet_grid(year~month)

