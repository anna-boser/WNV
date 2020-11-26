
library(here)
library(stringr)

files <- list.files(path = here::here("ECOSTRESS", "data", "June-Sept_2018-2020", "renamed"), pattern = "tif", recursive = TRUE)
time <- as.numeric(substr(files, 5, 6)) + as.numeric(substr(files, 8, 9))/60

save(time, file = here::here("ECOSTRESS", "data", "June-Sept_2018-2020", "times"), ascii = TRUE)

#had to get rid of header and footer...