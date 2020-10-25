# https://stackoverflow.com/questions/11433432/how-to-import-multiple-csv-files-at-once

library(tidyverse)
library(here)


temp <- list.files(path = here("responses"), pattern="*.csv", full.names = TRUE) # Worry about "full.names" when deployed to shinyapps
myfiles <- lapply(temp, read_csv)

myfiles %>% bind_rows()
