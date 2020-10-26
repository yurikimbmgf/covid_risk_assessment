# Testing the visuals


# packages ----------------------------------------------------------------

library(tidyverse)
library(here)


# data load ---------------------------------------------------------------

temp <- list.files(path = here("responses"), pattern="*.csv", full.names = TRUE) # Worry about "full.names" when deployed to shinyapps
myfiles <- lapply(temp, read_csv)

dat <- myfiles %>% bind_rows()



dat
