# Dummy Data Set
library(tidyverse)
library(here)

epochTime <- function() {
  return(as.integer(Sys.time()))
}

epochTime()+sample(1:50, 1)

real_dat <- read_csv(here("responses", "20201026-193607_07e266838d2671358e8b3bec133cb7d1.csv"))
real_dat


(dummy <- data.frame(x01 = sample(1:4, size = 25, replace = TRUE),
                     x02 = sample(1:4, size = 25, replace = TRUE),
                     x03 = sample(1:4, size = 25, replace = TRUE),
                     x04 = sample(1:4, size = 25, replace = TRUE),
                     x05 = sample(1:4, size = 25, replace = TRUE),
                     x06 = sample(1:4, size = 25, replace = TRUE),
                     x07 = sample(1:4, size = 25, replace = TRUE),
                     x08 = sample(1:4, size = 25, replace = TRUE),
                     x09 = sample(1:4, size = 25, replace = TRUE),
                     x10 = sample(1:4, size = 25, replace = TRUE),
                     
                     y_01 = sample(c("in the last month", "in the last quarter", "since February", "since before February"), size = 25, replace = TRUE),
                     y_02 = sample(c("in the last month", "in the last quarter", "since February", "since before February"), size = 25, replace = TRUE),
                     y_03 = sample(c("in the last month", "in the last quarter", "since February", "since before February"), size = 25, replace = TRUE),
                     y_04 = sample(c("in the last month", "in the last quarter", "since February", "since before February"), size = 25, replace = TRUE),
                     y_05 = sample(c("in the last month", "in the last quarter", "since February", "since before February"), size = 25, replace = TRUE),
                     y_06 = sample(c("in the last month", "in the last quarter", "since February", "since before February"), size = 25, replace = TRUE),
                     y_07 = sample(c("in the last month", "in the last quarter", "since February", "since before February"), size = 25, replace = TRUE),
                     y_08 = sample(c("in the last month", "in the last quarter", "since February", "since before February"), size = 25, replace = TRUE),
                     y_09 = sample(c("in the last month", "in the last quarter", "since February", "since before February"), size = 25, replace = TRUE),
                     y_10 = sample(c("in the last month", "in the last quarter", "since February", "since before February"), size = 25, replace = TRUE),
                     
                     m_01 = sample(1:4, size = 25, replace = TRUE),
                     m_02 = sample(1:4, size = 25, replace = TRUE),
                     
                     f_01 = sample(1:4, size = 25, replace = TRUE),
                     f_02 = sample(1:4, size = 25, replace = TRUE),
                     f_03 = sample(1:4, size = 25, replace = TRUE),
                     f_04 = sample(1:4, size = 25, replace = TRUE),
                     
                     a_01 = sample(c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+"), size = 25, replace = TRUE),
                     a_02 = sample(c("Female", "Male", "Transgender", "Gender Variant / Non-Conforming"), size = 25, replace = TRUE),
                     a_03 = sample(c("Yes", "No"), size = 25, replace = TRUE),
                     a_03 = sample(c("Yes", "No"), size = 25, replace = TRUE),
                     
                     d_01 = sample(1603708889:1603768889, size = 25, replace = TRUE)
                     
                     ) 
  
  )

real_colnames <- names(real_dat)
names(dummy) <- real_colnames

dummy


write_csv(dummy, here("responses", "dummy.csv"))
