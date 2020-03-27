#Loaded up 

library(readxl)
library(googlesheets4)
library(janitor)
library(tidyverse)

sheets_deauth()

data <- read_sheet("1NQd723ndO2vC6sw7LFBs_I8NZTqJ35EWadwkN8hRZng") %>%
  clean_names()
 
cleaned_data <- data %>%
  select(-timestamp, -email_address) %>%
  filter(!is.na(religion_check_all_that_apply))

cleaned_data

<<<<<<< HEAD
saveRDS(object = cleaned_data, file = "shiny_app/for_shiny.RDS")
=======
saveRDS(object = cleaned_data, file = "for_shiny.RDS")

>>>>>>> eliotdmin-master
