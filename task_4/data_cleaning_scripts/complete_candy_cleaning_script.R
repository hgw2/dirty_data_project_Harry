#packages
# Packages -----
library(tidyverse)
library(readxl)
library(janitor)

# Read in data -----
candy_2015 <- read_csv("raw_data/csv/candy_2015.csv")
candy_2016 <- read_csv("raw_data/csv/candy_2016.csv")
candy_2017 <- read_csv("raw_data/csv/candy_2017.csv")

candy_all <- rbind(candy_2015,candy_2016, candy_2017)

candy_all <- candy_all %>% 
  drop_na(rating)