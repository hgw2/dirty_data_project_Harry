# packages
# Packages -----
library(tidyverse)
library(readxl)
library(janitor)
library(assertr)

# Read in data -----
candy_2015 <- read_csv("raw_data/csv/candy_2015.csv")
candy_2016 <- read_csv("raw_data/csv/candy_2016.csv")
candy_2017 <- read_csv("raw_data/csv/candy_2017.csv")

candy_all <- rbind(candy_2015, candy_2016, candy_2017)

candy_all <- candy_all %>%
  drop_na(rating)

# Clean candy data

candy_cleaned <- candy_all %>%
  mutate(candy_type = str_replace_all(candy_type, "_", " ")) %>%
  mutate(candy_type = case_when(
    str_detect(candy_type, "m ms") ~ "m&ms",
    str_detect(candy_type, "jolly rancher") ~ "jolly ranchers",
    str_detect(candy_type, "bonkers") ~ "bonkers",
    str_detect(candy_type, "smarties") ~ "smarties",
    str_detect(candy_type, "anonymous brown globs") ~ "mary janes",
    str_detect(candy_type, "boxo raisins") ~ "box o raisins",
    str_detect(candy_type, "licorice") ~ "licorice",
    str_detect(candy_type, "sweetums") ~ "sweetums",
    str_detect(candy_type, "dvd | blue ray| comic") ~ "books or films",
    str_detect(candy_type, "acetaminophen| vicodin | dental") ~ "medical items",
    TRUE ~ candy_type
  ))


# Check data -----
stopifnot(
  ncol(candy_cleaned) == 7
)

candy_cleaned %>%
  verify(names(candy_cleaned) == c(
    "going_out",
    "year",
    "country",
    "gender",
    "age",
    "candy_type",
    "rating"
  ))

# recode country 
candy_countries <- candy_cleaned %>%
  mutate(country = recode(country, "us" = "us", 
                          "canada" = "canada", 
                          "uk" = "uk", 
                          .default = "other")
  ) 

# give ratings a score

candy_num_rated <- candy_countries%>%
  mutate(rating = case_when(
    rating == "despair" ~ -1,
    rating == "joy" ~ 1,
    rating == "meh" ~ 0
  ))



# write_csv
candy_countries %>%
  write_csv("clean_data/candy_all_clean.csv")

candy_num_rated %>%
  write_csv("clean_data/candy_all_rated_clean.csv")
