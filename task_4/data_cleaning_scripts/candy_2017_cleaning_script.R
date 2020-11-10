# Packages -----
library(tidyverse)
library(readxl)
library(janitor)

# Read in data -----
candy_2017 <- read_excel("raw_data/excel/boing-boing-candy-2017.xlsx") %>%
  clean_names() %>%
  rename_all(~ str_remove(., "q[0-9]+_")) %>%
  clean_names()


# Clean age  ------
candy_2017_age <- candy_2017 %>%
  mutate(age = str_remove(age, "[,\\- A-z\\+]+")) %>%
  mutate(age = as.integer(age)) %>%
  mutate(age = ifelse(age > 100, NA, age)) %>%
  mutate(age = ifelse(age < 10, NA, age))

# Fixed going trick or treating  -----
candy_2017_trick_or_treating <- candy_2017_age %>%
  mutate(going_out = if_else(going_out == "Yes", TRUE, FALSE)) %>% 
  mutate(going_out = coalesce(going_out, FALSE))

# clean_country ------
candy_2017_country <- candy_2017_trick_or_treating %>%
  mutate(country = str_to_lower(country)) %>%
  mutate(country = str_remove_all(country, "[0-9`]+")) %>%
  mutate(country = str_remove_all(country, "[[:punct:]]+")) %>%
  mutate(country = case_when(
    str_detect(country, "us") ~ "us",
    str_detect(country, "united kingdom") ~ "uk",
    str_detect(country, "united") ~ "us",
    str_detect(country, "murica") ~ "us",
    str_detect(country, "merica") ~ "us",
    str_detect(country, "cascadia") ~ "us",
    str_detect(country, "pittsburgh") ~ "us",
    str_detect(country, "u s") ~ "us",
    str_detect(country, "states") ~ "us",
    str_detect(country, "north carolina") ~ "us",
    str_detect(country, "yoo ess") ~ "us",
    str_detect(country, "trump") ~ "us",
    str_detect(country, "california") ~ "us",
    str_detect(country, "new york") ~ "us",
    str_detect(country, "merca") ~ "us",
    str_detect(country, "murrika") ~ "us",
    str_detect(country, "ud") ~ "us",
    str_detect(country, "england") ~ "uk",
    str_detect(country, "endland") ~ "uk",
    str_detect(country, "a tropical island ") ~ "NA",
    str_detect(country, "neverland") ~ "NA",
    str_detect(country, "one") ~ "NA",
    str_detect(country, "gods") ~ "NA",
    str_detect(country, "see above") ~ "NA",
    str_detect(country, "denial") ~ "NA",
    str_detect(country, "scotland") ~ "uk",
    str_detect(country, "can") ~ "canada",
    str_detect(country, "korea") ~ "south korea",
    str_detect(country, "alaska") ~ "us",
    str_detect(country, "new jersey") ~ "us",
    str_detect(country, "somewhere") ~ "NA",
    str_detect(country, "europe") ~ "NA",
    str_detect(country, "lately") ~ "NA",
    str_detect(country, "eua") ~ "NA",
    str_detect(country, "atlantis") ~ "NA",
    str_detect(country, "narnia") ~ "NA",
    str_detect(country, "subscribe") ~ "NA",
    str_detect(country, "anymore") ~ "NA",
    str_detect(country, "fear") ~ "NA",
    str_detect(country, "earth") ~ "NA",
    str_detect(country, "the netherlands") ~ "netherlands",
    str_detect(country, "^a$") ~ "NA",
    TRUE ~ country
  )) %>%
  mutate(country = na_if(country, "NA"))


# make long ----
candy_2017_long <- candy_2017_country %>%
  pivot_longer(
    cols = x100_grand_bar:york_peppermint_patties,
    names_to = "candy_type",
    values_to = "rating"
  ) %>% 
  mutate(year = 2017) %>% 
  select(going_out, year, age, country, gender, candy_type, rating) %>%
  mutate(rating = str_to_lower(rating)) %>% 
  drop_na(rating)

# Write CSV ----
candy_2017_long %>% 
  write_csv("raw_data/csv/candy_2017.csv")
