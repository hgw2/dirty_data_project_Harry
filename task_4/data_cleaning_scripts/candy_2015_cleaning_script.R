# Packages -----
library(tidyverse)
library(readxl)
library(janitor)

# Read in data -----
candy_2015 <- read_excel("raw_data/excel/boing-boing-candy-2015.xlsx") %>%
  clean_names()


# Clean age  ------
candy_2015_age <- candy_2015 %>%
  rename(age = how_old_are_you) %>%
  mutate(age = str_remove(age, "[,\\- A-z\\+]+")) %>%
  mutate(age = as.integer(age)) %>%
  mutate(age = ifelse(age > 100, NA, age)) %>%
  mutate(age = ifelse(age < 10, NA, age))

# Fixed going trick or treating  -----
candy_2015_trick_or_treating <- candy_2015_age %>%
  rename(going_out = are_you_going_actually_going_trick_or_treating_yourself) %>%
  mutate(going_out = if_else(going_out == "Yes", TRUE, FALSE))

# make long ----
candy_2015_long <- candy_2015_trick_or_treating %>%
  pivot_longer(
    cols = butterfinger:york_peppermint_patties,
    names_to = "candy_type",
    values_to = "rating"
  ) %>%
  select(going_out, age, candy_type, rating) %>%
  mutate(rating = str_to_lower(rating))

# add country column ----
candy_2015_country <- candy_2015_long %>%
  mutate(country = NA) %>%
  mutate(gender = NA) %>%
  mutate(year = 2015) %>% 
  select(going_out, year, country, gender, age, candy_type, rating)

# Write CSV ----
candy_2015_country %>%
  write_csv("raw_data/csv/candy_2015.csv")
