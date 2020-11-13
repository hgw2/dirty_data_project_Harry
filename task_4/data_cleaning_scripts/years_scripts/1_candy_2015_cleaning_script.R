# Packages -----
library(tidyverse)
library(readxl)
library(janitor)
library(assertr)

# Read in data -----
candy_2015 <- read_excel("raw_data/excel/boing-boing-candy-2015.xlsx") %>%
  clean_names()


# Clean age  ------
# impute median age
candy_2015_age <- candy_2015 %>%
  rename(age = how_old_are_you) %>%
  mutate(age = str_remove_all(age, "[[:alpha:]]+")) %>%
  mutate(age = str_remove_all(age, "[,\\- A-z\\+><?,!\\']+")) %>%
  mutate(age = str_replace(age, "[[:space:]]", "0")) %>%
  mutate(age = as.integer(age)) %>%
  mutate(age = coalesce(age, median(age, na.rm = TRUE))) %>%
  mutate(age = ifelse(age >= 82, median(age, rm.na = TRUE), age)) %>%
  mutate(age = ifelse(age <= 10, median(age, rm.na = TRUE), age))

# Check age is within the limits
candy_2015_age %>%
  verify(age < 82 & age > 10)


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

# Check data
stopifnot(
  ncol(candy_2015_country) == 7
)

candy_2015_country %>%
  verify(names(candy_2015_country) == c(
    "going_out",
    "year",
    "country",
    "gender",
    "age",
    "candy_type",
    "rating"
  ))
# Write CSV ----
candy_2015_country %>%
  write_csv("raw_data/csv/candy_2015.csv")
