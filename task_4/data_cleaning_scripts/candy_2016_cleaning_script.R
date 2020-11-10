# Packages -----
library(tidyverse)
library(readxl)
library(janitor)

# Read in data -----
candy_2016 <- read_excel("raw_data/excel/boing-boing-candy-2016.xlsx") %>%
  clean_names()

# Clean age  ------
candy_2016_age <- candy_2016 %>%
  rename(age = how_old_are_you) %>%
  mutate(age = str_remove(age, "[,\\- A-z\\+]+")) %>%
  mutate(age = as.integer(age)) %>%
  mutate(age = ifelse(age > 100, NA, age)) %>% 
  mutate(age = ifelse(age < 10, NA, age))

# Clean columns
candy_2016_gender <- candy_2016_age %>% 
  rename(gender = your_gender) 

# Clean going trick or treating  -----
candy_2016_trick_or_treating <- candy_2016_gender %>%
  rename(going_out = are_you_going_actually_going_trick_or_treating_yourself) %>%
  mutate(going_out = if_else(going_out == "Yes", TRUE, FALSE))

# Clean country
candy_2016_country <- candy_2016_trick_or_treating %>% 
  rename(country = which_country_do_you_live_in) %>% 
  mutate(country = str_to_lower(country)) %>% 
  mutate(country = str_remove_all(country, "[0-9]+")) %>% 
  mutate(country = str_remove_all(country, "[[:punct:]]+")) %>% 
  mutate(country = case_when(str_detect(country, "us") ~ "us",
                             str_detect(country, "united kingdom") ~ "uk",
                             str_detect(country, "united") ~ "us",
                             str_detect(country, "murica") ~ "us",
                             str_detect(country, "merica") ~ "us",
                             str_detect(country, "trump") ~ "us",
                             str_detect(country, "states") ~ "us",
                             str_detect(country, "cascadia") ~ "us",
                             str_detect(country, "yoo ess") ~ "us",
                             str_detect(country, "england") ~ "uk",
                             str_detect(country, "a tropical island ") ~ "NA",
                             str_detect(country, "neverland") ~ "NA",
                             str_detect(country, "one") ~ "NA",
                             str_detect(country, "gods") ~ "NA",
                             str_detect(country, "see above") ~ "NA",
                             str_detect(country, "korea") ~ "south korea",
                             str_detect(country, "denial") ~ "NA",
                             str_detect(country, "somewhere") ~ "NA",
                             str_detect(country, "eua") ~ "uea",
                             str_detect(country, "españa") ~ "spain",
      
                             str_detect(country, "the netherlands") ~ "netherlands",
                             TRUE ~ country)) %>% 
  mutate(country = na_if(country, "NA"))

# make long ----
candy_2016_long <- candy_2016_country %>%
  pivot_longer(
    cols = x100_grand_bar:york_peppermint_patties,
    names_to = "candy_type",
    values_to = "rating"
  ) %>%  
  mutate(year = 2016) %>% 
  select(going_out, year, age, country, gender, candy_type, rating) %>%
  mutate(rating = str_to_lower(rating))

# Write CSV ----
candy_2016_long %>%
  write_csv("raw_data/csv/candy_2016.csv")