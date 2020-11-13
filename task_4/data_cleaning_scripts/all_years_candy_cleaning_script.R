# packages
# Packages -----
library(tidyverse)
library(readxl)
library(janitor)
library(assertr)

# Read in data -----
candy_2015 <- read_excel("raw_data/excel/boing-boing-candy-2015.xlsx") %>%
  clean_names()

candy_2016 <- read_excel("raw_data/excel/boing-boing-candy-2016.xlsx") %>%
  clean_names()

candy_2017 <- read_excel("raw_data/excel/boing-boing-candy-2017.xlsx") %>%
  clean_names() %>%
  rename_all(~ str_remove(., "q[0-9]+_")) %>%
  clean_names()
# Make Long ----

#2015

  candy_2015_long <- candy_2015 %>%
  rename(age = how_old_are_you) %>%
  rename(going_out = are_you_going_actually_going_trick_or_treating_yourself) %>%
  pivot_longer(
    cols = butterfinger:york_peppermint_patties,
    names_to = "candy_type",
    values_to = "rating"
  ) %>%
  select(going_out, age, candy_type, rating) %>%
  mutate(rating = str_to_lower(rating))

# Add missing columns
candy_2015_long <- candy_2015_long %>%
  mutate(country = NA) %>%
  mutate(gender = NA) %>%
  mutate(year = 2015) %>%
  select(going_out, year, country, gender, age, candy_type, rating)

# Check data
stopifnot(
  ncol(candy_2015_long) == 7
)

candy_2015_long %>%
  verify(names(candy_2015_long) == c(
    "going_out",
    "year",
    "country",
    "gender",
    "age",
    "candy_type",
    "rating"
  ))

# 2016

candy_2016_long <- candy_2016 %>%
  rename(going_out = are_you_going_actually_going_trick_or_treating_yourself) %>%
  rename(age = how_old_are_you) %>% 
  rename(country = which_country_do_you_live_in) %>%
  rename(gender = your_gender) %>% 
  pivot_longer(
    cols = x100_grand_bar:york_peppermint_patties,
    names_to = "candy_type",
    values_to = "rating"
  ) %>%
  mutate(year = 2016) %>%
  select(going_out, year, country, gender, age,  candy_type, rating) %>%
  mutate(rating = str_to_lower(rating))

# check data
stopifnot(
  ncol(candy_2016_long) == 7
)

candy_2016_long %>%
  verify(names(candy_2016_long) == c(
    "going_out",
    "year",
    "country",
    "gender",
    "age",
    "candy_type",
    "rating"
  ))


# 2017
candy_2017_long <- candy_2017 %>%
  pivot_longer(
    cols = x100_grand_bar:york_peppermint_patties,
    names_to = "candy_type",
    values_to = "rating"
  ) %>%
  mutate(year = 2017) %>%
  select(going_out, year, country, gender, age, candy_type, rating) %>%
  mutate(rating = str_to_lower(rating))

# Check Data
stopifnot(
  ncol(candy_2017_long) == 7
)

candy_2017_long %>%
  verify(names(candy_2017_long) == c(
    "going_out",
    "year",
    "country",
    "gender",
    "age",
    "candy_type",
    "rating"
  ))


candy_all <- bind_rows(candy_2015_long, candy_2016_long, candy_2017_long)

candy_all <- candy_all %>%
  drop_na(rating)


#impute median age
candy_all_age <- candy_all %>%
  mutate(age = str_remove_all(age, "[[:alpha:]]+")) %>%
  mutate(age = str_remove_all(age, "[,\\- A-z\\+><?,!\\']+")) %>%
  mutate(age = str_replace(age, "[[:space:]]", "0")) %>%
  mutate(age = as.integer(age)) %>%
  mutate(age = coalesce(age, median(age, na.rm = TRUE))) %>%
  mutate(age = ifelse(age >= 82, median(age, rm.na = TRUE), age)) %>%
  mutate(age = ifelse(age <= 10, median(age, rm.na = TRUE), age))


candy_all_age %>%
  verify(age < 82 & age > 10)

# CLean Gender
candy_all_gender <- candy_all_age %>%

  mutate(gender = str_remove(gender, "I'd ")) %>%
  mutate(gender = str_to_lower(gender))



# Clean going trick or treating  -----
candy_all_trick_or_treating <- candy_all_gender %>%
  mutate(going_out = if_else(going_out == "Yes", TRUE, FALSE)) %>%  # make a logical column
mutate(going_out = coalesce(going_out, FALSE))

#Clean country names

candy_all_country <- candy_all_trick_or_treating %>%
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
    str_detect(country, "trump") ~ "us",
    str_detect(country, "states") ~ "us",
    str_detect(country, "north carolina") ~ "us",
    str_detect(country, "yoo ess") ~ "us",
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
    str_detect(country, "eua") ~ "uae",
    str_detect(country, "atlantis") ~ "NA",
    str_detect(country, "narnia") ~ "NA",
    str_detect(country, "subscribe") ~ "NA",
    str_detect(country, "anymore") ~ "NA",
    str_detect(country, "fear") ~ "NA",
    str_detect(country, "earth") ~ "NA",
    str_detect(country, "the netherlands") ~ "netherlands",
    str_detect(country, "españa") ~ "spain",
    str_detect(country, "^a$") ~ "NA",
    TRUE ~ country
  )) %>%
  mutate(country = na_if(country, "NA"))

# Clean candy data

candy_cleaned <- candy_all_country %>%
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
