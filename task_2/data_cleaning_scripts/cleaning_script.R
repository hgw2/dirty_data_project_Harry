# Packages ------
library(tidyverse)
library(janitor)

# Load in Data -----
cake_ingredients_short <- read_csv("raw_data/cake-ingredients-1961.csv")
ingredients_list <- read_csv("raw_data/cake_ingredient_code.csv")

# Pivot to long format -----
cake_ingredients_long <- cake_ingredients_short %>%
  pivot_longer(
    cols = AE:ZH,
    names_to = "ingredient_abreviation",
    values_to = "quantity"
  ) %>%
  clean_names()

# Add ingredients-----

full_ingredients <- cake_ingredients_long %>%
  left_join(ingredients_list, by = c("ingredient_abreviation" = "code")) %>%
  select(-ingredient_abreviation) %>%
  drop_na(quantity) %>%
  mutate(cake = str_to_lower(cake)) %>%
  mutate(ingredient = str_to_lower(ingredient))

# Changed Sour Cream NA-----

full_ingredients <- full_ingredients %>%
  select(cake, quantity, measure, ingredient) %>%
  mutate(measure = ifelse(ingredient == "sour cream cup", "cup", measure)) %>%
  mutate(ingredient = str_replace(ingredient, "sour cream cup", "sour cream"))

# Change one eggs to whole ----
full_ingredients <- full_ingredients %>%
  mutate(measure = ifelse(measure == "one", "whole", measure))

# write to CSV ----
full_ingredients %>%
  write_csv("clean_data/cake_ingredients.CSV")
