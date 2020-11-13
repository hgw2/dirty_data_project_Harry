# Packages ---------
library(tidyverse)
library(janitor)

# Read in Data -------
decathlon_data <- read_rds("raw_data/decathlon.rds") %>%
  rownames_to_column("athlete") %>%
  clean_names() %>%
  rename(javelin = javeline) %>%
  select(-rank)

# Clean athlete names ------
athletes_clean_names <- decathlon_data %>%
  mutate(athlete = str_to_lower(athlete))

# Clean competition names -----

competition_clean_names <- athletes_clean_names %>%
  mutate(competition = str_to_lower(competition)) %>%
  mutate(competition = str_replace(competition, "olympicg", "olympic_games"))

# Pivot test ----

athletes_long <- competition_clean_names %>%
  pivot_longer(
    cols = (x100m:x1500m),
    names_to = "event",
    values_to = "result"
  )

# Write CSV -----
athletes_long %>%
  write_csv("clean_data/clean_decathlon_results.csv")
