# Packages ----
library(tidyverse)
library(janitor)
library(assertr)

# read in data

rwa_data <- read_csv("raw_data/rwa.csv") %>%
  select(-(E1:E22), -(TIPI1:VCL16), -screenw, -screenh)

country_codes <- read.csv("raw_data/country_codes.csv") %>%
  clean_names() %>%
  select(-alpha_3_code) %>%
  rowid_to_column() %>%
  filter(!rowid %in% c("26", "33", "55", "119", "128", "155", "187", "194", "195", "222", "246", "248")) %>%
  select(-rowid)



# Drop NA - There are c. 200 response where responses to questions are not given,
# seeing as this skews the data I have decided to drop the NAs

rwa_data_drop_na <- rwa_data %>%
  mutate(across(Q1:Q22, ~ na_if(.x, 0))) %>%
  drop_na(Q1:Q22)

# These questions are negatively marked so multiply by -1 to give it a negative score
rwa_multiplied <- rwa_data_drop_na %>%
  mutate(across(.cols = c(Q4, Q6, Q8, Q9, Q11, Q13, Q15, Q18, Q20, Q21), ~ .x * -1))


# seeing as half of the questions are positive and half negative, all means fall between -4 and 4.
# If I add 4 to the score, divide by 8 and multiply by 100 it will give me a score out of 100
# the higher the score the more right wing the individual is.
rwa_scores <- rwa_multiplied %>%
  rowwise() %>%
  mutate(score = mean(c_across(Q3:Q22), na.rm = TRUE)) %>%
  mutate(score = (((score + 4)  / 8) * 100))

# Add time taking to complete the survey (not including welcome page)
# We now don't need the question responses so remove questions

rwa_condensed <- rwa_scores %>%
  mutate(total_time_taken = testelapse + surveyelapse) %>%
  select(-(Q1:Q22), -(introelapse:surveyelapse))


# Add country to code
rwa_country <- rwa_condensed %>%
  left_join(country_codes, by = c("IP_country" = "alpha_2_code"), na_matches = "never") %>%
  relocate(country, .before = IP_country) %>%
  select(-IP_country) %>%
  mutate(country = str_to_lower(country))


# Cleaned education column ------

rwa_education <- rwa_country %>%
  verify(education >= 0 & education <= 4) %>%
  mutate(education = case_when(
    education == 1 ~ "less than high school",
    education == 2 ~ "high school",
    education == 3 ~ "university degree",
    education == 4 ~ "graduate degree",
    TRUE ~ "not answered"
  ))

# Cleaned urban column ----

rwa_urban <- rwa_education %>%
  verify(urban >= 0 & urban <= 3) %>%
  mutate(urban = case_when(
    urban == 1 ~ "rural",
    urban == 2 ~ "suburban",
    urban == 3 ~ "urban",
    TRUE ~ "not answered"
  ))

# Cleaned gender column ----

rwa_gender <- rwa_urban %>%
  verify(gender >= 0 & gender <= 3) %>%
  mutate(gender = case_when(
    gender == 1 ~ "male",
    gender == 2 ~ "female",
    gender == 3 ~ "other",
    TRUE ~ "not answered"
  ))


# Cleaned engnat column -------
rwa_engnat <- rwa_gender %>%
  verify(engnat >= 0 & engnat <= 2) %>%
  mutate(engnat = case_when(
    engnat == 1 ~ "yes",
    engnat == 2 ~ "no",
    TRUE ~ "not answered"
  ))

# Cleaned hand column ------
rwa_hand <- rwa_engnat %>%
  verify(hand >= 0 & hand <= 3) %>%
  mutate(hand = case_when(
    hand == 1 ~ "right",
    hand == 2 ~ "left",
    hand == 3 ~ "both",
    TRUE ~ "not answered"
  ))

# Cleaned religion column -----

rwa_religion <- rwa_hand %>%
  verify(religion >= 0 & religion <= 12) %>%
  mutate(religion = case_when(
    religion == 1 ~ "agnostic",
    religion == 2 ~ "atheist",
    religion == 3 ~ "buddhist",
    religion == 4 ~ "christian catholic",
    religion == 5 ~ "christian mormon",
    religion == 6 ~ "christian protestant",
    religion == 7 ~ "christian other",
    religion == 8 ~ "hindu",
    religion == 9 ~ "jewish",
    religion == 10 ~ "muslim",
    religion == 11 ~ "sikh",
    religion == 12 ~ "other",
    TRUE ~ "not answered"
  ))

# Cleaned orientation column ----

rwa_orientation <- rwa_religion %>%
  verify(orientation >= 0 & orientation <= 5) %>%
  mutate(orientation = case_when(
    orientation == 1 ~ "heterosexual",
    orientation == 2 ~ "bisexual",
    orientation == 3 ~ "homosexual",
    orientation == 4 ~ "asexual",
    orientation == 5 ~ "other",
    TRUE ~ "not answered"
  ))

# Cleaned race column -----

rwa_race <- rwa_orientation %>%
  verify(race >= 0 & race <= 5) %>%
  mutate(race = case_when(
    race == 1 ~ "asian",
    race == 2 ~ "arab",
    race == 3 ~ "black",
    race == 4 ~ "indigenous australian, native american or white",
    race == 5 ~ "other",
    TRUE ~ "not answered"
  ))

# Cleaned voted column -----
rwa_voted <- rwa_race %>%
  verify(voted >= 0 & voted <= 2) %>%
  mutate(voted = case_when(
    voted == 1 ~ "yes",
    voted == 2 ~ "no",
    TRUE ~ "not answered"
  ))

# Cleaned married column ------
rwa_married <- rwa_voted %>%
  verify(married >= 0 & married <= 3) %>%
  mutate(married = case_when(
    married == 1 ~ "never married",
    married == 2 ~ "currently married",
    married == 3 ~ "previously married",
    TRUE ~ "not answered"
  ))

# Cleaned ages and created age group ---

rwa_ages <- rwa_married %>%
  mutate(age_imputed = if_else(age > 85, TRUE, FALSE)) %>%
  mutate(age = if_else(age > 85, NaN, age)) %>%
  mutate(age = coalesce(age, median(rwa_married$age, na.rm = TRUE)))


rwa_ages %>%
  verify((age >= 13 & age <= 85))

rwa_ages_grouped <- rwa_ages %>%
  mutate(age_group = case_when(
    age < 18 ~ "under 18",
    age < 26 ~ "18 to 25",
    age < 41 ~ "25 to 40",
    age < 60 ~ "41 to 60",
    TRUE ~ "over 60"
  )) %>%
  select(-age)

# Cleaned family size----

rwa_family_size <- rwa_ages_grouped %>%
  mutate(family_imputed = if_else(familysize > 10, TRUE, FALSE)) %>%
  mutate(familysize = if_else(familysize > 10, NaN, familysize)) %>%
  mutate(familysize = 
           coalesce(familysize, 
                               median(rwa_ages_grouped$familysize, na.rm = TRUE)))


rwa_family_size %>%
  verify((familysize >= 0 & familysize <= 10))

# Clean response time ----

rwa_time_taken <- rwa_family_size %>% 
  mutate(total_time_imputed = if_else(total_time_taken > 1200, TRUE, FALSE)) %>%
  mutate(total_time_taken = if_else(total_time_taken > 1200, NaN, total_time_taken)) %>%  
mutate(total_time_taken = 
         coalesce(total_time_taken, 
                  median(rwa_family_size$total_time_taken, na.rm = TRUE)))

rwa_time_taken %>%
  verify((total_time_taken >= 0 & total_time_taken <= 1200))


# Remove inaccurate responses----
rwa_responses <- rwa_time_taken %>%
  filter(surveyaccurate == 1)

# Write CSV -----
rwa_responses %>%
  write_csv("clean_data/rwa_responses_clean")
