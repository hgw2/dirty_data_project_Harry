# Packages ----
library(tidyverse)
library(janitor)

# read in data

rwa_data <- read_csv("raw_data/rwa.csv") %>%
  select(-(E1:E22), -(TIPI1:VCL16), -screenw, -screenh)

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
  mutate(score = (((score + 4) / 8) * 100))

# Add time taking to complete the survey (not including welcome page)
# We now don't need the question responses so remove questions

rwa_condensed <- rwa_scores %>%
  mutate(total_time_taken = testelapse + surveyelapse) %>%
  select(-(Q1:Q22), -(introelapse:surveyelapse))

# Add values to education

rwa_education <- rwa_condensed %>%
  mutate(education = as.integer(if_else(education < 1, mean(rwa_condensed$education), education))) %>% 
  mutate(education = case_when(
    education == 1 ~ "less than high school",
    education == 2 ~ "high school",
    education == 3 ~ "university degree",
    education == 4 ~ "graduate degree",
    TRUE ~ "not answered"
  ))

# Add values to urban

rwa_urban <- rwa_education %>%
  mutate(urban= as.integer(if_else(urban < 1, mean(rwa_education$urban), urban))) %>% 
  mutate(urban = case_when(
    urban == 1 ~ "rural",
    urban == 2 ~ "suburban",
    urban == 3 ~ "urban",
    TRUE ~ "not answered"
    
  ))

# Add values to gender

rwa_gender <- rwa_urban %>%
  mutate(gender= as.integer(if_else(gender < 1, mean(rwa_urban$gender), gender))) %>% 
  mutate(gender = case_when(
    gender == 1 ~ "male",
    gender == 2 ~ "female",
    gender == 3 ~ "other",
    TRUE ~ "not answered"
  ))


# engnat add values
rwa_engnat <- rwa_gender %>%
  mutate(engnat= as.integer(if_else(engnat < 1, mean(rwa_gender$engnat), engnat))) %>% 
  mutate(engnat = case_when(
    engnat == 1 ~ "yes",
    engnat == 2 ~ "no",
    TRUE ~ "not answered"
  ))

# add values to hand
rwa_hand <- rwa_engnat %>%
  mutate(hand = as.integer(if_else(hand < 1, mean(rwa_engnat$hand), hand))) %>% 
  mutate(hand = case_when(
    hand == 1 ~ "right",
    hand == 2 ~ "left",
    hand == 3 ~ "both",
    TRUE ~ "not answered"
  ))

# add values to religion

rwa_religion <- rwa_hand %>%
  mutate(religion = (if_else(religion < 1, mean(rwa_hand$religion), religion))) %>% 
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

# fixed orientation 

rwa_orientation <- rwa_religion %>% 
  mutate(orientation = (if_else(orientation< 1, mean(rwa_religion$orientation), orientation))) %>% 
  mutate(orientation = case_when(
 orientation == 1~"heterosexual", 
 orientation ==2~"bisexual", 
 orientation == 3~"homosexual",
 orientation ==4~"asexual", 
 orientation ==5~"other",
 TRUE ~ "not answered"
  ))
# race

rwa_race <- rwa_orientation %>% 
  mutate(race = as.integer(if_else(race < 1, mean(rwa_orientation$race), race))) %>% 
  mutate(race = case_when(
    race == 1~ "asian", 
    race == 2~ "arab", 
    race ==3~"black", 
    race ==4~"indigenous australian, native american or white", 
    race ==5~"other",
    TRUE ~ "not answered"
  ))

# voted 
rwa_voted <- rwa_race %>%
  mutate(race = as.integer(if_else(voted < 1, mean(rwa_race$voted), voted))) %>% 
  mutate(voted = case_when(
    voted == 1 ~ "yes",
    voted == 2 ~ "no",
    TRUE ~ "not answered"
  ))

# Married
rwa_married <- rwa_voted %>%
  mutate(married = as.integer(if_else(married < 1, mean(rwa_voted$married), married))) %>%
  mutate(married = case_when(
    married == 1 ~ "never married",
    married == 2 ~ "currently married",
    married == 3 ~ "previously married",
    TRUE ~ "not answered"
  ))

# remove inaccurate responses

rwa_responses <- rwa_married %>% 
  filter(surveyaccurate == 1)

# Write CSV -----
rwa_responses %>% 
  write_csv("clean_data/rwa_responses_clean")