# packages ---- 
library(tidyverse)
library(assertr)
library(janitor)

# Read in data ----
dogs <- read_csv("raw_data/dog_survey.csv") %>% 
  select(-X10, -X11) %>% 
  clean_names()

# Remove duplicated entries ----
dogs_duplicates <- dogs %>% 
  filter(!duplicated(id))

# Clean amount_spent_on_food ----
dogs_spending <- dogs_duplicates %>% 
  mutate(amount_spent_on_dog_food = 
      str_remove_all(amount_spent_on_dog_food, "£")) %>% 
  mutate(amount_spent_on_dog_food = 
           str_remove_all(amount_spent_on_dog_food, "[[:alpha:]]")) %>% 
  mutate(amount_spent_on_dog_food = 
           str_remove_all(amount_spent_on_dog_food, "[/,\\!-]")) %>% 
  mutate(amount_spent_on_dog_food = 
           as.numeric(amount_spent_on_dog_food))

# Clean dog size ----
dog_size <- dogs_spending %>% 
  mutate(dog_size = toupper(dog_size)) %>% 
  mutate(dog_size = case_when(
    str_detect(dog_size, "SMALL") ~ "S",
    str_detect(dog_size,"MEDIUM") ~"M",
    str_detect(dog_size,"LARGE") ~"L",
    str_detect(dog_size,"-") ~ NA_character_,
    str_detect(dog_size,"N") ~ NA_character_,
    TRUE ~ dog_size
  ))

# Clean dog gender
dog_gender <- dog_size %>% 
  mutate(dog_gender = toupper(dog_gender)) %>% 
  mutate(dog_gender = 
           str_remove_all(dog_gender,"[0-9]+")) %>% 
  mutate(dog_gender = 
           ifelse(str_detect(dog_gender,"[—-]"), NA_character_, dog_gender)) %>%  
  mutate(dog_gender= 
           str_replace_all(dog_gender, "FEMALE", "F")) %>% 
  mutate(dog_gender= 
           str_replace_all(dog_gender, "FEMLAE", "F")) %>% 
  mutate(dog_gender= 
           str_replace_all(dog_gender, "MALE", "M")) %>% 
  mutate(dog_gender= 
           if_else(str_detect(dog_gender, "K"), NA_character_, dog_gender)) %>% 
  mutate(dog_gender = str_replace_all(dog_gender," AND ", ","))

# Split enteries with more than one dog and flag
dogs_more <-  dog_gender %>% 
  mutate(dog_age = str_replace(dog_age, "and", ",")) %>% 
  mutate(dog_age = str_remove_all(dog_age, "[[\\+A-z]]+")) %>% 
  mutate(dog_age = str_split(dog_age, ",")) %>% 
  mutate(dog_gender = str_split(dog_gender, ",")) %>% 
  mutate(dog_age = str_split(dog_age, ",")) %>% 
  mutate(dog_size= str_split(dog_size, ",")) %>% 
  unnest(cols = c(dog_size, dog_gender, dog_age)) %>%  
  mutate(dog_age = str_remove_all(dog_age, "[[:punct:]c]+")) %>% 
  mutate(dog_gender = str_trim(dog_gender)) %>% 
  mutate(dog_age = as.integer(dog_age)) 

duplicates <- dogs_more %>% 
  filter(duplicated(id)) %>% 
  pull(id) 

stopifnot(c(119,174,174)== duplicates) # Check duplicates are the same ID

dog_final <- dogs_more %>% 
  mutate(multiple_entry = id %in% duplicates) %>% 
  mutate(
    amount_spent_on_dog_food = ifelse(id == 119, 
                                      amount_spent_on_dog_food/2 ,
                                      amount_spent_on_dog_food )) %>% 
  mutate(
    amount_spent_on_dog_food = ifelse(id == 174, 
                                      amount_spent_on_dog_food/3 ,
                                      amount_spent_on_dog_food )) %>% 
  mutate(amount_spent_on_dog_food = 
           round(amount_spent_on_dog_food, 2)) # Round to 2dp

# Check for valid email

dog_email <- dog_final %>% 
  mutate(valid_email = str_detect(email, ".com$"))

# Write CSV
dog_email %>% 
  write_csv("clean_data/dog_owners_survey.CSV")
  