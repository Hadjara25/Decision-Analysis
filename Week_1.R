library('tidyverse')


participants_da

participants_data$age


version

library('tidyverse')

browseVignettes('tidyverse')

library(dplyr)

#read.csv('teaching_R-master,participants_data.csv')

data <- read.csv("C:/Users/Farida/Documents/Bonn/R/Rprojects/teaching_R-master/participants_data.csv")

data


head(participants_data, 4)

names(participants_data)

str(participants_data)

# change the variable to gender

participants_data$gender

participants_data$age

# change the selection to batch and age

select(participants_data,batch,age)

select(participants_data,
       academic_parents,
       working_hour_per_day)

#filter

filter(participants_data,working_hours_per_day >5)

filter(participants_data, working_hours_per_day >10)

filter(participants_data, working_hours_per_day >10 & letters_in_first_name >6)

# rename
rename(participants_data, commute = km_home_to_office)

rename(participants_data,name_length = letters_in_first_name)

# mutate

mutate(participants_data, age_mean = age*mean(age))

mutate(participants_data, labor_mean = working_hours_per_day* mean(working_hours_per_day))

mutate(participants_data,commute = ifelse(km_home_to_office > 10,"commuter", "local"))


# summarize

summarise(participants_data,
          mean(number_of_siblings),
          median(years_of_study))

summarise(participants_data,
          mean(years_of_study),
          median(letters_in_first_name))

# group_by
participants_data %>%
  group_by(gender) %>%
  summarise(mean(days_to_email_response),
            median(letters_in_first_name),
            max(years_of_study))

participants_data %>%
  group_by(research_continent) %>%
  summarise(mean(days_to_email_response),
           median(letters_in_first_name),
           max(years_of_study))
participants_data %>%
  mutate(response_speed = ifelse(days_to_email_response > 1, "slow", "fast")) %>%
  group_by(response_speed) %>%
  summarise(mean(number_of_siblings),
            median(years_of_study),
            max(letters_in_first_name))









