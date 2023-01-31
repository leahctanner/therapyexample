## Import

library(readr)
therapyexample <- read_csv("Portfolio Project.csv", na = "NA")
View(therapyexample)

##Explore

glimpse(therapyexample)

#filter

therapyexample %>% 
  filter((state == "NY" | state == "NJ") & salary > 100000) %>%
  select(id, state, salary, nosessions) %>% 
  arrange(salary) %>% 
  View
  
  ##other way

  therapyexample %>% 
    select(id, state, salary, nosessions) %>% 
    filter(state %in% c("NY", "NJ") &
             salary > 100000) %>% 
    View

    

##Clean
  
therapyexample$gender[therapyexample$gender == "nonbiry"] <- "nonbinary" 

unique(therapyexample$gender)
therapyexample$gender <- factor((therapyexample$gender),
                                   levels = c("male", 
                                              "female",
                                              "nonbinary",
                                              "transman",
                                              "transwoman",
                                              "unspecified"))
levels(therapyexample$gender)
  
therapyexample$race <- factor((therapyexample$race),
                                 levels = c("White",
                                            "Asian",
                                            "Hispanic",
                                            "Indigenous",
                                            "Black"))
levels(therapyexample$race)


therapyexample$education <- factor((therapyexample$education),
                                      levels = c("Some High School",
                                                 "High School Diploma",
                                                 "Some College",
                                                 "Associates",
                                                 "Bachelors",
                                                 "Masters",
                                                 "Doctorate"))
levels(therapyexample$education)

therapyexample$state <- as.factor(therapyexample$state)

therapyexample$trigger <- as.factor(therapyexample$trigger)

View(therapyexample)

## Cleaning/dealing with missingness

##replacing blank values (in strings) with NA
therapyexample %>% 
  mutate_all(na_if,"") %>% 
  View

therapyexample %>% 
  na.omit() %>% 
  View

therapyexample %>% 
  filter(!complete.cases(.)) %>% 
  View



##Manipulate variable names
library(tidyverse)
therapyexample %>% 
  rename("education" = "educationlevel")

##creating subsets for nonbinary patients

nonbi <- subset()
  
## Describe and summarize

mean(therapyexample$age)
mean(therapyexample$age, na.rm = TRUE)

## Visualize 

##Analyze

