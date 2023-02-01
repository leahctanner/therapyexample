## Import

library(readr)
therapyexample <- read_csv("Portfolio Project.csv", na = "NA")
View(therapyexample)

##Explore

glimpse(therapyexample)

#intraclass correlation example - CHECK THIS AGAIN
library(multilevel)
icc(aov(gender ~ baselinestress, data = therapyexample))


#filter

therapyexample %>% 
  filter((state == "NY" | state == "NJ") & salary > 100000) %>%
  select(id, state, salary, nosessions) %>% 
  arrange(salary) %>% 
  View
  
  ##another way

  therapyexample %>% 
    select(id, state, salary, nosessions) %>% 
    filter(state %in% c("NY", "NJ") &
             salary > 100000) %>% 
    View

    

##Clean
  
therapyexample$gender[therapyexample$gender == "nonbiry"] <- "nonbinary" 


##Changing from character variable to factor variable (ordinal) and encoding to
  #prep for MICE
unique(therapyexample$gender)
therapyexample$gender <- factor((therapyexample$gender),
                                   levels = c("male", 
                                              "female",
                                              "nonbinary",
                                              "transman",
                                              "transwoman",
                                              "unspecified"),
                                   labels = c(1,2,3,4,5,6))
levels(therapyexample$gender)
  
therapyexample$race <- factor((therapyexample$race),
                                 levels = c("White",
                                            "Asian",
                                            "Hispanic",
                                            "Indigenous",
                                            "Black"),
                                 labels = c(1,2,3,4,5))
levels(therapyexample$race)


therapyexample$education <- factor((therapyexample$education),
                                      levels = c("Some High School",
                                                 "High School Diploma",
                                                 "Some College",
                                                 "Associates",
                                                 "Bachelors",
                                                 "Masters",
                                                 "Doctorate"),
                                      labels = c(1,2,3,4,5,6,7))
levels(therapyexample$education)

therapyexample$state <- factor((therapyexample$state), 
                               levels = c("NY",
                                          "NJ",
                                          "CT"),
                               labels = c(1,2,3))
levels(therapyexample$state)                                                

therapyexample$trigger <- factor((therapyexample$trigger), 
                                 levels = c("Dispute",
                                            "Financial",
                                            "Loss",
                                            "Unspecified"),
                                 labels = c(1,2,3,4))
levels(therapyexample$state)

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

##can just work with complete cases (much easier)

t2 <- therapyexample[complete.cases(therapyexample),]

##could replace missing values with means, but not great for ordinal variables

meanage <- mean(therapyexample$age, na.rm=TRUE)
t3 <- therapyexample
t3$age <- ifelse(is.na(therapyexample$age),meanage,therapyexample$age)
summary(t3)


##MICE

require(mice)
require(lattice)
require(pan)

imputed <- mice( therapyexample, m=5, maxit = 100, printFlag = TRUE, seed = 105732) 

##need to create the list_mice operator
list_complete <- ##need to define this to integrate MICE outcomes into main data

mice_complete <- lapply(
  X = list_mice,
  FUN = mice::complete
)

for(i in names(list_complete)) list_complete[[i]] <- data.frame(
  age = M$age,
  gender = M$gender,
  education = M$education,
  state = M$state,
  trigger = M$trigger,
  list_complete[[i]]
)


##Manipulate variable names
library(tidyverse)
t2 %>% 
  rename("education" = "educationlevel") ##also not working?

##creating subsets for nonbinary patients

nonbi <- subset(t2, gender == "nonbinary")
View(nonbi)
  
## Describe and summarize

summary(t2)

t2 %>% 
  select(age, baselinestress) %>% 
  summarise ##why isn't this working?

t2 %>% 
  group_by(race, gender) %>% 
  summarise(lower = min(baselinestress),
            average = mean(baselinestress),
            upper = max(baselinestress),
            difference = max(baselinestress)-min(baselinestress)) %>% 
  arrange(race) %>% 
  View()

##contingency tables

table(t2$gender, t2$race)
addmargins(table(t2$gender, t2$race),1)
prop.table(table(t2$gender, t2$race),2)*100
round(prop.table(table(t2$gender, t2$race),2)*100)
addmargins(prop.table(table(t2$gender, t2$race),2)*100)

##using tidyverse

t2 %>% 
  group_by(gender, race) %>% 
  summarise(number = n()) %>% 
  pivot_wider(names_from = gender,
              values_from = number)


## Visualize 

library(formattable)
plot(gender ~ baselinestress, data = t2)
plot(age ~ salary, data = t2)
plot(salary ~ baselinestress, data=t2)

##Analyze

cor(t2$age, t2$baselinestress)


slm.gender.base <- lm(baselinestress ~ gender, data = t2)
summary(slm.gender.base)

slm.gender.last <- lm(laststress ~ gender, data = t2)
  summary(slm.gender.last)

slm.salary.base <- lm(baselinestress ~ salary, data = t2)
summary(slm.salary.base)

