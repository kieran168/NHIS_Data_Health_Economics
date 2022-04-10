---
title: "NHIS Data"
font-family: 'Corbel'
author: Nicole Dodson & Kieran Yuen
output: rmarkdown::github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r Loading in NHIS data, include=FALSE}
library(readxl)
student_datav2 <- read_excel("student_datav2.xlsx")
View(student_datav2)
```


```{r Cleaning Data, include=FALSE}
library(tidyverse)
NHIS_data <- student_datav2 %>% 
  select(educ_a,
         raceallp_a,
         agep_a,
         sex_a,
         marstat_a,
         phstat_a,
         covidtest_a) %>%
  rename(Education_Level = educ_a,
         Race = raceallp_a,
         Age = agep_a,
         Sex = sex_a,
         Marital_Status = marstat_a,
         Health_Status = phstat_a,
         Covid_Test= covidtest_a) %>%
  na.omit() %>% 
  filter(Covid_Test =="Yes"|
           Covid_Test == "No") %>%
  mutate(Education_Level = recode(Education_Level,
                      "12th grade, no diploma" = "No High School",
                      "Associate degree: academic program" = "Associate's",
                      "Associate degree: occupational, technical, or vocational program" = "Associate's",
                      "Bachelor's degree (Example: BA, AB, BS, BBA)" = "Bachelor's",
                      "Doctoral degree (Example: PhD, EdD)" = "PhD or Professional",
                      "Don't Know" = NA_character_,
                      "GED or equivalent" = "High School",
                      "Grade 1-11" = "No High School",
                      "High School Graduate" = "High School",
                      "Master's degree (Example: MA, MS, MEng, MEd, MBA)" = "Master's",
                      "Never attended/kindergarten only" = "No High School",
                      "Professional School degree (Example: MD, DDS, DVM, JD)" = "PhD or Professional",
                      "Refused" = NA_character_,
                      "Some college, no degree" = "Some college")) %>%
  mutate(Race = recode(Race,
                      "AIAN and any other group" = "AIAN and any other group",
                      "AIAN only" = "AIAN only",
                      "Asian only" = "Asian only",
                      "Black/African American only" = "Black/African American only",
                      "Don't know" = NA_character_,
                      "Not Ascertained" = "Race Unknown",
                      "Other single and multiple races" = "Other single and multiple races",
                      "Refused" = NA_character_,
                      "White only" = "White only")) %>%
  mutate(Sex = recode(Sex,
                      "Don't Know" = NA_character_,
                      "Female" = "Female",
                      "Male" = "Male",
                      "Refused" = NA_character_)) %>%
  mutate(Marital_Status = recode(Marital_Status,
                      "Divorced" = "Divorced",
                      "Living with a partner" = "Living with a partner",
                      "Married, spouse is not present" = "Married, spouse is not present",
                      "Married, spouse is present" = "Married, spouse is present",
                      "Married, spouse presence unknown" = NA_character_,
                      "Never married" = "Never married",
                      "Separated" = "Separated",
                      "Unknown marital status" = "Marital status unknown",
                      "Widowed" = "Widowed")) %>%
  mutate(Health_Status = recode(Health_Status,
                      "Don't Know" = NA_character_,
                      "Excellent" = "Excellent",
                      "Fair" = "Fair",
                      "Good" = "Good",
                      "Poor" = "Poor",
                      "Refused" = NA_character_,
                      "Very Good" = "Very Good")) %>%
    mutate(Covid_Test = recode(Covid_Test,
                      "NA" = NA_character_,
                      "No" = "No",
                      "Yes" = "Yes",
                      "Not Ascertained" = NA_character_,
                      "Refused" = NA_character_,
                      "Don't Know" = NA_character_))
```



```{r Changing Data Types for variables, include=FALSE}
NHIS_data$Education_Level <- as.factor(NHIS_data$Education_Level)
NHIS_data$Education_Level <- factor(NHIS_data$Education_Level,
                              levels = c("No High School",
                                         "High School",
                                         "Some College",
                                         "Associate's",
                                         "Bachelor's",
                                         "Master's",
                                         "PhD or Professional"))

NHIS_data$Race <- as.factor(NHIS_data$Race)

NHIS_data$Sex <- as.factor(NHIS_data$Sex)

NHIS_data$Marital_Status <- as.factor(NHIS_data$Marital_Status)

NHIS_data$Health_Status <- as.factor(NHIS_data$Health_Status)

NHIS_data$Covid_Test <- as.factor(NHIS_data$Covid_Test)
```



```{r Create Groupings of Health Statuses, include=FALSE}
High_Health_Status <- ((NHIS_data$Health_Status == "Excellent") |
                         (NHIS_data$Health_Status == "Very Good") |
                         (NHIS_data$Health_Status == "Good"))

Low_Health_Status <- ((NHIS_data$Health_Status == "Fair") |
                        (NHIS_data$Health_Status == "Poor"))

Health_Status_Group <- factor((1*Low_Health_Status +
                                 2*High_Health_Status),
                              levels = c(1, 2),
                              labels = c( "Low_Health", "High_Health"))
```



# Regression Models
## Model #1: Demographic information only
```{r Model #1: Demographic information only}
Model_1 <- glm(Covid_Test ~ Race 
                        + Age 
                        + Sex 
                        + Marital_Status,
                        data = NHIS_data,
                        family = binomial)

summary(Model_1)

#This tells us the "Odds Ratios"
exp(coefficients(Model_1))

plot(Model_1)
```

## Model #2: Demo + Health Status Grouping
```{r Model #2: Demo + Health Status Grouping}
Model_2 <- glm(Covid_Test ~ Race 
                        + Age 
                        + Sex 
                        + Marital_Status
                        + Health_Status_Group,
                        data = NHIS_data,
                        family = binomial)

summary(Model_2)

#This tells us the "Odds Ratios"
exp(coefficients(Model_2))

plot(Model_2)
```



## Model #3: Demo + Health Status Grouping + Interaction
```{r Model #3: Demo + Health Status Grouping + Interaction}
Model_3 <- glm(Covid_Test ~ Race 
                        + Age 
                        + Sex 
                        + Marital_Status
                        + Health_Status_Group
                        + Education_Level*Health_Status_Group,
                        data = NHIS_data,
                        family = binomial)

summary(Model_3)

#This tells us the "Odds Ratios"
exp(coefficients(Model_3))

plot(Model_3)
```

