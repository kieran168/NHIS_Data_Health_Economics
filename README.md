NHIS Data
================
Nicole Dodson & Kieran Yuen

  - [Setup](#setup)
  - [Loading in NHIS data](#loading-in-nhis-data)
  - [Cleaning Data](#cleaning-data)
  - [Changing Data Types for
    variables](#changing-data-types-for-variables)
  - [Summary tables to check groupings are
    accurate](#summary-tables-to-check-groupings-are-accurate)
  - [Summary Statistics](#summary-statistics)
      - [Summary Statistics: COVID Test](#summary-statistics-covid-test)
      - [Summary Statistics: Health
        Status](#summary-statistics-health-status)
      - [Summary Statistics: Numeric
        Variables](#summary-statistics-numeric-variables)
  - [Correlation Check](#correlation-check)
  - [Logistic Regression](#logistic-regression)
      - [Model \#1: Demographic information
        only](#model-1-demographic-information-only)
      - [Model \#2: Demographic + Health
        Status](#model-2-demographic--health-status)
      - [Model \#3: Demographic + Health Status + Interaction of
        Education & Health
        Status](#model-3-demographic--health-status--interaction-of-education--health-status)
  - [Logistic Regression (with Age
    Groups)](#logistic-regression-with-age-groups)
      - [Model \#1a: Demographic information only (with Age
        Groups)](#model-1a-demographic-information-only-with-age-groups)
      - [Model \#2a: Demographic + Health Status (with Age
        Groups)](#model-2a-demographic--health-status-with-age-groups)
      - [Model \#3a: Demographic + Health Status + Interaction of
        Education & Health Status (with Age
        Groups)](#model-3a-demographic--health-status--interaction-of-education--health-status-with-age-groups)
  - [Logistic Regression (with Health Status
    levels)](#logistic-regression-with-health-status-levels)
      - [Model \#2b: Demographic + Health Status (with Health Status
        levels)](#model-2b-demographic--health-status-with-health-status-levels)
  - [OLS Regression (dep. var. Health
    Status)](#ols-regression-dep-var-health-status)
      - [Model \#4: Demographic + Health
        Status](#model-4-demographic--health-status)

# Setup

``` r
knitr::opts_chunk$set(echo = TRUE)
```

# Loading in NHIS data

``` r
library(readxl)
student_datav1 <- read_excel("student_datav1.xlsx")
```

# Cleaning Data

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.6     ✓ dplyr   1.0.6
    ## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
    ## ✓ readr   2.1.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
NHIS_data <- student_datav1 %>% 
  select(educ_a,
         hisp_a,
         raceallp_a,
         agep_a,
         sex_a,
         marstat_a,
         phstat_a,
         covidtest_a) %>%
  rename(Education = educ_a,
         Hisp = hisp_a,
         Race = raceallp_a,
         Age = agep_a,
         Sex = sex_a,
         Marital_Status = marstat_a,
         Health_Status = phstat_a,
         Covid_Test= covidtest_a) %>%
  
  filter(Covid_Test =="1"| #YestoCovidTest
           Covid_Test == "2") %>% #NotoCovidTest 
  
  mutate(Covid_Test = recode(Covid_Test,
                            '1' = 1,#YestoCovidTest
                            '2' = 0)) %>% #NotoCovidTest 
                              
  mutate(Education = recode(Education,
                            '0' = 1,#nohighschool
                            '1' = 1,#nohighschool
                            '2' = 1,#nohighschool
                            '3' = 2,#highschool
                            '4' = 2,#highschool
                            '5' = 3,#somecollege
                            '6' = 4,#associates
                            '7' = 4,#associates
                            '8' = 5,#bachelors
                            '9' = 6,#masters
                            '10' = 7,#phdprofessional
                            '11' = 7,#phdprofessional
                            '97' = NA_real_,
                            '99' = NA_real_)) %>%
  
  mutate(Hisp = recode(Hisp,
                            '1' = "Hispanic",
                            '2' = "Non_hispanic")) %>% 
  
  mutate(Race = recode(Race,
                            '1' = "White only",
                            '2' = "Black/African American only",
                            '3' = "Asian only",
                            '4' = "AIAN only",
                            '5' = "AIAN and any other group",
                            '6' = "Other single and multiple races",
                            '7' = "Refused",
                            '8' = "Not Ascertained",
                            '9' = "Don't know")) %>%
  
  mutate(Race_Hisp = ifelse(Hisp == 'Non_hispanic' & Race == 'Black/African American only', 
                            'Non-Hispanic Black',
                            ifelse(Hisp == 'Non_hispanic' & Race == 'White only', 
                                   'Non-Hispanic White',
                                   ifelse(Hisp == 'Non_hispanic' & Race == 'AIAN and any other group'|
                                            Hisp == 'Non_hispanic' & Race == 'AIAN only'|
                                            Hisp == 'Non_hispanic' & Race == 'Asian only'|
                                            Hisp == 'Non_hispanic' & Race == 'Other single and multiple races', 
                                          'Other',
                                          'Hispanic')))) %>%
                            
  mutate(Sex = recode(Sex,
                            '1' = "Male",
                            '2' = "Female",
                            '7' = NA_character_,
                            '9' = NA_character_)) %>%
  
  mutate(Marital_Status = recode(Marital_Status,
                            '1' = "Married",
                            '2' = "Married",
                            '3' = "Not Married",
                            '4' = "Not Married",
                            '5' = "Not Married",
                            '6' = "Not Married",
                            '7' = "Not Married",
                            '8' = "Not Married",
                            '9' = "Not Married")) %>%
  
  mutate(Health_Status = recode(Health_Status,
                            '1' = 5,
                            '2' = 4,
                            '3' = 3,
                            '4' = 2,
                            '5' = 1,
                            '7' = NA_real_,
                            '9' = NA_real_,
                            )) %>%
  
  mutate(Age_Group = case_when(
    Age <= 34            ~ "27-34",
    Age > 34 & Age <= 44 ~ "35-44",
    Age > 44 & Age <= 54 ~ "45-54",
    Age > 54 & Age <= 64 ~ "55-64"
    ))   %>%
  
  na.omit()
```

# Changing Data Types for variables

``` r
NHIS_data$Education <- as.numeric(NHIS_data$Education)

NHIS_data$Race_Hisp <- as.factor(NHIS_data$Race_Hisp)
NHIS_data$Race_Hisp <- factor(NHIS_data$Race_Hisp,
                              levels = c("Non-Hispanic White",
                                         "Non-Hispanic Black",
                                         "Hispanic",
                                         "Other"))

NHIS_data$Age <- as.numeric(NHIS_data$Age)

NHIS_data$Sex <- as.factor(NHIS_data$Sex)

NHIS_data$Marital_Status <- as.factor(NHIS_data$Marital_Status)

NHIS_data$Health_Status <- as.numeric(NHIS_data$Health_Status)

NHIS_data$Covid_Test <- as.factor(NHIS_data$Covid_Test)

NHIS_data$Age_Group <- as.factor(NHIS_data$Age_Group)
```

# Summary tables to check groupings are accurate

``` r
table(NHIS_data$Education)
```

    ## 
    ##    1    2    3    4    5    6    7 
    ##  691 2232 1469 1366 2795 1424  464

``` r
table(NHIS_data$Hisp)
```

    ## 
    ##     Hispanic Non_hispanic 
    ##         1426         9015

``` r
table(NHIS_data$Race)
```

    ## 
    ##        AIAN and any other group                       AIAN only 
    ##                             109                              93 
    ##                      Asian only     Black/African American only 
    ##                             645                            1136 
    ##                      Don't know                 Not Ascertained 
    ##                               4                             532 
    ## Other single and multiple races                         Refused 
    ##                             140                               4 
    ##                      White only 
    ##                            7778

``` r
table(NHIS_data$Race_Hisp)
```

    ## 
    ## Non-Hispanic White Non-Hispanic Black           Hispanic              Other 
    ##               6996               1102               1426                917

``` r
table(NHIS_data$Sex)
```

    ## 
    ## Female   Male 
    ##   5473   4968

``` r
table(NHIS_data$Marital_Status)
```

    ## 
    ##     Married Not Married 
    ##        5533        4908

``` r
table(NHIS_data$Health_Status)
```

    ## 
    ##    1    2    3    4    5 
    ##  283  995 2818 3733 2612

``` r
table(NHIS_data$Covid_Test)
```

    ## 
    ##    0    1 
    ## 7138 3303

``` r
table(NHIS_data$Age_Group)
```

    ## 
    ## 27-34 35-44 45-54 55-64 
    ##  1949  2664  2499  3329

# Summary Statistics

## Summary Statistics: COVID Test

``` r
library(gmodels)

CrossTable(NHIS_data$Education, NHIS_data$Covid_Test, prop.chisq=FALSE, prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  10441 
    ## 
    ##  
    ##                     | NHIS_data$Covid_Test 
    ## NHIS_data$Education |         0 |         1 | Row Total | 
    ## --------------------|-----------|-----------|-----------|
    ##                   1 |       504 |       187 |       691 | 
    ##                     |     0.729 |     0.271 |     0.066 | 
    ## --------------------|-----------|-----------|-----------|
    ##                   2 |      1605 |       627 |      2232 | 
    ##                     |     0.719 |     0.281 |     0.214 | 
    ## --------------------|-----------|-----------|-----------|
    ##                   3 |       994 |       475 |      1469 | 
    ##                     |     0.677 |     0.323 |     0.141 | 
    ## --------------------|-----------|-----------|-----------|
    ##                   4 |       941 |       425 |      1366 | 
    ##                     |     0.689 |     0.311 |     0.131 | 
    ## --------------------|-----------|-----------|-----------|
    ##                   5 |      1890 |       905 |      2795 | 
    ##                     |     0.676 |     0.324 |     0.268 | 
    ## --------------------|-----------|-----------|-----------|
    ##                   6 |       926 |       498 |      1424 | 
    ##                     |     0.650 |     0.350 |     0.136 | 
    ## --------------------|-----------|-----------|-----------|
    ##                   7 |       278 |       186 |       464 | 
    ##                     |     0.599 |     0.401 |     0.044 | 
    ## --------------------|-----------|-----------|-----------|
    ##        Column Total |      7138 |      3303 |     10441 | 
    ## --------------------|-----------|-----------|-----------|
    ## 
    ## 

``` r
CrossTable(NHIS_data$Race_Hisp, NHIS_data$Covid_Test, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  10441 
    ## 
    ##  
    ##                     | NHIS_data$Covid_Test 
    ## NHIS_data$Race_Hisp |         0 |         1 | Row Total | 
    ## --------------------|-----------|-----------|-----------|
    ##  Non-Hispanic White |      4886 |      2110 |      6996 | 
    ##                     |     0.698 |     0.302 |     0.670 | 
    ## --------------------|-----------|-----------|-----------|
    ##  Non-Hispanic Black |       676 |       426 |      1102 | 
    ##                     |     0.613 |     0.387 |     0.106 | 
    ## --------------------|-----------|-----------|-----------|
    ##            Hispanic |       928 |       498 |      1426 | 
    ##                     |     0.651 |     0.349 |     0.137 | 
    ## --------------------|-----------|-----------|-----------|
    ##               Other |       648 |       269 |       917 | 
    ##                     |     0.707 |     0.293 |     0.088 | 
    ## --------------------|-----------|-----------|-----------|
    ##        Column Total |      7138 |      3303 |     10441 | 
    ## --------------------|-----------|-----------|-----------|
    ## 
    ## 

``` r
CrossTable(NHIS_data$Sex,NHIS_data$Covid_Test, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  10441 
    ## 
    ##  
    ##               | NHIS_data$Covid_Test 
    ## NHIS_data$Sex |         0 |         1 | Row Total | 
    ## --------------|-----------|-----------|-----------|
    ##        Female |      3605 |      1868 |      5473 | 
    ##               |     0.659 |     0.341 |     0.524 | 
    ## --------------|-----------|-----------|-----------|
    ##          Male |      3533 |      1435 |      4968 | 
    ##               |     0.711 |     0.289 |     0.476 | 
    ## --------------|-----------|-----------|-----------|
    ##  Column Total |      7138 |      3303 |     10441 | 
    ## --------------|-----------|-----------|-----------|
    ## 
    ## 

``` r
CrossTable(NHIS_data$Marital_Status,NHIS_data$Covid_Test, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  10441 
    ## 
    ##  
    ##                          | NHIS_data$Covid_Test 
    ## NHIS_data$Marital_Status |         0 |         1 | Row Total | 
    ## -------------------------|-----------|-----------|-----------|
    ##                  Married |      3858 |      1675 |      5533 | 
    ##                          |     0.697 |     0.303 |     0.530 | 
    ## -------------------------|-----------|-----------|-----------|
    ##              Not Married |      3280 |      1628 |      4908 | 
    ##                          |     0.668 |     0.332 |     0.470 | 
    ## -------------------------|-----------|-----------|-----------|
    ##             Column Total |      7138 |      3303 |     10441 | 
    ## -------------------------|-----------|-----------|-----------|
    ## 
    ## 

``` r
CrossTable(NHIS_data$Health_Status,NHIS_data$Covid_Test, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  10441 
    ## 
    ##  
    ##                         | NHIS_data$Covid_Test 
    ## NHIS_data$Health_Status |         0 |         1 | Row Total | 
    ## ------------------------|-----------|-----------|-----------|
    ##                       1 |       172 |       111 |       283 | 
    ##                         |     0.608 |     0.392 |     0.027 | 
    ## ------------------------|-----------|-----------|-----------|
    ##                       2 |       655 |       340 |       995 | 
    ##                         |     0.658 |     0.342 |     0.095 | 
    ## ------------------------|-----------|-----------|-----------|
    ##                       3 |      1909 |       909 |      2818 | 
    ##                         |     0.677 |     0.323 |     0.270 | 
    ## ------------------------|-----------|-----------|-----------|
    ##                       4 |      2586 |      1147 |      3733 | 
    ##                         |     0.693 |     0.307 |     0.358 | 
    ## ------------------------|-----------|-----------|-----------|
    ##                       5 |      1816 |       796 |      2612 | 
    ##                         |     0.695 |     0.305 |     0.250 | 
    ## ------------------------|-----------|-----------|-----------|
    ##            Column Total |      7138 |      3303 |     10441 | 
    ## ------------------------|-----------|-----------|-----------|
    ## 
    ## 

``` r
CrossTable(NHIS_data$Age_Group,NHIS_data$Covid_Test, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  10441 
    ## 
    ##  
    ##                     | NHIS_data$Covid_Test 
    ## NHIS_data$Age_Group |         0 |         1 | Row Total | 
    ## --------------------|-----------|-----------|-----------|
    ##               27-34 |      1286 |       663 |      1949 | 
    ##                     |     0.660 |     0.340 |     0.187 | 
    ## --------------------|-----------|-----------|-----------|
    ##               35-44 |      1828 |       836 |      2664 | 
    ##                     |     0.686 |     0.314 |     0.255 | 
    ## --------------------|-----------|-----------|-----------|
    ##               45-54 |      1694 |       805 |      2499 | 
    ##                     |     0.678 |     0.322 |     0.239 | 
    ## --------------------|-----------|-----------|-----------|
    ##               55-64 |      2330 |       999 |      3329 | 
    ##                     |     0.700 |     0.300 |     0.319 | 
    ## --------------------|-----------|-----------|-----------|
    ##        Column Total |      7138 |      3303 |     10441 | 
    ## --------------------|-----------|-----------|-----------|
    ## 
    ## 

## Summary Statistics: Health Status

``` r
library(gmodels)

CrossTable(NHIS_data$Education, NHIS_data$Health_Status, prop.chisq=FALSE, prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  10441 
    ## 
    ##  
    ##                     | NHIS_data$Health_Status 
    ## NHIS_data$Education |         1 |         2 |         3 |         4 |         5 | Row Total | 
    ## --------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##                   1 |        56 |       153 |       245 |       126 |       111 |       691 | 
    ##                     |     0.081 |     0.221 |     0.355 |     0.182 |     0.161 |     0.066 | 
    ## --------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##                   2 |        78 |       329 |       745 |       655 |       425 |      2232 | 
    ##                     |     0.035 |     0.147 |     0.334 |     0.293 |     0.190 |     0.214 | 
    ## --------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##                   3 |        63 |       169 |       468 |       483 |       286 |      1469 | 
    ##                     |     0.043 |     0.115 |     0.319 |     0.329 |     0.195 |     0.141 | 
    ## --------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##                   4 |        43 |       126 |       419 |       503 |       275 |      1366 | 
    ##                     |     0.031 |     0.092 |     0.307 |     0.368 |     0.201 |     0.131 | 
    ## --------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##                   5 |        30 |       148 |       612 |      1152 |       853 |      2795 | 
    ##                     |     0.011 |     0.053 |     0.219 |     0.412 |     0.305 |     0.268 | 
    ## --------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##                   6 |        11 |        63 |       268 |       603 |       479 |      1424 | 
    ##                     |     0.008 |     0.044 |     0.188 |     0.423 |     0.336 |     0.136 | 
    ## --------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##                   7 |         2 |         7 |        61 |       211 |       183 |       464 | 
    ##                     |     0.004 |     0.015 |     0.131 |     0.455 |     0.394 |     0.044 | 
    ## --------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##        Column Total |       283 |       995 |      2818 |      3733 |      2612 |     10441 | 
    ## --------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ## 
    ## 

``` r
CrossTable(NHIS_data$Race_Hisp, NHIS_data$Health_Status, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  10441 
    ## 
    ##  
    ##                     | NHIS_data$Health_Status 
    ## NHIS_data$Race_Hisp |         1 |         2 |         3 |         4 |         5 | Row Total | 
    ## --------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##  Non-Hispanic White |       189 |       581 |      1800 |      2639 |      1787 |      6996 | 
    ##                     |     0.027 |     0.083 |     0.257 |     0.377 |     0.255 |     0.670 | 
    ## --------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##  Non-Hispanic Black |        36 |       153 |       370 |       325 |       218 |      1102 | 
    ##                     |     0.033 |     0.139 |     0.336 |     0.295 |     0.198 |     0.106 | 
    ## --------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##            Hispanic |        41 |       188 |       406 |       440 |       351 |      1426 | 
    ##                     |     0.029 |     0.132 |     0.285 |     0.309 |     0.246 |     0.137 | 
    ## --------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##               Other |        17 |        73 |       242 |       329 |       256 |       917 | 
    ##                     |     0.019 |     0.080 |     0.264 |     0.359 |     0.279 |     0.088 | 
    ## --------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##        Column Total |       283 |       995 |      2818 |      3733 |      2612 |     10441 | 
    ## --------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ## 
    ## 

``` r
CrossTable(NHIS_data$Sex,NHIS_data$Health_Status, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  10441 
    ## 
    ##  
    ##               | NHIS_data$Health_Status 
    ## NHIS_data$Sex |         1 |         2 |         3 |         4 |         5 | Row Total | 
    ## --------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##        Female |       170 |       537 |      1465 |      1973 |      1328 |      5473 | 
    ##               |     0.031 |     0.098 |     0.268 |     0.360 |     0.243 |     0.524 | 
    ## --------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##          Male |       113 |       458 |      1353 |      1760 |      1284 |      4968 | 
    ##               |     0.023 |     0.092 |     0.272 |     0.354 |     0.258 |     0.476 | 
    ## --------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##  Column Total |       283 |       995 |      2818 |      3733 |      2612 |     10441 | 
    ## --------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ## 
    ## 

``` r
CrossTable(NHIS_data$Marital_Status,NHIS_data$Health_Status, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  10441 
    ## 
    ##  
    ##                          | NHIS_data$Health_Status 
    ## NHIS_data$Marital_Status |         1 |         2 |         3 |         4 |         5 | Row Total | 
    ## -------------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##                  Married |        94 |       401 |      1392 |      2141 |      1505 |      5533 | 
    ##                          |     0.017 |     0.072 |     0.252 |     0.387 |     0.272 |     0.530 | 
    ## -------------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##              Not Married |       189 |       594 |      1426 |      1592 |      1107 |      4908 | 
    ##                          |     0.039 |     0.121 |     0.291 |     0.324 |     0.226 |     0.470 | 
    ## -------------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##             Column Total |       283 |       995 |      2818 |      3733 |      2612 |     10441 | 
    ## -------------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ## 
    ## 

``` r
CrossTable(NHIS_data$Covid_Test,NHIS_data$Health_Status, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  10441 
    ## 
    ##  
    ##                      | NHIS_data$Health_Status 
    ## NHIS_data$Covid_Test |         1 |         2 |         3 |         4 |         5 | Row Total | 
    ## ---------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##                    0 |       172 |       655 |      1909 |      2586 |      1816 |      7138 | 
    ##                      |     0.024 |     0.092 |     0.267 |     0.362 |     0.254 |     0.684 | 
    ## ---------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##                    1 |       111 |       340 |       909 |      1147 |       796 |      3303 | 
    ##                      |     0.034 |     0.103 |     0.275 |     0.347 |     0.241 |     0.316 | 
    ## ---------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##         Column Total |       283 |       995 |      2818 |      3733 |      2612 |     10441 | 
    ## ---------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ## 
    ## 

``` r
CrossTable(NHIS_data$Age_Group,NHIS_data$Health_Status, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |           N / Row Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  10441 
    ## 
    ##  
    ##                     | NHIS_data$Health_Status 
    ## NHIS_data$Age_Group |         1 |         2 |         3 |         4 |         5 | Row Total | 
    ## --------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##               27-34 |        12 |       105 |       403 |       770 |       659 |      1949 | 
    ##                     |     0.006 |     0.054 |     0.207 |     0.395 |     0.338 |     0.187 | 
    ## --------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##               35-44 |        40 |       191 |       683 |       998 |       752 |      2664 | 
    ##                     |     0.015 |     0.072 |     0.256 |     0.375 |     0.282 |     0.255 | 
    ## --------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##               45-54 |        66 |       259 |       713 |       894 |       567 |      2499 | 
    ##                     |     0.026 |     0.104 |     0.285 |     0.358 |     0.227 |     0.239 | 
    ## --------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##               55-64 |       165 |       440 |      1019 |      1071 |       634 |      3329 | 
    ##                     |     0.050 |     0.132 |     0.306 |     0.322 |     0.190 |     0.319 | 
    ## --------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ##        Column Total |       283 |       995 |      2818 |      3733 |      2612 |     10441 | 
    ## --------------------|-----------|-----------|-----------|-----------|-----------|-----------|
    ## 
    ## 

## Summary Statistics: Numeric Variables

``` r
library(psych)
```

    ## Warning: package 'psych' was built under R version 4.0.5

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
describe(NHIS_data$Age[NHIS_data$Covid_Test == "0"])
```

    ##    vars    n  mean    sd median trimmed   mad min max range skew kurtosis   se
    ## X1    1 7138 46.85 11.18     47   47.05 14.83  27  64    37 -0.1    -1.27 0.13

``` r
describe(NHIS_data$Age[NHIS_data$Covid_Test == "1"])
```

    ##    vars    n  mean    sd median trimmed   mad min max range  skew kurtosis  se
    ## X1    1 3303 46.27 11.21     46    46.4 14.83  27  64    37 -0.06    -1.27 0.2

``` r
describe(NHIS_data$Education[NHIS_data$Covid_Test == "0"])
```

    ##    vars    n mean   sd median trimmed  mad min max range  skew kurtosis   se
    ## X1    1 7138 3.84 1.67      4    3.84 1.48   1   7     6 -0.03    -1.14 0.02

``` r
describe(NHIS_data$Education[NHIS_data$Covid_Test == "1"])
```

    ##    vars    n mean   sd median trimmed  mad min max range  skew kurtosis   se
    ## X1    1 3303 4.05 1.67      4    4.06 1.48   1   7     6 -0.13    -1.07 0.03

``` r
describe(NHIS_data$Health_Status[NHIS_data$Covid_Test == "0"])
```

    ##    vars    n mean   sd median trimmed  mad min max range  skew kurtosis   se
    ## X1    1 7138 3.73 1.02      4    3.82 1.48   1   5     4 -0.52    -0.28 0.01

``` r
describe(NHIS_data$Health_Status[NHIS_data$Covid_Test == "1"])
```

    ##    vars    n mean   sd median trimmed  mad min max range skew kurtosis   se
    ## X1    1 3303 3.66 1.06      4    3.74 1.48   1   5     4 -0.5    -0.35 0.02

``` r
describeBy(NHIS_data, group = NHIS_data$Covid_Test)
```

    ## 
    ##  Descriptive statistics by group 
    ## group: 0
    ##                 vars    n  mean    sd median trimmed   mad min max range  skew
    ## Education          1 7138  3.84  1.67      4    3.84  1.48   1   7     6 -0.03
    ## Hisp*              2 7138  1.87  0.34      2    1.96  0.00   1   2     1 -2.20
    ## Race*              3 7138  7.80  2.22      9    8.26  0.00   1   9     8 -1.49
    ## Age                4 7138 46.85 11.18     47   47.05 14.83  27  64    37 -0.10
    ## Sex*               5 7138  1.49  0.50      1    1.49  0.00   1   2     1  0.02
    ## Marital_Status*    6 7138  1.46  0.50      1    1.45  0.00   1   2     1  0.16
    ## Health_Status      7 7138  3.73  1.02      4    3.82  1.48   1   5     4 -0.52
    ## Covid_Test*        8 7138  1.00  0.00      1    1.00  0.00   1   1     0   NaN
    ## Race_Hisp*         9 7138  1.63  1.02      1    1.42  0.00   1   4     3  1.31
    ## Age_Group*        10 7138  2.71  1.10      3    2.76  1.48   1   4     3 -0.21
    ##                 kurtosis   se
    ## Education          -1.14 0.02
    ## Hisp*               2.84 0.00
    ## Race*               0.61 0.03
    ## Age                -1.27 0.13
    ## Sex*               -2.00 0.01
    ## Marital_Status*    -1.97 0.01
    ## Health_Status      -0.28 0.01
    ## Covid_Test*          NaN 0.00
    ## Race_Hisp*          0.20 0.01
    ## Age_Group*         -1.31 0.01
    ## ------------------------------------------------------------ 
    ## group: 1
    ##                 vars    n  mean    sd median trimmed   mad min max range  skew
    ## Education          1 3303  4.05  1.67      4    4.06  1.48   1   7     6 -0.13
    ## Hisp*              2 3303  1.85  0.36      2    1.94  0.00   1   2     1 -1.95
    ## Race*              3 3303  7.67  2.29      9    8.09  0.00   1   9     8 -1.33
    ## Age                4 3303 46.27 11.21     46   46.40 14.83  27  64    37 -0.06
    ## Sex*               5 3303  1.43  0.50      1    1.42  0.00   1   2     1  0.26
    ## Marital_Status*    6 3303  1.49  0.50      1    1.49  0.00   1   2     1  0.03
    ## Health_Status      7 3303  3.66  1.06      4    3.74  1.48   1   5     4 -0.50
    ## Covid_Test*        8 3303  2.00  0.00      2    2.00  0.00   2   2     0   NaN
    ## Race_Hisp*         9 3303  1.67  1.00      1    1.49  0.00   1   4     3  1.17
    ## Age_Group*        10 3303  2.65  1.11      3    2.68  1.48   1   4     3 -0.15
    ##                 kurtosis   se
    ## Education          -1.07 0.03
    ## Hisp*               1.81 0.01
    ## Race*               0.16 0.04
    ## Age                -1.27 0.20
    ## Sex*               -1.93 0.01
    ## Marital_Status*    -2.00 0.01
    ## Health_Status      -0.35 0.02
    ## Covid_Test*          NaN 0.00
    ## Race_Hisp*         -0.08 0.02
    ## Age_Group*         -1.33 0.02

# Correlation Check

``` r
cor(NHIS_data$Age, NHIS_data$Health_Status)
```

    ## [1] -0.2017515

``` r
cor(NHIS_data$Age, NHIS_data$Education)
```

    ## [1] -0.1170322

``` r
cor(NHIS_data$Education, NHIS_data$Health_Status)
```

    ## [1] 0.2747467

# Logistic Regression

## Model \#1: Demographic information only

``` r
Model_1 <- glm(Covid_Test ~ Education
               + Age 
               + Sex 
               + Marital_Status
               + Race_Hisp,
               data = NHIS_data,
               family = binomial)

summary(Model_1)
```

    ## 
    ## Call:
    ## glm(formula = Covid_Test ~ Education + Age + Sex + Marital_Status + 
    ##     Race_Hisp, family = binomial, data = NHIS_data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.1930  -0.8896  -0.8060   1.4127   1.8073  
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 -1.092259   0.121524  -8.988  < 2e-16 ***
    ## Education                    0.097014   0.013307   7.290 3.09e-13 ***
    ## Age                         -0.002086   0.001929  -1.081  0.27960    
    ## SexMale                     -0.223434   0.042632  -5.241 1.60e-07 ***
    ## Marital_StatusNot Married    0.129830   0.043296   2.999  0.00271 ** 
    ## Race_HispNon-Hispanic Black  0.382474   0.068792   5.560 2.70e-08 ***
    ## Race_HispHispanic            0.299466   0.063693   4.702 2.58e-06 ***
    ## Race_HispOther              -0.076137   0.077848  -0.978  0.32806    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13032  on 10440  degrees of freedom
    ## Residual deviance: 12898  on 10433  degrees of freedom
    ## AIC: 12914
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
library(questionr)
```

    ## Warning: package 'questionr' was built under R version 4.0.5

    ## 
    ## Attaching package: 'questionr'

    ## The following object is masked from 'package:psych':
    ## 
    ##     describe

``` r
odds.ratio(Model_1) #This tells us the "Odds Ratios"
```

    ## Waiting for profiling to be done...

    ##                                  OR   2.5 % 97.5 %         p    
    ## (Intercept)                 0.33546 0.26424 0.4255 < 2.2e-16 ***
    ## Education                   1.10188 1.07354 1.1310 3.090e-13 ***
    ## Age                         0.99792 0.99415 1.0017  0.279597    
    ## SexMale                     0.79977 0.73561 0.8694 1.597e-07 ***
    ## Marital_StatusNot Married   1.13863 1.04599 1.2395  0.002712 ** 
    ## Race_HispNon-Hispanic Black 1.46591 1.28043 1.6769 2.699e-08 ***
    ## Race_HispHispanic           1.34914 1.19032 1.5280 2.579e-06 ***
    ## Race_HispOther              0.92669 0.79455 1.0782  0.328065    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# library(oddsratio)
# or_glm(data = NHIS_data, model = Model_1, 
#        incr = list(Education = 1, Age = 1))
# or_glm(data = NHIS_data, model = Model_1,
#        incr = list(Education = 2, Age = 1))
# or_glm(data = NHIS_data, model = Model_1,
#        incr = list(Education = 3, Age = 1))
# or_glm(data = NHIS_data, model = Model_1,
#        incr = list(Education = 4, Age = 1))
# or_glm(data = NHIS_data, model = Model_1,
#        incr = list(Education = 5, Age = 1))
# or_glm(data = NHIS_data, model = Model_1,
#        incr = list(Education = 6, Age = 1))
# or_glm(data = NHIS_data, model = Model_1,
#        incr = list(Education = 7, Age = 1))
```

## Model \#2: Demographic + Health Status

``` r
Model_2 <- glm(Covid_Test ~ Education
               + Age 
               + Sex 
               + Marital_Status
               + Race_Hisp
               + Health_Status,
               data = NHIS_data,
               family = binomial)

summary(Model_2)
```

    ## 
    ## Call:
    ## glm(formula = Covid_Test ~ Education + Age + Sex + Marital_Status + 
    ##     Race_Hisp + Health_Status, family = binomial, data = NHIS_data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.2682  -0.8883  -0.7999   1.4069   1.8544  
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 -0.672645   0.149194  -4.509 6.53e-06 ***
    ## Education                    0.113072   0.013752   8.222  < 2e-16 ***
    ## Age                         -0.003852   0.001967  -1.959   0.0502 .  
    ## SexMale                     -0.217212   0.042695  -5.088 3.63e-07 ***
    ## Marital_StatusNot Married    0.109557   0.043548   2.516   0.0119 *  
    ## Race_HispNon-Hispanic Black  0.368694   0.068928   5.349 8.84e-08 ***
    ## Race_HispHispanic            0.296347   0.063780   4.646 3.38e-06 ***
    ## Race_HispOther              -0.083819   0.077925  -1.076   0.2821    
    ## Health_Status               -0.105906   0.021887  -4.839 1.31e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13032  on 10440  degrees of freedom
    ## Residual deviance: 12875  on 10432  degrees of freedom
    ## AIC: 12893
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
library(questionr)
odds.ratio(Model_2) #This tells us the "Odds Ratios"
```

    ## Waiting for profiling to be done...

    ##                                  OR   2.5 % 97.5 %         p    
    ## (Intercept)                 0.51036 0.38085 0.6835 6.527e-06 ***
    ## Education                   1.11971 1.08997 1.1504 < 2.2e-16 ***
    ## Age                         0.99616 0.99232 1.0000   0.05016 .  
    ## SexMale                     0.80476 0.74011 0.8750 3.627e-07 ***
    ## Marital_StatusNot Married   1.11578 1.02448 1.2152   0.01188 *  
    ## Race_HispNon-Hispanic Black 1.44584 1.26257 1.6543 8.845e-08 ***
    ## Race_HispHispanic           1.34494 1.18641 1.5235 3.378e-06 ***
    ## Race_HispOther              0.91960 0.78835 1.0701   0.28209    
    ## Health_Status               0.89951 0.86173 0.9389 1.306e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# library(oddsratio)
# or_glm(data = NHIS_data, model = Model_2, 
#        incr = list(Education = 1, Age = 1, Health_Status = 1))
# or_glm(data = NHIS_data, model = Model_2,
#        incr = list(Education = 1, Age = 1, Health_Status = 2))
# or_glm(data = NHIS_data, model = Model_2,
#        incr = list(Education = 1, Age = 1, Health_Status = 3))
# or_glm(data = NHIS_data, model = Model_2,
#        incr = list(Education = 1, Age = 1, Health_Status = 4))
# or_glm(data = NHIS_data, model = Model_2,
#        incr = list(Education = 1, Age = 1, Health_Status = 5))
```

## Model \#3: Demographic + Health Status + Interaction of Education & Health Status

``` r
Model_3 <- glm(Covid_Test ~ Education
               + Age 
               + Sex 
               + Marital_Status
               + Race_Hisp
               + Health_Status
               + Education*Health_Status,
               data = NHIS_data,
               family = binomial)

summary(Model_3)
```

    ## 
    ## Call:
    ## glm(formula = Covid_Test ~ Education + Age + Sex + Marital_Status + 
    ##     Race_Hisp + Health_Status + Education * Health_Status, family = binomial, 
    ##     data = NHIS_data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.2080  -0.8912  -0.8025   1.4047   1.8970  
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 -0.348668   0.223092  -1.563 0.118078    
    ## Education                    0.023288   0.048094   0.484 0.628231    
    ## Age                         -0.003958   0.001968  -2.011 0.044300 *  
    ## SexMale                     -0.216859   0.042701  -5.079 3.80e-07 ***
    ## Marital_StatusNot Married    0.108218   0.043566   2.484 0.012992 *  
    ## Race_HispNon-Hispanic Black  0.369334   0.068935   5.358 8.43e-08 ***
    ## Race_HispHispanic            0.295660   0.063809   4.633 3.60e-06 ***
    ## Race_HispOther              -0.086955   0.077946  -1.116 0.264600    
    ## Health_Status               -0.195165   0.050790  -3.843 0.000122 ***
    ## Education:Health_Status      0.024259   0.012457   1.947 0.051488 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13032  on 10440  degrees of freedom
    ## Residual deviance: 12871  on 10431  degrees of freedom
    ## AIC: 12891
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
library(questionr)
odds.ratio(Model_3) #This tells us the "Odds Ratios"
```

    ## Waiting for profiling to be done...

    ##                                  OR   2.5 % 97.5 %         p    
    ## (Intercept)                 0.70563 0.45534 1.0919 0.1180777    
    ## Education                   1.02356 0.93154 1.1248 0.6282308    
    ## Age                         0.99605 0.99221 0.9999 0.0442997 *  
    ## SexMale                     0.80504 0.74036 0.8753 3.803e-07 ***
    ## Marital_StatusNot Married   1.11429 1.02308 1.2136 0.0129917 *  
    ## Race_HispNon-Hispanic Black 1.44677 1.26336 1.6554 8.427e-08 ***
    ## Race_HispHispanic           1.34401 1.18553 1.5225 3.596e-06 ***
    ## Race_HispOther              0.91672 0.78585 1.0668 0.2645998    
    ## Health_Status               0.82270 0.74476 0.9089 0.0001217 ***
    ## Education:Health_Status     1.02456 0.99983 1.0499 0.0514884 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Logistic Regression (with Age Groups)

## Model \#1a: Demographic information only (with Age Groups)

``` r
Model_1a <- glm(Covid_Test ~ Education
               + Age_Group
               + Sex 
               + Marital_Status
               + Race_Hisp,
               data = NHIS_data,
               family = binomial)

summary(Model_1a)
```

    ## 
    ## Call:
    ## glm(formula = Covid_Test ~ Education + Age_Group + Sex + Marital_Status + 
    ##     Race_Hisp, family = binomial, data = NHIS_data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.2122  -0.8865  -0.8040   1.4071   1.8128  
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 -1.10723    0.08580 -12.904  < 2e-16 ***
    ## Education                    0.09699    0.01332   7.283 3.26e-13 ***
    ## Age_Group35-44              -0.11618    0.06417  -1.810  0.07024 .  
    ## Age_Group45-54              -0.05337    0.06499  -0.821  0.41147    
    ## Age_Group55-64              -0.11684    0.06231  -1.875  0.06075 .  
    ## SexMale                     -0.22496    0.04265  -5.275 1.33e-07 ***
    ## Marital_StatusNot Married    0.12522    0.04346   2.881  0.00396 ** 
    ## Race_HispNon-Hispanic Black  0.38458    0.06889   5.583 2.37e-08 ***
    ## Race_HispHispanic            0.29811    0.06375   4.676 2.92e-06 ***
    ## Race_HispOther              -0.07628    0.07787  -0.980  0.32729    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13032  on 10440  degrees of freedom
    ## Residual deviance: 12895  on 10431  degrees of freedom
    ## AIC: 12915
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
library(questionr)
odds.ratio(Model_1a) #This tells us the "Odds Ratios"
```

    ## Waiting for profiling to be done...

    ##                                  OR   2.5 % 97.5 %         p    
    ## (Intercept)                 0.33047 0.27918 0.3908 < 2.2e-16 ***
    ## Education                   1.10185 1.07349 1.1310 3.262e-13 ***
    ## Age_Group35-44              0.89032 0.78515 1.0097  0.070238 .  
    ## Age_Group45-54              0.94803 0.83471 1.0769  0.411467    
    ## Age_Group55-64              0.88973 0.78757 1.0055  0.060752 .  
    ## SexMale                     0.79855 0.73446 0.8681 1.330e-07 ***
    ## Marital_StatusNot Married   1.13340 1.04084 1.2342  0.003962 ** 
    ## Race_HispNon-Hispanic Black 1.46900 1.28289 1.6807 2.368e-08 ***
    ## Race_HispHispanic           1.34731 1.18857 1.5261 2.924e-06 ***
    ## Race_HispOther              0.92655 0.79440 1.0781  0.327289    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Model \#2a: Demographic + Health Status (with Age Groups)

``` r
Model_2a <- glm(Covid_Test ~ Education
               + Age_Group 
               + Sex 
               + Marital_Status
               + Race_Hisp
               + Health_Status,
               data = NHIS_data,
               family = binomial)

summary(Model_2a)
```

    ## 
    ## Call:
    ## glm(formula = Covid_Test ~ Education + Age_Group + Sex + Marital_Status + 
    ##     Race_Hisp + Health_Status, family = binomial, data = NHIS_data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.2893  -0.8882  -0.7987   1.4031   1.8648  
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 -0.73754    0.11416  -6.461 1.04e-10 ***
    ## Education                    0.11330    0.01377   8.230  < 2e-16 ***
    ## Age_Group35-44              -0.13653    0.06439  -2.121  0.03396 *  
    ## Age_Group45-54              -0.09001    0.06551  -1.374  0.16947    
    ## Age_Group55-64              -0.16899    0.06332  -2.669  0.00761 ** 
    ## SexMale                     -0.21873    0.04271  -5.121 3.04e-07 ***
    ## Marital_StatusNot Married    0.10459    0.04372   2.392  0.01674 *  
    ## Race_HispNon-Hispanic Black  0.37127    0.06902   5.379 7.48e-08 ***
    ## Race_HispHispanic            0.29576    0.06384   4.633 3.60e-06 ***
    ## Race_HispOther              -0.08323    0.07794  -1.068  0.28559    
    ## Health_Status               -0.10671    0.02185  -4.883 1.05e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13032  on 10440  degrees of freedom
    ## Residual deviance: 12871  on 10430  degrees of freedom
    ## AIC: 12893
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
library(questionr)
odds.ratio(Model_2a) #This tells us the "Odds Ratios"
```

    ## Waiting for profiling to be done...

    ##                                  OR   2.5 % 97.5 %         p    
    ## (Intercept)                 0.47829 0.38225 0.5980 1.041e-10 ***
    ## Education                   1.11997 1.09019 1.1507 < 2.2e-16 ***
    ## Age_Group35-44              0.87238 0.76900 0.9898   0.03396 *  
    ## Age_Group45-54              0.91392 0.80383 1.0392   0.16947    
    ## Age_Group55-64              0.84451 0.74604 0.9562   0.00761 ** 
    ## SexMale                     0.80354 0.73896 0.8737 3.041e-07 ***
    ## Marital_StatusNot Married   1.11026 1.01907 1.2096   0.01674 *  
    ## Race_HispNon-Hispanic Black 1.44958 1.26560 1.6589 7.484e-08 ***
    ## Race_HispHispanic           1.34415 1.18559 1.5228 3.604e-06 ***
    ## Race_HispOther              0.92014 0.78879 1.0708   0.28559    
    ## Health_Status               0.89879 0.86109 0.9381 1.046e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Model \#3a: Demographic + Health Status + Interaction of Education & Health Status (with Age Groups)

``` r
Model_3a <- glm(Covid_Test ~ Education
               + Age_Group 
               + Sex 
               + Marital_Status
               + Race_Hisp
               + Health_Status
               + Education*Health_Status,
               data = NHIS_data,
               family = binomial)

summary(Model_3a)
```

    ## 
    ## Call:
    ## glm(formula = Covid_Test ~ Education + Age_Group + Sex + Marital_Status + 
    ##     Race_Hisp + Health_Status + Education * Health_Status, family = binomial, 
    ##     data = NHIS_data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.2283  -0.8929  -0.8006   1.4015   1.9088  
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 -0.40882    0.20015  -2.043  0.04110 *  
    ## Education                    0.02135    0.04813   0.444  0.65739    
    ## Age_Group35-44              -0.13912    0.06442  -2.160  0.03081 *  
    ## Age_Group45-54              -0.09031    0.06553  -1.378  0.16818    
    ## Age_Group55-64              -0.17312    0.06338  -2.731  0.00631 ** 
    ## SexMale                     -0.21844    0.04272  -5.113 3.17e-07 ***
    ## Marital_StatusNot Married    0.10321    0.04374   2.360  0.01829 *  
    ## Race_HispNon-Hispanic Black  0.37191    0.06903   5.388 7.13e-08 ***
    ## Race_HispHispanic            0.29501    0.06387   4.619 3.85e-06 ***
    ## Race_HispOther              -0.08645    0.07797  -1.109  0.26751    
    ## Health_Status               -0.19810    0.05080  -3.900 9.63e-05 ***
    ## Education:Health_Status      0.02484    0.01247   1.993  0.04628 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13032  on 10440  degrees of freedom
    ## Residual deviance: 12867  on 10429  degrees of freedom
    ## AIC: 12891
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
library(questionr)
odds.ratio(Model_3a) #This tells us the "Odds Ratios"
```

    ## Waiting for profiling to be done...

    ##                                  OR   2.5 % 97.5 %         p    
    ## (Intercept)                 0.66444 0.44840 0.9828  0.041097 *  
    ## Education                   1.02158 0.92967 1.1228  0.657389    
    ## Age_Group35-44              0.87013 0.76697 0.9873  0.030808 *  
    ## Age_Group45-54              0.91365 0.80356 1.0390  0.168177    
    ## Age_Group55-64              0.84104 0.74288 0.9524  0.006305 ** 
    ## SexMale                     0.80377 0.73917 0.8739 3.166e-07 ***
    ## Marital_StatusNot Married   1.10872 1.01762 1.2080  0.018289 *  
    ## Race_HispNon-Hispanic Black 1.45050 1.26639 1.6600 7.132e-08 ***
    ## Race_HispHispanic           1.34315 1.18463 1.5217 3.852e-06 ***
    ## Race_HispOther              0.91718 0.78622 1.0674  0.267511    
    ## Health_Status               0.82029 0.74256 0.9062 9.628e-05 ***
    ## Education:Health_Status     1.02516 1.00039 1.0505  0.046279 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Logistic Regression (with Health Status levels)

## Model \#2b: Demographic + Health Status (with Health Status levels)

``` r
Model_2b <- glm(Covid_Test ~ Education
               + Age 
               + Sex 
               + Marital_Status
               + Race_Hisp
               + as.factor(Health_Status),
               data = NHIS_data,
               family = binomial)

summary(Model_2b)
```

    ## 
    ## Call:
    ## glm(formula = Covid_Test ~ Education + Age + Sex + Marital_Status + 
    ##     Race_Hisp + as.factor(Health_Status), family = binomial, 
    ##     data = NHIS_data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3340  -0.8904  -0.8002   1.4045   1.8452  
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 -0.625889   0.176390  -3.548 0.000388 ***
    ## Education                    0.114088   0.013798   8.268  < 2e-16 ***
    ## Age                         -0.003955   0.001968  -2.009 0.044489 *  
    ## SexMale                     -0.216553   0.042713  -5.070 3.98e-07 ***
    ## Marital_StatusNot Married    0.106413   0.043598   2.441 0.014656 *  
    ## Race_HispNon-Hispanic Black  0.369539   0.069005   5.355 8.55e-08 ***
    ## Race_HispHispanic            0.295240   0.063836   4.625 3.75e-06 ***
    ## Race_HispOther              -0.084629   0.077943  -1.086 0.277574    
    ## as.factor(Health_Status)2   -0.253104   0.140377  -1.803 0.071383 .  
    ## as.factor(Health_Status)3   -0.372703   0.130418  -2.858 0.004266 ** 
    ## as.factor(Health_Status)4   -0.502404   0.130574  -3.848 0.000119 ***
    ## as.factor(Health_Status)5   -0.532533   0.133360  -3.993 6.52e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13032  on 10440  degrees of freedom
    ## Residual deviance: 12871  on 10429  degrees of freedom
    ## AIC: 12895
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
library(questionr)
odds.ratio(Model_2b) #This tells us the "Odds Ratios"
```

    ## Waiting for profiling to be done...

    ##                                  OR   2.5 % 97.5 %         p    
    ## (Intercept)                 0.53479 0.37799 0.7549 0.0003877 ***
    ## Education                   1.12085 1.09098 1.1516 < 2.2e-16 ***
    ## Age                         0.99605 0.99222 0.9999 0.0444885 *  
    ## SexMale                     0.80529 0.74057 0.8756 3.978e-07 ***
    ## Marital_StatusNot Married   1.11228 1.02117 1.2115 0.0146562 *  
    ## Race_HispNon-Hispanic Black 1.44707 1.26345 1.6560 8.545e-08 ***
    ## Race_HispHispanic           1.34345 1.18497 1.5220 3.747e-06 ***
    ## Race_HispOther              0.91885 0.78769 1.0693 0.2775740    
    ## as.factor(Health_Status)2   0.77639 0.59031 1.0238 0.0713827 .  
    ## as.factor(Health_Status)3   0.68887 0.53428 0.8913 0.0042664 ** 
    ## as.factor(Health_Status)4   0.60507 0.46914 0.7831 0.0001193 ***
    ## as.factor(Health_Status)5   0.58712 0.45270 0.7639 6.518e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# OLS Regression (dep. var. Health Status)

## Model \#4: Demographic + Health Status

``` r
Model_4 <- lm(Health_Status ~ as.factor(Education)
               + Age_Group 
               + Sex 
               + Marital_Status
               + Race_Hisp
               + Covid_Test,
               data = NHIS_data)

summary(Model_4)
```

    ## 
    ## Call:
    ## lm(formula = Health_Status ~ as.factor(Education) + Age_Group + 
    ##     Sex + Marital_Status + Race_Hisp + Covid_Test, data = NHIS_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.1779 -0.6243  0.0280  0.7349  2.2819 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  3.561144   0.049020  72.646  < 2e-16 ***
    ## as.factor(Education)2        0.326790   0.043088   7.584 3.63e-14 ***
    ## as.factor(Education)3        0.382528   0.045697   8.371  < 2e-16 ***
    ## as.factor(Education)4        0.462699   0.046515   9.947  < 2e-16 ***
    ## as.factor(Education)5        0.749449   0.042942  17.453  < 2e-16 ***
    ## as.factor(Education)6        0.840460   0.046772  17.969  < 2e-16 ***
    ## as.factor(Education)7        1.033795   0.059635  17.335  < 2e-16 ***
    ## Age_Group35-44              -0.186963   0.029026  -6.441 1.24e-10 ***
    ## Age_Group45-54              -0.337361   0.029452 -11.454  < 2e-16 ***
    ## Age_Group55-64              -0.483837   0.028102 -17.217  < 2e-16 ***
    ## SexMale                      0.054237   0.019036   2.849 0.004392 ** 
    ## Marital_StatusNot Married   -0.193218   0.019430  -9.945  < 2e-16 ***
    ## Race_HispNon-Hispanic Black -0.120071   0.032011  -3.751 0.000177 ***
    ## Race_HispHispanic           -0.001072   0.029566  -0.036 0.971084    
    ## Race_HispOther              -0.067598   0.034252  -1.974 0.048460 *  
    ## Covid_Test1                 -0.100202   0.020488  -4.891 1.02e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9668 on 10425 degrees of freedom
    ## Multiple R-squared:  0.1197, Adjusted R-squared:  0.1184 
    ## F-statistic: 94.47 on 15 and 10425 DF,  p-value: < 2.2e-16
