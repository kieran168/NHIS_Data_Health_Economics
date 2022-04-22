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
  - [Regression Models](#regression-models)
      - [Model \#1: Demographic information
        only](#model-1-demographic-information-only)
      - [Model \#2: Demo + Health Status](#model-2-demo--health-status)
      - [Model \#3: Demo + Health Status +
        Interaction](#model-3-demo--health-status--interaction)

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
  
  na.omit() %>% 
  
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
                            '1' = "Live with someone",
                            '2' = "Live alone",
                            '3' = "Live alone",
                            '4' = "Live alone",
                            '5' = "Live alone",
                            '6' = "Live alone",
                            '7' = "Live alone",
                            '8' = "Live with someone",
                            '9' = "Live alone")) %>%
  
  mutate(Health_Status = recode(Health_Status,
                            '1' = 5,
                            '2' = 4,
                            '3' = 3,
                            '4' = 2,
                            '5' = 1,
                            '7' = NA_real_,
                            '9' = NA_real_,
                            ))
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
```

# Summary tables to check groupings are accurate

``` r
table(NHIS_data$Education)
```

    ## 
    ##    1    2    3    4    5    6    7 
    ##  692 2232 1470 1367 2797 1425  464

``` r
table(NHIS_data$Hisp)
```

    ## 
    ##     Hispanic Non_hispanic 
    ##         1438         9054

``` r
table(NHIS_data$Race)
```

    ## 
    ##        AIAN and any other group                       AIAN only 
    ##                             110                              95 
    ##                      Asian only     Black/African American only 
    ##                             648                            1146 
    ##                      Don't know                 Not Ascertained 
    ##                               4                             538 
    ## Other single and multiple races                         Refused 
    ##                             140                               4 
    ##                      White only 
    ##                            7807

``` r
table(NHIS_data$Race_Hisp)
```

    ## 
    ## Non-Hispanic White Non-Hispanic Black           Hispanic              Other 
    ##               7021               1112               1438                921

``` r
table(NHIS_data$Sex)
```

    ## 
    ## Female   Male 
    ##   5493   4998

``` r
table(NHIS_data$Marital_Status)
```

    ## 
    ##        Live alone Live with someone 
    ##              4430              6062

``` r
table(NHIS_data$Health_Status)
```

    ## 
    ##    1    2    3    4    5 
    ##  286 1001 2831 3749 2620

``` r
table(NHIS_data$Covid_Test)
```

    ## 
    ##    0    1 
    ## 7173 3319

``` r
library(gmodels)
CrossTable(NHIS_data$Sex, NHIS_data$Covid_Test, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## | Chi-square contribution |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |         N / Table Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  10491 
    ## 
    ##  
    ##               | NHIS_data$Covid_Test 
    ## NHIS_data$Sex |         0 |         1 | Row Total | 
    ## --------------|-----------|-----------|-----------|
    ##        Female |      3617 |      1876 |      5493 | 
    ##               |     5.086 |    10.990 |           | 
    ##               |     0.658 |     0.342 |     0.524 | 
    ##               |     0.504 |     0.565 |           | 
    ##               |     0.345 |     0.179 |           | 
    ## --------------|-----------|-----------|-----------|
    ##          Male |      3555 |      1443 |      4998 | 
    ##               |     5.590 |    12.079 |           | 
    ##               |     0.711 |     0.289 |     0.476 | 
    ##               |     0.496 |     0.435 |           | 
    ##               |     0.339 |     0.138 |           | 
    ## --------------|-----------|-----------|-----------|
    ##  Column Total |      7172 |      3319 |     10491 | 
    ##               |     0.684 |     0.316 |           | 
    ## --------------|-----------|-----------|-----------|
    ## 
    ## 

``` r
library(gmodels)
CrossTable(NHIS_data$Race_Hisp, NHIS_data$Covid_Test, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## | Chi-square contribution |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |         N / Table Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  10492 
    ## 
    ##  
    ##                     | NHIS_data$Covid_Test 
    ## NHIS_data$Race_Hisp |         0 |         1 | Row Total | 
    ## --------------------|-----------|-----------|-----------|
    ##  Non-Hispanic White |      4903 |      2118 |      7021 | 
    ##                     |     2.210 |     4.776 |           | 
    ##                     |     0.698 |     0.302 |     0.669 | 
    ##                     |     0.684 |     0.638 |           | 
    ##                     |     0.467 |     0.202 |           | 
    ## --------------------|-----------|-----------|-----------|
    ##  Non-Hispanic Black |       683 |       429 |      1112 | 
    ##                     |     7.846 |    16.958 |           | 
    ##                     |     0.614 |     0.386 |     0.106 | 
    ##                     |     0.095 |     0.129 |           | 
    ##                     |     0.065 |     0.041 |           | 
    ## --------------------|-----------|-----------|-----------|
    ##            Hispanic |       937 |       501 |      1438 | 
    ##                     |     2.163 |     4.674 |           | 
    ##                     |     0.652 |     0.348 |     0.137 | 
    ##                     |     0.131 |     0.151 |           | 
    ##                     |     0.089 |     0.048 |           | 
    ## --------------------|-----------|-----------|-----------|
    ##               Other |       650 |       271 |       921 | 
    ##                     |     0.657 |     1.421 |           | 
    ##                     |     0.706 |     0.294 |     0.088 | 
    ##                     |     0.091 |     0.082 |           | 
    ##                     |     0.062 |     0.026 |           | 
    ## --------------------|-----------|-----------|-----------|
    ##        Column Total |      7173 |      3319 |     10492 | 
    ##                     |     0.684 |     0.316 |           | 
    ## --------------------|-----------|-----------|-----------|
    ## 
    ## 

# Regression Models

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
    ## -1.1874  -0.8897  -0.8094   1.4178   1.7973  
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                     -0.951126   0.120137  -7.917 2.43e-15 ***
    ## Education                        0.094566   0.013264   7.130 1.01e-12 ***
    ## Age                             -0.002536   0.001926  -1.317   0.1880    
    ## SexMale                         -0.222930   0.042606  -5.232 1.67e-07 ***
    ## Marital_StatusLive with someone -0.086324   0.043694  -1.976   0.0482 *  
    ## Race_HispNon-Hispanic Black      0.388773   0.069122   5.624 1.86e-08 ***
    ## Race_HispHispanic                0.299673   0.063667   4.707 2.52e-06 ***
    ## Race_HispOther                  -0.080602   0.077835  -1.036   0.3004    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13038  on 10445  degrees of freedom
    ## Residual deviance: 12908  on 10438  degrees of freedom
    ##   (46 observations deleted due to missingness)
    ## AIC: 12924
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
library(questionr)
```

    ## Warning: package 'questionr' was built under R version 4.0.5

``` r
odds.ratio(Model_1) #This tells us the "Odds Ratios"
```

    ## Waiting for profiling to be done...

    ##                                      OR   2.5 % 97.5 %         p    
    ## (Intercept)                     0.38631 0.30513 0.4887 2.432e-15 ***
    ## Education                       1.09918 1.07101 1.1282 1.005e-12 ***
    ## Age                             0.99747 0.99371 1.0012    0.1880    
    ## SexMale                         0.80017 0.73602 0.8698 1.674e-07 ***
    ## Marital_StatusLive with someone 0.91730 0.84205 0.9994    0.0482 *  
    ## Race_HispNon-Hispanic Black     1.47517 1.28770 1.6885 1.861e-08 ***
    ## Race_HispHispanic               1.34942 1.19063 1.5282 2.516e-06 ***
    ## Race_HispOther                  0.92256 0.79103 1.0734    0.3004    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
library(oddsratio)
or_glm(data = NHIS_data, model = Model_1, 
       incr = list(Education = 1, Age = 1))
```

    ##                         predictor oddsratio ci_low (2.5) ci_high (97.5)
    ## 1                       Education     1.099        1.071          1.128
    ## 2                             Age     0.997        0.994          1.001
    ## 3                         SexMale     0.800        0.736          0.870
    ## 4 Marital_StatusLive with someone     0.917        0.842          0.999
    ## 5     Race_HispNon-Hispanic Black     1.475        1.288          1.689
    ## 6               Race_HispHispanic     1.349        1.191          1.528
    ## 7                  Race_HispOther     0.923        0.791          1.073
    ##            increment
    ## 1                  1
    ## 2                  1
    ## 3 Indicator variable
    ## 4 Indicator variable
    ## 5 Indicator variable
    ## 6 Indicator variable
    ## 7 Indicator variable

``` r
or_glm(data = NHIS_data, model = Model_1,
       incr = list(Education = 2, Age = 1))
```

    ##                         predictor oddsratio ci_low (2.5) ci_high (97.5)
    ## 1                       Education     1.208        1.147          1.273
    ## 2                             Age     0.997        0.994          1.001
    ## 3                         SexMale     0.800        0.736          0.870
    ## 4 Marital_StatusLive with someone     0.917        0.842          0.999
    ## 5     Race_HispNon-Hispanic Black     1.475        1.288          1.689
    ## 6               Race_HispHispanic     1.349        1.191          1.528
    ## 7                  Race_HispOther     0.923        0.791          1.073
    ##            increment
    ## 1                  2
    ## 2                  1
    ## 3 Indicator variable
    ## 4 Indicator variable
    ## 5 Indicator variable
    ## 6 Indicator variable
    ## 7 Indicator variable

``` r
or_glm(data = NHIS_data, model = Model_1,
       incr = list(Education = 3, Age = 1))
```

    ##                         predictor oddsratio ci_low (2.5) ci_high (97.5)
    ## 1                       Education     1.328        1.228          1.436
    ## 2                             Age     0.997        0.994          1.001
    ## 3                         SexMale     0.800        0.736          0.870
    ## 4 Marital_StatusLive with someone     0.917        0.842          0.999
    ## 5     Race_HispNon-Hispanic Black     1.475        1.288          1.689
    ## 6               Race_HispHispanic     1.349        1.191          1.528
    ## 7                  Race_HispOther     0.923        0.791          1.073
    ##            increment
    ## 1                  3
    ## 2                  1
    ## 3 Indicator variable
    ## 4 Indicator variable
    ## 5 Indicator variable
    ## 6 Indicator variable
    ## 7 Indicator variable

``` r
or_glm(data = NHIS_data, model = Model_1,
       incr = list(Education = 4, Age = 1))
```

    ##                         predictor oddsratio ci_low (2.5) ci_high (97.5)
    ## 1                       Education     1.460        1.316          1.620
    ## 2                             Age     0.997        0.994          1.001
    ## 3                         SexMale     0.800        0.736          0.870
    ## 4 Marital_StatusLive with someone     0.917        0.842          0.999
    ## 5     Race_HispNon-Hispanic Black     1.475        1.288          1.689
    ## 6               Race_HispHispanic     1.349        1.191          1.528
    ## 7                  Race_HispOther     0.923        0.791          1.073
    ##            increment
    ## 1                  4
    ## 2                  1
    ## 3 Indicator variable
    ## 4 Indicator variable
    ## 5 Indicator variable
    ## 6 Indicator variable
    ## 7 Indicator variable

``` r
or_glm(data = NHIS_data, model = Model_1,
       incr = list(Education = 5, Age = 1))
```

    ##                         predictor oddsratio ci_low (2.5) ci_high (97.5)
    ## 1                       Education     1.605        1.409          1.828
    ## 2                             Age     0.997        0.994          1.001
    ## 3                         SexMale     0.800        0.736          0.870
    ## 4 Marital_StatusLive with someone     0.917        0.842          0.999
    ## 5     Race_HispNon-Hispanic Black     1.475        1.288          1.689
    ## 6               Race_HispHispanic     1.349        1.191          1.528
    ## 7                  Race_HispOther     0.923        0.791          1.073
    ##            increment
    ## 1                  5
    ## 2                  1
    ## 3 Indicator variable
    ## 4 Indicator variable
    ## 5 Indicator variable
    ## 6 Indicator variable
    ## 7 Indicator variable

``` r
or_glm(data = NHIS_data, model = Model_1,
       incr = list(Education = 6, Age = 1))
```

    ##                         predictor oddsratio ci_low (2.5) ci_high (97.5)
    ## 1                       Education     1.764        1.509          2.062
    ## 2                             Age     0.997        0.994          1.001
    ## 3                         SexMale     0.800        0.736          0.870
    ## 4 Marital_StatusLive with someone     0.917        0.842          0.999
    ## 5     Race_HispNon-Hispanic Black     1.475        1.288          1.689
    ## 6               Race_HispHispanic     1.349        1.191          1.528
    ## 7                  Race_HispOther     0.923        0.791          1.073
    ##            increment
    ## 1                  6
    ## 2                  1
    ## 3 Indicator variable
    ## 4 Indicator variable
    ## 5 Indicator variable
    ## 6 Indicator variable
    ## 7 Indicator variable

``` r
or_glm(data = NHIS_data, model = Model_1,
       incr = list(Education = 7, Age = 1))
```

    ##                         predictor oddsratio ci_low (2.5) ci_high (97.5)
    ## 1                       Education     1.939        1.616          2.326
    ## 2                             Age     0.997        0.994          1.001
    ## 3                         SexMale     0.800        0.736          0.870
    ## 4 Marital_StatusLive with someone     0.917        0.842          0.999
    ## 5     Race_HispNon-Hispanic Black     1.475        1.288          1.689
    ## 6               Race_HispHispanic     1.349        1.191          1.528
    ## 7                  Race_HispOther     0.923        0.791          1.073
    ##            increment
    ## 1                  7
    ## 2                  1
    ## 3 Indicator variable
    ## 4 Indicator variable
    ## 5 Indicator variable
    ## 6 Indicator variable
    ## 7 Indicator variable

## Model \#2: Demo + Health Status

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
    ## -1.2598  -0.8889  -0.8025   1.4070   1.8452  
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                     -0.548212   0.145203  -3.775  0.00016 ***
    ## Education                        0.111471   0.013724   8.122 4.58e-16 ***
    ## Age                             -0.004248   0.001961  -2.166  0.03034 *  
    ## SexMale                         -0.215697   0.042679  -5.054 4.33e-07 ***
    ## Marital_StatusLive with someone -0.068634   0.043913  -1.563  0.11806    
    ## Race_HispNon-Hispanic Black      0.374789   0.069263   5.411 6.26e-08 ***
    ## Race_HispHispanic                0.297328   0.063769   4.663 3.12e-06 ***
    ## Race_HispOther                  -0.087799   0.077916  -1.127  0.25981    
    ## Health_Status                   -0.108427   0.021856  -4.961 7.01e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13032  on 10440  degrees of freedom
    ## Residual deviance: 12879  on 10432  degrees of freedom
    ##   (51 observations deleted due to missingness)
    ## AIC: 12897
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
library(questionr)
odds.ratio(Model_2) #This tells us the "Odds Ratios"
```

    ## Waiting for profiling to be done...

    ##                                      OR   2.5 % 97.5 %         p    
    ## (Intercept)                     0.57798 0.43470 0.7681 0.0001597 ***
    ## Education                       1.11792 1.08829 1.1484 4.578e-16 ***
    ## Age                             0.99576 0.99194 0.9996 0.0303413 *  
    ## SexMale                         0.80598 0.74126 0.8763 4.328e-07 ***
    ## Marital_StatusLive with someone 0.93367 0.85672 1.0177 0.1180604    
    ## Race_HispNon-Hispanic Black     1.45468 1.26946 1.6656 6.265e-08 ***
    ## Race_HispHispanic               1.34626 1.18760 1.5249 3.122e-06 ***
    ## Race_HispOther                  0.91594 0.78524 1.0658 0.2598067    
    ## Health_Status                   0.89724 0.85962 0.9365 7.014e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
library(oddsratio)
or_glm(data = NHIS_data, model = Model_2, 
       incr = list(Education = 1, Age = 1, Health_Status = 1))
```

    ##                         predictor oddsratio ci_low (2.5) ci_high (97.5)
    ## 1                       Education     1.118        1.088          1.148
    ## 2                             Age     0.996        0.992          1.000
    ## 3                         SexMale     0.806        0.741          0.876
    ## 4 Marital_StatusLive with someone     0.934        0.857          1.018
    ## 5     Race_HispNon-Hispanic Black     1.455        1.269          1.666
    ## 6               Race_HispHispanic     1.346        1.188          1.525
    ## 7                  Race_HispOther     0.916        0.785          1.066
    ## 8                   Health_Status     0.897        0.860          0.937
    ##            increment
    ## 1                  1
    ## 2                  1
    ## 3 Indicator variable
    ## 4 Indicator variable
    ## 5 Indicator variable
    ## 6 Indicator variable
    ## 7 Indicator variable
    ## 8                  1

``` r
or_glm(data = NHIS_data, model = Model_2,
       incr = list(Education = 1, Age = 1, Health_Status = 2))
```

    ##                         predictor oddsratio ci_low (2.5) ci_high (97.5)
    ## 1                       Education     1.118        1.088          1.148
    ## 2                             Age     0.996        0.992          1.000
    ## 3                         SexMale     0.806        0.741          0.876
    ## 4 Marital_StatusLive with someone     0.934        0.857          1.018
    ## 5     Race_HispNon-Hispanic Black     1.455        1.269          1.666
    ## 6               Race_HispHispanic     1.346        1.188          1.525
    ## 7                  Race_HispOther     0.916        0.785          1.066
    ## 8                   Health_Status     0.805        0.739          0.877
    ##            increment
    ## 1                  1
    ## 2                  1
    ## 3 Indicator variable
    ## 4 Indicator variable
    ## 5 Indicator variable
    ## 6 Indicator variable
    ## 7 Indicator variable
    ## 8                  2

``` r
or_glm(data = NHIS_data, model = Model_2,
       incr = list(Education = 1, Age = 1, Health_Status = 3))
```

    ##                         predictor oddsratio ci_low (2.5) ci_high (97.5)
    ## 1                       Education     1.118        1.088          1.148
    ## 2                             Age     0.996        0.992          1.000
    ## 3                         SexMale     0.806        0.741          0.876
    ## 4 Marital_StatusLive with someone     0.934        0.857          1.018
    ## 5     Race_HispNon-Hispanic Black     1.455        1.269          1.666
    ## 6               Race_HispHispanic     1.346        1.188          1.525
    ## 7                  Race_HispOther     0.916        0.785          1.066
    ## 8                   Health_Status     0.722        0.635          0.821
    ##            increment
    ## 1                  1
    ## 2                  1
    ## 3 Indicator variable
    ## 4 Indicator variable
    ## 5 Indicator variable
    ## 6 Indicator variable
    ## 7 Indicator variable
    ## 8                  3

``` r
or_glm(data = NHIS_data, model = Model_2,
       incr = list(Education = 1, Age = 1, Health_Status = 4))
```

    ##                         predictor oddsratio ci_low (2.5) ci_high (97.5)
    ## 1                       Education     1.118        1.088          1.148
    ## 2                             Age     0.996        0.992          1.000
    ## 3                         SexMale     0.806        0.741          0.876
    ## 4 Marital_StatusLive with someone     0.934        0.857          1.018
    ## 5     Race_HispNon-Hispanic Black     1.455        1.269          1.666
    ## 6               Race_HispHispanic     1.346        1.188          1.525
    ## 7                  Race_HispOther     0.916        0.785          1.066
    ## 8                   Health_Status     0.648        0.546          0.769
    ##            increment
    ## 1                  1
    ## 2                  1
    ## 3 Indicator variable
    ## 4 Indicator variable
    ## 5 Indicator variable
    ## 6 Indicator variable
    ## 7 Indicator variable
    ## 8                  4

``` r
or_glm(data = NHIS_data, model = Model_2,
       incr = list(Education = 1, Age = 1, Health_Status = 5))
```

    ##                         predictor oddsratio ci_low (2.5) ci_high (97.5)
    ## 1                       Education     1.118        1.088          1.148
    ## 2                             Age     0.996        0.992          1.000
    ## 3                         SexMale     0.806        0.741          0.876
    ## 4 Marital_StatusLive with someone     0.934        0.857          1.018
    ## 5     Race_HispNon-Hispanic Black     1.455        1.269          1.666
    ## 6               Race_HispHispanic     1.346        1.188          1.525
    ## 7                  Race_HispOther     0.916        0.785          1.066
    ## 8                   Health_Status     0.582        0.469          0.720
    ##            increment
    ## 1                  1
    ## 2                  1
    ## 3 Indicator variable
    ## 4 Indicator variable
    ## 5 Indicator variable
    ## 6 Indicator variable
    ## 7 Indicator variable
    ## 8                  5

## Model \#3: Demo + Health Status + Interaction

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
    ## -1.2027  -0.8912  -0.8034   1.4076   1.8879  
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                     -0.224730   0.219683  -1.023   0.3063    
    ## Education                        0.021305   0.048071   0.443   0.6576    
    ## Age                             -0.004350   0.001963  -2.216   0.0267 *  
    ## SexMale                         -0.215349   0.042685  -5.045 4.53e-07 ***
    ## Marital_StatusLive with someone -0.066905   0.043935  -1.523   0.1278    
    ## Race_HispNon-Hispanic Black      0.375599   0.069270   5.422 5.89e-08 ***
    ## Race_HispHispanic                0.296652   0.063798   4.650 3.32e-06 ***
    ## Race_HispOther                  -0.090870   0.077935  -1.166   0.2436    
    ## Health_Status                   -0.198079   0.050770  -3.901 9.56e-05 ***
    ## Education:Health_Status          0.024365   0.012454   1.956   0.0504 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13032  on 10440  degrees of freedom
    ## Residual deviance: 12875  on 10431  degrees of freedom
    ##   (51 observations deleted due to missingness)
    ## AIC: 12895
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
library(questionr)
odds.ratio(Model_3) #This tells us the "Odds Ratios"
```

    ## Waiting for profiling to be done...

    ##                                      OR   2.5 % 97.5 %         p    
    ## (Intercept)                     0.79873 0.51888 1.2278   0.30632    
    ## Education                       1.02153 0.92974 1.1226   0.65762    
    ## Age                             0.99566 0.99184 0.9995   0.02669 *  
    ## SexMale                         0.80626 0.74150 0.8766 4.534e-07 ***
    ## Marital_StatusLive with someone 0.93528 0.85817 1.0195   0.12780    
    ## Race_HispNon-Hispanic Black     1.45586 1.27047 1.6669 5.885e-08 ***
    ## Race_HispHispanic               1.34535 1.18673 1.5240 3.322e-06 ***
    ## Race_HispOther                  0.91314 0.78280 1.0626   0.24363    
    ## Health_Status                   0.82031 0.74262 0.9062 9.560e-05 ***
    ## Education:Health_Status         1.02466 0.99994 1.0500   0.05042 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
