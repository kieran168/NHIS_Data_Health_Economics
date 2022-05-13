NHIS Data
================
Kieran Yuen

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
      - [Summary Statistics: Overall](#summary-statistics-overall)
      - [Summary Statistics: Prof.De’s](#summary-statistics-profdes)
  - [Logistic Regression](#logistic-regression)
      - [Model \#1: Demographic information
        only](#model-1-demographic-information-only)
      - [Model \#2: Demographic + Health
        Status](#model-2-demographic--health-status)
      - [Model \#2a: Demographic + Health Status (with only “Other” as
        Race)](#model-2a-demographic--health-status-with-only-other-as-race)
      - [Model \#2b: Demographic + Health Status (with only
        “Non\_Hispanic\_White” as
        Race)](#model-2b-demographic--health-status-with-only-non_hispanic_white-as-race)

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
  
  filter(Covid_Test =="1"| #Yes_Covid_Test
           Covid_Test == "2") %>% #No_Covid_Test 
  
  mutate(Covid_Test = recode(Covid_Test,
                            '1' = 1,#Yes_Covid_Test
                            '2' = 0)) %>% #No_Covid_Test 
                              
  mutate(Education = recode(Education,
                            '0' = 0,#Below_Bachelors
                            '1' = 0,#Below_Bachelors
                            '2' = 0,#Below_Bachelors
                            '3' = 0,#Below_Bachelors
                            '4' = 0,#Below_Bachelors
                            '5' = 0,#Below_Bachelors
                            '6' = 0,#Below_Bachelors
                            '7' = 0,#Below_Bachelors
                            '8' = 1,#Bachelors_Or_Above
                            '9' = 1,#Bachelors_Or_Above
                            '10' = 1,#Bachelors_Or_Above
                            '11' = 1,#Bachelors_Or_Above
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
                            
                            ifelse(Hisp == 'Non_hispanic' & Race == 'White only', 'Non-Hispanic White',
                                   
                                   ifelse(Hisp == 'Non_hispanic' & Race == 'AIAN and any other group'|
                                            Hisp == 'Non_hispanic' & Race == 'AIAN only'|
                                            Hisp == 'Non_hispanic' & Race == 'Asian only'|
                                            Hisp == 'Non_hispanic' & Race == 'Other single and multiple races', 
                                          'Other',
                                          
                                          'Hispanic')))) %>%
                            
  mutate(Race_Hisp = recode(Race_Hisp,
                            "Non-Hispanic Black" = 0, #Other
                            "Non-Hispanic White" = 1, #Non_Hispanic_White
                            "Other" = 0,  #Other
                            "Hispanic" = 0)) %>%  #Other
  
  mutate(Sex = recode(Sex,
                            '1' = 1, #Male
                            '2' = 0, #Female
                            '7' = NA_real_,
                            '9' = NA_real_)) %>%
  
  mutate(Marital_Status = recode(Marital_Status,
                            '1' = 1, #Married
                            '2' = 1, #Married
                            '3' = 0, #Not_Married
                            '4' = 0, #Not_Married
                            '5' = 0, #Not_Married
                            '6' = 0, #Not_Married
                            '7' = 0, #Not_Married
                            '8' = 0, #Not_Married
                            '9' = 0)) %>%  #Not_Married
  
  mutate(Health_Status = recode(Health_Status,
                            '1' = 1, #High_Health_Status
                            '2' = 1, #High_Health_Status
                            '3' = 1, #High_Health_Status
                            '4' = 0, #Low_Health_Status
                            '5' = 0, #Low_Health_Status
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
NHIS_data$Education <- as.factor(NHIS_data$Education)

NHIS_data$Race_Hisp <- as.factor(NHIS_data$Race_Hisp)

NHIS_data$Age <- as.numeric(NHIS_data$Age)

NHIS_data$Sex <- as.factor(NHIS_data$Sex)

NHIS_data$Marital_Status <- as.factor(NHIS_data$Marital_Status)

NHIS_data$Health_Status <- as.factor(NHIS_data$Health_Status)

NHIS_data$Covid_Test <- as.factor(NHIS_data$Covid_Test)

NHIS_data$Age_Group <- as.factor(NHIS_data$Age_Group)
```

# Summary tables to check groupings are accurate

``` r
table(NHIS_data$Education)
table(NHIS_data$Hisp)
table(NHIS_data$Race)
table(NHIS_data$Race_Hisp)
table(NHIS_data$Sex)
table(NHIS_data$Marital_Status)
table(NHIS_data$Health_Status)
table(NHIS_data$Covid_Test)
table(NHIS_data$Age_Group)
```

# Summary Statistics

## Summary Statistics: COVID Test

``` r
library(gmodels)

CrossTable(NHIS_data$Education, NHIS_data$Covid_Test, prop.chisq=FALSE, prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)

CrossTable(NHIS_data$Race_Hisp, NHIS_data$Covid_Test, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)

CrossTable(NHIS_data$Sex,NHIS_data$Covid_Test, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)

CrossTable(NHIS_data$Marital_Status,NHIS_data$Covid_Test, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)

CrossTable(NHIS_data$Health_Status,NHIS_data$Covid_Test, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)

CrossTable(NHIS_data$Age_Group,NHIS_data$Covid_Test, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)
```

## Summary Statistics: Health Status

``` r
library(gmodels)

CrossTable(NHIS_data$Covid_Test,NHIS_data$Health_Status, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)

CrossTable(NHIS_data$Marital_Status,NHIS_data$Health_Status, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)

CrossTable(NHIS_data$Sex,NHIS_data$Health_Status, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)

CrossTable(NHIS_data$Age_Group,NHIS_data$Health_Status, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)

CrossTable(NHIS_data$Race_Hisp, NHIS_data$Health_Status, prop.chisq=FALSE,  prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)

CrossTable(NHIS_data$Education, NHIS_data$Health_Status, prop.chisq=FALSE, prop.t=FALSE, prop.r=TRUE, prop.c=FALSE)
```

## Summary Statistics: Overall

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
describeBy(NHIS_data, group = NHIS_data$Health_Status)
describeBy(NHIS_data)
```

    ## Warning in describeBy(NHIS_data): no grouping variable requested

## Summary Statistics: Prof.De’s

``` r
library(tidyverse)
#Health Status
NHIS_data %>% 
  count(Health_Status) %>% 
  mutate(percent = n / sum(n))

sd(as.numeric(as.factor(NHIS_data$Health_Status==1)))
sd(as.numeric(as.factor(NHIS_data$Health_Status==0)))


#Education
NHIS_data %>% 
  count(Education) %>% 
  mutate(percent = n / sum(n))

sd(as.numeric(as.factor(NHIS_data$Education==1)))
sd(as.numeric(as.factor(NHIS_data$Education==0)))

#Race
NHIS_data %>% 
  count(Race_Hisp) %>% 
  mutate(percent = n / sum(n))

sd(as.numeric(as.factor(NHIS_data$Race_Hisp==1)))
sd(as.numeric(as.factor(NHIS_data$Race_Hisp==0)))

#Sex
NHIS_data %>% 
  count(Sex) %>% 
  mutate(percent = n / sum(n))

sd(as.numeric(as.factor(NHIS_data$Sex==1)))
sd(as.numeric(as.factor(NHIS_data$Sex==0)))

#Marital Status
NHIS_data %>% 
  count(Marital_Status) %>% 
  mutate(percent = n / sum(n))

sd(as.numeric(as.factor(NHIS_data$Marital_Status==1)))
sd(as.numeric(as.factor(NHIS_data$Marital_Status==0)))

#Covid Test
NHIS_data %>% 
  count(Covid_Test) %>% 
  mutate(percent = n / sum(n))

sd(as.numeric(as.factor(NHIS_data$Covid_Test==1)))
sd(as.numeric(as.factor(NHIS_data$Covid_Test==0)))
```

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
    ## -1.0567  -0.8928  -0.8168   1.4223   1.6981  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     -0.451346   0.099432  -4.539 5.65e-06 ***
    ## Education1       0.218999   0.043300   5.058 4.24e-07 ***
    ## Age             -0.002162   0.001920  -1.126  0.26009    
    ## Sex1            -0.238831   0.042474  -5.623 1.88e-08 ***
    ## Marital_Status1 -0.140654   0.042816  -3.285  0.00102 ** 
    ## Race_Hisp1      -0.202609   0.045223  -4.480 7.46e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13032  on 10440  degrees of freedom
    ## Residual deviance: 12942  on 10435  degrees of freedom
    ## AIC: 12954
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

    ##                      OR   2.5 % 97.5 %         p    
    ## (Intercept)     0.63677 0.52388 0.7736 5.645e-06 ***
    ## Education1      1.24483 1.14355 1.3551 4.242e-07 ***
    ## Age             0.99784 0.99409 1.0016  0.260088    
    ## Sex1            0.78755 0.72459 0.8559 1.877e-08 ***
    ## Marital_Status1 0.86879 0.79885 0.9448  0.001019 ** 
    ## Race_Hisp1      0.81660 0.74740 0.8924 7.457e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
nobs(Model_1)
```

    ## [1] 10441

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
    ## -1.1373  -0.8919  -0.8114   1.4268   1.7189  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     -0.228762   0.117724  -1.943 0.051992 .  
    ## Education1       0.243361   0.043923   5.541 3.01e-08 ***
    ## Age             -0.003136   0.001941  -1.615 0.106269    
    ## Sex1            -0.235481   0.042508  -5.540 3.03e-08 ***
    ## Marital_Status1 -0.126684   0.043031  -2.944 0.003240 ** 
    ## Race_Hisp1      -0.195755   0.045291  -4.322 1.54e-05 ***
    ## Health_Status1  -0.230922   0.065220  -3.541 0.000399 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13032  on 10440  degrees of freedom
    ## Residual deviance: 12930  on 10434  degrees of freedom
    ## AIC: 12944
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
library(questionr)
odds.ratio(Model_2) #This tells us the "Odds Ratios"
```

    ## Waiting for profiling to be done...

    ##                      OR   2.5 % 97.5 %         p    
    ## (Intercept)     0.79552 0.63144 1.0018 0.0519917 .  
    ## Education1      1.27553 1.17034 1.3902 3.013e-08 ***
    ## Age             0.99687 0.99308 1.0007 0.1062693    
    ## Sex1            0.79019 0.72697 0.8588 3.031e-08 ***
    ## Marital_Status1 0.88101 0.80975 0.9585 0.0032396 ** 
    ## Race_Hisp1      0.82221 0.75244 0.8986 1.545e-05 ***
    ## Health_Status1  0.79380 0.69887 0.9025 0.0003992 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
nobs(Model_2)
```

    ## [1] 10441

## Model \#2a: Demographic + Health Status (with only “Other” as Race)

``` r
Model_2a <- glm(Covid_Test ~ Education
               + Age 
               + Sex 
               + Marital_Status
               + Health_Status,
               data = subset(NHIS_data,
                             Race_Hisp == 0),
               family = binomial)

summary(Model_2a)
```

    ## 
    ## Call:
    ## glm(formula = Covid_Test ~ Education + Age + Sex + Marital_Status + 
    ##     Health_Status, family = binomial, data = subset(NHIS_data, 
    ##     Race_Hisp == 0))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.1456  -0.9222  -0.8746   1.3863   1.6291  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     -0.123456   0.194523  -0.635 0.525651    
    ## Education1       0.097099   0.076537   1.269 0.204566    
    ## Age             -0.001401   0.003422  -0.409 0.682254    
    ## Sex1            -0.242189   0.072530  -3.339 0.000840 ***
    ## Marital_Status1 -0.251485   0.073521  -3.421 0.000625 ***
    ## Health_Status1  -0.311833   0.102393  -3.045 0.002323 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 4444.9  on 3444  degrees of freedom
    ## Residual deviance: 4410.2  on 3439  degrees of freedom
    ## AIC: 4422.2
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
library(questionr)
odds.ratio(Model_2a) #This tells us the "Odds Ratios"
```

    ## Waiting for profiling to be done...

    ##                      OR   2.5 % 97.5 %         p    
    ## (Intercept)     0.88386 0.60349 1.2940 0.5256507    
    ## Education1      1.10197 0.94831 1.2802 0.2045657    
    ## Age             0.99860 0.99192 1.0053 0.6822538    
    ## Sex1            0.78491 0.68074 0.9046 0.0008404 ***
    ## Marital_Status1 0.77765 0.67313 0.8980 0.0006248 ***
    ## Health_Status1  0.73210 0.59932 0.8955 0.0023233 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
nobs(Model_2a)
```

    ## [1] 3445

## Model \#2b: Demographic + Health Status (with only “Non\_Hispanic\_White” as Race)

``` r
Model_2b <- glm(Covid_Test ~ Education
               + Age 
               + Sex 
               + Marital_Status
               + Health_Status,
               data = subset(NHIS_data,
                             Race_Hisp == 1),
               family = binomial)

summary(Model_2b)
```

    ## 
    ## Call:
    ## glm(formula = Covid_Test ~ Education + Age + Sex + Marital_Status + 
    ##     Health_Status, family = binomial, data = subset(NHIS_data, 
    ##     Race_Hisp == 1))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.0448  -0.8624  -0.7971   1.4366   1.7213  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     -0.533456   0.150438  -3.546 0.000391 ***
    ## Education1       0.317617   0.053877   5.895 3.74e-09 ***
    ## Age             -0.003725   0.002358  -1.579 0.114226    
    ## Sex1            -0.225017   0.052566  -4.281 1.86e-05 ***
    ## Marital_Status1 -0.059919   0.053300  -1.124 0.260935    
    ## Health_Status1  -0.166677   0.085229  -1.956 0.050509 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 8566.1  on 6995  degrees of freedom
    ## Residual deviance: 8504.1  on 6990  degrees of freedom
    ## AIC: 8516.1
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
library(questionr)
odds.ratio(Model_2b) #This tells us the "Odds Ratios"
```

    ## Waiting for profiling to be done...

    ##                      OR   2.5 % 97.5 %         p    
    ## (Intercept)     0.58657 0.43648 0.7872 0.0003911 ***
    ## Education1      1.37385 1.23628 1.5270 3.741e-09 ***
    ## Age             0.99628 0.99169 1.0009 0.1142264    
    ## Sex1            0.79850 0.72026 0.8851 1.864e-05 ***
    ## Marital_Status1 0.94184 0.84846 1.0456 0.2609348    
    ## Health_Status1  0.84647 0.71700 1.0015 0.0505089 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
nobs(Model_2b)
```

    ## [1] 6996
