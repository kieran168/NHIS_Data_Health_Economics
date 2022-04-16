NHIS Data
================
Nicole Dodson & Kieran Yuen

``` r
knitr::opts_chunk$set(echo = TRUE)
```

``` r
library(readxl)
student_datav1 <- read_excel("student_datav1.xlsx")
```

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
#This tells us the "Odds Ratios"
exp(coef(Model_1))
```

    ##                     (Intercept)                       Education 
    ##                       0.3863056                       1.0991820 
    ##                             Age                         SexMale 
    ##                       0.9974677                       0.8001712 
    ## Marital_StatusLive with someone     Race_HispNon-Hispanic Black 
    ##                       0.9172967                       1.4751692 
    ##               Race_HispHispanic                  Race_HispOther 
    ##                       1.3494181                       0.9225612

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
#This tells us the "Odds Ratios"
exp(coef(Model_2))
```

    ##                     (Intercept)                       Education 
    ##                       0.5779826                       1.1179214 
    ##                             Age                         SexMale 
    ##                       0.9957614                       0.8059797 
    ## Marital_StatusLive with someone     Race_HispNon-Hispanic Black 
    ##                       0.9336680                       1.4546848 
    ##               Race_HispHispanic                  Race_HispOther 
    ##                       1.3462572                       0.9159450 
    ##                   Health_Status 
    ##                       0.8972446

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
#This tells us the "Odds Ratios"
exp(coef(Model_3))
```

    ##                     (Intercept)                       Education 
    ##                       0.7987316                       1.0215337 
    ##                             Age                         SexMale 
    ##                       0.9956599                       0.8062603 
    ## Marital_StatusLive with someone     Race_HispNon-Hispanic Black 
    ##                       0.9352843                       1.4558632 
    ##               Race_HispHispanic                  Race_HispOther 
    ##                       1.3453470                       0.9131366 
    ##                   Health_Status         Education:Health_Status 
    ##                       0.8203050                       1.0246638
