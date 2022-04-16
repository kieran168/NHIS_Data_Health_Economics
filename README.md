NHIS Data
================
Nicole Dodson & Kieran Yuen

# Regression Models

## Model \#1: Demographic information only

``` r
Model_1 <- glm(Covid_Test ~ Race 
                        + Age 
                        + Sex 
                        + Marital_Status,
                        data = NHIS_data,
                        family = binomial)

summary(Model_1)
```

    ## 
    ## Call:
    ## glm(formula = Covid_Test ~ Race + Age + Sex + Marital_Status, 
    ##     family = binomial, data = NHIS_data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.1266  -0.8882  -0.8165   1.4414   1.7516  
    ## 
    ## Coefficients:
    ##                                               Estimate Std. Error z value
    ## (Intercept)                                  -0.111565   0.226610  -0.492
    ## RaceAIAN only                                -0.038824   0.290241  -0.134
    ## RaceAsian only                               -0.525277   0.216744  -2.423
    ## RaceBlack/African American only               0.005823   0.206089   0.028
    ## RaceOther single and multiple races          -0.106625   0.264767  -0.403
    ## RaceRace Unknown                             -0.211253   0.217400  -0.972
    ## RaceWhite only                               -0.293797   0.198575  -1.480
    ## Age                                          -0.004720   0.001997  -2.363
    ## SexMale                                      -0.232432   0.042574  -5.459
    ## Marital_StatusLiving with a partner           0.052327   0.097812   0.535
    ## Marital_StatusMarital status unknown         -0.152704   0.130546  -1.170
    ## Marital_StatusMarried, spouse is not present  0.019354   0.150142   0.129
    ## Marital_StatusMarried, spouse is present     -0.119746   0.064546  -1.855
    ## Marital_StatusNever married                  -0.046763   0.076784  -0.609
    ## Marital_StatusSeparated                       0.121933   0.160197   0.761
    ## Marital_StatusWidowed                        -0.134487   0.150063  -0.896
    ##                                              Pr(>|z|)    
    ## (Intercept)                                    0.6225    
    ## RaceAIAN only                                  0.8936    
    ## RaceAsian only                                 0.0154 *  
    ## RaceBlack/African American only                0.9775    
    ## RaceOther single and multiple races            0.6872    
    ## RaceRace Unknown                               0.3312    
    ## RaceWhite only                                 0.1390    
    ## Age                                            0.0181 *  
    ## SexMale                                      4.78e-08 ***
    ## Marital_StatusLiving with a partner            0.5927    
    ## Marital_StatusMarital status unknown           0.2421    
    ## Marital_StatusMarried, spouse is not present   0.8974    
    ## Marital_StatusMarried, spouse is present       0.0636 .  
    ## Marital_StatusNever married                    0.5425    
    ## Marital_StatusSeparated                        0.4466    
    ## Marital_StatusWidowed                          0.3701    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13086  on 10482  degrees of freedom
    ## Residual deviance: 12998  on 10467  degrees of freedom
    ##   (9 observations deleted due to missingness)
    ## AIC: 13030
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
#This tells us the "Odds Ratios"
exp(coefficients(Model_1))
```

    ##                                  (Intercept) 
    ##                                    0.8944330 
    ##                                RaceAIAN only 
    ##                                    0.9619203 
    ##                               RaceAsian only 
    ##                                    0.5913916 
    ##              RaceBlack/African American only 
    ##                                    1.0058401 
    ##          RaceOther single and multiple races 
    ##                                    0.8988623 
    ##                             RaceRace Unknown 
    ##                                    0.8095695 
    ##                               RaceWhite only 
    ##                                    0.7454278 
    ##                                          Age 
    ##                                    0.9952909 
    ##                                      SexMale 
    ##                                    0.7926038 
    ##          Marital_StatusLiving with a partner 
    ##                                    1.0537205 
    ##         Marital_StatusMarital status unknown 
    ##                                    0.8583842 
    ## Marital_StatusMarried, spouse is not present 
    ##                                    1.0195422 
    ##     Marital_StatusMarried, spouse is present 
    ##                                    0.8871458 
    ##                  Marital_StatusNever married 
    ##                                    0.9543136 
    ##                      Marital_StatusSeparated 
    ##                                    1.1296780 
    ##                        Marital_StatusWidowed 
    ##                                    0.8741640

``` r
plot(Model_1)
```

![](NHIS_Data_files/figure-gfm/Model%20#1:%20Demographic%20information%20only-1.png)<!-- -->![](NHIS_Data_files/figure-gfm/Model%20#1:%20Demographic%20information%20only-2.png)<!-- -->![](NHIS_Data_files/figure-gfm/Model%20#1:%20Demographic%20information%20only-3.png)<!-- -->![](NHIS_Data_files/figure-gfm/Model%20#1:%20Demographic%20information%20only-4.png)<!-- -->

## Model \#2: Demo + Health Status Grouping

``` r
Model_2 <- glm(Covid_Test ~ Race 
                        + Age 
                        + Sex 
                        + Marital_Status
                        + Health_Status_Group,
                        data = NHIS_data,
                        family = binomial)

summary(Model_2)
```

    ## 
    ## Call:
    ## glm(formula = Covid_Test ~ Race + Age + Sex + Marital_Status + 
    ##     Health_Status_Group, family = binomial, data = NHIS_data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.1656  -0.8879  -0.8166   1.4372   1.7592  
    ## 
    ## Coefficients:
    ##                                               Estimate Std. Error z value
    ## (Intercept)                                   0.029015   0.233942   0.124
    ## RaceAIAN only                                -0.030084   0.290343  -0.104
    ## RaceAsian only                               -0.492065   0.217183  -2.266
    ## RaceBlack/African American only               0.025628   0.206273   0.124
    ## RaceOther single and multiple races          -0.080919   0.265008  -0.305
    ## RaceRace Unknown                             -0.191209   0.217562  -0.879
    ## RaceWhite only                               -0.266937   0.198886  -1.342
    ## Age                                          -0.005379   0.002019  -2.664
    ## SexMale                                      -0.229761   0.042598  -5.394
    ## Marital_StatusLiving with a partner           0.053656   0.097844   0.548
    ## Marital_StatusMarital status unknown         -0.154063   0.130601  -1.180
    ## Marital_StatusMarried, spouse is not present  0.020870   0.150177   0.139
    ## Marital_StatusMarried, spouse is present     -0.109562   0.064691  -1.694
    ## Marital_StatusNever married                  -0.045631   0.076822  -0.594
    ## Marital_StatusSeparated                       0.111214   0.160676   0.692
    ## Marital_StatusWidowed                        -0.162212   0.150866  -1.075
    ## Health_Status_GroupHigh_Health               -0.161395   0.064475  -2.503
    ##                                              Pr(>|z|)    
    ## (Intercept)                                   0.90129    
    ## RaceAIAN only                                 0.91748    
    ## RaceAsian only                                0.02347 *  
    ## RaceBlack/African American only               0.90112    
    ## RaceOther single and multiple races           0.76010    
    ## RaceRace Unknown                              0.37947    
    ## RaceWhite only                                0.17954    
    ## Age                                           0.00773 ** 
    ## SexMale                                       6.9e-08 ***
    ## Marital_StatusLiving with a partner           0.58343    
    ## Marital_StatusMarital status unknown          0.23814    
    ## Marital_StatusMarried, spouse is not present  0.88947    
    ## Marital_StatusMarried, spouse is present      0.09034 .  
    ## Marital_StatusNever married                   0.55253    
    ## Marital_StatusSeparated                       0.48883    
    ## Marital_StatusWidowed                         0.28228    
    ## Health_Status_GroupHigh_Health                0.01231 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13080  on 10477  degrees of freedom
    ## Residual deviance: 12987  on 10461  degrees of freedom
    ##   (14 observations deleted due to missingness)
    ## AIC: 13021
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
#This tells us the "Odds Ratios"
exp(coefficients(Model_2))
```

    ##                                  (Intercept) 
    ##                                    1.0294402 
    ##                                RaceAIAN only 
    ##                                    0.9703645 
    ##                               RaceAsian only 
    ##                                    0.6113626 
    ##              RaceBlack/African American only 
    ##                                    1.0259597 
    ##          RaceOther single and multiple races 
    ##                                    0.9222684 
    ##                             RaceRace Unknown 
    ##                                    0.8259596 
    ##                               RaceWhite only 
    ##                                    0.7657216 
    ##                                          Age 
    ##                                    0.9946355 
    ##                                      SexMale 
    ##                                    0.7947239 
    ##          Marital_StatusLiving with a partner 
    ##                                    1.0551214 
    ##         Marital_StatusMarital status unknown 
    ##                                    0.8572176 
    ## Marital_StatusMarried, spouse is not present 
    ##                                    1.0210892 
    ##     Marital_StatusMarried, spouse is present 
    ##                                    0.8962269 
    ##                  Marital_StatusNever married 
    ##                                    0.9553945 
    ##                      Marital_StatusSeparated 
    ##                                    1.1176344 
    ##                        Marital_StatusWidowed 
    ##                                    0.8502605 
    ##               Health_Status_GroupHigh_Health 
    ##                                    0.8509562

``` r
plot(Model_2)
```

![](NHIS_Data_files/figure-gfm/Model%20#2:%20Demo%20+%20Health%20Status%20Grouping-1.png)<!-- -->![](NHIS_Data_files/figure-gfm/Model%20#2:%20Demo%20+%20Health%20Status%20Grouping-2.png)<!-- -->![](NHIS_Data_files/figure-gfm/Model%20#2:%20Demo%20+%20Health%20Status%20Grouping-3.png)<!-- -->![](NHIS_Data_files/figure-gfm/Model%20#2:%20Demo%20+%20Health%20Status%20Grouping-4.png)<!-- -->

## Model \#3: Demo + Health Status Grouping + Interaction

``` r
Model_3 <- glm(Covid_Test ~ Race 
                        + Age 
                        + Sex 
                        + Marital_Status
                        + Health_Status_Group
                        + Education_Level*Health_Status_Group,
                        data = NHIS_data,
                        family = binomial)

summary(Model_3)
```

    ## 
    ## Call:
    ## glm(formula = Covid_Test ~ Race + Age + Sex + Marital_Status + 
    ##     Health_Status_Group + Education_Level * Health_Status_Group, 
    ##     family = binomial, data = NHIS_data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3066  -0.8909  -0.7965   1.4094   1.9215  
    ## 
    ## Coefficients:
    ##                                                                    Estimate
    ## (Intercept)                                                       -0.308413
    ## RaceAIAN only                                                      0.022144
    ## RaceAsian only                                                    -0.717749
    ## RaceBlack/African American only                                   -0.043217
    ## RaceOther single and multiple races                               -0.148389
    ## RaceRace Unknown                                                  -0.118048
    ## RaceWhite only                                                    -0.362675
    ## Age                                                               -0.004180
    ## SexMale                                                           -0.205174
    ## Marital_StatusLiving with a partner                                0.086444
    ## Marital_StatusMarital status unknown                              -0.181696
    ## Marital_StatusMarried, spouse is not present                      -0.038216
    ## Marital_StatusMarried, spouse is present                          -0.134960
    ## Marital_StatusNever married                                       -0.046751
    ## Marital_StatusSeparated                                            0.194740
    ## Marital_StatusWidowed                                             -0.063380
    ## Health_Status_GroupHigh_Health                                    -0.085755
    ## Education_LevelHigh School                                         0.350188
    ## Education_LevelAssociate's                                         0.325249
    ## Education_LevelBachelor's                                          0.349718
    ## Education_LevelMaster's                                            0.659672
    ## Education_LevelPhD or Professional                                -0.199672
    ## Health_Status_GroupHigh_Health:Education_LevelHigh School         -0.314093
    ## Health_Status_GroupHigh_Health:Education_LevelAssociate's         -0.066563
    ## Health_Status_GroupHigh_Health:Education_LevelBachelor's           0.018677
    ## Health_Status_GroupHigh_Health:Education_LevelMaster's            -0.174931
    ## Health_Status_GroupHigh_Health:Education_LevelPhD or Professional  0.962513
    ##                                                                   Std. Error
    ## (Intercept)                                                         0.300430
    ## RaceAIAN only                                                       0.336059
    ## RaceAsian only                                                      0.243439
    ## RaceBlack/African American only                                     0.233566
    ## RaceOther single and multiple races                                 0.293011
    ## RaceRace Unknown                                                    0.246054
    ## RaceWhite only                                                      0.225001
    ## Age                                                                 0.002211
    ## SexMale                                                             0.046335
    ## Marital_StatusLiving with a partner                                 0.107515
    ## Marital_StatusMarital status unknown                                0.143467
    ## Marital_StatusMarried, spouse is not present                        0.166734
    ## Marital_StatusMarried, spouse is present                            0.070932
    ## Marital_StatusNever married                                         0.084243
    ## Marital_StatusSeparated                                             0.179488
    ## Marital_StatusWidowed                                               0.166877
    ## Health_Status_GroupHigh_Health                                      0.186239
    ## Education_LevelHigh School                                          0.187134
    ## Education_LevelAssociate's                                          0.225623
    ## Education_LevelBachelor's                                           0.222357
    ## Education_LevelMaster's                                             0.285385
    ## Education_LevelPhD or Professional                                  0.819681
    ## Health_Status_GroupHigh_Health:Education_LevelHigh School           0.219887
    ## Health_Status_GroupHigh_Health:Education_LevelAssociate's           0.255007
    ## Health_Status_GroupHigh_Health:Education_LevelBachelor's            0.247710
    ## Health_Status_GroupHigh_Health:Education_LevelMaster's              0.307782
    ## Health_Status_GroupHigh_Health:Education_LevelPhD or Professional   0.831504
    ##                                                                   z value
    ## (Intercept)                                                        -1.027
    ## RaceAIAN only                                                       0.066
    ## RaceAsian only                                                     -2.948
    ## RaceBlack/African American only                                    -0.185
    ## RaceOther single and multiple races                                -0.506
    ## RaceRace Unknown                                                   -0.480
    ## RaceWhite only                                                     -1.612
    ## Age                                                                -1.891
    ## SexMale                                                            -4.428
    ## Marital_StatusLiving with a partner                                 0.804
    ## Marital_StatusMarital status unknown                               -1.266
    ## Marital_StatusMarried, spouse is not present                       -0.229
    ## Marital_StatusMarried, spouse is present                           -1.903
    ## Marital_StatusNever married                                        -0.555
    ## Marital_StatusSeparated                                             1.085
    ## Marital_StatusWidowed                                              -0.380
    ## Health_Status_GroupHigh_Health                                     -0.460
    ## Education_LevelHigh School                                          1.871
    ## Education_LevelAssociate's                                          1.442
    ## Education_LevelBachelor's                                           1.573
    ## Education_LevelMaster's                                             2.312
    ## Education_LevelPhD or Professional                                 -0.244
    ## Health_Status_GroupHigh_Health:Education_LevelHigh School          -1.428
    ## Health_Status_GroupHigh_Health:Education_LevelAssociate's          -0.261
    ## Health_Status_GroupHigh_Health:Education_LevelBachelor's            0.075
    ## Health_Status_GroupHigh_Health:Education_LevelMaster's             -0.568
    ## Health_Status_GroupHigh_Health:Education_LevelPhD or Professional   1.158
    ##                                                                   Pr(>|z|)    
    ## (Intercept)                                                        0.30462    
    ## RaceAIAN only                                                      0.94746    
    ## RaceAsian only                                                     0.00319 ** 
    ## RaceBlack/African American only                                    0.85320    
    ## RaceOther single and multiple races                                0.61256    
    ## RaceRace Unknown                                                   0.63140    
    ## RaceWhite only                                                     0.10699    
    ## Age                                                                0.05864 .  
    ## SexMale                                                           9.51e-06 ***
    ## Marital_StatusLiving with a partner                                0.42139    
    ## Marital_StatusMarital status unknown                               0.20535    
    ## Marital_StatusMarried, spouse is not present                       0.81871    
    ## Marital_StatusMarried, spouse is present                           0.05709 .  
    ## Marital_StatusNever married                                        0.57892    
    ## Marital_StatusSeparated                                            0.27793    
    ## Marital_StatusWidowed                                              0.70409    
    ## Health_Status_GroupHigh_Health                                     0.64519    
    ## Education_LevelHigh School                                         0.06130 .  
    ## Education_LevelAssociate's                                         0.14943    
    ## Education_LevelBachelor's                                          0.11577    
    ## Education_LevelMaster's                                            0.02080 *  
    ## Education_LevelPhD or Professional                                 0.80754    
    ## Health_Status_GroupHigh_Health:Education_LevelHigh School          0.15317    
    ## Health_Status_GroupHigh_Health:Education_LevelAssociate's          0.79407    
    ## Health_Status_GroupHigh_Health:Education_LevelBachelor's           0.93990    
    ## Health_Status_GroupHigh_Health:Education_LevelMaster's             0.56979    
    ## Health_Status_GroupHigh_Health:Education_LevelPhD or Professional  0.24705    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 11175  on 8965  degrees of freedom
    ## Residual deviance: 11027  on 8939  degrees of freedom
    ##   (1526 observations deleted due to missingness)
    ## AIC: 11081
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
#This tells us the "Odds Ratios"
exp(coefficients(Model_3))
```

    ##                                                       (Intercept) 
    ##                                                         0.7346119 
    ##                                                     RaceAIAN only 
    ##                                                         1.0223907 
    ##                                                    RaceAsian only 
    ##                                                         0.4878493 
    ##                                   RaceBlack/African American only 
    ##                                                         0.9577035 
    ##                               RaceOther single and multiple races 
    ##                                                         0.8620957 
    ##                                                  RaceRace Unknown 
    ##                                                         0.8886537 
    ##                                                    RaceWhite only 
    ##                                                         0.6958126 
    ##                                                               Age 
    ##                                                         0.9958286 
    ##                                                           SexMale 
    ##                                                         0.8145058 
    ##                               Marital_StatusLiving with a partner 
    ##                                                         1.0902905 
    ##                              Marital_StatusMarital status unknown 
    ##                                                         0.8338545 
    ##                      Marital_StatusMarried, spouse is not present 
    ##                                                         0.9625049 
    ##                          Marital_StatusMarried, spouse is present 
    ##                                                         0.8737508 
    ##                                       Marital_StatusNever married 
    ##                                                         0.9543245 
    ##                                           Marital_StatusSeparated 
    ##                                                         1.2149949 
    ##                                             Marital_StatusWidowed 
    ##                                                         0.9385866 
    ##                                    Health_Status_GroupHigh_Health 
    ##                                                         0.9178195 
    ##                                        Education_LevelHigh School 
    ##                                                         1.4193350 
    ##                                        Education_LevelAssociate's 
    ##                                                         1.3843755 
    ##                                         Education_LevelBachelor's 
    ##                                                         1.4186673 
    ##                                           Education_LevelMaster's 
    ##                                                         1.9341575 
    ##                                Education_LevelPhD or Professional 
    ##                                                         0.8189992 
    ##         Health_Status_GroupHigh_Health:Education_LevelHigh School 
    ##                                                         0.7304510 
    ##         Health_Status_GroupHigh_Health:Education_LevelAssociate's 
    ##                                                         0.9356037 
    ##          Health_Status_GroupHigh_Health:Education_LevelBachelor's 
    ##                                                         1.0188526 
    ##            Health_Status_GroupHigh_Health:Education_LevelMaster's 
    ##                                                         0.8395150 
    ## Health_Status_GroupHigh_Health:Education_LevelPhD or Professional 
    ##                                                         2.6182666

``` r
plot(Model_3)
```

![](NHIS_Data_files/figure-gfm/Model%20#3:%20Demo%20+%20Health%20Status%20Grouping%20+%20Interaction-1.png)<!-- -->![](NHIS_Data_files/figure-gfm/Model%20#3:%20Demo%20+%20Health%20Status%20Grouping%20+%20Interaction-2.png)<!-- -->![](NHIS_Data_files/figure-gfm/Model%20#3:%20Demo%20+%20Health%20Status%20Grouping%20+%20Interaction-3.png)<!-- -->![](NHIS_Data_files/figure-gfm/Model%20#3:%20Demo%20+%20Health%20Status%20Grouping%20+%20Interaction-4.png)<!-- -->
