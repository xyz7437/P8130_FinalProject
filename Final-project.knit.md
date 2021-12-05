---
title: "Final Project"
author: "Yijia Jiang"
date: "11/16/2021"
output: pdf_document
---



## Purpose
We will be analyzing data from the "County Demographic Information" (CDI) data set, which contains characteristics of 440 counties in the United States collected from 1990-1992. The primary objective of this investigation is to develop insight relevant to predicting the crime rate in counties, namely to summarize as the crime rate per 1,000 population (CRM_1000). 


## Import the package we need


## Data preprocessing

```r
rm(list = ls())
cdi <- read.csv("./data/cdi.csv") %>%
  mutate(crime_rate = crimes/pop, 
         pcarea = area/pop,
         pcdocs = docs/pop,
         pcbeds = beds/pop,
         region = relevel(factor(region),ref = 3))
cdi_new = cdi %>%
  select(crime_rate, everything(), -id, -cty, -state, -area, -docs, -beds, -crimes, -pop, -totalinc)
summary(cdi_new)
```

```
##    crime_rate           pop18           pop65            hsgrad     
##  Min.   :0.004601   Min.   :16.40   Min.   : 3.000   Min.   :46.60  
##  1st Qu.:0.038102   1st Qu.:26.20   1st Qu.: 9.875   1st Qu.:73.88  
##  Median :0.052429   Median :28.10   Median :11.750   Median :77.70  
##  Mean   :0.057286   Mean   :28.57   Mean   :12.170   Mean   :77.56  
##  3rd Qu.:0.072597   3rd Qu.:30.02   3rd Qu.:13.625   3rd Qu.:82.40  
##  Max.   :0.295987   Max.   :49.70   Max.   :33.800   Max.   :92.90  
##      bagrad         poverty           unemp           pcincome     region 
##  Min.   : 8.10   Min.   : 1.400   Min.   : 2.200   Min.   : 8899   3:152  
##  1st Qu.:15.28   1st Qu.: 5.300   1st Qu.: 5.100   1st Qu.:16118   1:103  
##  Median :19.70   Median : 7.900   Median : 6.200   Median :17759   2:108  
##  Mean   :21.08   Mean   : 8.721   Mean   : 6.597   Mean   :18561   4: 77  
##  3rd Qu.:25.32   3rd Qu.:10.900   3rd Qu.: 7.500   3rd Qu.:20270          
##  Max.   :52.30   Max.   :36.300   Max.   :21.300   Max.   :37541          
##      pcarea              pcdocs              pcbeds         
##  Min.   :3.086e-05   Min.   :0.0003559   Min.   :0.0001649  
##  1st Qu.:1.323e-03   1st Qu.:0.0012127   1st Qu.:0.0021972  
##  Median :2.977e-03   Median :0.0017509   Median :0.0033287  
##  Mean   :4.760e-03   Mean   :0.0021230   Mean   :0.0036493  
##  3rd Qu.:5.199e-03   3rd Qu.:0.0024915   3rd Qu.:0.0045649  
##  Max.   :7.542e-02   Max.   :0.0170377   Max.   :0.0196982
```



## Rank of the crime rate by state

```r
cdi_state <- cdi %>%
  group_by(state) %>%
  summarize(crime_rate = mean(crime_rate))
cdi_state_new <- cdi_state[order(-rank(cdi_state$crime_rate)),]

ggplot(cdi_state,aes(x=reorder(state,crime_rate),y=crime_rate,fill=crime_rate)) + 
  geom_bar(stat ='identity')+
  coord_flip() + 
  theme_grey() + 
  labs(title = 'Ranking of Counties by crime rate',
       y='Crime rate',x='Counties') +
  geom_hline(yintercept = mean(cdi$crime_rate),color = 'blue')+
  theme(plot.title = element_text(hjust = 0.5,size = 14, face = "bold"),
        axis.text=element_text(size=6.5))
```

![](Final-project_files/figure-latex/unnamed-chunk-3-1.pdf)<!-- --> 


# Exploratory Data Analysis

```r
# Boxplots for each variable
par(mfrow=c(3,4))
boxplot(cdi_new$crime_rate, main='crime_rate')
boxplot(cdi_new$pop18, main='pop18')
boxplot(cdi_new$pop65, main='pop65')
boxplot(cdi_new$pcincome, main='pcincome')
boxplot(cdi_new$unemp,main='unemp')
boxplot(cdi_new$hsgrad, main='hsgrad')
boxplot(cdi_new$bagrad, main='bagrad')
boxplot(cdi_new$poverty, main='poverty')
boxplot(cdi_new$pcarea, main='pcarea')
boxplot(cdi_new$pcdocs, main='pcdocs')
boxplot(cdi_new$pcbeds, main='pcbeds')

# Boxplot for transforming pcarea, looks more normal
cdi_new %>%
  mutate(lnpcarea = log(pcarea)) %>%
  ggplot(aes(y = lnpcarea)) + 
  geom_boxplot()

# Scatterplot Matrix
#plot(x=cdi$area,y=cdi$crime_rate)
#plot(x=cdi$pcincome,y=cdi$crime_rate)
#plot(x=cdi$unemp,y=cdi$crime_rate)
#plot(x=cdi$hsgrad,y=cdi$crime_rate)
#plot(x=cdi$poverty,y=cdi$crime_rate)
# pairs(~crime_rate + pcarea + pop18 + pop65 + pcdocs + pcbeds + hsgrad + bagrad + poverty + unemp + pcincome +      factor(region), data = cdi_new, panel = panel.smooth, upper.panel = NULL, main = "Scatterplot Matrix")
pairs(~crime_rate +.,data=cdi_new,panel = panel.smooth, upper.panel = NULL, main = "Scatterplot Matrix")
```

![](Final-project_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> ![](Final-project_files/figure-latex/unnamed-chunk-4-2.pdf)<!-- --> 

```r
# Correlation plot
corrplot(cor(subset(cdi_new,select = -region)), type = "lower", diag = FALSE)
```

![](Final-project_files/figure-latex/unnamed-chunk-4-3.pdf)<!-- --> 




## Data split

```r
#split <- 0.80
#set.seed(1234)
#cdi<- cdi %>%
#  select(-c(id,cty,state,pop,crimes))
#trainIndex <- sample(1:nrow(cdi), 0.8*nrow(cdi))
#data_train <- cdi[ trainIndex,]
#data_test <- cdi[-trainIndex,]
#dim(data_train)
#dim(data_test) 
```

## Modelling

```r
# fit regression using all predictors
mult.fit = lm(crime_rate ~ ., data = cdi_new)
summary(mult.fit)
```

```
## 
## Call:
## lm(formula = crime_rate ~ ., data = cdi_new)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.049868 -0.010696 -0.000262  0.009115  0.222726 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -5.110e-02  3.107e-02  -1.645 0.100775    
## pop18        1.320e-03  3.721e-04   3.547 0.000433 ***
## pop65        2.234e-04  3.488e-04   0.640 0.522201    
## hsgrad       1.678e-04  3.053e-04   0.549 0.582951    
## bagrad      -5.330e-04  3.417e-04  -1.560 0.119567    
## poverty      2.799e-03  4.323e-04   6.475 2.62e-10 ***
## unemp        3.280e-04  6.065e-04   0.541 0.588897    
## pcincome     2.251e-06  5.292e-07   4.254 2.58e-05 ***
## region1     -2.352e-02  3.025e-03  -7.776 5.71e-14 ***
## region2     -1.491e-02  2.945e-03  -5.064 6.13e-07 ***
## region4      1.269e-03  3.452e-03   0.368 0.713311    
## pcarea      -6.520e-01  1.695e-01  -3.847 0.000138 ***
## pcdocs       6.635e-01  1.157e+00   0.574 0.566606    
## pcbeds       2.319e+00  9.046e-01   2.564 0.010699 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02033 on 426 degrees of freedom
## Multiple R-squared:  0.4628,	Adjusted R-squared:  0.4464 
## F-statistic: 28.23 on 13 and 426 DF,  p-value: < 2.2e-16
```


## Backwards Elimination

```r
mult.fit.back <- step(mult.fit, direction='backward')
```

```
## Start:  AIC=-3414.29
## crime_rate ~ pop18 + pop65 + hsgrad + bagrad + poverty + unemp + 
##     pcincome + region + pcarea + pcdocs + pcbeds
## 
##            Df Sum of Sq     RSS     AIC
## - unemp     1  0.000121 0.17624 -3416.0
## - hsgrad    1  0.000125 0.17624 -3416.0
## - pcdocs    1  0.000136 0.17625 -3416.0
## - pop65     1  0.000170 0.17629 -3415.9
## <none>                  0.17612 -3414.3
## - bagrad    1  0.001006 0.17712 -3413.8
## - pcbeds    1  0.002717 0.17883 -3409.6
## - pop18     1  0.005201 0.18132 -3403.5
## - pcarea    1  0.006118 0.18223 -3401.3
## - pcincome  1  0.007483 0.18360 -3398.0
## - poverty   1  0.017333 0.19345 -3375.0
## - region    3  0.033187 0.20930 -3344.3
## 
## Step:  AIC=-3415.99
## crime_rate ~ pop18 + pop65 + hsgrad + bagrad + poverty + pcincome + 
##     region + pcarea + pcdocs + pcbeds
## 
##            Df Sum of Sq     RSS     AIC
## - hsgrad    1  0.000090 0.17633 -3417.8
## - pcdocs    1  0.000139 0.17638 -3417.6
## - pop65     1  0.000206 0.17644 -3417.5
## <none>                  0.17624 -3416.0
## - bagrad    1  0.001199 0.17744 -3415.0
## - pcbeds    1  0.002597 0.17883 -3411.6
## - pop18     1  0.005296 0.18153 -3405.0
## - pcarea    1  0.006032 0.18227 -3403.2
## - pcincome  1  0.008072 0.18431 -3398.3
## - poverty   1  0.020229 0.19647 -3370.2
## - region    3  0.034275 0.21051 -3343.8
## 
## Step:  AIC=-3417.76
## crime_rate ~ pop18 + pop65 + bagrad + poverty + pcincome + region + 
##     pcarea + pcdocs + pcbeds
## 
##            Df Sum of Sq     RSS     AIC
## - pcdocs    1  0.000119 0.17645 -3419.5
## - pop65     1  0.000185 0.17651 -3419.3
## <none>                  0.17633 -3417.8
## - bagrad    1  0.001366 0.17769 -3416.4
## - pcbeds    1  0.002791 0.17912 -3412.9
## - pop18     1  0.005207 0.18153 -3407.0
## - pcarea    1  0.006153 0.18248 -3404.7
## - pcincome  1  0.008474 0.18480 -3399.1
## - region    3  0.034432 0.21076 -3345.3
## - poverty   1  0.032602 0.20893 -3345.1
## 
## Step:  AIC=-3419.47
## crime_rate ~ pop18 + pop65 + bagrad + poverty + pcincome + region + 
##     pcarea + pcbeds
## 
##            Df Sum of Sq     RSS     AIC
## - pop65     1  0.000185 0.17663 -3421.0
## <none>                  0.17645 -3419.5
## - bagrad    1  0.001252 0.17770 -3418.4
## - pop18     1  0.005485 0.18193 -3408.0
## - pcarea    1  0.006170 0.18262 -3406.3
## - pcbeds    1  0.007404 0.18385 -3403.4
## - pcincome  1  0.009338 0.18578 -3398.8
## - poverty   1  0.032979 0.20943 -3346.1
## - region    3  0.034895 0.21134 -3346.1
## 
## Step:  AIC=-3421.01
## crime_rate ~ pop18 + bagrad + poverty + pcincome + region + pcarea + 
##     pcbeds
## 
##            Df Sum of Sq     RSS     AIC
## <none>                  0.17663 -3421.0
## - bagrad    1  0.001396 0.17803 -3419.5
## - pop18     1  0.005894 0.18252 -3408.6
## - pcarea    1  0.006021 0.18265 -3408.3
## - pcincome  1  0.009446 0.18608 -3400.1
## - pcbeds    1  0.009589 0.18622 -3399.7
## - region    3  0.034710 0.21134 -3348.1
## - poverty   1  0.032938 0.20957 -3347.8
```

```r
mult.fit.back
```

```
## 
## Call:
## lm(formula = crime_rate ~ pop18 + bagrad + poverty + pcincome + 
##     region + pcarea + pcbeds, data = cdi_new)
## 
## Coefficients:
## (Intercept)        pop18       bagrad      poverty     pcincome      region1  
##  -3.242e-02    1.214e-03   -4.595e-04    2.727e-03    2.288e-06   -2.279e-02  
##     region2      region4       pcarea       pcbeds  
##  -1.458e-02    2.190e-03   -6.410e-01    2.750e+00
```
crime_rate ~ pop18 + bagrad + poverty + pcincome + region + pcarea + pcbeds





## Criterion Based Procedures








