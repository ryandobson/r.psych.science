

#CHAPTER 7: ADVANCED STATISTICAL ANALYSIS 

library(tidyverse)


#Multilevel Analyses 

#Simulating example stroop task data 

set.seed(1000)
stroop_dat <- tibble(
  subject = seq(1:60),
  congruent = rnorm(mean = 400, sd = 30, n = 60),
  incongruent = congruent + rnorm(mean = 60, sd = 10, n = 60),
  neutral = congruent + rnorm(mean = 30, sd = 10, n = 60)
) %>%
  gather(key = cond, value = reaction_time, 2:4) %>% 
  mutate(
    cond = as.factor(cond),
    log_RT = log(reaction_time)
  )

str(stroop_dat)


#simulating data for drug intervention on blood oxygen levels 

# set parameters
set.seed(1000)
means <- rep(c(80, 80, 100), 100)
sds <- rep(c(5, 5, 7), 100)

# simulate data
cond <- rep(c("control", "drug_one", "drug_two"), 100)
subject <- seq(1:300)

smoking_dat <- tibble(
  subject = subject,
  cond = as.factor(cond),
  outcome = rnorm(n = 300, mean = means, sd = sds)
)


#Preparation of Data 

#looking at the factors of a variable 
levels(stroop_dat$cond)
smoking_dat$cond %>% levels()

#If your data is not stored as a factor you can convert it using:
#as.factor() or 
#factor() 
#data <- as.factor(data$factor) 


#Contrast Matrices 

#We can see how we specify our contrasts for our tests by using:
#contrasts() function on any factors 

#make data 2 levels and convert condition to a factor 
stroop_subset <- stroop_dat %>% 
  filter(cond %in% c("congruent", "incongruent")) %>% 
  mutate(cond = factor(cond))

str(stroop_subset)

#see your contrasts 
contrasts(stroop_subset$cond)

#from our contrasts matrix we see that the congruent condition is assigned 1 
#and the incongruent condition is assigned 0. 
#This means our intercept in our linear model is the congruent condition. 
#Remember, in a linear model the intercept is the point on the y-axis where x = 0 


#Fitting a linear model 

lm(log_RT ~ cond, stroop_subset) %>% summary() 

## 
## Call:
## lm(formula = log_RT ~ cond, data = stroop_subset)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.15379 -0.04888  0.00581  0.03781  0.19539 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     5.978613   0.009196  650.14   <2e-16 ***
## condincongruent 0.145808   0.013005   11.21   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.07123 on 118 degrees of freedom
## Multiple R-squared:  0.5158, Adjusted R-squared:  0.5117 
## F-statistic: 125.7 on 1 and 118 DF,  p-value: < 2.2e-16


#We see that our parameter estimate for the intercept is 5.979 
#being in the congruent condition adds .145 to the intercept value. 
#That sums to 6.124, we can check this against our mean score. 

stroop_subset %>% 
  group_by(cond) %>% 
  summarise(mean = mean(log_RT))


#Changing our contrasts 
contrasts(stroop_subset$cond) <- contr.sum
contrasts(stroop_subset$cond) 

#refitting our model 
lm(log_RT ~ cond, stroop_subset) %>% summary()

#the intercept now equals the mean across conditions 
#checking that
stroop_subset %>% summarize(mean = mean(log_RT))


#Multilevel Regression 

#Contrasts with three levels of data 

contrasts(stroop_dat$cond)
#since congruent sums to 0, this will be our intercept 

lm(log_RT ~ cond, data = stroop_dat) %>% summary()


## Call:
## lm(formula = log_RT ~ cond, data = stroop_dat)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.180561 -0.052211  0.002842  0.042382  0.195386 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     5.978613   0.009308 642.314  < 2e-16 ***
## condincongruent 0.145808   0.013163  11.077  < 2e-16 ***
## condneutral     0.073206   0.013163   5.561 9.74e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0721 on 177 degrees of freedom
## Multiple R-squared:  0.4094, Adjusted R-squared:  0.4027 
## F-statistic: 61.35 on 2 and 177 DF,  p-value: < 2.2e-16


#Checking how these parameters correspond to our means for each group 

stroop_dat %>% group_by(cond) %>% 
  summarize(mean = mean(log_RT))

##   cond         mean
##   <fct>       <dbl>
## 1 congruent    5.98
## 2 incongruent  6.12
## 3 neutral      6.05


#Sum coded effects (changing the contrast)
contrasts(stroop_dat$cond) <- contr.sum
contrasts(stroop_dat$cond)


lm(log_RT ~ cond, data = stroop_dat) %>% summary
#Now the intercept has changed and the congruent and incongruet conditions
#are considered against the grand mean across levels. 
stroop_dat %>% summarise(mean = mean(log_RT))
##    mean
##   <dbl>
## 1  6.05


#Regardless of how you edit your contrasts to see what is compared, 
#it does not change the results. It just changes how you see the results. 

#Using a treatment condition coding 
contrasts(stroop_dat$cond) <- contr.treatment
stroop_dat$cond <- factor(stroop_dat$cond, levels = c("neutral", "congruent", "incongruent"))

contrasts(stroop_dat$cond)

lm(log_RT ~ cond, data = stroop_dat) %>% summary

## Call:
## lm(formula = log_RT ~ cond, data = stroop_dat)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.180561 -0.052211  0.002842  0.042382  0.195386 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)      6.051819   0.009308 650.179  < 2e-16 ***
## condcongruent   -0.073206   0.013163  -5.561 9.74e-08 ***
## condincongruent  0.072602   0.013163   5.515 1.22e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0721 on 177 degrees of freedom
## Multiple R-squared:  0.4094, Adjusted R-squared:  0.4027 
## F-statistic: 61.35 on 2 and 177 DF,  p-value: < 2.2e-16


#This happens to be very similar to the grand mean. 
#But it is really focusing in on the neutral condition, which happens to be
#really close to the grand mean. 


#Multilevel ANOVA 

stroop_aov <- aov(log_RT ~ cond, data = stroop_dat)
summary(stroop_aov)

##              Df Sum Sq Mean Sq F value Pr(>F)    
## cond          2 0.6378  0.3189   61.35 <2e-16 ***
## Residuals   177 0.9201  0.0052                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


#Now our model corresponds to the model output that we got using the lm() 
#the F, df, and p-values are the same! 
#This is because the aov() function is simplya wrapper for our linear model 
#that gives us main effects. 
#Under the hood, aov() is just lm(), only without the additional model 
#coefficients from the lm() 


#If we have already fitted a model using the aov() function, but want the 
#model coefficients, we can use a different type of summary 
summary.lm(stroop_aov)

## Call:
## aov(formula = log_RT ~ cond, data = stroop_dat)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.180561 -0.052211  0.002842  0.042382  0.195386 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)      6.051819   0.009308 650.179  < 2e-16 ***
## condcongruent   -0.073206   0.013163  -5.561 9.74e-08 ***
## condincongruent  0.072602   0.013163   5.515 1.22e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0721 on 177 degrees of freedom
## Multiple R-squared:  0.4094, Adjusted R-squared:  0.4027 
## F-statistic: 61.35 on 2 and 177 DF,  p-value: < 2.2e-16


#Contrasts coding becomes more important as we fit more factors in our models. 


#Multiple Contrasts 

smoking_lm <- lm(outcome ~ cond, data = smoking_dat)
summary(smoking_lm)

## lm(formula = outcome ~ cond, data = smoking_dat)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -15.7906  -3.1192   0.0561   3.1747  18.4927 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   79.6866     0.5508 144.670   <2e-16 ***
## conddrug_one   0.7748     0.7790   0.995    0.321    
## conddrug_two  20.5112     0.7790  26.331   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.508 on 297 degrees of freedom
## Multiple R-squared:   0.75,  Adjusted R-squared:  0.7483 
## F-statistic: 445.4 on 2 and 297 DF,  p-value: < 2.2e-16

#Checking means 
smoking_dat %>% group_by(cond) %>% 
  summarize(mean = mean(outcome))


#Here the two drug conditions are compared against the intercept for the 
#placebo condition. 
#What if we want to consider the effectiveness of the two drugs against one-another? 

#Two Options: 
#(1) Relevel your factor so that you can compare drug_ong to drug_two
#i.e set drug_one as the intercept 
#(2) Calculate the differences in the parameter estimates for the two rows in the table 

#The first option is easiest if we only have a few levels in a factor. 
#Second is handy if we have lots of levels, but requires us to specify a complex 
#contrast matrix 


#First Option 
smoking_dat$cond <- factor(smoking_dat$cond, levels = c("drug_one", "drug_two", "control"))
smoking_lm2 <- lm(outcome ~ cond, data = smoking_dat)
summary(smoking_lm2)

## lm(formula = outcome ~ cond, data = smoking_dat)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -15.7906  -3.1192   0.0561   3.1747  18.4927 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   80.4614     0.5508 146.077   <2e-16 ***
## conddrug_two  19.7364     0.7790  25.337   <2e-16 ***
## condcontrol   -0.7748     0.7790  -0.995    0.321    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.508 on 297 degrees of freedom
## Multiple R-squared:   0.75,  Adjusted R-squared:  0.7483 
## F-statistic: 445.4 on 2 and 297 DF,  p-value: < 2.2e-16

#Now our intercept is drug_one and the parameter estimate for conddrug_two
#is the difference between drug one and drug two. 


#For more complex cases with several levels there are helper packages 
#such as multcomp which allows you to specify a contrast matrix and conduct
#multiple comparisons on your model (don't get into that here)



#Multifactorial Analyses 

#Data set 
# DV = tooth length 
# supplement type (2 levels)
# dose (3 levels) 

# load the data
data("ToothGrowth")

# convert to tibble and make dose a factor
tooth <- ToothGrowth %>%
  mutate(dose = factor(dose)) %>%
  as.tibble()

# see the output
tooth


#Checking our contrasts 
contrasts(tooth$supp) 
contrasts(tooth$dose)

#Interested in the main effects and interaction of both factors 
tooth_aov <- aov(len ~ supp * dose, data = tooth)
summary(tooth_aov)

##             Df Sum Sq Mean Sq F value   Pr(>F)    
## supp         1  205.4   205.4  15.572 0.000231 ***
## dose         2 2426.4  1213.2  92.000  < 2e-16 ***
## supp:dose    2  108.3    54.2   4.107 0.021860 *  
## Residuals   54  712.1    13.2                     
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


#Checking with a boxplot 

ggplot(data = tooth, mapping = aes(x = dose, y = len, colour = supp)) +
  geom_boxplot()


#Multiple follow-up comparisons using Tukey Honest Significant Difference 

TukeyHSD(tooth_aov, conf.level = 0.95)

## Fit: aov(formula = len ~ supp * dose, data = tooth)
## 
## $supp
##       diff       lwr       upr     p adj
## VC-OJ -3.7 -5.579828 -1.820172 0.0002312
## 
## $dose
##         diff       lwr       upr   p adj
## 1-0.5  9.130  6.362488 11.897512 0.0e+00
## 2-0.5 15.495 12.727488 18.262512 0.0e+00
## 2-1    6.365  3.597488  9.132512 2.7e-06
## 
## $`supp:dose`
##                diff        lwr        upr     p adj
## VC:0.5-OJ:0.5 -5.25 -10.048124 -0.4518762 0.0242521
## OJ:1-OJ:0.5    9.47   4.671876 14.2681238 0.0000046
## VC:1-OJ:0.5    3.54  -1.258124  8.3381238 0.2640208
## OJ:2-OJ:0.5   12.83   8.031876 17.6281238 0.0000000
## VC:2-OJ:0.5   12.91   8.111876 17.7081238 0.0000000
## OJ:1-VC:0.5   14.72   9.921876 19.5181238 0.0000000
## VC:1-VC:0.5    8.79   3.991876 13.5881238 0.0000210
## OJ:2-VC:0.5   18.08  13.281876 22.8781238 0.0000000
## VC:2-VC:0.5   18.16  13.361876 22.9581238 0.0000000
## VC:1-OJ:1     -5.93 -10.728124 -1.1318762 0.0073930
## OJ:2-OJ:1      3.36  -1.438124  8.1581238 0.3187361
## VC:2-OJ:1      3.44  -1.358124  8.2381238 0.2936430
## OJ:2-VC:1      9.29   4.491876 14.0881238 0.0000069
## VC:2-VC:1      9.37   4.571876 14.1681238 0.0000058
## VC:2-OJ:2      0.08  -4.718124  4.8781238 1.0000000

#We have a lot of output here so we can remedy this by saving the output 
#to an object and selecting only the relevant information. 


tukey_aov <- TukeyHSD(tooth_aov, conf.level = 0.95)
tukey_aov$`supp:dose`



#Multiple Regression 

#Three-Level Factors 
#See whether there's a main effect or interaction for 3 level factors 
#simply run an ANOVA. Comparisons for lm() become more difficult when we 
#have factors with more than 2 levels. 

#It becomes more difficult because we have to construct some numeric variables 
#for these factors, and then perform several different comparisons. 

# code new centered variables
tooth$supp_dev <- (tooth$supp == "VC") - mean(tooth$supp == "VC")
tooth$dose_dev_one <- (tooth$dose == "0.5") - mean(tooth$dose == "0.5")
tooth$dose_dev_two <- (tooth$dose == "1") - mean(tooth$dose == "1")

# inspect changes
head(tooth)

#The above centering is called Deviation Coding 
#similar to sum coding, but the parameter esimate and SE are half as large 


tooth_lm_full <- lm(len ~ supp_dev * (dose_dev_one + dose_dev_two), data = tooth)

#Constructing a reduced model, and use anove() to compare the two models against eachother 

tooth_lm_reduced <- lm(len ~ supp_dev + (dose_dev_one + dose_dev_two), data = tooth)
anova(tooth_lm_full, tooth_lm_reduced)

## 
## Model 1: len ~ supp_dev * (dose_dev_one + dose_dev_two)
## Model 2: len ~ supp_dev + (dose_dev_one + dose_dev_two)
##   Res.Df    RSS Df Sum of Sq     F  Pr(>F)  
## 1     54 712.11                             
## 2     56 820.43 -2   -108.32 4.107 0.02186 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#This checks for how much variance is explained by each model 
#The interaction term in our model makes a sign contribution to the variance 
#This indicates that there's an interaction in our model and we should explore this. 

#A quick and easy way to do so is to run a number of t-tests or linear models
#on subsets of data. Adjusting for the p-values when necessary. 

#You can achieve this by creating a new column which combines two factors 

# create combined factor column
tooth$interact <- interaction(tooth$supp, tooth$dose)

# check levels of our new factor
levels(tooth$interact)


#Then we simply use the pairwise.t.test() function 

pairwise.t.test(tooth$len, tooth$interact)

##  Pairwise comparisons using t tests with pooled SD 
## 
## data:  tooth$len and tooth$interact 
## 
##        OJ.0.5  VC.0.5  OJ.1   VC.1    OJ.2  
## VC.0.5 0.0105  -       -      -       -     
## OJ.1   3.2e-06 2.6e-11 -      -       -     
## VC.1   0.1346  1.0e-05 0.0035 -       -     
## OJ.2   1.6e-09 1.9e-14 0.1346 3.8e-06 -     
## VC.2   1.4e-09 1.7e-14 0.1346 3.6e-06 0.9609
## 
## P value adjustment method: holm


#Two-Level Factors 

#Subsetting our data to only 2 levels for dose 
tooth_sub <- tooth %>% 
  dplyr::select(1 : 3) %>%
  filter(dose %in% c(0.5, 1)) %>%
  mutate(dose = factor(dose))

#Checking contrasts
contrasts(tooth_sub$supp)
contrasts(tooth_sub$dose)

#Making them sum coded contrasts (so we can get main effects)
contrasts(tooth_sub$supp) <- contr.sum
contrasts(tooth_sub$dose) <- contr.sum

#Fitting our model 
tooth_sub_lm <- lm(len ~ supp * dose, data = tooth_sub)
summary(tooth_sub_lm)


## Call:
## lm(formula = len ~ supp * dose, data = tooth_sub)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
##  -8.20  -2.72  -0.27   2.65   8.27 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  15.1700     0.5537  27.397  < 2e-16 ***
## supp1         2.7950     0.5537   5.048 1.30e-05 ***
## dose1        -4.5650     0.5537  -8.244 8.25e-10 ***
## supp1:dose1  -0.1700     0.5537  -0.307    0.761    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.502 on 36 degrees of freedom
## Multiple R-squared:  0.7221, Adjusted R-squared:  0.6989 
## F-statistic: 31.18 on 3 and 36 DF,  p-value: 4.098e-10


#If your factors are not sum/deviation coded, then you won't observe main effects
#So we can test for the effect of dose at the first level of supplement like this: 

contrasts(tooth_sub$supp) <- contr.treatment
contrasts(tooth_sub$supp)

tooth_sub_lm2 <- lm(len ~ supp * dose, data = tooth_sub)
summary(tooth_sub_lm2)

## Call:
## lm(formula = len ~ supp * dose, data = tooth_sub)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
##  -8.20  -2.72  -0.27   2.65   8.27 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  17.9650     0.7831  22.942  < 2e-16 ***
## supp2        -5.5900     1.1074  -5.048 1.30e-05 ***
## dose1        -4.7350     0.7831  -6.047 6.02e-07 ***
## supp2:dose1   0.3400     1.1074   0.307    0.761    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.502 on 36 degrees of freedom
## Multiple R-squared:  0.7221, Adjusted R-squared:  0.6989 
## F-statistic: 31.18 on 3 and 36 DF,  p-value: 4.098e-10




### MIXED ANALYSES 

