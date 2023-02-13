library(tidyverse)

#Variance 
#how much your data is spread out from the mean 
#average of the squared differences from the mean 
#we square the values so the negative values don't cancel out positive values

#sqrt of variance is the standard deviation 

#Covariance 
#how much two variables vary together 
#If we have a positive number for covariance, the variables are related 
#covariance is sensitive to the scale you have:
#having big numbers for each variable will give a larger covariance 

#Correlation Coefficient 
#covariance divided by the standard deviation 
# -1 to 1 


# variables 
scale_max <- 6 


#Load and clean data 

corr_data <- read_csv( 
  "https://raw.githubusercontent.com/gpwilliams/r4psych/master/lesson_materials/06_simple_statistical_tests/inputs/dawtry-et-al_2015.csv"
  ) %>% 
  
  mutate(redist2 = (scale_max + 1) - redist2, #recoding the reverse scored questions 
         redist4 = (scale_max + 1) - redist4) %>%
  
  transmute (PS, #PS = participant ID #creating composite variables 
             Fair_Satisfied = (fairness + satisfaction) / 2,
             Support = (redist1 + redist2 + redist3 + redist4) / 4 
         ) %>% 
  rename_all(tolower)


# calculate means and standard deviations 
# summarize_at() allows us to pass two column names, and a list of functions 
#to produce our stats 


corr_data %>% summarize_at(
  c("fair_satisfied", "support"), 
  list(mean = mean, sd = sd)
)


#Checking Assumptions 
#are the factors linearly related? (not done here)
#do the factors follow a normal distribution? 


#Two Methods To Check for Normality 
#1: Visual inspection of the quantile-quantile plot which plots the correlation
#between our sample (observed values) and the normal distribution 
#2: The Shapiro-Wilk test, which is a statistical test for normality. 
#Significant results indicate that the data are non-normal 


corr_data %>%
  ggplot(aes(sample = fair_satisfied)) +
  geom_qq()

corr_data %>%
  ggplot(aes(sample = support)) +
  geom_qq()


shapiro.test(corr_data$fair_satisfied)

##  Shapiro-Wilk normality test
## 
## data:  corr_data$fair_satisfied
## W = 0.92602, p-value = 3.697e-11

shapiro.test(corr_data$support)

##  Shapiro-Wilk normality test
## 
## data:  corr_data$support
## W = 0.97622, p-value = 5.993e-05


#Both the plot and test show that our data are non-normal. 
#Because of this, we souldn't use a parametric test such as Pearson's R 
#Should use a non-parametric alternative. 
#Kendall's Tau -rank orders our observations, in an attempt to account for 
#factors being non-normally distributed 

#Running the correlation 

cor.test(corr_data$fair_satisfied, corr_data$support,
         method = "kendall", #change to "pearson" if data is normally distributed 
         alternative = "less" #specify that we had a directional hypothesis 
)

#defaults to Pearson's R and a two-sided test. 
#use alternative "greater" or "less" if you had a positive or negative 
#directional hypothesis 

##  Kendall's rank correlation tau
## 
## data:  corr_data$fair_satisfied and corr_data$support
## z = -13.157, p-value < 2.2e-16
## alternative hypothesis: true tau is less than 0
## sample estimates:
##        tau 
## -0.5473132

#significant negative correlation 


#T-TESTS 

#t-value: ratio of the difference between two groups of scores and the 
#difference between two groups of scores and the difference within the groups 
#of scores 
#large t-value tells us the difference between the groups is large 
#a t-value of 10 tells us that the groups are 10 times as different from 
#one another as they are within each other 

lexdec <- read_csv("initial.learning/inputs/lexical_decision_raw_data.csv") %>%
  filter(!subject %in% (as.character(22:30)))

#preparing the data 

lexdec_onesamp <- lexdec %>% 
  group_by(word) %>% 
  summarize(freq = mean(frequency))

#Running the Test 
#One Sample T-Test 
#used when you want to compare your sample mean to a population mean 
#should run when you don't have population sd or have a small sample size

t.test(lexdec_onesamp$freq, mu = 4)
#mu is to specify the mean score in the population by which to compare the scores 

## data:  lexdec_onesamp$freq
## t = 5.2044, df = 78, p-value = 1.535e-06
## alternative hypothesis: true mean is not equal to 4
## 95 percent confidence interval:
##  4.463784 5.038436
## sample estimates:
## mean of x 
##   4.75111


#Independent Samples T-Test 

#Preparing the Data 

lexdec_ind <- lexdec %>% 
  group_by(subject, native_language) %>% 
  summarize(log_RT = mean(log(RT), na.rm = T)) 
#we created log-transformed mean reaction times 
#reaction times are typically not normally distributed, so we transformed them
#to make them more normal prior to analysis. Log transformation is one way to do this 

#calculating descriptives 
lexdec_ind %>% 
  group_by(native_language) %>% 
  summarize(mean_log_RT = mean(log_RT, na.rm = T),
            sd_log_RT = sd(log_RT, na.rm = T), 
            n = n())

#Because our data are in a long format, we have to do the t-test using a formula 
#log_RT column contains data for both conditions of the experiment 
#specify formula like this:
#dependent variable ~ condition 
# "~" means that the thing on the left is a function of the thing on the right 

#additionally specify where the data is stored and whether the data is paired or not 

t.test(log_RT ~ native_language, data = lexdec_ind, paired = FALSE)

## data:  log_RT by native_language
## t = -2.2213, df = 11.903, p-value = 0.0465
## alternative hypothesis: true difference in means between group English and group Other is not equal to 0
## 95 percent confidence interval:
##  -0.29139463 -0.00268428
## sample estimates:
## mean in group English   mean in group Other 
##              6.326837              6.473876


#Checking assumptions 
#Homogeneity of Variance 
#Variance across samples is roughly equal 
#Barlett test for parametric data 
#Fligner test for non-parametric data 

#parametric relies on statistical distributions in the data (normal distribution)
#nonparametric do not depend on the distribution of data 

bartlett.test(log_RT ~ native_language, data = lexdec_ind)

##  Bartlett test of homogeneity of variances
## 
## data:  log_RT by native_language
## Bartlett's K-squared = 2.9197, df = 1, p-value = 0.08751

#non-significant result indicates that the variances are not signficanlty 
#different from one another. So we meet the test assumption. 

#PAIRED-SAMPLES T-TESTS 

#creating data 

set.seed(1000)
stroop_dat <- tibble(
  subject = seq(1:60),
  congruent = rnorm(mean = 400, sd = 30, n = 60),
  incongruent = congruent + rnorm(mean = 30, sd = 10, n = 60)
) %>%
  gather(key = condition, value = reaction_time, 2:3) %>% 
  mutate(
    log_RT = log(reaction_time)
  )
#used gather() to turn data into long format because this is how data typically is 


#If data is in wide format the formula looks like this: 
#
#t.test(data$dependent_variable_one, data$dependent_variable_two, data = data_name, paired = TRUE)
#

#Running the test 

t.test(log_RT ~ condition, data = stroop_dat, paired = TRUE)

## data:  log_RT by condition
## t = -25.573, df = 59, p-value < 2.2e-16
## alternative hypothesis: true mean difference is not equal to 0
## 95 percent confidence interval:
##  -0.08379794 -0.07163601
## sample estimates:
## mean difference 
##     -0.07771698



#ANOVA 

#f-value lets you know whether two groups of scores are similar or different 
#from one another 
#Gives you the ratio of variance between two samples 

#ANOVA and T-TESTS are BOTH part of the General Linear Model family. 

#aov() function 

#One-Way ANOVA 


#Independent Samples ANOVA 
anova_ind <- aov(log_RT ~ native_language, data = lexdec_ind)
summary(anova_ind)

##                 Df Sum Sq Mean Sq F value Pr(>F)  
## native_language  1 0.1070 0.10702   5.523 0.0304 *
## Residuals       18 0.3488 0.01938                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Here the data is formatted such that we have a column which specifies condition 
#and one column where all of the data is stored on a specific DV 
#This is similar to having a column for gender or condition and looking at if people differed 
#on one dependent variable.  

#Paired-Samples ANOVA 

anova_paired <- aov(log_RT ~ condition + Error(subject/condition), data = stroop_dat)
summary(anova_paired)

## Error: subject
##           Df   Sum Sq  Mean Sq F value Pr(>F)
## Residuals  1 0.005198 0.005198               
## 
## Error: subject:condition
##           Df Sum Sq Mean Sq
## condition  1 0.1359  0.1359
## 
## Error: Within
##            Df Sum Sq Mean Sq F value  Pr(>F)   
## condition   1 0.0453 0.04535   8.302 0.00472 **
## Residuals 116 0.6336 0.00546                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


#Linear Regression 
#Typically fit a linear model when we predict a linear relationship between
#one or more variables. (e.g. X, and a continuous variable, Y)
# Y = β0 + β1X + e
#Y - outcome or dependent variable 
#calculated by the individual's intercept value β0 (the score at 0 on the x-axis)
#multiplied by their value along the x-axis, and an error term e, which is the
#residual from our model 

#Residual error (or simply residuals) show the difference between our predicted 
#values from our model and observed values from our data. 

#We have the error term because we do not want to fit a perfect model to our data
#If we fit a perfect model it would work well for our sample but would not
#generalize to other samples. 
#We do not overfit our model so that it keeps explanatory power for more samples 

#Predictors can be ratio (numbers on a scale) or categorical 

#General Linear Model 
#Use this when we have outcome data that is a continuous variable 
#Not appropriate when we have bounded data (Likert scales, binomial -0 or 1- respones)
  #For these cases we need a genralised linear model 

# dependent variable ~ conditions 

lm_ind <- lm(log_RT ~ native_language, data = lexdec_ind)
summary(lm_ind)

## Call:
## lm(formula = log_RT ~ native_language, data = lexdec_ind)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.22075 -0.07276 -0.00893  0.07599  0.33469 
## 
## Coefficients:
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           6.32684    0.04197  150.75   <2e-16 ***
## native_languageOther  0.14704    0.06256    2.35   0.0304 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.1392 on 18 degrees of freedom
## Multiple R-squared:  0.2348, Adjusted R-squared:  0.1923 
## F-statistic: 5.523 on 1 and 18 DF,  p-value: 0.03037

# p-value is under "Pr(>|t|)" 
#t-value is the intercept term for our condition, divided by the standard error 
#p-value here is the proportion of the t distribution which is greater than the t-statistic
#we also get our Intercept - mean score for the English langauge group

#Comparing intercept to mean from normal descriptive 
lexdec_means <- lexdec_ind %>% 
  group_by(native_language) %>% 
  summarise(mean = mean(log_RT))
lexdec_means

##   native_language  mean
##   <chr>           <dbl>
## 1 English          6.33  matches the intercept 
## 2 Other            6.47

#Under intercept we have the difference between the means 
#Comparing the differences 
lexdec_means$mean[2] - lexdec_means$mean[1]
## [1] 0.1470395   matches native_languageOther 

#R defaults to going allphabetically to set which condition is the intercept 
#Here it doesn't matter, but with more complex models we need to pay attention 
#to how we set our contrasts in R 


#Paired-Samples Linear Model 

lm_pair <- lm(log_RT ~ condition, data = stroop_dat)
summary(lm_pair)
#we do not need to specify error with the paired-samples GLM like we did with 
#ANOVA
#This means that our model does not account for how much of the outcome variable
#is predicted by subject-specific factors (variation between people). 
#Can do a bit better with these models by fitting a mixed-effects model that can 
#account for subject level effects. 

## Call:
## lm(formula = log_RT ~ condition, data = stroop_dat)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.153786 -0.049960  0.006172  0.039194  0.202953 
## 
## Coefficients:
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          5.978613   0.009499 629.391  < 2e-16 ***
## conditionincongruent 0.077717   0.013434   5.785 6.07e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.07358 on 118 degrees of freedom
## Multiple R-squared:  0.221,  Adjusted R-squared:  0.2144 
## F-statistic: 33.47 on 1 and 118 DF,  p-value: 6.066e-08


#Matching mean scores with intercept 
stroop_means <- stroop_dat %>% 
  group_by(condition) %>% 
  summarise(mean = mean(log_RT))
stroop_means

##   condition    mean
##   <chr>       <dbl>
## 1 congruent    5.98
## 2 incongruent  6.06


#matching mean difference 

stroop_means$mean[2] - stroop_means$mean[1]

## [1] 0.07771698

#R^2 tells us about how much our explanatory predictors (condition) accounts for
#changes in the DV. 


#Assumptions of General Linear Models 
#Linear Relationship - your variables must be linearly related 
#Normality - variables must be normally distributed 
#Homoscedasticity (homogeneity of variance): residuals are equal across the 
#regression line 
#No Auto-Correlation - your observations for your DV must not impact one-another


#Generalised Linear Model 
#same form as general linear model, but uses a link function, allowing the DV
# to be a function of some error term other than the normal distribution 

#We focus on a binomial distribution 

#Preparing/Simulating Data 

set.seed(1000)
prob <- rep(c(0.3, 0.6), 50)
cond <- rep(c("control", "intervention"), 50)
subject <- seq(1:100)

smoking_dat <- tibble(
  subject = subject,
  cond = cond,
  outcome = rbinom(n = 100, size = 1, prob = prob)
)

#Looking at rates of smoking
smoking_dat %>% group_by(cond) %>% summarise(mean = mean(outcome))

##   cond          mean
##   <chr>        <dbl>
## 1 control       0.24
## 2 intervention  0.54


#Running the Test 

glm_ind <- glm(outcome ~ cond, data = smoking_dat, family = "binomial")
summary(glm_ind)
#asked to fit the model using binomial link function since our data come from a
#binomial decision and follow a binomial distribution 

## glm(formula = outcome ~ cond, family = "binomial", data = smoking_dat)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.2462  -0.7409  -0.7409   1.1101   1.6894  
## 
## Coefficients:
##                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)       -1.1527     0.3311  -3.481   0.0005 ***
## condintervention   1.3130     0.4361   3.011   0.0026 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 133.75  on 99  degrees of freedom
## Residual deviance: 124.10  on 98  degrees of freedom
## AIC: 128.1
## 
## Number of Fisher Scoring iterations: 4

#instead of t-values, we now get z-values. 
#everything else remains the same as the GLM 


#Proportion Data 

#Fitting models with proportion data for the dependent variable 
#need the number of successes and the number of observations 
#Specify model as so: 
# glm(successes/observations ~ cond, data = smoking_dat, family = "binomial")

#OR, if you just have the proportion and number of observations do this: 
# glm(proportion ~ cond, data = smoking_dat, family = "binomial", weights = observations)
#here you just specify to weight successes by the number of observations that 
#you had for each data point. 

