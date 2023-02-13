library(tidyverse)

binom_b_data <- read_csv("https://raw.githubusercontent.com/gpwilliams/r4psych/master/lesson_materials/06_simple_statistical_tests/inputs/binom_between_data.csv")

binom_w_data <- read_csv("https://raw.githubusercontent.com/gpwilliams/r4psych/master/lesson_materials/06_simple_statistical_tests/inputs/binom_within_data.csv")

bsubj_data <- read_csv("https://raw.githubusercontent.com/gpwilliams/r4psych/master/lesson_materials/06_simple_statistical_tests/inputs/bsubj_data.csv")

wsubj_data <- read_csv("https://raw.githubusercontent.com/gpwilliams/r4psych/master/lesson_materials/06_simple_statistical_tests/inputs/wsubj_data.csv")

corr_data <- read_csv("https://raw.githubusercontent.com/gpwilliams/r4psych/master/lesson_materials/06_simple_statistical_tests/inputs/corr_data.csv")


#Question 1 
#Using the corr_data set, prepare to run a correlation for height and weight 
#Check all of the assumptions are met for a parametric test 
#Do a visual inspection of the normality of the variables 
#Check that the two are linearly related 

#calculating descriptives 
corr_data %>% summarize_at( 
  c("weight", "height"),
  list(mean = mean, sd = sd)
       )

#checking assumptions 

corr_data %>% ggplot(aes(sample = height)) + 
  geom_qq()

corr_data %>% ggplot(aes(sample = weight)) + 
  geom_qq()

shapiro.test(corr_data$weight)
## data:  corr_data$weight
## W = 0.96969, p-value = 0.141


shapiro.test(corr_data$height)
## data:  corr_data$height
## W = 0.98611, p-value = 0.7277


#Plotting the points to check for correlation visually 
corr_data %>% 
  ggplot(aes(x = weight, y = height)) +
   geom_point() +
  geom_smooth(method = "lm")


#Question 2 
#Using information from question 1, run a correlation between height and weight
#adjust to a non-parametric test if necessary (not necessary)

cor.test(corr_data$height, corr_data$weight,
         method = "pearson")
# r = .98 
# R^2 = .96 


#Question 3 
#Aggregate the scores of the bsubj_data set by subject as an object called 
#bsujb_agg 
#Output the mean scores aggregated by subject 
#Then run a t-test between the two groups of A. Is there a sig diff? 

bsubj_agg <- bsubj_data %>% 
  group_by(subj_id, A) %>% 
  summarize(Y = mean(Y))
bsubj_agg

t.test(Y ~ A, data = bsubj_agg, paired = FALSE)

#yes, there is a sig diff. P > .001, t = -57.145 

#Question 4 
#Check that a t-test was appropriate for question 3. 
#Check homogeneity of variance. 

bartlett.test(Y ~ A, data = bsubj_agg) 
#yes a t-test was appropriate 
#bartless was non-sig 


#Question 5 
#Aggregate the wsubj_data by subjects and save this as wsubj_agg 
#Then submit this aggregated data to an ANOVA, saving the output as the object 
#wsubj_aov 

wsubj_agg <- wsubj_data %>% 
  group_by(subj_id, A,) %>%
  summarize(Y = mean(Y))

wsubj_aov <- aov(Y ~ A + Error(subj_id/A), data = wsubj_agg)
summary(wsubj_aov)


#Question 6 
# Make a plot of the two conditions from question 5 using wsubj_agg 
# Output a table of means, sd, and se for the two groups 
#To calculate the SE you need to take the SD of the DV divided by the sqrt of
#numbers of observations for the dependent variable 
# sd(output)/squrt(length(output)) 

ggplot(data = wsubj_agg, mapping = aes(x = A, y = Y)) + 
  geom_boxplot()

wsubj_agg %>% 
  group_by(A) %>% 
  summarize(mean = mean(Y),
            sd = sd(Y),
            se = sd(Y)/sqrt(length(Y))
            )

#Question 7 
#Fit a linear model to the wsubj_agg and save this as wsubj_lm 

wsubj_lm <- lm(Y ~ A, data = wsubj_agg) 
summary(wsubj_lm)


#Question 8 
#Fit a linear model to the binom_b_data, save it as binom_b_lm 
#Look at the summary output 

binom_b_lm <- glm(outcome ~ cond, data = binom_b_data, family = "binomial")
summary(binom_b_lm)
#sign at the p < .05 level 

#Question 9 
#Fit a linear model to the binom_w_data, save it as binom_b_lm 
#You will first have to convert the data to long format in order to run this 
 #For this, set the key to "test_time", and the value to "success" 

binom_w_data <- binom_w_data %>% 
  gather(key = "test_time", value = "success", 2:3)

binom_w_lm <- glm(success ~ test_time, data = binom_w_data, family = "binomial")
summary(binom_w_lm)


#Question 10 
#Using the long-formatted binom_w_data from question 9, create a table 
#containing the means, sd's, se's and 95% CI's for the two test times. 
#To calculate the upper and lower bounds of CI's you will have to take the mean
#of the DV - 1.96 times the SE, and the mean of the DV + 1.96 times the SE. 
 #call these columns lower_confint and upper_confint 
#Last, plot a var plot of this data with standard error bars 

binom_w_data %>% 
  group_by(test_time) %>% 
  summarize(mean = mean(success),
            sd = sd(success),
            se = sd(success)/sqrt(length(success)),
            lower_confint = mean - 1.96 * se,
            upper_confint = mean + 1.96 * se
            )

ggplot(data = binom_w_data, mapping = aes(x = test_time,
                                          y = success)) +
  stat_summary(fun.y = "mean", geom = "bar") + 
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.25)








