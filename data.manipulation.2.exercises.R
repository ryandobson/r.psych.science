library(tidyverse)
library(lubridate)

load("initial.learning/inputs/sim_data.RData")
#loads in the objects saved from the R File 

glimpse(data) 

#Start by making a more detailed plot to understand the general trands in data

#Question 1 
#Make a plot of your data that looks at the relationship between caffeine and DV
#Split this plot into two to show the differences across the two response cond. 

ggplot(data = data, mapping = aes(x = caffeine, y = DV), 
       ) + 
  geom_boxplot() + 
  facet_wrap(~response)

?ggplot
?geom_bar

#Question 2 
#Subset your data to remove the ages from the data set 

data %>% select(-age)
#If you don't assign it to a new object, you must specify this filter at the 
#start of a new argument. The data object will remained untouched really


#Question 3 
#Rename the DV to something more informative, like reaction_time 

rename(data, reaction_time = DV)

#Question 4 
#What if we care about differences in ages? 
#We predict that caffeine only has an effect on those >30 years old 
#Subset your data to just those above 30 years of age 

data %>% filter(age > 30)

#Question 5 
#Rearrange the data set by age. Starting from the highest age. 

data %>% arrange(desc(age))


#Question 6 
#Calculate mean centered scores for each subject and add these to a new 
#column called DV_c (DV, centered). 
#forumla is subjects_score - mean(all_scores)

DV_c <- data %>% summarize(DV_c = DV - mean(DV))


#Question 7 
#Assume we predict that response times should be slower above 30 years of age 
#Create a new column age_group that puts participants into two groups: 
#30_or_under and above_30 

?ifelse() 

newdata <- data %>% mutate(age_group = ifelse(age > 30, "above_30", "30_or_under"))

newdata


#Question 8 
#Calculate the mean, SD, and n for each group 

data %>% summarize(mean_reaction_time = mean(DV, na.rm = TRUE),
          sd_reaction_time = sd(DV, na.rm = TRUE),
          N = n())

data %>% 
  group_by(caffeine) %>% 
  summarize(mean_reaction_time = mean(DV, na.rm = TRUE),
            sd_reaction_time = sd(DV, na.rm = TRUE),
            N = n())

data %>% 
  group_by(response, caffeine) %>% 
  summarize(mean_reaction_time = mean(DV, na.rm = TRUE),
            sd_reaction_time = sd(DV, na.rm = TRUE),
            N = n())


#Question 10 
#Take the data and do all of this together 
# Rename the DV column to response_time 
# Remove any observations where the age of the part. is above 60 
# Combine the two columns, response and caffeine into one column called condition
  #Hint: use paste() here. Use an underscore separator for the condition names
#Remove the caffeine and response columns and reorder the data so we have: 
  #subject, age, condition, and response_time 
#Calculate the mean, SD, and n for condition on the response time column 
  #Call your new column names anything sensible 

data %>% 
  rename(response_time = DV) %>% 
  filter(age < 60) %>%  
  mutate(condition = paste(caffeine, response, sep = "_")) %>%
  select(subject, age, condition, response_time) %>% 
  group_by(condition) %>% 
  summarize(mean_RT = mean(response_time),
            sd_RT = sd(response_time),
            n = n()
            )

?paste


#Question 11 
#Do the same process without making a summary of the data. 
#Feed this into a boxplot with condition on the x-axis, and response_time on Y 
#The reason we combined the caffeine and response variable into one column 

data %>% 
  rename(response_time = DV) %>% 
  filter(age < 60) %>%  
  mutate(condition = paste(caffeine, response, sep = "_")) %>%
  select(subject, age, condition, response_time) %>% 
  group_by(condition) %>% 
  ggplot(mapping = aes(x = condition, y = response_time)) + 
  geom_boxplot()
  
  
data %>% 
  rename(response_time = DV) %>% 
  filter(age < 60) %>%  
  select(subject, age, caffeine, response, response_time) %>% 
  group_by(caffeine, response) %>% 
  ggplot(mapping = aes(x = caffeine, y = response_time)) + 
  geom_boxplot()
#if we hadn't combined caffeine and response into one variable 
#we wouldn't be able to see it split up by all 4 things 



data %>% 
  rename(response_time = DV) %>% 
  filter(age < 60) %>%  
  select(subject, age, caffeine, response, response_time) %>% 
  group_by(caffeine, response) %>% 
  ggplot(mapping = aes(x = caffeine, y = response_time)) + 
  geom_boxplot() + 
  facet_wrap(~response)
#But you can add a facet wrap to split it up. Same output but looks a bit diff 


