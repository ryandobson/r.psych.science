# load the tidyverse
library(tidyverse)

# demographic data
ex_demo_data <- tibble(
  subject = seq(1: 6),
  height = c(NA, 170, 160, 165, NA, 180),
  weight = c(70, 65, 80, NA, 77, 90),
  age = c(18, 19, 19, NA, 22, 28)
)

# IQ test scores
ex_test_data <- tibble(
  subject = c(1, 3, 4, 4, 5, 6, 7),
  IQ = c(150, 160, 155, 155, 190, 120, 140)
)


# Question 1 
# Put the ex_demo_data into a long format with three columns 
# subject = subject number
# measurement = height, weight, and age 
# measurement_id = text specifying which measurement belongs to which variable 
#(height, weight, or age_.)
# Assign this to the variable long_data 

long_data <- ex_demo_data %>%
  gather(
    key = measurement_id,
    value = measurement,
    2:4
  )

#Run output 
long_data



#Question 2
#Turn your new long_data back into a wide format 

wide_data <- long_data %>% 
  spread( 
    key = measurement_id,
    value = measurement,
  )

#Run output 
wide_data 


#Uniting and separating columns 
#two values for two variables in one column "height_weight"

messy_demo_data <- unite(ex_demo_data, 
                         "height_weight", 
                         c("height", "weight"), 
                         sep = "_"
)
messy_demo_data

# Question 3 
#Separate the messy columns into two tidy columns for height and weight 

ex_tidy_data <- messy_demo_data %>% 
  separate(
    col = height_weight,
    into = c("height", "weight"),
    sep = "_",
    convert = TRUE
  )

#Run output 
ex_tidy_data 

?separate



#Question 4
#Join the ex_demo_data and ex_test_data together by subject number 
#Keeping only data with a match in ex_test_data 

ex.right_join <- right_join(ex_demo_data, ex_test_data, by = c("subject"))

#Run output 
ex.right_join
                            
  
#Question 5 

ex.left_join <- left_join(ex_demo_data, ex_test_data, by = c("subject"))

#Run output 
ex.left_join

# Question 6
# Why do we get different results in question 4 and 5? 
# Basically just ordered differently based upon what its selecting first 

# Question 7 
# Return all of the values from ex_demo_data that have a match in 
#ex_test_data. Look at subject 4. Why do we get different results
#than in question 5? Why do the columns returned differ from question 5? 


semi_join(ex_demo_data, ex_test_data, by = c("subject"))

#semi_join only gives us data that matches from BOTH sets 


#New Data for next questions 

new_demographics <- tibble(
  subject = c(9, 10),
  height = c(170, 190),
  weight = c(76, 85),
  age = c(40, 59)
)

eye_colour <- tibble(
  eye_colour = sample(c("blue", "brown", "green"), 
                      size = 8, 
                      replace = TRUE
  )
)


#Question 8 
#Add the rows from new_demographics to ex_demo_data 
#Assign this to all_demo_data and return this table 

all_demo_data <- bind_rows(new_demographics, ex_demo_data,)

#Run output 
all_demo_data


#Question 9 
#Add the eye color column to the all_demo_data table 
#We do not have a subject identifier in the eye_color data set 
#What would happen if we did have this information? 
# would likely get a second subject identifier column 

all_demo_data_2 <- bind_cols(all_demo_data, eye_colour)

#Run output 
all_demo_data_2


#New data for next question 

extra_test_data <- tibble(
  subject = c(1, 9, 10),
  IQ = c(150, 156, 179)
)

#Question 10 
# Return rows with duplicates from the ex_test_data and extra_test_data 

intersect(ex_test_data, extra_test_data)

