library(tidyverse)

# Question 1: What is wrong with the following code? 

# create a sequence from 0 to 40 in intervals of 2
sequence < - seq(from = 0, to = 40, by = 2)

# There cannot be a space between < and - 
sequence <- seq(from = 0, to = 40, by = 2)


# Question 2: Why doesn't this return anything? 

# draw 100 times from a uniform distribution between 1 and 10
uniform_distribution <- runif(n = 100, min = 1, max = 10)
head(uniform_distrbiution)


#Question 3: Lets pretend we have 100 participant. 
#Create a variable that will store participant IDs.
#Their IDs can be anything you like, but some sort of logical numbering looks best.
#Hint: seq() is your friend here!


mainSequence <-seq(from = 1, to = 100, by = 1)



#Question 4: Create a variable that will store their scores on some test.
#Lets assume participant scores are drawn from a normal distribution
#Mean of 10 and SD of 0.75 

#hint: rnorm() is a wonderful thing 

test.score <- rnorm(n =100, mean = 10, sd = 0.75)



#Question 5: Create some ages for your participants

#age <- sample(mainSequence, replace = FALSE, prob = NULL)

age <- sample(18:65, 100, replace = TRUE)

#Question 6: Create a data frame consisting of your participant IDs, their ages, and test scores 

participantData <- tibble::tibble(
  pariticpantID = mainSequence,
  age = age,
  test.score = test.score
)


participantDataFrame <- data.frame(mainSequence, age, test.score)

#Question 7: Take a look at the start and end of your data frame

head(participantData)
tail(participantData)


#Question 8: Access the 20th row (and all columns). 

participantData[20, ]

#Question 9: Access just the test score for row 73

participantData[73, "test.score"]


#Question 10: Output simple descriptive and save them as a data frame to use later


age.mean <- mean(age)

test.mean <- mean(test.score)

test.sd <- sd(test.score)

descriptives <- data.frame(age.mean = mean(age),
                          test.mean = mean(test.score),
                          test.sd = sd(test.score)
  )

#This second version is obtaining the results of the calculations by looking at the ParticipantData variable. 
#Because I did not update the participantData variables age, the mean for age is higher. 

descriptives.2 <- data.frame(age.mean = mean(participantData$age),
                           test.mean = mean(participantData$test.score),
                           test.sd = sd(participantData$test.score)
)

#Question 11: Access all rows and columns where the test score is greater than 11 
#*Hint*: define the column as `participantDataFrame$column` in your subsetting command, and perform a logical operation on this.

participantDataFrame[participantDataFrame$test.score > 11, ]


#Question 12: Access all rows from the mainSequence and test.scores columns where the test score is greater than 11 
#*Hint*: use the `c()` function to select multiple columns.

participantDataFrame[participantDataFrame$test.score > 11, c("mainSequence", "test.score")]

