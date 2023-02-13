# Data Manipulation 2 

#Renaming variables
#Ordering variables 
#Preparing data to run analyses 
#Creating summaries of data 

#group_by() - allows you to change the scope by which these functions work 
  #arrange() - order variables (ordering in rows)
  #select() - pick out variables (subsetting by columns, changing column order)
  #filter() - pick out observations (subsetting by observations)
  #mutate() - create new variables 
  #summarize() - create a summary of variables (mean, SD, n) 


#can group_by(condition) and then get summary data for specific subsets of data 
#men, women, experimental, control, etc. 

#rename() -lets you rename a variable 


library(tidyverse)

demo <- read_csv("initial.learning/inputs/lexical_decision_demographic_data_wide.csv")

#won't see all the columns in the console if you have a lot them. 
#use glimpse() to look at them 

glimpse(demo)

#"<dbl>" means that the data is numeric in form 
#"chr" means that data is simply a string of characters 


#Splitting up variables that are in one column - did in last unit 

demo <- demo %>%
  separate(col = funRec, into = c("fun", "recommend")) %>%
  separate(col = completion_time, into = c("start", "end"), sep = "_")
glimpse(demo)


#First, drop any columns from the data that we don't need to use. 
#Not using any of the programming languages here so we can drop these. 

#select() picks out all of the columns that you want to keep. 
#Can do it by name or column number 

# by name
demo %>% select(ID, 
                LANGUAGE, 
                progress, 
                gender, 
                age, 
                tester, 
                fun,
                recommend,
                start,
                end
)

# by number
demo %>% select(c(1, 8:16))


#Or you can specify which columns you want dropped and use select(-xxx)


demo_sub <- demo %>% select(-c(2:7))
demo_sub

#Additional helper functions with select()
#starts_with("string") - keeps any columns with names starting with "string" 
#ends_with("string") - keeps any columns with names ending with "string" 
#contains("string") - keeps any columns with names containing "string"
#matches(regular_expression) - keeps any column that match a regular expression 
#num_range("prefix", range) - keeps any columns with a matching prefix and a 
#a following range of numbers. For example num_range("measurement", 1:3) keeps 
#measurement1, measurement2, and measurement3 


#Renaming variables (and overwriting old data set)

demo_sub <- demo_sub %>% rename(language = LANGUAGE)
demo_sub
#first word is what you want column changed to, second word is current old name 


#Putting age and gender columns next to the ID column 

demo_sub %>% select(ID, age, gender, everything())
#everything() sticks all of the remaining columns on the end 

#If we want to create a new column or change how a column is represented use
#mutate() 

#Creating a new column that subtracts the start time from the end time 

# this will not run
demo_sub <- demo_sub %>%
mutate(time = end - start)
#The start and end times are stored as characters! Not numeric digits. 


#Installing a package that helps with working with date times 

#install.packages("lubridate") # uncomment and run this only once
library(lubridate)

#ymd_hms() - converts data to different format so we can subtract 

demo_sub <- demo_sub %>%
  mutate(start = ymd_hms(start), #changes format of start column and renames it start 
         end = ymd_hms(end), #changes format of end column and renames it end
         time = end - start #creates new column by subtracting other columns 
  )
glimpse(demo_sub)
#You can refer to columns we've asked to create within mutate() 
#If you don't want to keep the old columns you can use transmute() 

# calculate difference from mean completion time
transmuted_time <- demo_sub %>%
  mutate(
    start = ymd_hms(start),
    end = ymd_hms(end),
    time = end - start
  ) %>%
  transmute(
    ID,
    time_diff = time - mean(time)
  )

# define the units of time, this can be mins, hours, days, etc.
units(transmuted_time$time_diff) <- "mins"

# print the object
transmuted_time


#Can change a column to many different data types by setting the column name to
#itself and the data type you want 
#mutate(start = as.numeric(start)) 

#Can also mutate multiple columns at once using mutate_all() or mutate_at() 
#Check out how to do this on the dplyr verbs to see how you can improve your code 


#Filtering Observations 
#filter() allows us to filter our data to certain observations 

glimpse(demo_sub)

#check for unique labels in the data by using unique() 
#useful for seeing how many response options there was to a MC question 
#Alternativley, if our data is a factor, then it should have defined labels 
#use the levels() function here 

unique(demo_sub$progress)
#We have a few labels "FINISH", "no", "END", "ethics", NA 
#Finish and End both indicate the same thing. 

#Filtering people who completed the experiment 

demo_sub %>% filter(progress == "FINISH" | progress == "END")
#remember, to check if something is equal to another we have to use double 
#equals "==" because a single "=" is an assignment operator. 
#The "|" is an OR operator. So we keep any observations that are equal to 
#FINISH or END

#Can get confusing if we have more than 2 conditions. 
#Can use the %in% operator which evaluates to TRUE if an observation is in 
#a sequence that you provide. 

demo_sub %>% filter(progress %in% c("FINISH", "END"))
#Can easily add more values in parentheses to be evaluated. 

#What if we wanted to get only those who completed the experiment and were 
#tested by "RHB", and are over the age of 30 

demo_sub %>% filter(progress %in% c("FINISH", "END"),
                    tester == "RHB",
                    age > 30
)


#Removing participants by criteria 

demo_sub %>% filter(!progress %in% c("FINISH", "END"))
# "!" is equal to "not" 
#Asking not to keep those who have FINISH or END 

#Handling NAs 
#NA = unknown value 
#R is very literally and if you ask whether something is equal to "NA" it will
#not know whether it is or not 

# this will not run
demo_sub %>% filter(language == NA)

#have to use is.na() function to check whether values are NA 

demo_sub %>% filter(is.na(language))

#filter excludes cases that don't meet your criteria or are NAs. 
#If you want to keep NAs, you have to ask explicitly. 

demo_sub %>% filter(progress == "FINISH" | progress == "END" | is.na(progress))
#Alternatively, you can filter to anything that is not an NA using "!" 

#Or can ask to not keep those that match the above criteria. 
#Throw away those who have FINISH, END, and NA 

demo_sub %>% filter(!progress %in% c("FINISH", "END") & !is.na(progress))


#We often want to order columns by the subject number, then their observation 
#arrange() takes the data and then sorts by any columns that you give it. 
#this function defaults to having the lowest numbers first. 
#So, if sorted by Subject ID and trial ID, you would get the lowest value 
#for subjects first, and their lowest number for trials first 

#Ordering IDs in descending order 
demo_sub %>% arrange(desc(ID))
#numbers typically come before letters so desc will put the letters first 

#Sorting by who tested them first, and then by who started the experiment first. 
demo_sub %>% arrange(tester, start)


#flipping order 
demo_sub %>% arrange(desc(tester), start)
#NOTE: IF WE HAVE MISSING VALUES THEY COME AT THE END OF THE TABLE 


#Reporting Summaries of Data 

#summarize() collapses across all observations in your data set to produce 
#a single row of data. 
#Within this function, we have to specify what we would like to create. 

#Calculating average time spent on the experiment. 
#Specify what the column is called and how we should create this column


demo_sub %>% summarize(mean_time = mean(time))

#"mean_time" specifies what the new column will be called. 
# "= mean(time)" specifies what operation to do to create the new column 

#adding other arguments 
demo_sub %>% 
  summarise(mean_time = mean(time),
            sd_time = sd(time),
            N = n()
  )

#What happens when there is a missing value though? 

#Introducing a missing value 
demo_sub %>% 
  mutate(time = replace(time, 1, NA)) %>%
  summarise(mean_time = mean(time),
            sd_time = sd(time),
            N = n()
  )
#The package is strict here and if there is an NA in the set, it will spit 
#NA back out at you. 
#You have to specify to remove NAs when calculating the mean and SD 

demo_sub %>% 
  mutate(time = replace(time, 1, NA)) %>%
  
  summarise(mean_time = mean(time, na.rm = TRUE),
            sd_time = sd(time, na.rm = TRUE),
            N = n()
  )
#"na.rm = TRUE" tells R that it should remove NAs when calculating values 


#Grouping Data 

demo_sub %>% 
  group_by(language) %>%
  summarise(mean_time = mean(time, na.rm = TRUE),
            sd_time = sd(time, na.rm = TRUE),
            N = n()
  )
#Unfortunately R does not reconize values as the same if they have different 
#spelling. For example, english and ENGLISH are split into different groups 

#Fixing the names 
#tolower() function to get consistent spelling

demo_sub %>% 
  mutate(language = tolower(language)) %>%
  group_by(language) %>%
  summarise(mean_time = mean(time, na.rm = TRUE),
            sd_time = sd(time, na.rm = TRUE),
            N = n()
  )

#rank() orders all scores from smallest to the largest value 


demo_sub %>% 
  mutate(language = tolower(language)) %>%
  group_by(language) %>%
  filter(rank(time) == 1) %>%
  glimpse()


#If you want to perform futher operations on a set of data after grouping it 
#you can ungroup it using ungroup() 

demo_sub %>% 
  mutate(language = tolower(language)) %>%
  group_by(language) %>%
  filter(rank(time) != 1) %>% #remove the quickest people before calculating mean
  ungroup() %>%
  summarise(mean_time = mean(time, na.rm = TRUE),
            sd_time = sd(time, na.rm = TRUE),
            N = n()
  )

#Chaining Many Functions 


# load package
library(lubridate)

# load, clean, and transform data
demo_clean <- read_csv("initial.learning/inputs/lexical_decision_demographic_data_wide.csv") %>%
  separate(col = funRec, into = c("fun", "recommend")) %>%
  separate(col = completion_time, into = c("start", "end"), sep = "_") %>%
  select(c(1, 8:16)) %>%
  rename(language = LANGUAGE) %>%
  mutate(start = ymd_hms(start),
         end = ymd_hms(end),
         time = end - start,
         language = tolower(language)
  )
#Did most of the cleaning at once here. 

#Reproducing summaries 
demo_clean %>%
group_by(language) %>%
  summarise(mean_time = mean(time, na.rm = TRUE),
            sd_time = sd(time, na.rm = TRUE),
            N = n()
  )

#Plotting the data! 

demo_clean %>%
  ggplot(mapping = aes(x = language, y = time)) + 
  geom_boxplot()

#Saving Data 

#Saving as a .csv file is good because it is compatible with lots of things 
#and allows other people to easily load your data 

#The downside of .csv is that we will lose any customized data types. 
#Good to have a backup of your data that is platform agnostic though 

#Save data to outputs folder as a csv using write_csv() 

write_csv(demo_clean, "initial.learning/outputs/filtered_demographic_data_data.manipulation.2.csv")
#specified what data object to save "demo_clean" 
#specified where we should save the file to 
#specified what we should name the file "filtered_demographic_data.manipulation.2.csv"
#must end with .csv to specify the file type 

#Can also save the data as an excel file, but generally best to keep platform 
#agnostic. 

#You can also save your data file as an RData file so you can keep using the 
#R objects and further manipulate the data more easily. 


#You don't want to run all of your pre-analysis code every time you make a 
#graph/model data. 

#Its best to clean your data and save it as a new file. 
#You can lose somethings if you save it as a .csv though so also save it as an
#RData file. 

#We can also save object types which don't play well with .csv 
#Think about nested data sets (a table of data within each cell of a larger table)

#You can save multiple objects at once if you want! 

save(demo_clean, file = "initial.learning/outputs/data.manipulation.2.RData")

#can load the file back up using load() 

load("initial.learning/outputs/data.manipluation.2.RData")

#Saving it as "RData" saves it as an original R file. 
#If I want to save it to  my RStudio I can just do "R" 







