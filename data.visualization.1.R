library(tidyverse)

#a data set that came with the packages that I have downloaded from this course 
starwars

#plotting mass and height of characters against each other 

ggplot(data = starwars) + 
  geom_point(mapping = aes(x = mass, y = height))

#ggplot function always needs to take a data set. 
#ggplot(data = starwars) tells the function where it should look for data. Creates a background. 
#Next you need to add geometric objects "geoms." Because we want to look at individual points we use "geom.point()" 
#"aes" is the aesthetics of your graph. Which variables to map where. Makes it very flexible. 

#28 people were not plotted because they didn't have height and/or mass values. 
#Cleaning before plotting 

filtered_starwars <-starwars %>%
  drop_na(height, mass)

#Showing the relationship between mass and height within genders by adding color. 

ggplot(data = filtered_starwars) + 
  geom_point(mapping = aes(x = mass, 
                           y = height,
                           colour = gender
                           )
             )
#When using the filtered_starwars variable, we no longer get the warning of part. being removed because they were already removed. 

filtered_starwars %>% filter(mass > 1000)

#Code to remove Jabba the Hutt from our data before running analysis. 
#filtered_starwars <- filtered_starwars %>% filter(mass < 1000)


#Code to limit our graph without throwing out data 

ggplot(data = filtered_starwars) + 
  geom_point(mapping = aes(x = mass, 
                           y = height,
                           colour = gender)
             ) +
    coord_cartesian(xlim = c(0,180))
  
#added a limitation to the X axis! 
#This has the added advantage of keeping data in the set so if you draw a line of best fit the outlier will still be included 

ggplot(data = filtered_starwars) + 
  geom_point(mapping = aes(x = mass, 
                           y = height,
                           colour = gender), 
             alpha = 0.7,
             shape = 17,  
             size = 4)  +
  coord_cartesian(xlim = c(0,180))

alpha = 0.7, # opacity 
shape = 17, # triangles 
size = 4 + # bigger points
#added the above three variables outside of the aes so all the plot points are changed in the same way 

#If you specify the color outside of the aes on top of adding color inside, the outside will override it. 
  
  ggplot(data = filtered_starwars) +
  geom_point(mapping = aes(x = mass, 
                           y = height, 
                           colour = gender
  ),
  colour = "red") +
  coord_cartesian(xlim = c(0, 180))
  
#Can use hex color numbers to specify colors. 
#htmlcolorcodes.com 
  
ggplot(data = filtered_starwars, na.rm = T) +
  geom_point(mapping = aes(x = mass, 
                           y = height, 
                           colour = gender
  ),
  colour = "#af8dc3",
  fill = "#7fbf7b",
  shape = 21,
  size = 8,
  stroke = 3
  ) +
  coord_cartesian(xlim = c(0, 180))


# A graph that uses continuous data 

ggplot(data = filtered_starwars, na.rm = T) +
  geom_point(mapping = aes(x = mass, 
                           y = height, 
                           colour = birth_year
  )
  ) +
  coord_cartesian(xlim = c(0, 180))

#With the continuous data we get a slider with different color
#Makes it difficult to see what the birth year actually is  

??geom 



# simulating data for bar graphs- because I couldn't download data file 



###############################################################################
# Simulate Data
#
# Generates a data.frame of n participants
#  randomly sampling between male and female genders
#  and from two response conditions
#
# Generates a dependent variable (reaction_time)
#  sampling from a mean and standard deviation for each group
#  and adding noise for each group
#  
###############################################################################

# set seed to ensure we all get the same data
set.seed(1000)

# how many samples (change to your liking)?
n <- 60

# set up data format
rt_data <- data.frame(particpant = seq(1: n),
                      gender = sample(c("male", "female"), n, replace = T),
                      response_condition = c(rep("match", n/2), 
                                             rep("mismatch", n/2)
                      )
)

# create data for our dependent variable
rt_data$reaction_time <- ifelse(rt_data$response_condition == "match", 
                                rnorm(n, mean = 300, sd = 40) + # 
                                  runif(n, 0, 50),
                                rnorm(n, mean = 400, sd = 50) + 
                                  runif(n, 0, 60)
)

# save data as a .csv for both lessons
write.csv(rt_data, "../inputs/rt_data.csv", row.names = F)
write.csv(rt_data, "../../03_data_visualisation_two/inputs/rt_data.csv", 
          row.names = F
)


#read_csv versus read.csv (both are for loading data set into R)
#read_csv is faster and attempts to guess the data types for your columns of data. And loads it as a tibble 
#read.csv is the bse R function that does not do as much for you. 


getwd()

rt_data <- read.csv("initial.learning/inputs/rt_data")

#need tidyverse package loaded for read_csv function 
rt_data <- read_csv("initial.learning/inputs/rt_data")



### Box Plot 


ggplot(data = rt_data, 
       mapping = aes(x = gender)
) +
  geom_bar()

ggplot(data = rt_data, 
       mapping = aes(x = response_condition, 
                     y = reaction_time
       )
) +
  geom_boxplot()


### Violin Plot 

ggplot(data = rt_data, 
       mapping = aes(x = response_condition, 
                     y = reaction_time
       )
) +
  geom_violin(
    trim = FALSE,
    draw_quantiles = c(0.25, 0.5, 0.75)
  )

#trim = False is set to see full tails of the data 
#if set to True, the tails are trimmed to the range of the data 


ggplot(data = rt_data, 
       mapping = aes(x = response_condition, 
                     y = reaction_time
       )
) +
  geom_violin(
    trim = TRUE,
    draw_quantiles = c(0.25, 0.5, 0.75)
  )

### Density Plot 
# Good for telling whether your data are skewed
# - prior to running inferential tests 

ggplot(data = rt_data, 
       mapping = aes(x = reaction_time, 
                     fill = response_condition
       )
) +
  geom_density(alpha = 0.5) # alpha = opacity


### Histogram 

ggplot(data = rt_data, mapping = aes(x = reaction_time)) +
  geom_histogram(binwidth = 50, 
                 fill = "white", 
                 colour = "black"
  )





