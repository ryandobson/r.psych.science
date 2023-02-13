library(tidyverse)

data(chickwts)


# Question 1 
#Take a look at the first 6 rows of the data set. 
#How does the data look? 
#Is this appropriate for plotting?

head(chickwts)

# Question 2 
#Calculate the overall means for the chick weights.

mean(chickwts$weight)

# Question 3 
# Calculate the overall SD for the chick weights 

sd(chickwts$weight)

# Question 4 
# Create the basis of a ggplot by defining the chickwts 

chick.plot <- ggplot(data = chickwts)


# Question 5 
# Make a box plot of the chick weights by feed 
# Use the chick.plot object and add the function for creating a boxplot 


chick.plot + geom_boxplot( 
       mapping = aes(x = feed, y = weight)
)

# Question 6 
# Add color to your box plot by the feed type 

chick.plot + geom_boxplot( 
  mapping = aes(x = feed, 
                y = weight,
                color = feed)
)

# Question 7 
# Create a density distribution of the chick weights by feed type 
# Set different colors and fills by feed type 
# Set transparency for all distributions to 0.4 

chick.plot + geom_density(mapping = aes(x = feed, 
                                        y = weight,
                                        color = feed, 
                                        fill = feed, 
                                        alpha = 0.4)
                          )

## CORRECT RESPONSE 

chick.plot + 
  geom_density(mapping = aes(x = feed, 
                             colour = feed,
                             fill = feed
  ), 
  alpha = 0.4
  )

# Question 8 
# Make a bar plot to show the counts of each feed type 

chick.plot +
  geom_bar(mapping = aes(x = feed
                        ))

# Question 9 
# Pick 6 hex colors and put them into a variable 
# Create bar plot again, but fill the bars with colors in variable you created 



bar.colours <- c("#b2182b", 
                          "#ef8a62", 
                          "#fddbc7", 
                          "#d1e5f0", 
                          "#67a9cf", 
                          "#2166ac"
)
chick.plot + 
  geom_bar(mapping = aes(x = feed),
           fill = bar.colours
  )
                         
# Question 10 
# Make a histrogram showing overall distribution of chick weights 
# Set bin width to 50, and make bars have a white fill and black border 

chick.plot + 
  geom_histogram(mapping = aes(x = weight),
                 binwidth = 50,
                 fill = "white",
                 color = "black"
  )

