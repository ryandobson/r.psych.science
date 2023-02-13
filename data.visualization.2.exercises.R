library(tidyverse)
library(cowplot)
install.packages("languageR")
library(languageR) 

# lexical decision times split by subject and item
lex_dec <- lexdec %>%
  select(Subject, NativeLanguage, Trial, Word, Class, Frequency, RT) %>%
  rename_all(tolower) %>%
  rename(native_language = nativelanguage, reaction_time = rt) %>%
  as.tibble()


# aggregate by items to explore differences in items
lex_dec_items <- lex_dec %>%
  group_by(word, class, frequency) %>%
  summarise(mean_RT = mean(reaction_time),
            sd_RT = sd(reaction_time)
  )


# ensure same values for gender and age columns
set.seed(1892)

# create demographic information
# here, I've simulated gender and age data
lex_demog <- lex_dec %>%
  distinct(subject, native_language) %>%
  mutate(age = rnorm(n = nrow(.), mean = 35, sd = 7),
         gender = sample(c("male", "female"), 
                         size = nrow(.), 
                         replace = TRUE)
  )

# Question 1 
# Use the "lex_dec_items" data set to make a density plot 
# word frequency by class. Save plot as "lexical_density" and output the plot 


lexical_density <- 
  ggplot(data = lex_dec_items,
       mapping = aes(x = frequency,
                     color = class,
                     fill = class)
)+
         geom_density(alpha = 0.5) 

lexical_density

# Question 2 
# Add a black and white theme to your plot 
# Give the axis labels and legend labels uppercase names 
# Give your legend the title "Word Class" 
#Assign it all to lexical_density_improved

lexical_density_improved <- 
  lexical_density +
    labs(x = "Frequency", 
       y = "Density"
       ) +
  guides(fill = guide_legend(title = "Word Class"),
         color = guide_legend(title = "Word Class")
  ) + 
    scale_fill_discrete(labels = c("Animal", "Plant")) + 
    scale_color_discrete(labels = c("Animal", "Plant")) +
  theme_bw()
        
lexical_density_improved 

# Question 3 
# There's some repetition in the code above. Remove it. 

# define labels 
legend_title_name <- guide_legend(title = "Word Class")
legend_label_names <- c("Animal", "Plant")

lexical_density_improved <- 
  lexical_density +
  labs(x = "Frequency", 
       y = "Density"
  ) +
  guides(fill = legend_title_name,
         color = legend_title_name
  ) + 
  scale_fill_discrete(labels = legend_label_names) + 
  scale_color_discrete(labels = legend_label_names) +
  theme_bw()

lexical_density_improved 


# Question 4 
# Want to check distributions by reactions times depending on word class 
# and language spoken 
# Create a faceted plot that looks at the density of reaction times 
# Should be a grid of densitites split by native language and word class 
# Assign this to the object rt_density
# Use lex_dec data 

rt_density <- 
  ggplot(data = lex_dec,
         mapping = aes(x = reaction_time 
                      )
         ) +
  geom_density() +
  facet_grid(native_language ~ class)

#output plot 
rt_density 


# Question 5 
# Using lex_dec_items plot the correlation between word frequency 
# and mean reaction time as a scatter plot 
# Create a fitted line and points for mean reaction time 

reactiontime.frequency.scatter <- 
  ggplot(data = lex_dec_items, 
         mapping = aes(x = frequency, y = mean_RT,
                       )
         ) +
    geom_point() +
    geom_smooth(method = "lm")
  
# output scatter plot 
reactiontime.frequency.scatter

?geom_point

# Question 6 
# Determine how many males and females 
# Use lex_demog data set 
# Create a count of the number of males and females. 
# Make all the text in the plot uppercase 
# Make the plot theme black and white 
# Assign this to the object gender_count 

gender_count <- 
  ggplot(data = lex_demog, 
         mapping = aes(x = gender)
         ) + 
  geom_bar()+
  labs(x = "Gender", y = "Count") +
  scale_x_discrete(labels = c("Female", "Male")) +
   theme_bw() 

gender_count

?geom_count
?ggplot 

# Question 7 
# Want to know the mean age and distribution of ages 
#split by language spoken and gender 
#Create a pirate plot of the ages by each gender 
#Facet your plot by native language 
# Assign to demographic_age 

demographic_age <- 
  ggplot(data = lex_demog, 
         mapping = aes(x = gender, y = age, 
         color = gender,
         fill = gender)
         ) +
  geom_pirate() + 
  facet_wrap(~ native_language)

#output graph 
demographic_age 


# Question 8 
# Want mean and SE of reaction times to words with 
# different word frequencies 
# Create a scatter plot of reaction time by word frequency 
# Use lex_dec 
# HINT: Use stat_summary function to get pointranges 
# Assign to rt_pointrange and output 


rt_pointrange <-
  ggplot(data = lex_dec, 
         mapping = aes(x = frequency, 
                       y = reaction_time)
         ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange") + 
  geom_smooth() +
  facet_wrap(~ class)


#output graph
rt_pointrange 


#Question 9 
# Using cowplot library, stitch together graphs 
# Stitch together graphs from Questions 4,6,7,8 
# Add the labels A-D to identify each plot 
# Save these plots uner the object combined_plots 

combined_plots_practice <- 
  plot_grid(gender_count, 
            demographic_age,
            rt_density,
            rt_pointrange, 
            labels = c("A", "B", "C", "D")
            ) 

combined_plots_practice
  

# define title
title <- ggdraw() +
  draw_label("The relationship between word type, word frequency, and language spoken",
             fontface = "bold")
# combine plots
combined_plots <- plot_grid(
  combined_plots,
  title,
  ncol = 1,
  rel_heights = c(1, 0.1)
)
# return plot
combined_plots
# save plot
ggsave(filename = "outputs/combined_plots.png", 
       plot = combined_plots
)

