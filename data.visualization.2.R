library(tidyverse)

getwd()

rt_data <- read_csv("initial.learning/inputs/rt_data")


#functional but not super great density plot 

ggplot(data = rt_data, 
       mapping = aes(x = reaction_time, 
                     fill = response_condition
       )
) +
  geom_density(alpha = 0.5)

#an improved density plot 

ggplot(data = rt_data, 
       mapping = aes(x = reaction_time, 
                     fill = response_condition
       )
) +
  geom_density(alpha = 0.5) +
  labs(x = "Reaction Time (ms)", 
       y = "Density"
  ) +
  scale_x_continuous(limits = c(200, 600), 
                     breaks = seq(from = 200, 
                                  to = 600, 
                                  by = 100
                     )
  ) +
  guides(fill = guide_legend(title = "Response Condition")) +
  theme_bw()


#an improved histogram 

ggplot(data = rt_data, 
       mapping = aes(x = reaction_time)) +
  geom_histogram(binwidth = 50, 
                 fill = "#bcbddc",
                 colour = "#756bb1"
  ) +
  scale_x_continuous(limits = c(200, 600),
                     breaks = seq(from = 200, 
                                  to = 600, 
                                  by = 25
                     )
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Reaction Time (ms)", y = "Count") +
  theme_classic()

#expand indicates how much extra space should be on the top and bottom 
#here we specify there is no extra space 

# PIRATE PLOTS 
# these show individual points, a bar plot, CIs, and a violin plot all in one. 
# can see raw, descriptive, and inferential data on one plot! 

# Good for data that are grouped by categories but with a continuous DV 

install.packages("devtools")
devtools::install_github("mikabr/ggpirate")
library(ggpirate)

1

ggplot(data = rt_data, 
       mapping = aes(x = response_condition, 
                     y = reaction_time,
                     colour = response_condition,
                     fill = response_condition)
) +
  geom_pirate() +
  labs(x = "Motor Skill", y = "Reaction Time (ms)") +
  scale_x_discrete(labels = c("Impaired", "Intact")) +
  scale_y_continuous(limits = c(0, 600), 
                     breaks = seq(from = 0, to = 600, by = 100)
  ) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  scale_colour_manual(values = c("#af8dc3", "#7fbf7b")) +
  scale_fill_manual(values = c("#af8dc3", "#7fbf7b"))

#"labs" allows you to manually change the x and y axis labels 
#"scale_x_discrete" allows you to manipulate your x scale 
# within this, use "labels" to change to the names 
#"scale_y_continuous" allows you to manipulate your y scale 
# within this, set "limits" and "breaks" to increment the "seq" 
#"theme_bw()" is a black and white theme that is good 
#"theme" allows you to specify other aspects of how your plot should look 
#"panel.grid.major.x" set to "element_blank()" removes vertical lines 
#define color using manual hex values "scale_color_manual()" 
# and "scale_fill_manual()" 

#faceting - good way to display dating when you have multiple categorical variables 
# you get a plot for each category in your data 

# Create facets from one variable using "facet_wrap()"

ggplot(data = rt_data, mapping = aes(x = reaction_time)) +
  geom_histogram(binwidth = 50, 
                 fill = "white", 
                 colour = "black"
  ) +
  facet_wrap(~ response_condition)

# Splitting data by multiple facets using "facet_grid()" 

ggplot(data = rt_data, mapping = aes(x = reaction_time)) +
  geom_histogram(binwidth = 50, 
                 fill = "white", 
                 colour = "black"
  ) +
  facet_grid(gender ~ response_condition)
#the order you specify the grid matters for how it lays out the graphs 


# Means and Error Bars 

# bar plot with error bars using "stat_summary()"

ggplot(data = rt_data, 
       mapping = aes(
         x = response_condition,
         y = reaction_time, 
         fill = response_condition
       )
) + 
  stat_summary(fun.y = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.25)

#run the "fun.y = "mean" to tell R to mean over our data 
#that make up the y-axis 
#change width to .25 to make it 1/4 the size of the total bar. 

ggplot(data = rt_data, 
       mapping = aes(
         x = response_condition,
         y = reaction_time, 
         fill = response_condition
       )
) + 
  stat_summary(fun.y = "mean", geom = "point") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.25)
#changing the geam to "point" removes the bars and just gives SE 

ggplot(data = rt_data, 
       mapping = aes(
         x = response_condition,
         y = reaction_time, 
         fill = response_condition
       )
) + 
  stat_summary(fun.y = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 1)
#example of what width does to the error bar 

#He doesn't often use "stat_summary" because you don't know exactly how it calculates the SE 
#Relevant for when you have within versus between subject designs 
#"stat_summary" is useful if you want a fast plot and to avoid writing extra code 


# MODEL FITS 

starwars 

# remove NA and mass above 1000
filtered_starwars <- starwars %>% 
  drop_na(height, mass) %>% 
  filter(mass < 1000)

# plot
ggplot(data = filtered_starwars, 
       mapping = aes(x = mass, y = height)
) + 
  geom_point() + 
  geom_smooth()

#"geom_smooth()" fits a model to the data 
# defaults to a "loess method," but we can change it to linear (or others)
#by default these plots display a ribbon displaying 95% CI

ggplot(data = filtered_starwars, 
       mapping = aes(x = mass, y = height)
) + 
  geom_point() + 
  geom_smooth(method = 'lm', formula = y ~ x)
#here we fit a linear model to the data
# you can also fit a quadratic model to the data "y ~ poly(x, 2)"
# "poly(x, 2)" calculates orthogonal polynomials to a certain degree
# or a cubic model 
#You can also use a fitted model from your saved analyses using "stat_summary"

data(ChickWeight)

head(ChickWeight)

#if you want to understand a data set down the road - use good names


chick_time_plot <- ggplot(data = ChickWeight, # data
                          mapping = aes(x = Time, # x-axis
                                        y = weight, # y-axis
                                        colour = Diet, # colour ID
                                        fill = Diet # fill ID
                          )
) +
  stat_summary(fun.y = mean, geom = "point") + # point means
  geom_smooth(method = 'lm', formula = y ~ x) # linear fit
#saving our plot as an object so we can build it up bit by bit 

#we just return the plot to see it 
chick_time_plot

#adding a variety of details to the original plot 

chick_time_plot <- chick_time_plot +
  geom_hline(yintercept = 150, linetype = 2) + # horizontal line
  annotate(geom = "text", x = 2, y = 158, label = "Target Weight") # line label
#note, we are overwriting the original chart here 

# return the plot
chick_time_plot


#improving the general look of the graph 
#overwriting it again 

chick_time_plot <- chick_time_plot +
  labs(x = "Trial", y = "Weight") + # axis labels
  coord_cartesian(ylim = c(0, 300)) + # range of y-axis
  theme_bw() + # general theme
  theme(legend.position = c(x = 0.15, y = 0.8), # position of legend
        legend.background = element_rect(color = "black",
                                         size = 1,
                                         linetype = "solid"
        ) # legend styling
  ) +
  guides(colour = guide_legend("Experimental Diet"),
         fill = guide_legend("Experimental Diet")
  ) # legend title

# return the plot
chick_time_plot

#You can use a general theme and then specify a few further details 
#Must specify further details after theme so the total theme doesn't overwrite 


# COMBINING PLOTS

install.packages("cowplot")
library(cowplot) # run this each time

# To stitch plots togther you have to save them as objects 

# create a plot of points
point_plot <- ggplot(data = filtered_starwars, 
                     mapping = aes(x = mass, y = height)
) +
  geom_point() +
  coord_cartesian(ylim = c(0, 300))

# create a linear plot by adding the fit to the plot of points
linear_plot <-  point_plot + 
  geom_smooth(method = 'lm', formula = y ~ x)


# create a quadratic plot by adding the fit to the plot of points
quadratic_plot <- point_plot + 
  geom_smooth(method = 'lm', formula = y ~ poly(x, 2)) # fit quadratic

# return the plots
quadratic_plot

point_plot

linear_plot

#You can save a base graph as an object and then add things as needed 


#Combining the graphs! 
combined_plots <- plot_grid(linear_plot, 
                            quadratic_plot, 
                            labels = c("A", "B"), # label plots
                            align = "h" # align axes
) 
combined_plots

#Naming and adding some details to the combined graphs 

# create title
title <- ggdraw() + 
  draw_label(paste("Linear (A) and Quadratic",
                   "(B) fits of Height and Weight"
  ), 
  fontface = "bold"
  )

# print plot as a grid
combined_plots <- plot_grid(combined_plots, 
                            title, 
                            ncol = 1, #specifys we want 1 column so plot and title stack 
                            rel_heights = c(1, 0.1) #specify size of title relative to plot 
)

# return the combined plots
combined_plots


#cowplot comes with some different default themes 
#can reset default styles by doing: 
#theme_set(theme_gray())

#Saving your plot! Best to save as PNG -JPEG is fine but has occasional issues 

ggsave(filename = "initial.learning/outputs/starwars.mass.by.hieght.png", 
         plot = combined_plots
       )



