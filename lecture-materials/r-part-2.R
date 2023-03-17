# load library
library(tidyverse)

# make sure to set the "CSB-BIOl425" as the working directory
getwd()
#setwd("../")
#getwd()
###########################
# Data visualization

popsize <- read_tsv("data_wrangling/data/FauchaldEtAl2017/pop_size.csv")

ndvi <- read_tsv("data_wrangling/data/FauchaldEtAl2017/ndvi.csv")

seaice <- read_tsv("data_wrangling/data/FauchaldEtAl2017/sea_ice.csv")

snow <- read_tsv("data_wrangling/data/FauchaldEtAl2017/snow.csv")

# converting to tidy / long table
seaice.long <- seaice %>% 
  pivot_longer(3:14, names_to = "month", values_to = "cover")
seaice.long

# Visualization

# Frequency plots (histogram: single numerical variable)
ndvi %>% 
  ggplot(aes(x = NDVI_May)) + # map a variable to x
  geom_histogram() 
#  scale_x_log10() # log transformation

# boxplots & violin plots (x is a categorical variable, y is a numerical variable); this is preferred than barplot
ndvi %>% 
  ggplot(aes(x = Herd, y = NDVI_May)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape= 1)  # add points to show sample size
#  coord_flip()

ndvi %>% 
  ggplot(aes(x = Herd, y = NDVI_May)) +
  geom_violin() +
  geom_jitter(shape= 1)  # add points to show sample size

popsize %>% 
  ggplot(aes(x= Year, y = Pop_Size, color = Herd)) + # mapping variables to graph elements: year to x, pop to y, herd to color (as 3rd dimension)
  geom_point() + # define plot (scatterplot)
  geom_line()  # add a line based on herd

# scatterplot: two numerical variables
popsize %>% 
  ggplot(aes(x= Year, y = Pop_Size)) + # mapping variables to graph elements: year to x, pop to y, herd to color
  geom_point() + # define plot (scatterplot)
  geom_line() + # add a line based on herd
  facet_wrap(~Herd)  # multi-panel plot (panel as 3rd dim)

popsize %>% 
  ggplot(aes(x= Year, y = Pop_Size)) + # mapping variables to graph elements: year to x, pop to y, herd to color
  geom_point() + # define plot (scatterplot)
  geom_smooth(method = "lm") + # add a linear regression line
  facet_wrap(~Herd) + # multi-panel plot (panel as 3rd dim)
  theme_bw()

####################
# iris data set
data(iris)
iris %>% 
  ggplot(aes(x = Sepal.Length)) + 
  geom_histogram() +
  facet_wrap(~Species) +
  scale_x_log10()

iris %>% 
  ggplot(aes(x = Species, y = Sepal.Length)) + 
  geom_boxplot() +
  geom_jitter(shape = 1) + 
  theme_bw()

iris %>% 
  ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm") + 
  theme_bw()
