# Part 1. Data wrangling & visualization

###############################
# 1. load library, set working directory, read data
##############################
# load library
library(tidyverse)

# show working directory (pwd in UNIX)
getwd()

# set working directory
# setwd("C:/Users/weiga/Dropbox/CSB-BIOL425/")

# read table into a tibble (smart table)
popsize <- read_tsv("data_wrangling/data/FauchaldEtAl2017/pop_size.csv")
popsize
head(popsize)
tail(popsize)
glimpse(popsize)
ncol(popsize)
nrow(popsize)

#########################
# 2. data filter
######################

# read vegetation data
ndvi <- read_tsv("data_wrangling/data/FauchaldEtAl2017/ndvi.csv")
glimpse(ndvi)

# SELECT columns (no quotes needed) 
# convention for table layouts in data science: 
# columns are variables; rows are samples
select(ndvi, Herd, NDVI_May)
select(ndvi, c(1,3)) # c() is the concatenation function
select(ndvi, -c(1,3)) # remove columns

# FILTER rows
filter(popsize, Herd == 'WAH') # filter by character column
filter(popsize, Year >= 1970 & Year <= 1980) # filter by a numeric col
filter(popsize, Year %in% c(1970, 1980, 1990)) # in operator
slice(popsize, 20:30) # consecutive rows

# top rows
top_n(popsize, 10, Pop_Size) # pick top 10 by pop_size
top_n(popsize, 10, desc(Pop_Size)) # pick bottom 10 by pop_size
sample_n(popsize, 5) # pick 5 random rows
sample_frac(popsize, 0.02)

# USE pipes to serialize commands/operations
# get unique Herd; hard syntax
# keyboard shortcut: Ctl-Shift-M
unique(popsize[order(popsize$Herd),1])

distinct(select(popsize, Herd))

# sort by a column
arrange(distinct(select(popsize, Herd)), Herd)

# use PIPE (Recommended syntax)
heads <- popsize %>%  # data to operate on
  select(Herd) %>% # select a column
  distinct() %>% # get distinct values
  arrange(Herd) # sort by Herd alphabtically

# Intermezzo 9.1

# Q1
popsize %>%  # load the data table
  select(Year) %>% # select a column
  arrange()  # sort the column, smallest first

popsize %>%  # load the data table
  select(Year) %>% # select a column
  arrange(desc(Year))  # sort the column, largest first

# Q2
ndvi %>% 
  top_n(10, NDVI_May) # largest NDVI_May

ndvi %>% 
  top_n(10, desc(NDVI_May)) # smallest NDVI_May

# Q3
popsize %>% 
  filter(Herd == 'WAH') %>% 
  arrange(Pop_Size) %>% 
  tail(3)

popsize %>% 
  filter(Herd == 'WAH') %>% 
  arrange(desc(Pop_Size)) %>% 
  head(3)

###############
# 3. renaming & add columns

popsize %>% 
  rename(h = Herd)

# add a computed column
ndvi %>% 
  mutate(meanNDVI = (NDVI_May + NDVI_June_August)/2)

# summarise to calculate summary stats for columns
ndvi %>% 
  summarise(mean_May = mean(NDVI_May), sd_May = sd(NDVI_May))

# group by (********; groups samples by a category)
popsize %>% 
  group_by(Herd) %>% 
  summarise(mean_May = mean(Pop_Size), sd_May = sd(Pop_Size)) %>% 
  arrange(mean_May)

popsize %>% 
  group_by(Herd) %>% 
  tally() %>% 
  arrange(n)

# Intermezzo 9.2


