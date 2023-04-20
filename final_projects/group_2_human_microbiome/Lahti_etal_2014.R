# Group 3. Microbiome data analysis (Lahti_etal_2014)

# set working directory (yours would be different!!!)
setwd("C:/Users/weiga/Dropbox/CSB-BIOL425/final_projects/group_2_human_microbiome/")

# make sure you're in the right directory
getwd()

# load library
library(tidyverse)

# read meta data
meta <- read_tsv("Metadata.tab")

# To Do:
# plot age distribution
# plot nationality counts
# plot diversity distribution (and explain)
# plot BMI counts
# select subjects with more than one time point

# read count data
seq_cts <- read_tsv("HITChip.tab")
colnames(seq_cts)

# transform to a tidy/long table
ct.long <- seq_cts %>% 
  pivot_longer(2:131, values_to = "count", names_to = "genus")

# Replicate Fig 1.
bimodal_groups <- c( "Bacteroides fragilis et rel.",   "Prevotella melaninogenica et rel.", "Dialister", "Prevotella oralis et rel.",  "Uncultured Clostridium (sensu stricto)les I" , "Uncultured Clostridium (sensu stricto)les II")

ct.bimodal <- ct.long %>% 
  filter(genus %in% bimodal_groups)

ct.bimodal %>% 
  ggplot(aes(x = count)) +
  geom_histogram() +
  facet_wrap(~genus) +
  scale_x_log10()

# To Do

# Replicate Fig 3. Temporal variation (using subjects with more than one time points)

# Replicate Fig 6. Heatmap of combination

# Replicate Fig 7. Heatmap of covariation

