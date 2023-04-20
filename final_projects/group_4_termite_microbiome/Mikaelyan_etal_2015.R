# Group 4. Termite microbiome (Mikaelyan_etal_2015)

library(tidyverse)

# set working directory (yours would be different!!!)
setwd("C:/Users/weiga/Dropbox/CSB-BIOL425/final_projects/group_4_termite_microbiome/")

# make sure you're in the right directory
getwd()

# Load OTU table
otu_cts <- read_tsv("Dryad_OTUtable.txt")

otu.long <- otu_cts %>% 
  pivot_longer(2:20, names_to = "species", values_to = "count")

#############################
# To Do

# Replicate Table 1 using Excel sheet and then read into R

# Read Sheet S2 and S3 to replicate Figs 1 & 4

# count the number of OTUs found in each species

# List the OTUS that are found in four or more species

# Delete all records where count is zero

# Plot trees, to replicate Fig 5.

# Using the "vegan" package, compute Shannon diversity index for each species

# Run PCA analysis, to replicate Fig 2
