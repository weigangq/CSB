# Group 1. Transcriptome data analysis (Marra2014)

# DESeq Workflow:
# http://bioconductor.org/packages/devel/bioc/vignettes/DESeq2/inst/doc/DESeq2.html

# A simpler example:
# https://lashlock.github.io/compbio/R_presentation.html

# set working directory (yours would be different!!!)
setwd("C:/Users/weiga/Dropbox/CSB-BIOL425/final_projects/group_1_rodent_transcriptomes/")

# make sure you're in the right directory
getwd()

# install DESeq2 (run once only)
#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#library(BiocManager)
#BiocManager::install("DESeq2")

# Answer "no" to the question "Do you want to install from source"

# load library
library(tidyverse)
library(DESeq2)

# read count data
rna_cts <- read.csv("Marra2014_count_table_spleen.tsv", sep = "\t", row.names = "gene_id")
rna_cts <- as.matrix(rna_cts)
colnames(rna_cts)

# create a dataframe of sample info
col_data <- data.frame(
  row.names=colnames(rna_cts),
  condition=c("desert", "desert", "desert", "desert", "desert", "desert", "desert", "desert","mesic","mesic","mesic", "mesic"),
  species=c("Dipodomys", "Dipodomys", "Dipodomys", "Dipodomys", "Chaetodipus", "Chaetodipus", "Chaetodipus", "Chaetodipus", "Heteromys", "Heteromys", "Heteromys", "Heteromys")
)

# Construct a DESeq data set
de_obj <- DESeqDataSetFromMatrix(
  countData = rna_cts,
  colData = col_data,
  design = ~ condition)

# Run differential gene expression analysis
dds <- DESeq(de_obj)

# get results
res <- results(dds)
resSig <- subset(res, padj < 0.1)
########################
# To Do

# plot count distributions: with & without log transformation, is it necessary to do log transformation? why?

# plot boxplots by samples

# sort by p value and pick to top 100 genes. Which are up-regulated, which are down-regulated?

# make MA plot & explain

# make volcano plot & explain

# make heatmap & explain

# For your final report: Compose an R Markdown file, including all your commands, graphs, outputs, and explanations/captions/references

