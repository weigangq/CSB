# Group 1. Transcriptome data analysis (He_etal2021)

library(tidyverse)
library(stringr)

# bulk mRNA
x <- read_csv("https://raw.githubusercontent.com/julianstanley/QMW2023_BulkRNASeq/main/data/full_data_S4_He2021.csv")
colnames(x)
x.long <- x %>%
  pivot_longer(2:10, names_to = "cell_treat", values_to = "FPKM")

x.long <- x.long %>%
  mutate(treat = str_split_fixed(cell_treat, "_",2), cell = str_split_fixed(cell_treat, "_", 2))

x.long %>%
  ggplot(aes(x=FPKM)) +
  geom_histogram(bins = 100) +
  scale_x_log10() +
  facet_wrap(~cell_treat)

m2_anti_inflam_marker <- 'PPARG'
m1_pro_inflam_marker <- 'CCR5'

marker_genes <- c('PPARG', 'CCR5')
genes_of_interest <- c("IL1B", 'PDGFB', 'ICAM1', 'MYC')

x.long %>%
  filter(Gene %in% marker_genes) %>%
  ggplot(aes(x = cell, y = FPKM, color = Gene)) +
  geom_point()

x.long %>%
  filter(str_detect(cell_treat, "M2")) %>%
  filter(Gene %in% genes_of_interest) %>%
  ggplot(aes(x = cell_treat, y = FPKM)) +
  geom_col() +
  theme_bw() +
  #  scale_y_log10() +
  facet_wrap(~Gene)

# identify genes down-regulated by PANO treatment in M2 cells
# "Next, we assessed genes associated with IL-4-induced M2-type polarization whose expression is inhibited by trametinib and/or panobinostat. The expression of a subset of those IL-4-induced genes inhibited by these small molecules is required for M2-type polarization because their inhibition by trametinib and panobinostat was linked to a block in M2-type polarization. "
# "Genes inhibited by both panobinostat and trametinib >2-fold in IL-4-treated THP-1-derived macrophages included MYC, MMP1, MRC1, the adhesion molecules ICAM1 and VCAM1, and key genes of PPAR??/RA signaling, including PPAR??, ALDH1A2, RDH10, DHRS9, and ANGPTL4 (Figure 4E). "

x.pano <- x %>%
  mutate(fc_pano = log2(PANO_M2_1+1) - log2(DMSO_M2_1+1), fpkm = log2(PANO_M2_1+1) + log2(DMSO_M2_1 + 1) )

x.pano %>%
  ggplot(aes(y = fc_pano, x = fpkm)) +
  geom_point(shape = 1, alpha = 0.1) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(data = x.pano %>% filter(Gene %in% genes_of_interest), aes(x = fpkm, y = fc_pano), color = "red")

#############################
# To Do

# Table S1. Global proteomics. Plot heatmap in Fig 1

# Table S2. Proteomics time-course, heatmap in Fig 2

# Table S4
# Identify differentially expressed genes between:
## DMSO vs panobinostat treatment
## DMSO vs trametinib treatment

# An analysis was performed to assess M2-type polarization-associated genes whose expression was inhibited by trametinib and/or panobinostat. 

## Expression changes for genes with a log2 ratio of DMSO_M2 over DMSO_M0 larger than one (log2 [DMSO_M2/DMSO_M0] > 1) were regarded as significantly upregulated in the M2-type group. 

##To identify genes whose expression was inhibited by trametinib and/or panobinostat > 2-fold compared to DMSO groups, the log2 ratio of DMSO_M2 over Trametinib_M2 or Panobinostat_M2 > 1 were assessed.

# Principal component analysis (PCA) used transcripts that have 10 or more reads in at least 2 samples. The data for these genes were log2 transformed, normalized to mean zero across the conditions, and the vector for each condition (treatment) converted to z-scores. The right singular vectors of these matrices were computed as principal components of the data.
