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
  mutate(fc_pano = log2(PANO_M2_1+1) - log2(DMSO_M2_1), fpkm = log2(PANO_M2_1+1) + log2(DMSO_M2_1) ) 

x.pano %>% 
  ggplot(aes(y = fc_pano, x = fpkm)) +
  geom_point(shape = 1, alpha = 0.1) + 
  theme_bw() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_point(data = x.pano %>% filter(Gene %in% genes_of_interest), aes(x = fpkm, y = fc_pano), color = "red")


# Iris
data("iris")

summary(lm(Petal.Width ~ Petal.Length, data = iris %>% filter(Species == 'setosa')))

iris %>% top_n(1, Sepal.Length)

iris %>% top_n(-1, Sepal.Length)

iris %>% group_by(Species) %>% 
  summarise(mean(Sepal.Length))

iris.long <- iris %>% 
  pivot_longer(1:4, names_to = "feature", values_to = "size")

iris.long %>% group_by(Species, feature) %>% 
  summarise(mean(size))

# final project assignments
library(readxl)
# read name
roster <- read_excel("weigang/roster.xlsx", sheet = 2)

# create a list of projects
final_pro <- c(rep(1,5), rep(2,4), rep(3,4), rep(4,4), rep(5,4))

# create a list of roles
role <- c("bkg_question", "sample_data", "comp_method",  "stat_method", "result_conclusion")

# randomly assign 5 projects
roster <- roster <- roster %>% 
  mutate(proj = sample(final_pro))

roster %>% 
  group_by(proj) %>% 
  tally()


mutate(proj = sample(final_pro, nrow(roster), replace =T), role = sample(role, nrow(roster), replace = T))

# R/Rstudio Quiz

# Q8.23.1. plant self-incompatibility
x <- read_csv("r/data/Goldberg2010_data.csv")
x %>% 
  group_by(Status) %>% 
  tally()

x %>% 
  mutate(genus = str_split_i(Species, "_", 1)) %>% 
  group_by(genus) %>% 
  group_by(genus, Status) %>% 
  tally() %>% 
  ggplot(aes(x = genus, y = n)) +
  geom_col() +
  facet_wrap(~Status)

# Q8.23.2. mammal body mass
x <- read_tsv("r/data/Smith2003_data_2.txt", col_names = F)
colnames(x) <- c("Continent", "Status", "Order", "Family", "Genus", "Species", "LogMass", "CombinedMass", "Refrence")
x <- x %>% 
  mutate(LogMass = if_else(LogMass == -999, NA, LogMass))

x %>% 
  group_by(Family) %>%
  summarise(meanMass = mean(CombinedMass))

# Q 8.23.3 Leaf area by image processing
source("r/solutions/getArea.R") # read function & 
# then install & load two image libraries:
# library(OpenImageR)
# if (!requireNamespace ("BiocManager", quietly = TRUE))
#  install.packages ("BiocManager")
# BiocManager::install ("EBImage")
# library(EBImage)
# Also need to reset directory

t1.results <- sapply(list.files("r/data/leafarea/", "t1"), function(x) getArea(x))
t2.results <- sapply(list.files("r/data/leafarea/", "t2"), function(x) getArea(x))
t.test(t1.results, t2.results, paired = T)

# Q8.23.4 Citation analysis
x <- read_csv("r/data/Letchford2015_data.csv")
y <- x %>% filter(year == 2010)
y <- y %>% mutate(rank.length = rank(title_length), rank.cite = rank(cites))
cor.test(data = y, ~rank.length + y$rank.cite, method = "kendall", use = "pairwise")
y %>% 
  ggplot(aes(x = title_length, y = cites)) +
  geom_point(shape = 1) +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm")

x.sci <- x %>% filter(journal %in% c("Science", "Nature"))
x.med <- x %>% filter(journal %in% c("Lancet", "New Eng J Med"))

x.sci %>% group_by(journal, year) %>% 
  tally()

x.med %>% group_by(journal, year) %>% 
  tally()

x.sci %>% 
  group_by(year) %>% 
  ggplot(aes(x = title_length, y = cites)) +
  geom_point(shape = 1) +
  theme_bw() +
#  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm") + 
  facet_wrap(~year)
  
x.med %>% 
  group_by(year) %>% 
  ggplot(aes(x = title_length, y = cites)) +
  geom_point(shape = 1) +
  theme_bw() +
  #  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm") + 
  facet_wrap(~year)

# rank by year
library(broom)
rbind(x.sci, x.med) %>% 
  group_by(journal, year) %>% 
  mutate(rank.length = rank(title_length), rank.cite = rank(cites)) %>% 
  do(tidy(cor.test(data = ., ~ rank.length + rank.cite, method = "kendall"))) %>% 
  arrange(estimate)

# Q9.8.1. Life history in song birds
x <- read_tsv("data_wrangling/data/Martin2015_data.csv")
colnames(x)
x %>% 
  ggplot(aes(x=nstldpr, y=krate, color = as.factor(site))) +
  geom_point(aes(shape = as.factor(site))) +
  geom_smooth(method = "lm") +
  theme_bw()

x %>% 
  ggplot(aes(x=krate, y=nstl, color = as.factor(site))) +
  geom_point(aes(shape = as.factor(site))) +
  geom_smooth(method = "lm") +
  theme_bw()

# Q9.8.2. Drosophila wings
y <- read_csv("data_wrangling/data/Bolstad2015_data.csv")
colnames(y)
 y.mean <- y %>% 
  group_by(Species) %>% 
  summarise(mean.wing.size = mean(WingSize), mean.length = mean(L2Length))
 
 y %>% 
  ggplot(aes(x = WingSize, y = L2Length, group = Species)) +
  geom_smooth(method = "lm") + 
  facet_wrap(~Sex) +
  geom_point(data = y.mean, aes(x = mean.wing.size, y = mean.length)) + 
  theme_bw()

 # Q9.8.3. Meta-analysis of extinction rate (pretty challenging)