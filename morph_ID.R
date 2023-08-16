## Setup -----

setwd("~/documents/Mandeville_Lab/Phenotype_Project")

library(tidyverse)
library(ggplot2)

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## Load Data -----

df_genopheno <- read_csv("MardenAndClair.csv")
view(df_genopheno)

df_marden <- df_genopheno[df_genopheno$site == "marden_creek", ]
view(df_marden)

df_clair <- df_genopheno[df_genopheno$site == "clair_creek", ]
view(df_clair)

#Exclude non-focal species and hybrids

df_Main3WithHybrids_marden <- df_marden[df_marden$Geno_ID == "Common_Shiner" | df_marden$Geno_ID == "Creek_Chub" | df_marden$Geno_ID == "Western_Blacknose_Dace" | df_marden$Geno_ID == "Creek_Chub x Western_Blacknose_Dace" | df_marden$Geno_ID == "Western_Blacknose_Dace x Creek_Chub", ]

df_Main3WithHybrids_clair <- df_clair[df_clair$Geno_ID == "Common_Shiner" | df_clair$Geno_ID == "Creek_Chub" | df_clair$Geno_ID == "Western_Blacknose_Dace" | df_clair$Geno_ID == "Creek_Chub x Western_Blacknose_Dace" | df_clair$Geno_ID == "Western_Blacknose_Dace x Creek_Chub", ]

df_Main3WithHybrids_all <- rbind(df_Main3WithHybrids_marden, df_Main3WithHybrids_clair)
view(df_Main3WithHybrids_all)

## Means and SDs -----

# MARDEN CREEK

# Common Shiner
df_CS_marden <- df_marden[df_marden$Geno_ID == "Common_Shiner", ]

#Means
mean(df_CS_marden$anal_fin_rays)
mean(df_CS_marden$lateral_scales)
#SDs
sd(df_CS_marden$anal_fin_rays)
sd(df_CS_marden$lateral_scales)


# Creek Chub
df_CC_marden <- df_marden[df_marden$Geno_ID == "Creek_Chub", ]

#Means
mean(df_CC_marden$anal_fin_rays)
mean(df_CC_marden$lateral_scales)
#SDs
sd(df_CC_marden$anal_fin_rays)
sd(df_CC_marden$lateral_scales)


# Western Blacknose Dace
df_WBD_marden <- df_marden[df_marden$Geno_ID == "Western_Blacknose_Dace", ]

#Means
mean(df_WBD_marden$anal_fin_rays)
mean(df_WBD_marden$lateral_scales)
#SDs
sd(df_WBD_marden$anal_fin_rays)
sd(df_WBD_marden$lateral_scales)


# CLAIR CREEK

# Common Shiner
df_CS_clair <- df_clair[df_clair$Geno_ID == "Common_Shiner", ]

#Means
mean(df_CS_clair$anal_fin_rays)
mean(df_CS_clair$lateral_scales)
#SDs
sd(df_CS_clair$anal_fin_rays)
sd(df_CS_clair$lateral_scales)

# Creek Chub
df_CC_clair <- df_clair[df_clair$Geno_ID == "Creek_Chub", ]
df_CC_clair_afr <- df_CC_clair[!is.na(df_CC_clair$anal_fin_rays),]
df_CC_clair_ls <- df_CC_clair[!is.na(df_CC_clair$lateral_scales),]

#Means
mean(df_CC_clair_afr$anal_fin_rays)
mean(df_CC_clair_ls$lateral_scales)
#SDs
sd(df_CC_clair_afr$anal_fin_rays)
sd(df_CC_clair_ls$lateral_scales)

# Western Blacknose Dace
df_WBD_clair <- df_clair[df_clair$Geno_ID == "Western_Blacknose_Dace", ]
df_WBD_clair_afr <- df_WBD_clair[!is.na(df_WBD_clair$anal_fin_rays),]
df_WBD_clair_ls <- df_WBD_clair[!is.na(df_WBD_clair$lateral_scales),]

#Means
mean(df_WBD_clair_afr$anal_fin_rays)
mean(df_WBD_clair_ls$lateral_scales)
#SDs
sd(df_WBD_clair_afr$anal_fin_rays)
sd(df_WBD_clair_ls$lateral_scales)

## Histograms/Bar Plots -----

# Histogram of anal fin rays coded by group, geno then pheno ID

# MARDEN CREEK
ggplot(df_Main3WithHybrids_marden, aes(x = anal_fin_rays, fill = Geno_ID)) + 
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 binwidth = 1,
                 position = "dodge") +
  scale_fill_manual(values=cbPalette) +
  geom_vline(xintercept = (7:9)+0.5)

ggplot(df_marden, aes(x = anal_fin_rays, fill = Morph_ID)) + 
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 binwidth = 1,
                 position = "dodge") +
  scale_fill_manual(values=cbPalette) +
  geom_vline(xintercept = (7:9)+0.5)

# CLAIR CREEK
ggplot(df_Main3WithHybrids_clair, aes(x = anal_fin_rays, fill = Geno_ID)) + 
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 binwidth = 1,
                 position = "dodge") +
  scale_fill_manual(values=cbPalette) +
  geom_vline(xintercept = (7:9)+0.5)

ggplot(df_clair, aes(x = anal_fin_rays, fill = Morph_ID)) + 
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 binwidth = 1,
                 position = "dodge") +
  scale_fill_manual(values=cbPalette) +
  geom_vline(xintercept = (7:9)+0.5)


# Histogram of lateral line scales coded by group, geno then pheno ID

# MARDEN CREEK
ggplot(df_Main3WithHybrids_marden, aes(x = lateral_scales, fill = Geno_ID)) + 
  geom_histogram(colour = "black",
                 lwd = 1,
                 linetype = 1,
                 binwidth = 1) +
  scale_fill_manual(values=cbPalette)
ggplot(df_marden, aes(x = lateral_scales, fill = Morph_ID)) + 
  geom_histogram(colour = "black",
                 lwd = 1,
                 linetype = 1,
                 binwidth = 1) +
  scale_fill_manual(values=cbPalette)

# CLAIR CREEK
ggplot(df_Main3WithHybrids_clair, aes(x = lateral_scales, fill = Geno_ID)) + 
  geom_histogram(colour = "black",
                 lwd = 1,
                 linetype = 1,
                 binwidth = 1) +
  scale_fill_manual(values=cbPalette)
ggplot(df_clair, aes(x = lateral_scales, fill = Morph_ID)) + 
  geom_histogram(colour = "black",
                 lwd = 1,
                 linetype = 1,
                 binwidth = 1) +
  scale_fill_manual(values=cbPalette)


# Dorsal fin spot across species, pheno then geno ID

# MARDEN CREEK
ggplot(df_marden, aes(Morph_ID, fill = dorsal_fin_spot_present)) +
  geom_bar(colour = "black",
           lwd = 0.75,
           linetype = 1,
           position = "dodge") +
  scale_fill_manual(values=cbPalette)
ggplot(df_Main3WithHybrids_marden, aes(Geno_ID, fill = dorsal_fin_spot_present)) +
  geom_bar(colour = "black",
           lwd = 0.75,
           linetype = 1,
           position = "dodge") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values=cbPalette)

# CLAIR CREEK
ggplot(df_clair, aes(Morph_ID, fill = dorsal_fin_spot_present)) +
  geom_bar(colour = "black",
           lwd = 0.75,
           linetype = 1) +
  scale_fill_manual(values=cbPalette)
ggplot(df_Main3WithHybrids_clair, aes(Geno_ID, fill = dorsal_fin_spot_present)) +
  geom_bar(colour = "black",
           lwd = 0.75,
           linetype = 1,
           position = "dodge") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values=cbPalette)

## Boxplots -----

# shows lateral lines of each species and hybrid group

level_order <- c('Common_Shiner', 'Creek_Chub', 'Creek_Chub x Western_Blacknose_Dace', 'Western_Blacknose_Dace x Creek_Chub', 'Western_Blacknose_Dace') #order for x-axes

#MARDEN CREEK
bp_marden_ls <- ggplot(df_Main3WithHybrids_marden, aes(x=factor(Geno_ID, level = level_order), y=lateral_scales)) + 
  geom_boxplot()
bp_marden_ls + geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  xlab("Species/Hybrid Group") + ylab("Lateral scales")

#CLAIR CREEK  
bp_clair_ls <- ggplot(df_Main3WithHybrids_clair, aes(x=factor(Geno_ID, level = level_order), y=lateral_scales)) + 
  geom_boxplot()
bp_clair_ls + geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  xlab("Species/Hybrid Group") + ylab("Lateral scales")

#BOTH SITES
bp_all_ls <- ggplot(df_Main3WithHybrids_all, aes(x=factor(Geno_ID, level = level_order), y=lateral_scales)) + 
  geom_boxplot()
bp_all_ls + geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  xlab("Species/Hybrid Group") + ylab("Lateral scales")


# Creek chub ancestry x dorsal spot

#MARDEN CREEK
bp_cc_ancestry_marden <- ggplot(df_marden, aes(dorsal_fin_spot_present, Creek_Chub)) +
  geom_boxplot(colour = "black",
               lwd = 0.75,
               linetype = 1) +
  scale_fill_manual(values=cbPalette)
bp_cc_ancestry_marden + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)

#CLAIR CREEK
bp_cc_ancestry_clair <- ggplot(df_clair, aes(dorsal_fin_spot_present, Creek_Chub)) +
  geom_boxplot(colour = "black",
               lwd = 0.75,
               linetype = 1) +
  scale_fill_manual(values=cbPalette)
bp_cc_ancestry_clair + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)


## Scatterplots and Correlation Tests -----

# Fin ray x Scale Count (pheno, geno, geno with only focal species and hybrids)

#MARDEN CREEK
ggplot(data = df_marden, aes(anal_fin_rays, lateral_scales)) +
  geom_point(aes(colour = Morph_ID), size = 3) +
  scale_colour_manual(values=cbPalette)
cor.test(df_marden$anal_fin_rays, df_marden$lateral_scales)

ggplot(data = df_marden, aes(anal_fin_rays, lateral_scales)) +
  geom_point(aes(colour = Geno_ID), size = 3) 
cor.test(df_marden$anal_fin_rays, df_marden$lateral_scales)

ggplot(data = df_Main3WithHybrids_marden, aes(anal_fin_rays, lateral_scales)) +
  geom_point(aes(colour = Geno_ID), size = 3) +
  scale_colour_manual(values=cbPalette)
cor.test(df_Main3WithHybrids_marden$anal_fin_rays, df_Main3WithHybrids_marden$lateral_scales)

#CLAIR CREEK
ggplot(data = df_clair, aes(anal_fin_rays, lateral_scales)) +
  geom_point(aes(colour = Morph_ID), size = 3) +
  scale_colour_manual(values=cbPalette)
cor.test(df_clair$anal_fin_rays, df_clair$lateral_scales)

ggplot(data = df_clair, aes(anal_fin_rays, lateral_scales)) +
  geom_point(aes(colour = Geno_ID), size = 3) 
cor.test(df_clair$anal_fin_rays, df_clair$lateral_scales)

ggplot(data = df_Main3WithHybrids_clair, aes(anal_fin_rays, lateral_scales)) +
  geom_point(aes(colour = Geno_ID), size = 3) +
  scale_colour_manual(values=cbPalette)
cor.test(df_Main3WithHybrids_clair$anal_fin_rays, df_Main3WithHybrids_clair$lateral_scales)


#Ancestry x Scales

#MARDEN CREEK
ggplot(data = df_marden, aes(Western_Blacknose_Dace, lateral_scales)) +
  geom_point(aes(colour = Geno_ID), size = 3) 
cor.test(df_marden$Western_Blacknose_Dace, df_marden$lateral_scales)

ggplot(data = df_marden, aes(Creek_Chub, lateral_scales)) +
  geom_point(aes(colour = Geno_ID), size = 3) 
cor.test(df_marden$Creek_Chub, df_marden$lateral_scales)

ggplot(data = df_marden, aes(Common_Shiner, lateral_scales)) +
  geom_point(aes(colour = Geno_ID), size = 3) 
cor.test(df_marden$Common_Shiner, df_marden$lateral_scales)

#CLAIR CREEK
ggplot(data = df_clair, aes(Western_Blacknose_Dace, lateral_scales)) +
  geom_point(aes(colour = Geno_ID), size = 3) 
cor.test(df_clair$Western_Blacknose_Dace, df_clair$lateral_scales)

ggplot(data = df_clair, aes(Creek_Chub, lateral_scales)) +
  geom_point(aes(colour = Geno_ID), size = 3) 
cor.test(df_clair$Creek_Chub, df_clair$lateral_scales)

ggplot(data = df_clair, aes(Common_Shiner, lateral_scales)) +
  geom_point(aes(colour = Geno_ID), size = 3) 
cor.test(df_clair$Common_Shiner, df_clair$lateral_scales)

