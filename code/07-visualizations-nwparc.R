# -------------------------------------------------------------------------------------------------------
##
## 07-visualizations-nwparc.R 
##
## Visualizations of habitat variables across treatments 
##
## Jasmine Williamson
## Date Created: 03-19-2025
##

## goals 
# pairwise comparisons of significant variables from ANOVA and Kruskal-Wallis
# boxplots for those variables showing significance between treatments

## insights 
# 


## settings -----------------------------------------------------------------------------------------------

    rm(list=ls())
    setwd("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data")
    library(tidyverse)
    library(ggpubr)
    library(ggplot2)

## load data ----------------------------------------------------------------------------------------------

    dat <- read.csv("covariate matrices/env_subset_corr.csv", row.names = 1)
    dat2 <- readRDS("covariate matrices/site_level_matrix.rds") #need this for the treatments
    row.names(dat2) <- dat2[,1]
    dat2$trt <- as.factor(dat2$trt)
    
    dat2$trt <- factor(dat2$trt, levels = c("BS", "BU", "HB", "HU", "UU"))

## ANOVA variables:
    # dwd count
    # decay class
    
## KW variables:
    # canopy cover
    # fwd cover
    # veg cover
    
    
                        
## dwd boxplot  ------------------------------------------------------------------------------------------
    
    
  ## Make a list of the boxplot levels you want compared
    list_comparisons <- list(c("BU", "BS"),
                             c("UU", "BS"),
                             c("HB", "BU"),
                             c("HU", "BU"),
                             c("UU", "HB"),
                             c("UU", "HU"))
    

  # fancy boxplot
    
   box.colors <- c(
     "UU" = 'lightgreen',
     "BU" = 'steelblue',
     "HB" = 'coral2',
     "HU" = '#f9d62e',
     "BS" = '#b967ff'
   )
   
   ggplot(dat2, aes(y = trt, x = dwd_count, fill=trt)) +
      geom_boxplot(linewidth = 0.7) +
      scale_fill_manual(values=box.colors) +
      stat_compare_means(method = "wilcox.test", comparisons = list_comparisons, label = "p.signif",
                         p.adjust.method = "holm",
                         label.y = c(82,78,75, -3, 1, 5), # horizontal adjustment
                         tip.length = c(0.03, 0.03, 0.03, -0.03, -0.03, -0.03), # direction of bracket
                         vjust = c(4,0,0,1.1,0,0,1,0,0,0.5,0,0,4,0,0,4,0,0)) + # placement of asterisks
      scale_y_discrete(
        limits = c("BS", "HU", "HB", "BU", "UU"),
        labels = c("Salvage", "Harvest", "Harvest/Burn", "Burn", "Control")) +
      theme_bw() +
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 20),
            panel.grid = element_blank()) +
      labs(y = "Treatment",
           x = "Downed wood count")
    
    
   ggsave("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/figures/dwd-trt-comparisons.png", 
          width = 10, height = 8, dpi = 300)
    
   