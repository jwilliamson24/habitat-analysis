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
    library(vegan)
    library(car)
    library(FSA)
    library(ggplot2)

## load data ----------------------------------------------------------------------------------------------


    dat <- read.csv("covariate matrices/env_subset_corr.csv", row.names = 1)
    dat2 <- readRDS("covariate matrices/site_level_matrix.rds") #need this for the treatments
    row.names(dat2) <- dat2[,1]
    dat2$trt <- as.factor(dat2$trt)
    
    
## ANOVA variables
    
    # important variables:
    # dwd count
    # decay class
    
## KW variables
    
    # important variables:
    # canopy cover
    # fwd cover
    # veg cover
    
    
## dwd boxplot  ------------------------------------------------------------------------------------------
    
    ggplot(dat2, aes(x = trt, y = dwd_count)) +
      geom_boxplot() +
      labs(title = "Distribution of Downed Wood Count Across Treatment Classes",
           x = "Treatment Class",
           y = "Downed Wood Count") +
      theme_classic()
    
    

    
    
    
    
    
    
    
    
