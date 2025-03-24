# -------------------------------------------------------------------------------------------------------
##
## 04-permanova.R 
##
## PERMANOVA using habitat variables for Ch 2 analyses
##
## Jasmine Williamson
## Date Created: 02-06-2025
##
## 
## goals --------------------------------------------------------------------------------------------------

# Use PERMANOVA to test whether habitat composition differs significantly among treatments 

## insights --------------------------------------------------------------------------------------------------

# treatments are statistically significantly different from each other
# 30.9% of the variation in habitat data can be explained by treatment
# there are significant differences in habitat composition between every possible treatment pair

## settings -----------------------------------------------------------------------------------------------

    rm(list=ls())
    setwd("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data")
    library(vegan)
    library(pairwiseAdonis)
    source("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/multivariate-analysis/biostats.R")


## load data ----------------------------------------------------------------------------------------------

    
    dat <- read.csv("env_subset_corr.csv", row.names = 1)
    dat2 <- readRDS("site_level_matrix.rds") #need this for the treatments
    row.names(dat2) <- dat2[,1]


# subset
    
    # removing detection covariates from the matrix so that we are only assessing 
    # habitat variables that are impacted by the treatment
    # took out aspect, precip, days since rain
    
    dat <- dat[,-c(11:13)]
    
# standardize
    
    dat_std <- decostand(dat, "standardize") #z-scores



## permanova ----------------------------------------------------------------------------------------------

    # ??adonis2


    permanova <- adonis2(dat_std ~ trt, data = dat2, method = "euclidean", permutations = 999)

    permanova    

          # adonis2(formula = dat_std ~ trt, data = dat2, permutations = 999, method = "euclidean")
          #             Df SumOfSqs      R2      F Pr(>F)    
          #   Model      4   428.11 0.30888 13.632  0.001 ***
          #   Residual 122   957.89 0.69112                  
          #   Total    126  1386.00 1.00000 
    

    # R-squared (0.30888) = 30.9% of the variation in habitat data can be explained by treatment
    # p-value (0.001) = effect of the treatment factor on habitat composition is statistically significant
    # "strong evidence to reject the null hypothesis that habitat composition does not differ among the treatment groups"


## pairwise comparisons -----------------------------------------------------------------------------------
    
    pairwise_result <- pairwise.adonis(dat_std, dat2$trt, sim.method = "euclidean", perm = 999)
    print(pairwise_result)
    
            #         pairs Df SumsOfSqs   F.Model        R2 p.value p.adjusted sig
            #   1  BS vs HB  1  49.14050  7.154337 0.1321101   0.001       0.01   *
            #   2  BS vs UU  1 155.94449 19.549777 0.2937617   0.001       0.01   *
            #   3  BS vs HU  1  38.86383  4.428321 0.0828834   0.001       0.01   *
            #   4  BS vs BU  1  80.13962 10.638809 0.1814295   0.001       0.01   *
            #   5  HB vs UU  1 209.65198 29.302185 0.3790602   0.001       0.01   *
            #   6  HB vs HU  1 120.15806 15.074170 0.2316460   0.001       0.01   *
            #   7  HB vs BU  1  73.51422 10.912609 0.1821421   0.001       0.01   *
            #   8  UU vs HU  1 132.61550 14.714138 0.2273713   0.001       0.01   *
            #   9  UU vs BU  1  77.33809  9.915697 0.1683031   0.001       0.01   *
            #   10 HU vs BU  1 132.30093 15.429453 0.2322683   0.001       0.01   *


    # All comparisons show a p-value of 0.001
    # there are significant differences in habitat composition between each treatment pair
    
    # highest R2 = HB vs UU (0.379), most different
    # lowest R2 = BS vs UU (0.293), least different





