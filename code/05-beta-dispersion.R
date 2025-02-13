# -------------------------------------------------------------------------------------------------------
##
## 05-beta-dispersion.R 
##
## Beta dispersion (PERMDISP) using habitat variables for Ch 2 analyses
##
## Jasmine Williamson
## Date Created: 02-06-2025
##
## 
## goals --------------------------------------------------------------------------------------------------

# Use PERMDISP to test whether the variation (dispersion) in habitat variables differs among treatments
# If some treatments show higher variability in habitat conditions than others, 
# this can inform how disturbances affect habitat heterogeneity

## insights --------------------------------------------------------------------------------------------------

# habitat variability differs significantly among treatment groups
# HU has the highest dispersion (2.992), meaning greater habitat variability
# The first few PCoA axes capture most of the variation in habitat dispersion

# Since some treatment types have significantly higher or lower dispersion than others,
# this suggests that disturbances may influence habitat heterogeneity


## settings -----------------------------------------------------------------------------------------------

    rm(list=ls())
    setwd("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data")
    library(vegan)
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

# euclidean distance

    env_euc <- vegdist(dat_std, method="euclidean") 

    
## permdisp ----------------------------------------------------------------------------------------------
    
    
    treatment_labels <- dat2$trt  # treatment labels
    permdisp_result <- betadisper(env_euc, treatment_labels)
    print(permdisp_result)
    plot(permdisp_result)
    
              # Average distance to median:
              #   BS    BU    HB    HU    UU 
              # 2.649 2.591 2.324 2.992 2.733 
              # 
              # Eigenvalues for PCoA axes:
              #   (Showing 8 of 11 eigenvalues)
              # PCoA1  PCoA2  PCoA3  PCoA4  PCoA5  PCoA6  PCoA7  PCoA8 
              # 291.14 266.17 191.93 163.10 150.32 114.44  67.83  50.70 
    
    
        # HU has the highest dispersion (2.992), meaning greater habitat variability
        # The first few PCoA axes capture most of the variation in habitat dispersion
    
    
    
    
    
## other metrics --------------------------------------------------------------------------    
    
    
    anova(permdisp_result)
    
              # Response: Distances
              #               Df Sum Sq Mean Sq F value  Pr(>F)  
              #   Groups      4  6.059 1.51466  3.4004 0.01128 *
              #   Residuals 122 54.343 0.44544
              
        # habitat variability differs significantly across treatment groups
        # Since some treatment types have significantly higher or lower dispersion than others,
        # this suggests that disturbances (wildfire, harvesting) influence habitat heterogeneity
    
    
    
    TukeyHSD(permdisp_result)
    
        # which treatments differ from each other significantly in their variance
    

    
## plot --------------------------------------------------------------------------          
    
# boxplot of variances by treatment
    
    png("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/habitat-analysis/figures/beta-disp/permdisp_boxplot.png",
        width = 800, height = 600)  
    boxplot(permdisp_result$distances ~ treatment_labels, 
            main = "Beta Dispersion by Treatment",
            ylab = "Distance to Median", xlab = "Treatment Type",
            col = "lightgray", border = "black")
    dev.off()  
  
    

# multivariate plot of PCoA axes with dispersion visualizations
    
    png("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/habitat-analysis/figures/beta-disp/permdisp_pcoa_plot.png", 
        width = 800, height = 600)  
    plot(permdisp_result, hull=TRUE, ellipse=TRUE,
         main= "Beta Dispersion by Treatment")
    dev.off()
    
    # convex hulls (polygons) show the spread of treatments in multivariate space
    # ellipses show variability within each group
    
    
    
    
    
    
