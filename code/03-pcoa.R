# -------------------------------------------------------------------------------------------------------
##
## 03-pcoa.R 
##
## PCoA using habitat variables for Ch 2 analyses
##
## Jasmine Williamson
## Date Created: 02-06-2025
##
## 
## goals --------------------------------------------------------------------------------------------------

# redoing the pcoa code from multivariate class 
# (from lab 7 copy and 03.1-pcoa-multiv-class)
# using env dataframe that has been checked for corr 

## insights --------------------------------------------------------------------------------------------------

# sites are kinda spread out!!!
# first two axes explain ~40% of the total variance
# axis 1: dwd_cov, fwd_cov, char_cl, avg_volume, dwd_count
# axis 2: canopy cov, dwd_count


## settings -----------------------------------------------------------------------------------------------

    rm(list=ls())
    setwd("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data")
    library(vegan)
    library(viridis)
    source("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/multivariate-analysis/biostats.R")
    

## load data ----------------------------------------------------------------------------------------------

    dat <- read.csv("covariate matrices/env_subset_corr.csv", row.names = 1)
    dat2 <- readRDS("covariate matrices/site_level_matrix.rds") #need this for the treatments
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

    
## pcoa ----------------------------------------------------------------------------------------------
    
    env.pcoa <- cmdscale(env_euc, 
                         k=5, 
                         eig=TRUE, 
                         add=T)
    env.pcoa
    
    
    
    
#broken stick plot:
    
    #see percentage of variation explained by the first few principal 
    #coordinates compared to what would be expected by chance
    
    plot(100*env.pcoa$eig/sum(env.pcoa$eig),
         type="b",
         lwd=2,
         col="blue",
         xlab="Principal Component from PCoA",
         ylab="Percent variation explained",
         main="Broken Stick Model")
    lines(bstick(length(env.pcoa$eig))*100, 
          type="b", 
          lwd=2, 
          col="red")
    
    
    
    #first few axes explain more variation than would be expected by chance so we're good
    
    
    
## eigenvalues and loadings ---------------------------------------------------------------------------------
    
    
    # Extract eigenvalues
    eigenvalues <- env.pcoa$eig
    
    # Calculate the percentage of variance explained by each axis
    percent_explained <- (eigenvalues / sum(eigenvalues)) * 100
    
    # View the percentages for the first few axes
    percent_explained[1:5]
    
              # 21.00597 19.20389 13.84748 11.76746 10.84572
    
    
    
    #calculate loadings of env variables and use permutation to ascribe significance to them
    
    env.sc <- wascores(env.pcoa$points[,1:2], dat)
              #                 [,1]         [,2]     1   2
              # temp        0.03120062 -0.028465292
              # canopy_cov  0.22527852  1.779431723       ##
              # veg_cov    -0.11607589  0.165569841
              # dwd_cov     0.33185820 -0.009843821  ##
              # fwd_cov     0.42841604  0.063079715  ##
              # soil_moist -0.03579351  0.148405817
              # dwd_count   0.26214667 -0.272785356  ##   ##
              # size_cl     0.04169879  0.007586462
              # decay_cl   -0.08978888  0.187432950
              # char_cl    -0.36239278 -0.222368005  ##
              # avg_volume  0.36901288  0.255545059  ##
    
      # axis 1: dwd_cov, fwd_cov, char_cl, avg_volume, dwd_count
      # axis 2: canopy cov, dwd_count
    
    vec.sp <- envfit(as.data.frame(env.pcoa$points), dat, perm=1000)  
              #                    V1       V2     r2   Pr(>r)    
              #   temp        0.70783 -0.70638 0.0242 0.216783    
              #   canopy_cov  0.11497  0.99337 0.8197 0.000999 ***
              #   veg_cov    -0.53961  0.84192 0.1664 0.000999 ***
              #   dwd_cov     0.99947 -0.03243 0.6284 0.000999 ***
              #   fwd_cov     0.98728  0.15901 0.5439 0.000999 ***
              #   soil_moist -0.21532  0.97654 0.1304 0.000999 ***
              #   dwd_count   0.66002 -0.75125 0.5650 0.000999 ***
              #   size_cl     0.98077  0.19518 0.0969 0.001998 ** 
              #   decay_cl   -0.40116  0.91601 0.5353 0.000999 ***
              #   char_cl    -0.83031 -0.55730 0.4796 0.000999 ***
              #   avg_volume  0.79712  0.60382 0.4332 0.000999 ***
    
      # canopy_cov has high values for V2 (0.99337); it is strongly associated with the second axis
      # canopy_cov has an r² = 0.8197; explains 81.97% of the variance along the axes it is associated with
      # p-value= statistical significance of the relationship between the variable and the ordination axes
    
    
## plot with site polygons --------------------------------------------------------------------------------
    
    
    groups <- levels(factor(dat2$trt))
    pt_col <- viridis(length(groups))
    site.sc <- scores(env.pcoa, choices=c(1,2))
    
    png(filename = "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/habitat-analysis/figures/pcoa/pcoa_0325.png",
        width = 1200, height = 1000, res = 150)
    
    # Define custom colors - replace these with your preferred colors
    custom_colors <- c(
      "#CC99FF",   # Light purple
      "#66B2FF",  # Steel blue
      "#EE6A50",  # Light red
      "#F8D531",  # Light yellow
      "#99FF99"  # Light green
      
    )
    
    # Ensure we have enough colors for all groups
    pt_col <- custom_colors[1:length(groups)]
   
    # First two dimensions
    plot(site.sc[, 1:2],  
         main = "Env PCoA", 
         xlab = "PCoA 1", 
         ylab = "PCoA 2", 
         pch = 19,
         xlim = c(min(site.sc[,1]) - 0.3, max(site.sc[,1]) + 0.3),  # Adjust limits if needed
         ylim = c(min(site.sc[,2]) - 0.3, max(site.sc[,2]) + 0.3))  # Adjust limits if needed
          
    # add points
    for (i in 1:length(groups))
    {
      dim_choice <- site.sc[dat2$trt==groups[i],]
      points(dim_choice[,1], dim_choice[,2], 
             pch=19, 
             cex=1.4, 
             col=pt_col[i])
      
      # add polygons
      chull_points <- dim_choice[chull(dim_choice[, 1], dim_choice[, 2]), ]
      
      polygon(chull_points[, 1], chull_points[, 2], 
              border=pt_col[i], 
              col=adjustcolor(pt_col[i], alpha.f = 0.3),  # Semi-transparent fill
              lwd=2)
    }
    
    arrows(0, 0, env.sc[,1]*2, env.sc[,2]*2, 
           lwd=2, 
           length=0.1,
           cex= 1.5)
    
    # text(env.sc*3.5, row.names(env.sc),
    #      cex = 1.5)
    # legend(x="bottomleft", 
    #        legend=levels(factor(dat2$trt)), 
    #        col=pt_col[1:6], 
    #        pch=19, 
    #        cex=1.2)
    

    dev.off() 
    
    
    