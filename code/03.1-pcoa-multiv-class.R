# -------------------------------------------------------------------------------------------------------
##
## 03.1-pcoa-multiv-class.R 
##
## PCoA code from the final project for multivariate class
##
## Jasmine Williamson
## Date Created: 02-06-2025
##
## 
## This code resulted in a different and more intuitive plot than the one I made
## with the uncorrelated variables dataframe. Not sure what to do with that,
## just saving it for future reference.
##
## settings -----------------------------------------------------------------------------------------------

    rm(list=ls())
    setwd("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data")
    library(vegan)
    library(viridis)
    source("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/multivariate-analysis/biostats.R")
    

## load data ----------------------------------------------------------------------------------------------

#site-level data

    dat <- readRDS("site_level_matrix.rds")
    row.names(dat) <- dat[,1]
    
    #dat2 <- subset(dat, year=="2024")
    sals <- dat[26:27]
    env <- dat[1:25]
    
    drop <- c("lat","long","stand","tree_farm","landowner","site_id","year","weather")
    env <- env[,!(colnames(env) %in% drop)]
    
    env_cont <- env[,-1] #subset all continuous
    
    #subset the ones I want to test
    drop <- c("jul_date","veg_cov","fwd_cov","dwd_count","size_cl","decay_cl","char_cl","length_cl" )
    env_subset <- env_cont[,!(colnames(env_cont) %in% drop)]
    
## standardize
    
    env_std <- decostand(env_cont, "standardize") #Z-scores the data in each column

## euclidean distance
    
    env_std_subset <- env_std[,c("temp","dwd_cov","soil_moist","stumps","logs","decay_cl","canopy_cov")]	
    env_euc <- vegdist(env_std_subset, method="euclidean")
    
    
### PCoA -------------------------------------------------------------------------------
    
    
    env.pcoa <- cmdscale(env_euc, 
                         k=5, 
                         eig=TRUE, 
                         add=T)
    
    spe.sc <- wascores(env.pcoa$points[,1:2], env_subset)
    
    vec.sp <- envfit(as.data.frame(env.pcoa$points), env_subset, perm=1000)
    
    
## eigenvalues -------------------------------------------------------------------------
    
    
    # Extract eigenvalues
    eigenvalues <- env.pcoa$eig
    
    # Calculate the percentage of variance explained by each axis
    percent_explained <- (eigenvalues / sum(eigenvalues)) * 100
    
    # View the percentages for the first few axes
    percent_explained[1:5]
    

## plot --------------------------------------------------------------------------------   
    
    
    groups <- levels(factor(dat$trt))
    pt_col <- viridis(length(groups))
    site.sc <- scores(env.pcoa, choices=c(1,2))
    
    plot(site.sc[, 1:2],  # First two dimensions
         main = "Env PCoA", 
         xlab = "PCoA 1", 
         ylab = "PCoA 2", 
         pch = 19)
    
    for (i in 1:length(groups))
    {
      dim_choice <- site.sc[dat$trt==groups[i],]
      points(dim_choice[,1], dim_choice[,2], 
             pch=19, 
             cex=1.4, 
             col=pt_col[i])
      
      # Calculate and add convex hull for the group
      chull_points <- dim_choice[chull(dim_choice[, 1], dim_choice[, 2]), ]
      
      polygon(chull_points[, 1], chull_points[, 2], 
              border=pt_col[i], 
              col=adjustcolor(pt_col[i], alpha.f = 0.3),  # Semi-transparent fill
              lwd=2)
    }
    
    text(spe.sc*1.5, row.names(spe.sc),
         cex = 1.5)
    arrows(0, 0, spe.sc[,1]*1.4, spe.sc[,2]*1.4, 
           lwd=2, 
           length=0.1,
           cex= 1.5)
    legend(x="bottomleft", 
           legend=levels(factor(dat$trt)), 
           col=pt_col[1:6], 
           pch=19, 
           cex=1.2)
    
    








