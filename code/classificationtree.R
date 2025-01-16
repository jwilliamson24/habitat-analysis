# -------------------------------------------------------------------------------------------------------
##
## classificationtree.R 
##
## Random forest code, continued effort from multivariate class
##
## Jasmine Williamson
## Date Created: 01-16-2025
##
## 
## settings -----------------------------------------------------------------------------------------------

    rm(list=ls())
    setwd("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/habitat-analysis/code")
    
    library(ggplot2)

## load data--------------------------------------------------------------------------------------------------

    # site-level data
    dat <- readRDS("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data/site_level_matrix.rds")
    row.names(dat) <- dat[,1]
    
    sals <- dat[26:27]
    env <- dat[1:25]
    
    drop <- c("lat","long","stand","tree_farm","landowner","site_id","year","weather")
    env <- env[,!(colnames(env) %in% drop)]
    
    env_cont <- env[,-1]
    
    #took out elevation, jul date, temp (we already know theyre elevationally/temporally specific) 
    drop <- c("jul_date","elev","temp")
    env_subset <- env_cont[,!(colnames(env_cont) %in% drop)]
    
    # sal presences absence
    oss_PA <- ifelse(sals$oss > 0, "Present", "Absent")
    enes_PA <- ifelse(sals$enes > 0, "Present", "Absent")

## oss classification tree --------------------------------------------------------------------------------------------------

## using all continuous env variables
    oss.tree <- rpart(oss_PA ~ ., data=env_cont, minsplit=2, xval=5)
    rpart.plot(oss.tree)
    plotcp(oss.tree)
    
    oss.tree.prune <- prune(oss.tree, 0.055)
    rpart.plot(oss.tree.prune, cex=1.3)   
 
    #save
    png(filename = "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/habitat-analysis/figures/classificationtree/oss_classtree_cont.png",
        width = 1200, height = 1000, res = 150)
    rpart.plot(oss.tree.prune, cex = 1.3)
    dev.off()
    
       
## subsetted env data
    oss.tree.sub <- rpart(oss_PA ~ ., data=env_subset, minsplit=2, xval=5)
    rpart.plot(oss.tree.sub)
    
    plotcp(oss.tree.sub)
    
    oss.tree.sub.prune <- prune(oss.tree.sub, 0.053)
    rpart.plot(oss.tree.sub.prune) 
    
    #save
    png(filename = "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/habitat-analysis/figures/classificationtree/oss_classtree_sub.png",
        width = 1200, height = 1000, res = 150)
    rpart.plot(oss.tree.sub.prune) 
    dev.off()
    
    
## enes classification tree --------------------------------------------------------------------------------------------------
    
    enes.tree <- rpart(enes_PA ~ ., data=env_cont, minsplit=2, xval=5)
    rpart.plot(enes.tree)
    
    plotcp(enes.tree)
    
    enes.tree.prune <- prune(enes.tree, 0.019)
    rpart.plot(enes.tree.prune)    
    
    #save
    png(filename = "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/habitat-analysis/figures/classificationtree/enes_classtree_cont.png",
        width = 1200, height = 1000, res = 150)
    rpart.plot(enes.tree.prune) 
    dev.off()
    
    
    ## subsetted env data
    
    enes.tree.sub <- rpart(enes_PA ~ ., data=env_subset, minsplit=2, xval=5)
    rpart.plot(enes.tree.sub)
    
    plotcp(enes.tree.sub)
    
    enes.tree.sub.prune <- prune(enes.tree.sub, 0.051)
    rpart.plot(enes.tree.sub.prune) 
    
    #save
    png(filename = "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/habitat-analysis/figures/classificationtree/enes_classtree_sub.png",
        width = 1200, height = 1000, res = 150)
    rpart.plot(enes.tree.sub.prune)
    dev.off()
    
    
    
    
    