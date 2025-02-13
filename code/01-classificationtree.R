# -------------------------------------------------------------------------------------------------------
##
## classificationtree.R 
##
## Continued effort from multivariate class
##
## Jasmine Williamson
## Date Created: 01-16-2025
##
## 01/21/25 - tried to optimize the pruning process by adding a step to give me the best CP based on the 
## 1-SE rule, but this resulted in very large CP values and the pruned trees were just the root node.
## this might be indicative that the variables dont explain the occupancy data well, or the data is noisy.
## ended up choosing the CP with the lowest xerror, which may overfit the data.
## 
## settings -----------------------------------------------------------------------------------------------

    rm(list=ls())
    setwd("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data")
    
    library(ggplot2)
    library(rpart)
    library(rpart.plot)

## load data (old method) -------------------------------------------------------------------------------------

    # # site-level data
    # dat <- readRDS("site_level_matrix.rds")
    # 
    # sals <- dat[26:27]
    # env <- dat[1:25]
    # 
    # drop <- c("lat","long","stand","tree_farm","landowner","site_id","year","weather")
    # env <- env[,!(colnames(env) %in% drop)]
    # 
    # env_cont <- env[,-1]
    # 
    # #take out elevation, jul date, temp (we already know theyre elevationally/temporally specific) 
    # drop <- c("jul_date","elev","temp")
    # env_subset <- env_cont[,!(colnames(env_cont) %in% drop)]
    # 
    # #add extra downed wood metrics to env subset
    # dwd_extra <- read.csv("dwd.extra.metrics.csv")
    # dwdsub <- dwd_extra[,c("dwd_dens","log_dens","stump_dens","avg_volume")]
    # env_subset_dwd <- cbind(env_subset, dwdsub)
    # 
    # #load env df that was checked for correlations
    # env_subset_corr <- read.csv("env_subset_corr.csv")
    # 
    # # sal presences absence
    # oss_PA <- ifelse(sals$oss > 0, "Present", "Absent")
    # enes_PA <- ifelse(sals$enes > 0, "Present", "Absent")
    
    
## load data (updated 02/04/2025) ----------------------------------------------------------------------------
    
    dat <- read.csv("env_subset_corr.csv")
    row.names(dat) <- dat[,1]
    dat <- subset(dat, select = -X)
    env_subset_corr <- dat
    
    dat2 <- readRDS("site_level_matrix.rds")
    sals <- dat2[26:27]
    oss_PA <- ifelse(sals$oss > 0, "Present", "Absent")
    enes_PA <- ifelse(sals$enes > 0, "Present", "Absent")
    
## oss classification trees  ---------------------------------------------------------------------------
## these options not used in final report
    
## using all continuous env variables
    set.seed(123) # set seed for plotcp function
    oss.tree <- rpart(oss_PA ~ ., data=env_cont, minsplit=2, xval=10) #upping xval to 10 or 20 didnt help the pruning step
    rpart.plot(oss.tree)
    plotcp(oss.tree)
    printcp(oss.tree)
 
    #prune
    #chose the CP with the lowest xerror because using the 1-SE rule have me a high CP that gave only a root node tree
    oss.tree.prune <- prune(oss.tree, 0.05) 
    rpart.plot(oss.tree.prune, cex=1.3)   
 
    #save
    png(filename = "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/habitat-analysis/figures/classificationtree/oss_classtree_cont.png",
        width = 1200, height = 1000, res = 150)
    rpart.plot(oss.tree.prune, cex = 1.3)
    dev.off()
    
       
## subsetted env data
    #didnt try to update this with new CP, dont care about it
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
    
## subsetted env data with dwd extra metrics
    set.seed(123)
    oss.tree.sub2 <- rpart(oss_PA ~ ., data=env_subset_dwd, minsplit=2, xval=10)
    rpart.plot(oss.tree.sub2)
    plotcp(oss.tree.sub2)
    printcp(oss.tree.sub2)
    
    # Get the best CP based on the 1-SE rule
    # min_xerror <- min(oss.tree.sub2$cptable[, "xerror"])
    # best_cp <- oss.tree.sub2$cptable[oss.tree.sub2$cptable[, "xerror"] <= min_xerror + oss.tree.sub2$cptable[, "xstd"], "CP"][1]

    #prune
    #the above method gave a CP that was too high and the tree was 
    #just the root node with no splits; i chose the CP with the lowest xerror,
    #but it is now overfitting to some extent
    oss.tree.sub.prune2 <- prune(oss.tree.sub2, 0.03)
    rpart.plot(oss.tree.sub.prune2) 
    printcp(oss.tree.sub.prune2)

    #save
    png(filename = "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/habitat-analysis/figures/classificationtree/oss_classtree_subdwd.png",
        width = 1200, height = 1000, res = 150)
    rpart.plot(oss.tree.sub.prune2) 
    dev.off()
    
#### final oss tree -------------------------------------------------------------------------------
# using subset env data checked for correlations 
    
    set.seed(123)
    oss.tree.sub3 <- rpart(oss_PA ~ ., data=env_subset_corr, minsplit=2, xval=10)
    rpart.plot(oss.tree.sub3)
    plotcp(oss.tree.sub3)
    printcp(oss.tree.sub3)
    
    # Get the best CP based on the 1-SE rule
    # min_xerror <- min(oss.tree.sub3$cptable[, "xerror"])
    # best_cp <- oss.tree.sub3$cptable[oss.tree.sub3$cptable[, "xerror"] <= min_xerror + oss.tree.sub3$cptable[, "xstd"], "CP"][1]
    
    #prune
    #the above method gave a CP that was too high and the tree was 
    #just the root node with no splits; i chose the CP with the lowest xerror,
    #but it is now likely overfitting to some extent
    oss.tree.sub.prune3 <- prune(oss.tree.sub3, 0.045)
    rpart.plot(oss.tree.sub.prune3) 
    printcp(oss.tree.sub.prune3) 
    
    #save
    png(filename = "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/habitat-analysis/figures/classificationtree/oss_classtree_0204.png",
        width = 1200, height = 1000, res = 150)
    rpart.plot(oss.tree.sub.prune3) 
    dev.off()
    
    
## enes classification tree --------------------------------------------------------------------------------------------------
 
## using all continuous env variables   
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
    
## subsetted env data with dwd extra metrics
    set.seed(123)
    enes.tree.sub2 <- rpart(enes_PA ~ ., data=env_subset_corr, minsplit=2, xval=10)
    rpart.plot(enes.tree.sub2)
    plotcp(enes.tree.sub2)
    printcp(enes.tree.sub2)
    
    # Get the best CP based on the 1-SE rule
    min_xerror <- min(enes.tree.sub2$cptable[, "xerror"])
    best_cp <- enes.tree.sub2$cptable[enes.tree.sub2$cptable[, "xerror"] <= min_xerror + enes.tree.sub2$cptable[, "xstd"], "CP"][1]
    
    enes.tree.sub.prune2 <- prune(enes.tree.sub2, 0.05)
    rpart.plot(enes.tree.sub.prune2) 
    
    #save
    png(filename = "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/habitat-analysis/figures/classificationtree/enes_classtree_0204.png",
        width = 1200, height = 1000, res = 150)
    rpart.plot(enes.tree.sub.prune2) 
    dev.off() 
    
    
    