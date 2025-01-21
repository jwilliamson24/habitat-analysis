# -------------------------------------------------------------------------------------------------------
##
## randomforest.R 
##
## Random forest code, continued effort from multivariate class
##
## Jasmine Williamson
## Date Created: 01-15-2025
##
## 
## settings -----------------------------------------------------------------------------------------------
    
    rm(list=ls())
    setwd("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data")

    library(randomForest)
    library(ggplot2)
 
## load data--------------------------------------------------------------------------------------------------

# site-level data
    dat <- readRDS("site_level_matrix.rds")
    row.names(dat) <- dat[,1]
    
    sals <- dat[26:27]
    env <- dat[1:25]
    
    drop <- c("lat","long","stand","tree_farm","landowner","site_id","year","weather")
    env <- env[,!(colnames(env) %in% drop)]
    
    env_cont <- env[,-1]
    
    #took out elevation, jul date (we already know theyre elevationally/temporally specific) 
    drop <- c("jul_date","elev")
    env_subset <- env_cont[,!(colnames(env_cont) %in% drop)]
    
    #add extra downed wood metrics to env subset
    dwd_extra <- read.csv("dwd.extra.metrics.csv")
    dwdsub <- dwd_extra[,c("dwd_dens","log_dens","stump_dens","avg_volume")]
    env_subset_dwd <- cbind(env_subset, dwdsub)
    
    
# sal presences absence
    oss_PA <- ifelse(sals$oss > 0, "Present", "Absent")
    enes_PA <- ifelse(sals$enes > 0, "Present", "Absent")
    
## oss random forest --------------------------------------------------------------------------------------------------

#all variables    
    oss.forest <- randomForest(as.factor(oss_PA) ~ ., data=env_cont, ntree = 5000, mtry = 5, 
                               importance=TRUE, keep.forest=FALSE, na.action=na.omit)
    oss.forest
    table(oss_PA)
    importance(oss.forest)
    # The error rate is pretty bad (29.92%), and most of that is due to the absences (62%). I tried tuning the mtry 
    # and ntree to improve the matrix but I havent figured out a way to make it better yet. 
    
#subset of variables
    oss.forest.sub <- randomForest(as.factor(oss_PA) ~ ., data=env_subset, ntree = 5000, mtry = 5, 
                               importance=TRUE, keep.forest=FALSE, na.action=na.omit)

#subset with extra dwd metrics
    oss.forest.subdwd <- randomForest(as.factor(oss_PA) ~ ., data=env_subset_dwd, ntree = 5000, mtry = 5, 
                                   importance=TRUE, keep.forest=FALSE, na.action=na.omit)  
## plot - all variables -----------------------------------------------------------------------------------------------------

#with all variables
    varImpPlot(oss.forest)
    
    ossForestData <- as.data.frame(importance(oss.forest))
    ossForestData <- ossForestData[order(ossForestData[,1]),]
    ossForestData$Var.Names <- row.names(ossForestData)
    colnames(ossForestData) <- c("Absent","Present","MeanDec","IncNodePurity","Var.Names")
    ossForestData

    #ggplot     
    p1 <- ggplot(ossForestData, aes(x = Var.Names, y = MeanDec)) +
      geom_segment(aes(x = Var.Names, xend = Var.Names, y = 0, yend = MeanDec, 
                       color = ifelse(MeanDec > 18, "Above 18", 
                                      ifelse(MeanDec >= 10, "10-18", "Below 10"))), 
                   show.legend = FALSE) +
      geom_point(aes(size = IncNodePurity, 
                     color = ifelse(MeanDec > 18, "Above 18", 
                                    ifelse(MeanDec >= 10, "10-18", "Below 10"))), 
                 alpha = 0.6) +
      theme_light() +
      coord_flip() +
      scale_color_manual(values = c("Above 18" = "blue", "10-18" = "#66C2A5", "Below 10" = "#FFD700")) +
      labs(color = "Range", size = "Node Purity") +
      theme(
        text = element_text(size = 14)
      ) +
      labs(
        title = "Variable Importance from Random Forest Model",
        x = "Environmental Variables",
        y = "Mean Decrease in Accuracy",
        size = "Node Purity"
      )
 
ggsave(filename = "oss_varimp.png", plot = p1, device = "png",
       path = "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/habitat-analysis/figures/randomforest")


## plot - subset variables  --------------------------------------------------------------------------------------------------------
    
    ossForestData.sub <- as.data.frame(importance(oss.forest.sub))
    ossForestData.sub <- ossForestData.sub[order(ossForestData.sub[,1]),]
    ossForestData.sub$Var.Names <- row.names(ossForestData.sub)
    colnames(ossForestData.sub) <- c("Absent","Present","MeanDec","IncNodePurity","Var.Names")
    
    #ggplot     
    p2 <- ggplot(ossForestData.sub, aes(x = Var.Names, y = MeanDec)) +
      geom_segment(aes(x = Var.Names, xend = Var.Names, y = 0, yend = MeanDec, 
                       color = ifelse(MeanDec > 15, ">15", 
                                      ifelse(MeanDec >= 10, "10-15", "<10"))), 
                   show.legend = FALSE) +
      geom_point(aes(size = IncNodePurity, 
                     color = ifelse(MeanDec > 15, ">15", 
                                    ifelse(MeanDec >= 10, "10-15", "<10"))), 
                 alpha = 0.6) +
      theme_light() +
      coord_flip() +
      scale_color_manual(values = c(">15" = "blue", "10-15" = "#66C2A5", "<10" = "#FFD700")) +
      labs(color = "Range", size = "Node Purity") +
      theme(
        text = element_text(size = 20)
      ) +
      labs(
        title = "Variable Importance from Random Forest Model - OSS",
        x = "Environmental Variables",
        y = "Mean Decrease in Accuracy",
        size = "Node Purity"
      )
 
    
ggsave(filename = "oss_varimp_subset.png", plot = p2, device = "png",
           path = "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/habitat-analysis/figures/randomforest")
 

## plot - subset with dwd  --------------------------------------------------------------------------------------------------------

    ossForestData.subdwd <- as.data.frame(importance(oss.forest.subdwd))
    ossForestData.subdwd <- ossForestData.subdwd[order(ossForestData.subdwd[,1]),]
    ossForestData.subdwd$Var.Names <- row.names(ossForestData.subdwd)
    colnames(ossForestData.subdwd) <- c("Absent","Present","MeanDec","IncNodePurity","Var.Names")
    
    #ggplot     
    p5 <- ggplot(ossForestData.subdwd, aes(x = Var.Names, y = MeanDec)) +
      geom_segment(aes(x = Var.Names, xend = Var.Names, y = 0, yend = MeanDec, 
                       color = ifelse(MeanDec > 15, ">15", 
                                      ifelse(MeanDec >= 10, "10-15", "<10"))), 
                   show.legend = FALSE) +
      geom_point(aes(size = IncNodePurity, 
                     color = ifelse(MeanDec > 15, ">15", 
                                    ifelse(MeanDec >= 10, "10-15", "<10"))), 
                 alpha = 0.6) +
      theme_light() +
      coord_flip() +
      scale_color_manual(values = c(">15" = "blue", "10-15" = "#66C2A5", "<10" = "#FFD700")) +
      labs(color = "Range", size = "Node Purity") +
      theme(
        text = element_text(size = 20)
      ) +
      labs(
        title = "Variable Importance from Random Forest Model - OSS",
        x = "Environmental Variables",
        y = "Mean Decrease in Accuracy",
        size = "Node Purity"
      )


ggsave(filename = "oss_varimp_subset_dwd.png", plot = p5, device = "png",
       path = "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/habitat-analysis/figures/randomforest")

   
## enes random forest --------------------------------------------------------------------------------------------------
    
    enes.forest <- randomForest(as.factor(enes_PA) ~ ., data=env_cont, ntree = 5000, mtry = 5, 
                               importance=TRUE, keep.forest=FALSE, na.action=na.omit)
    enes.forest
    table(enes_PA)
    importance(enes.forest)  
    
    # The error rate is  
 
       
#subset of variables
    enes.forest.sub <- randomForest(as.factor(enes_PA) ~ ., data=env_subset, ntree = 5000, mtry = 5, 
                                   importance=TRUE, keep.forest=FALSE, na.action=na.omit)

#subset with extra dwd metrics
    enes.forest.subdwd <- randomForest(as.factor(enes_PA) ~ ., data=env_subset_dwd, ntree = 5000, mtry = 5, 
                                      importance=TRUE, keep.forest=FALSE, na.action=na.omit)  
    
## plot - all variables ------------------------------------------------------------------------------------------------
    
    varImpPlot(enes.forest)
    
    enesForestData <- as.data.frame(importance(enes.forest))
    enesForestData <- enesForestData[order(enesForestData[,1]),]
    enesForestData$Var.Names <- row.names(enesForestData)
    colnames(enesForestData) <- c("Absent","Present","MeanDec","IncNodePurity","Var.Names")
    enesForestData

#variable importance ggplot    
    p3 <- ggplot(enesForestData, aes(x = Var.Names, y = MeanDec)) +
      geom_segment(aes(x = Var.Names, xend = Var.Names, y = 0, yend = MeanDec, 
                       color = ifelse(MeanDec > 18, "Above 18", 
                                      ifelse(MeanDec >= 10, "10-18", "Below 10"))), 
                   show.legend = FALSE) +
      geom_point(aes(size = IncNodePurity, 
                     color = ifelse(MeanDec > 18, "Above 18", 
                                    ifelse(MeanDec >= 10, "10-18", "Below 10"))), 
                 alpha = 0.6) +
      theme_light() +
      coord_flip() +
      scale_color_manual(values = c("Above 18" = "blue", "10-18" = "#66C2A5", "Below 10" = "#FFD700")) +
      labs(color = "Range", size = "Node Purity") +
      theme(
        text = element_text(size = 14)
      ) +
      labs(
        title = "Variable Importance from Random Forest Model",
        x = "Environmental Variables",
        y = "Mean Decrease in Accuracy",
        size = "Node Purity"
      )


ggsave(filename = "enes_varimp.png", plot = p3, device = "png",
           path = "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/habitat-analysis/figures/randomforest")
  

  
## plot - subset variables  -----------------------------------------------------------------------------------------------------

    enesForestData.sub <- as.data.frame(importance(enes.forest.sub))
    enesForestData.sub <- enesForestData.sub[order(enesForestData.sub[,1]),]
    enesForestData.sub$Var.Names <- row.names(enesForestData.sub)
    colnames(enesForestData.sub) <- c("Absent","Present","MeanDec","IncNodePurity","Var.Names")
    
    #ggplot     
    p4 <- ggplot(enesForestData.sub, aes(x = Var.Names, y = MeanDec)) +
      geom_segment(aes(x = Var.Names, xend = Var.Names, y = 0, yend = MeanDec, 
                       color = ifelse(MeanDec > 15, ">15", 
                                      ifelse(MeanDec >= 10, "10-15", "<10"))), 
                   show.legend = FALSE) +
      geom_point(aes(size = IncNodePurity, 
                     color = ifelse(MeanDec > 15, ">15", 
                                    ifelse(MeanDec >= 10, "10-15", "<10"))), 
                 alpha = 0.6) +
      theme_light() +
      coord_flip() +
      scale_color_manual(values = c(">15" = "blue", "10-15" = "#66C2A5", "<10" = "#FFD700")) +
      labs(color = "Range", size = "Node Purity") +
      theme(
        text = element_text(size = 20)
      ) +
      labs(
        title = "Variable Importance from Random Forest Model - ENES",
        x = "Environmental Variables",
        y = "Mean Decrease in Accuracy",
        size = "Node Purity"
      )
    
    
ggsave(filename = "enes_varimp_subset.png", plot = p4, device = "png",
           path = "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/habitat-analysis/figures/randomforest")
    
## plot - subset with dwd  --------------------------------------------------------------------------------------------------------

    enesForestData.subdwd <- as.data.frame(importance(enes.forest.subdwd))
    enesForestData.subdwd <- enesForestData.subdwd[order(enesForestData.subdwd[,1]),]
    enesForestData.subdwd$Var.Names <- row.names(enesForestData.subdwd)
    colnames(enesForestData.subdwd) <- c("Absent","Present","MeanDec","IncNodePurity","Var.Names")
    
    #ggplot     
    p6 <- ggplot(enesForestData.subdwd, aes(x = Var.Names, y = MeanDec)) +
      geom_segment(aes(x = Var.Names, xend = Var.Names, y = 0, yend = MeanDec, 
                       color = ifelse(MeanDec > 15, ">15", 
                                      ifelse(MeanDec >= 10, "10-15", "<10"))), 
                   show.legend = FALSE) +
      geom_point(aes(size = IncNodePurity, 
                     color = ifelse(MeanDec > 15, ">15", 
                                    ifelse(MeanDec >= 10, "10-15", "<10"))), 
                 alpha = 0.6) +
      theme_light() +
      coord_flip() +
      scale_color_manual(values = c(">15" = "blue", "10-15" = "#66C2A5", "<10" = "#FFD700")) +
      labs(color = "Range", size = "Node Purity") +
      theme(
        text = element_text(size = 20)
      ) +
      labs(
        title = "Variable Importance from Random Forest Model - enes",
        x = "Environmental Variables",
        y = "Mean Decrease in Accuracy",
        size = "Node Purity"
      )


ggsave(filename = "enes_varimp_subset_dwd.png", plot = p6, device = "png",
       path = "~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/habitat-analysis/figures/randomforest")

    