# -------------------------------------------------------------------------------------------------------
##
## 06-anova-kruskal-wallis.R 
##
## ANOVA and Kruskal-Wallis using habitat variables for Ch 2 analyses
##
## Jasmine Williamson
## Date Created: 02-11-2025
##
## 
## goals --------------------------------------------------------------------------------------------------

# Univariate methods to identify which specific habitat characteristics differ among treatments 
# ANOVA for variables that meet normality and homogeneity of variance assumptions
# Kruskal-Wallis for those that do not (mostly the categorical ones)

## insights --------------------------------------------------------------------------------------------------

# anova : dwd count and decay cl significantly differ among treatments
# dwd count pairs that are sig diff : BU-BS, UU-BS, HB-BU, HU-BU, UU-HB, UU-HU
# decay cl paris that are sig diff : BU-BS, HB-BS, UU-BS, HU-HB, UU-HB, UU-HU

# kruskal-wallis : canopy cov, veg cov, dwd cov, fwd cov, char cl, avg vol significantly differ among trts
# lots of significant difference in site pairs, not sure what to do with that info

# added avg dwd count per subplot just to see; its the same as the site-level sum dwd count, both highly significant

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


# subset

    # removing detection covariates from the matrix so that we are only assessing 
    # habitat variables that are impacted by the treatment
    # took out aspect, precip, days since rain

    dat <- dat[,-c(11:13)]
    dat$avg_dwd_count <- (dat$dwd_count/7)

# standardize

    dat_std <- decostand(dat, "standardize") #z-scores
    dat$temp_sq <- sqrt(dat$temp)


## test for normality ----------------------------------------------------------------------------------------------
    
    
# Q-Q plots
    
    par(mfrow = c(3, 4))  # Arrange plots in a grid (3 rows, 4 columns)
    for (var in names(dat)) {
      qqnorm(dat[[var]], main = paste("Q-Q Plot:", var))
      qqline(dat[[var]], col = "red")
    }
    par(mfrow = c(1,1))  # Reset plot layout
    
    
    # If points closely follow the red line, the variable is approximately normal.
    # If points deviate significantly (e.g., S-shape or heavy tails), normality is violated.
 
    
# Shapiro Wilk test
    
    shapiro_results <- sapply(dat, function(x) shapiro.test(x)$p.value)
    shapiro_results

    # If p > 0.05 in Shapiro-Wilk, use ANOVA
    # If p ≤ 0.05, use Kruskal-Wallis 
    
    # normal: 
        # soil_moist (p = 0.123)
        # dwd_count (p = 0.570)
        # decay_cl (p = 0.069)
    # not normal: 
        # temp (p = 0.032)
        # canopy_cov (p = 6.99e-14)
        # veg_cov (p = 1.60e-09)
        # dwd_cov (p = 1.16e-05)
        # fwd_cov (p = 8.37e-05)
        # size_cl (p = 7.65e-06)
        # char_cl (p = 1.89e-11)
        # avg_volume (p = 0.004)
    
    
# try transforming slightly skewed variables and rerun
    
    #going to use sqrt temp in ANOVA
    #not sure about avg vol since it was more skewed so just gonna leave it for kruskal
    
    dat$temp_log <- log(dat$temp)
    dat$temp_sq <- sqrt(dat$temp)
  
    dat$avg_volume_log <- log(dat$avg_volume)
    dat$avg_volume_sq <- sqrt(dat$avg_volume)
    
    shapiro.test(dat$temp_log)
    shapiro.test(dat$temp_sq)
    
    qqnorm(dat$temp_sq)
    qqline(dat$temp_sq, col = "red")
    
    shapiro.test(dat$avg_volume_log)
    shapiro.test(dat$avg_volume_sq)
    
    qqnorm(dat$avg_volume_sq)
    qqline(dat$avg_volume_sq, col = "red")
    
    
    
## test for homogeneity of variance ----------------------------------------------------------------------------
    
    # using normal variables
    # tests whether the variance of x (a numeric covariate) differs between the levels of treatment
    
    leveneTest(dat$soil_moist ~ dat2$trt)  
    leveneTest(dat$dwd_count ~ dat2$trt)
    leveneTest(dat$decay_cl ~ dat2$trt)
    leveneTest(dat$temp_sq ~ dat2$trt)

    # If the p-value is > 0.05, it suggests that the variances are homogeneous across treatments
    
    # all pass test
    
    
## anova ------------------------------------------------------------------------------------------------------
    
    # using soil moist, dwd count, decay cl, temp_sq
    
    anova_results <- lapply(c("temp_sq", "soil_moist", "dwd_count", "decay_cl", "avg_dwd_count"), function(var) {
      model <- aov(dat[[var]] ~ dat2$trt)
      summary(model)
    })
    names(anova_results) <- c("temp_sq", "soil_moist", "dwd_count", "decay_cl","avg_dwd_count")
    anova_results
  
    
    # If p < 0.05, the variable significantly differs among treatments
    
    # dwd count and decay cl are significant
    

## tukey hsd ------------------------------------------------------------------------------------------------------    

#Follow up with Tukey’s HSD test for pairwise comparisons 
    
    # used to determine which specific group means are different from each other
    
    # While ANOVA tells you if there are any significant differences between the means of multiple groups, 
    # it doesn't indicate which groups differ. Tukey HSD is used to find the exact pairs of group means 
    # that are significantly different.
    
    
    # Run Tukey HSD on each aov object
    tukey_results <- lapply(c("temp_sq", "soil_moist", "dwd_count", "decay_cl"), function(var) {
      model <- aov(dat[[var]] ~ dat2$trt)  # Run ANOVA again
      return(TukeyHSD(model))  # Apply Tukey HSD
    })
    
    # View Tukey HSD results
    names(tukey_results) <- c("temp_sq", "soil_moist", "dwd_count", "decay_cl")
    tukey_results
    
    
## summarize anova/tukey results in table ---------------------------------------------------------------------
    
    # Create a function to format Tukey HSD results
    format_tukey <- function(tukey_result) {
      # Get the treatment comparisons
      comparisons <- tukey_result[[1]]
      
      # If comparisons is a vector, convert it to a data frame
      if (is.atomic(comparisons)) {
        comparisons <- as.data.frame(comparisons)
      }
      
      # Filter for significant comparisons (p < 0.05)
      significant_comps <- comparisons[comparisons$p.adj < 0.05, ]
      
      # If no significant comparisons, return a message
      if (nrow(significant_comps) == 0) {
        return("No significant pairwise comparisons")
      }
      
      # Format the significant comparisons
      formatted_comps <- paste(
        paste(significant_comps$comparison, "diff =", 
              format(significant_comps$diff, digits = 3)),
        collapse = "\n"
      )
      
      return(formatted_comps)
    }
    
    # Create the main table
    anova_table <- data.frame(
      Variable = names(anova_results),
      F_value = sapply(anova_results, function(x) x[[1]]$`F value`[1]),
      p_value = sapply(anova_results, function(x) x[[1]]$`Pr(>F)`[1])
    )
    
    # Format p-values nicely
    anova_table$p_value <- ifelse(
      anova_table$p_value < 0.001,
      "< 0.001",
      format(anova_table$p_value, scientific = TRUE)
    )
    
    # add significant col
    anova_table$significant <- c("FALSE","FALSE","TRUE","TRUE")
    

# Display the table
    print(anova_table)
    
    # Variable    F_value      p_value        Tukey Significant Comparisons (added by hand, code wouldnt work)
    # temp_sq     1.1091130    3.5540e-01
    # soil_moist  0.6810519    6.0638e-01
    # dwd_count   13.7254583    < 0.001       BU-BS \ UU-BS \ HB-BU \ HU-BU \ UU-HB \ UU-HU
    # decay_cl    17.1435557    < 0.001       BU-BS \ UU-BS \ HU-BU \ HU-HB \ UU-HB \ UU-HU
    
 
    
    
## visualize     ---------------------------------------------------------------------
    
    # F-value barplot
    
    ggplot(anova_table, aes(x = Variable, y = F_value)) +
      geom_col(aes(fill = significant), position = "identity") +
      theme_minimal() +
      labs(
        title = "ANOVA Results",
        x = "Variable",
        y = "F-value"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      ) +
      scale_fill_manual(values = c("gray", "steelblue"))
    
    
    
## kruskal-wallis -----------------------------------------------------------------------------------------------
    
    # non-parametric version of anova, using my non-normal vars:
    # canopy cov, veg cov, dwd cov, fwd cov, size cl, char cl, avg vol
    
    
    kruskal_results <- lapply(c("canopy_cov", "veg_cov", "dwd_cov", "fwd_cov", "size_cl", "char_cl", "avg_volume"), function(var) {
      kruskal.test(dat[[var]] ~ dat2$trt)
    })
    names(kruskal_results) <- c("canopy_cov", "veg_cov", "dwd_cov", "fwd_cov", "size_cl", "char_cl", "avg_volume")
    kruskal_results
    
    
    # If p < 0.05, the variable significantly differs among treatments
    
    # significant : canopy cov, veg cov, dwd cov, fwd cov, char cl, avg vol
    # (all except size cl)
    
    
   
## dunns -----------------------------------------------------------------------------------------------
    
#Follow up with Dunn’s test (post hoc test for Kruskal-Wallis):
    
    # for a single variable
    dunnTest(dat$canopy_cov ~ dat2$trt, method = "bonferroni")
    
    
    # Run Dunn's test for each variable
    dunn_results <- lapply(c("canopy_cov", "veg_cov", "dwd_cov", "fwd_cov", "size_cl", "char_cl", "avg_volume"), function(var) {
        
         kruskal_test <- kruskal.test(dat[[var]] ~ dat2$trt)  # Kruskal-Wallis test for each variable
      
      if (kruskal_test$p.value < 0.05) { # Apply Dunn's test if Kruskal-Wallis is significant
        return(dunnTest(dat[[var]] ~ dat2$trt, kw=TRUE))  # Dunn's test
      } else {
        return(NULL)  # If Kruskal-Wallis is not significant, return NULL
      }
    })

    
    names(dunn_results) <- c("canopy_cov", "veg_cov", "dwd_cov", "fwd_cov", "size_cl", "char_cl", "avg_volume")
    dunn_results
    
    

## format K-W and Dunn's results into table ------------------------------------------------------------
# much of code sourced from phind    
    
# organize K-W results
    
    # Create a data frame with the test statistics
    results_table <- data.frame(
      Variable = names(kruskal_results),
      Chi_Squared = sapply(kruskal_results, function(x) x$statistic),
      p_value = sapply(kruskal_results, function(x) x$p.value),
      Effect_Size = sapply(kruskal_results, function(x) 
        format(x$statistic / (sum(!is.na(dat[[names(kruskal_results)[1]]]) & 
                                    !is.na(dat2$trt)) * (length(unique(dat2$trt)) - 1)), digits = 3))
    )
    
    # Format p-values nicely
    results_table$p_value <- ifelse(
      results_table$p_value < 0.001,
      "< 0.001",
      format(results_table$p_value, scientific = TRUE)
    )
    
    # Sort by chi-squared value (strongest to weakest effect)
    results_table <- results_table[order(-results_table$Chi_Squared), ]
    

# organize Dunn's results    
    
    # Create a function to get Dunn's test comparisons
    get_dunn_comparisons <- function(var_name) {
      clean_var_name <- gsub("\\.Kruskal-Wallis chi-squared", "", var_name)
      
      if (!is.null(dunn_results[[clean_var_name]])) {
        df <- dunn_results[[clean_var_name]]$res        # Get the data frame from the dunnTest object
        comparisons <- paste(df$Comparison, "p =", format(df$P.adj, scientific = TRUE))        # Create comparison strings
        return(paste(comparisons, collapse = "\n"))        # Return all significant comparisons
      }
      return("Not significant")
    }
    
    # Add Dunn's test results to the table
    results_table$Dunn_Comparisons <- sapply(
      rownames(results_table),
      function(x) get_dunn_comparisons(x)
    )
    
    
    # Create a function to format the comparisons nicely
    format_comparisons <- function(comparisons) {
      comp_lines <- strsplit(comparisons, "\n")[[1]]      # Split the comparisons string into individual lines
      comparisons_only <- gsub(" p = .*", "", comp_lines)       # Extract just the comparison names and p-values
      p_values <- as.numeric(gsub(".*p = ", "", comp_lines))
      significant_comps <- comparisons_only[p_values < 0.05]   # Filter for significant comparisons (p < 0.05)

      # If no significant comparisons, return a message
      if (length(significant_comps) == 0) {
        return("Comparisons: No significant pairwise comparisons")
      }
      return(paste("Comparisons:", paste(significant_comps, collapse = "\n")))   # Return formatted significant comparisons
    }
    
    # Create a new table with formatted comparisons
    results_table$Dunn_Comparisons <- sapply(
      results_table$Dunn_Comparisons,
      function(x) format_comparisons(x)
    )
    

# Display the full table
    print(results_table)
    
    
    # Variable     Chi_Squared     p_value    Effect_Size    Dunn_Comparisons
    # canopy_cov   119.8949828     < 0.001       0.236       Comparisons: BS - BU\nBU - HB\nBU - HU\nBS - UU\nHB - UU\nHU - UU
    # char_cl      103.8020187     < 0.001       0.204       Comparisons: BS - HB\nBS - HU\nBU - HU\nHB - HU\nBS - UU\nBU - UU\nHB - UU
    # fwd_cov      55.5070828      < 0.001       0.109       Comparisons: BS - HB\nBU - HB\nBU - HU\nHB - HU\nHB - UU
    # veg_cov      23.7868232      < 0.001      0.0468       Comparisons: BS - BU\nBU - HB\nBU - HU
    # avg_volume   19.2191300      < 0.001      0.0378       Comparisons: HB - UU\nHU - UU
    # dwd_cov      15.6389435     3.543e-03     0.0308       Comparisons: BS - HB\nHB - UU
    # size_cl      0.7071337      9.504e-01    0.00139       Comparisons: Not significant
    
    results_table$Effect_Size <- as.numeric(results_table$Effect_Size)
    
    ggplot(results_table, aes(x = reorder(Variable, Effect_Size), y = Effect_Size)) +
      geom_bar(stat = "identity", width = 0.6, fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()
      ) +
      labs(
        title = "Effect Sizes of Variables",
        x = "Variables",
        y = "Effect Size"
      )  
    