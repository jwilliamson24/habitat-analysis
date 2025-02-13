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


## settings -----------------------------------------------------------------------------------------------

    rm(list=ls())
    setwd("~/Library/CloudStorage/OneDrive-Personal/Documents/Academic/OSU/Git/oss-occu/data")
    library(vegan)
    library(car)
    library(FSA)
    

## load data ----------------------------------------------------------------------------------------------


    dat <- read.csv("env_subset_corr.csv", row.names = 1)
    dat2 <- readRDS("site_level_matrix.rds") #need this for the treatments
    row.names(dat2) <- dat2[,1]
    dat2$trt <- as.factor(dat2$trt)


# subset

    # removing detection covariates from the matrix so that we are only assessing 
    # habitat variables that are impacted by the treatment
    # took out aspect, precip, days since rain

    dat <- dat[,-c(11:13)]

# standardize

    dat_std <- decostand(dat, "standardize") #z-scores



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
    
    anova_results <- lapply(c("temp_sq", "soil_moist", "dwd_count", "decay_cl"), function(var) {
      model <- aov(dat[[var]] ~ dat2$trt)
      summary(model)
    })
    names(anova_results) <- c("temp_sq", "soil_moist", "dwd_count", "decay_cl")
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
    
          # $dwd_count
          # Tukey multiple comparisons of means
          # 95% family-wise confidence level
          # 
          # Fit: aov(formula = dat[[var]] ~ dat2$trt)
          # 
          # $`dat2$trt`
          #            diff        lwr       upr     p adj
          # BU-BS -17.807692 -26.753910 -8.861475 0.0000020 ***
          # HB-BS  -2.800000 -11.831693  6.231693 0.9113655
          # HU-BS  -1.259259 -10.125598  7.607079 0.9948848
          # UU-BS -15.080000 -24.111693 -6.048307 0.0000916 ***
          # HB-BU  15.007692   6.155005 23.860380 0.0000685 ***
          # HU-BU  16.548433   7.864507 25.232359 0.0000057 ***
          # UU-BU   2.727692  -6.124995 11.580380 0.9131645
          # HU-HB   1.540741  -7.231217 10.312698 0.9884906
          # UU-HB -12.280000 -21.219057 -3.340943 0.0020561 **
          # UU-HU -13.820741 -22.592698 -5.048783 0.0002585 ***
          # 
          # 
          # $decay_cl
          # Tukey multiple comparisons of means
          # 95% family-wise confidence level
          # 
          # Fit: aov(formula = dat[[var]] ~ dat2$trt)
          # 
          # $`dat2$trt`
          #             diff         lwr         upr     p adj
          # BU-BS  0.42596154  0.16559254  0.68633054 0.0001334 ***
          # HB-BS  0.24550000 -0.01735666  0.50835666 0.0792753 .
          # HU-BS -0.08101852 -0.33906275  0.17702571 0.9075524
          # UU-BS  0.55750000  0.29464334  0.82035666 0.0000004 ***
          # HB-BU -0.18046154 -0.43810847  0.07718540 0.3022904
          # HU-BU -0.50698006 -0.75971538 -0.25424473 0.0000016 ***
          # UU-BU  0.13153846 -0.12610847  0.38918540 0.6200937
          # HU-HB -0.32651852 -0.58181590 -0.07122114 0.0050321 **
          # UU-HB  0.31200000  0.05183938  0.57216062 0.0102347 *
          # UU-HU  0.63851852  0.38322114  0.89381590 0.0000000 ***
          #
          # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
          
## kruskal-wallis -----------------------------------------------------------------------------------------------
    
    # non-parametric version of anove, using my non-normal vars:
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
    
    
    # there are lots of significant comparisons here, need to interpret
    
          # $canopy_cov
          # Dunn (1964) Kruskal-Wallis multiple comparison
          # p-values adjusted with the Holm method.
          # 
          # Comparison          Z      P.unadj        P.adj
          # 1     BS - BU -5.7656890 8.132492e-09 4.066246e-08 ***
          # 2     BS - HB  0.0000000 1.000000e+00 1.000000e+00
          # 3     BU - HB  5.8266041 5.656660e-09 3.959662e-08 ***
          # 4     BS - HU -0.1529876 8.784080e-01 1.000000e+00
          # 5     BU - HU  5.7836359 7.310307e-09 4.386184e-08 ***
          # 6     HB - HU -0.1546337 8.771101e-01 1.000000e+00
          # 7     BS - UU -7.9926990 1.320162e-15 1.056129e-14 ***
          # 8     BU - UU -2.3277105 1.992748e-02 7.970992e-02
          # 9     HB - UU -8.0755271 6.718578e-16 6.718578e-15 ***
          # 10    HU - UU -8.0747268 6.762788e-16 6.086509e-15 ***
          # 
          # $veg_cov
          # Dunn (1964) Kruskal-Wallis multiple comparison
          # p-values adjusted with the Holm method.
          # 
          # Comparison          Z      P.unadj        P.adj
          # 1     BS - BU -4.1461939 3.380476e-05 0.0003380476 ***
          # 2     BS - HB -0.2305423 8.176704e-01 0.8176704214
          # 3     BU - HB  3.9547950 7.660032e-05 0.0006894029 ***
          # 4     BS - HU -1.0914295 2.750839e-01 0.8252518134
          # 5     BU - HU  3.1570707 1.593627e-03 0.0127490179 *
          # 6     HB - HU -0.8658041 3.865976e-01 0.7731952072
          # 7     BS - UU -2.2968370 2.162807e-02 0.1513964955
          # 8     BU - UU  1.8467189 6.478789e-02 0.3239394382
          # 9     HB - UU -2.0877077 3.682421e-02 0.2209452761
          # 10    HU - UU -1.2616730 2.070665e-01 0.8282658933
          # 
          # $dwd_cov
          # Dunn (1964) Kruskal-Wallis multiple comparison
          # p-values adjusted with the Holm method.
          # 
          # Comparison          Z      P.unadj       P.adj
          # 1     BS - BU  1.6980483 0.0894986547 0.536991928
          # 2     BS - HB  3.5454378 0.0003919615 0.003919615 **
          # 3     BU - HB  1.9011397 0.0572837177 0.400986024
          # 4     BS - HU  1.0829655 0.2788237459 1.000000000
          # 5     BU - HU -0.6436225 0.5198202784 1.000000000
          # 6     HB - HU -2.5557996 0.0105944103 0.084755282 .
          # 7     BS - UU  0.4082664 0.6830781000 0.683078100
          # 8     BU - UU -1.2994666 0.1937838506 0.968919253
          # 9     HB - UU -3.1696818 0.0015260594 0.013734535 .
          # 10    HU - UU -0.6742625 0.5001444346 1.000000000
          # 
          # $fwd_cov
          # Dunn (1964) Kruskal-Wallis multiple comparison
          # p-values adjusted with the Holm method.
          # 
          # Comparison          Z      P.unadj        P.adj
          # 1     BS - BU  2.0378464 4.156529e-02 1.662612e-01
          # 2     BS - HB  5.4397523 5.335471e-08 4.268377e-07 ***
          # 3     BU - HB  3.4903698 4.823526e-04 3.376468e-03 **
          # 4     BS - HU -0.9927208 3.208460e-01 9.625381e-01
          # 5     BU - HU -3.1129717 1.852138e-03 1.111283e-02 .
          # 6     HB - HU -6.6042236 3.996059e-11 3.996059e-10 ***
          # 7     BS - UU -0.2250784 8.219183e-01 8.219183e-01
          # 8     BU - UU -2.2890061 2.207900e-02 1.103950e-01
          # 9     HB - UU -5.7235352 1.043300e-08 9.389697e-08 ***
          # 10    HU - UU  0.7716590 4.403164e-01 8.806329e-01
          # 
          # $size_cl
          # NULL
          # 
          # $char_cl
          # Dunn (1964) Kruskal-Wallis multiple comparison
          # p-values adjusted with the Holm method.
          # 
          # Comparison          Z      P.unadj        P.adj
          # 1     BS - BU -0.8813160 3.781468e-01 7.562936e-01
          # 2     BS - HB -2.7806286 5.425377e-03 2.170151e-02 .
          # 3     BU - HB -1.9462268 5.162750e-02 1.548825e-01
          # 4     BS - HU  4.7332616 2.209405e-06 1.104702e-05 ***
          # 5     BU - HU  5.7406230 9.432888e-09 6.603022e-08 ***
          # 6     HB - HU  7.6471509 2.054811e-14 1.849330e-13 ***
          # 7     BS - UU  5.2448207 1.564344e-07 9.386061e-07 ***
          # 8     BU - UU  6.2415003 4.333935e-10 3.467148e-09 ***
          # 9     HB - UU  8.1086168 5.119923e-16 5.119923e-15 ***
          # 10    HU - UU  0.6159296 5.379410e-01 5.379410e-01
          # 
          # $avg_volume
          # Dunn (1964) Kruskal-Wallis multiple comparison
          # p-values adjusted with the Holm method.
          # 
          # Comparison          Z      P.unadj        P.adj
          # 1     BS - BU -0.2682586 7.885003e-01 0.7885002978
          # 2     BS - HB  1.6206957 1.050829e-01 0.5254146291
          # 3     BU - HB  1.9245595 5.428449e-02 0.3257069252
          # 4     BS - HU  1.0384166 2.990761e-01 0.8972283985
          # 5     BU - HU  1.3365905 1.813563e-01 0.7254253684
          # 6     HB - HU -0.6190946 5.358540e-01 1.0000000000
          # 7     BS - UU -2.3648086 1.803939e-02 0.1443151444
          # 8     BU - UU -2.1415332 3.223106e-02 0.2256174481
          # 9     HB - UU -4.0268059 5.653967e-05 0.0005653967 ***
          # 10    HU - UU -3.4844192 4.932063e-04 0.0044388564 ***
          #
          # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
    
    
    
    
    
    
    