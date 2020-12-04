### TESTING ANOVA ASSUMPTIONS
### Jocelyn P. Colella

# Load libraries
library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)
library(agricolae)
library(plyr)

#ASSUMPTIONS
# 1 - Independence of observations (automatically VIOLATED for hot/cold conditions b/c we used the same animals)
# 2 - No significant outiers (removed prior to analysis)
# 3 - Normality 
# 4 - Homogeneity of variances


#######################################
### ASSUMPTION 2 - No Outliers?
#Check out the data  for FEMALES (repeat workflow for males)
all_noOL_F %>% sample_n_by(experiment, size = 1)
levels(as.factor(all_noOL_F$experiment))
all_noOL_F %>% group_by(experiment) %>% get_summary_stats(EE, type = "mean_sd")

### ASSUMPTION 2 - Detect outliers
OLs <- all_noOL_F %>% group_by(experiment) %>% identify_outliers(EE)
ggboxplot(all_noOL_F, x = "experiment", y = "EE")
# Note: there are still outliers <- assumption violated

# Create data subset without outliers (AGAIN...) - this is iterative outlier trimming, BEWARE! (hint: it doesn't make a difference, there are more outliers once these are removed)
dependent_variables = c("EE", "RQ", "VO2", "VCO2", "H2Omg")

# Identify outliers for each dependent variable for FEMALES
for (DV in dependent_variables){
  print(DV)
  OLs <- all_noOL_F %>% group_by(experiment) %>% identify_outliers(DV) ### UPDATE dataset being analyzed
  OL_list = c()
  masterlist = c()
  for (each_outlier_row in 1:nrow(OLs)){
    this_weight <- as.numeric(OLs[each_outlier_row, "weight"])
    this_DV  <- as.numeric(OLs[each_outlier_row, DV])
    OL_list <- c(OL_list, (which(all_noOL_F$weight == this_weight & all_noOL_F[DV] == this_DV))) ### UPDATE dataset being analyzed
  }
  print(OL_list)
  assign(paste("F_OL_list_", DV, sep = ""), OL_list)  ### UPDATE to M_ or F_ based on input data
} #Repeat for males

all_noEEol_F <- all_noOL_F[-c(F_OL_list_EE),]
all_noRQol_F <- all_noOL_F[-c(F_OL_list_RQ),]
all_noVO2ol_F <- all_noOL_F[-c(F_OL_list_VO2),]
all_noVCO2ol_F <- all_noOL_F[-c(F_OL_list_VCO2),]
all_noH2Ool_F <- all_noOL_F[-c(F_OL_list_H2Omg),]
# Repeat for males

# Box plots following second round of outlier removal
all_noEEol_F %>% group_by(experiment) %>% identify_outliers(EE)
ggboxplot(all_noEEol_F, x = "experiment", y = "EE")
#aaaaaand there's more outliers



#######################################
### ASSUMPTION 3 - Normality
allexp_df_list = c("all_noOL_F", "all_day_noOL_F", "all_night_noOL_F",
                   "all_noOL_M", "all_day_noOL_M", "all_night_noOL_M")
#also tested with logged datasets - results consistent
#also tested with datasets output from the second round of outlier removal - results consistent

### Use Shapiro-Wilks test to test for the normality of our data
### Adjust p-values using a Bonferroni correction
k <- length(allexp_df_list) ### UPDATE based on dataset being analyzed
num_pw_comparisons <- k *(k-1)/2
for(i in allexp_df_list){ ### UPDATE based on dataset being analyzed
  print(i)
  model <- lm(VO2 ~ experiment, data = get(i))
  model.metrics <- augment(model) 
  if (dim(model.metrics)[1] < 5000){
    this_shaptest <- shapiro_test(model.metrics$.resid)
    new_p <- p.adjust(this_shaptest$p.value, method = "bonferroni", num_pw_comparisons)
    print(this_shaptest$p.value)
    print(new_p)
  } else {
    subsamp <- model.metrics[sample(nrow(model.metrics), 4999), ] #shapiro_test can only examine <5000 samples, so randomly downsample to 4999 observations
    this_shaptest <- shapiro_test(subsamp$.resid)
    new_p <- p.adjust(this_shaptest$p.value, method = "bonferroni", num_pw_comparisons)
    print(this_shaptest$p.value)
    print(new_p)
  }
}





#######################################
### ASSUMPTION 4 - Homogeneity of variance
# Levene's test: Run for each DV and on logged/unlogged datasets
k <- length(allexp_df_list) ### UPDATE based on dataset being analyzed
num_pw_comparisons <- k * (k-1)/2
for(i in l_allexp_df_list){ ### UPDATE based on dataset being analyzed
  print(i)
  model <- lm(EE ~ experiment, data = get(i))
  model.metrics <- augment(model)
  my_metrics <- model.metrics %>% levene_test(.resid ~ model.metrics$experiment)
  new_p <- p.adjust(my_metrics$p, method = "bonferroni", num_pw_comparisons)
  print(new_p)
}
