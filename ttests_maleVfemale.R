# R script to run t-test between sexes (M and F) to see if they differ in respirometry variables and plot 
# Author: Jocelyn P Colella

library(ggplot2)
library(tidyverse)
library(car) 
library(lubridate)
library(RColorBrewer)
library(ggpubr)
library(ggpmisc)
library(gridExtra)
library(rlist)
library(ggplot2)
library(broom)
library(nlme)
library(rstatix)
library(compositions)
library(dplyr)
library(viridis)
library(tidyverse)
library(Hmisc)
library(reshape2)

setwd("~/Documents/Jocie/projects/Pero_respo/Analyses/data/")

### Are male and female weights significantly different?
######## BASELINE F (day and night)
F_animalweight <- all_noOL_F %>% group_by(Animal_ID) %>% summarise(weight = mean(weight)) %>% select(weight)
M_animalweight <- all_noOL_M %>% group_by(Animal_ID) %>% summarise(weight = mean(weight)) %>% select(weight)

#First, run  shapiro-wilks test to determine that data are normally distributed
#Female shapiro-wilks test
this_shaptest <- shapiro_test(F_animalweight$weight)
new_p <- p.adjust(this_shaptest$p.value, method = "bonferroni", num_pw_comparisons)
print(this_shaptest$p.value) #0.78
print(new_p) #1

#Male shapiro-wilks test
this_shaptest <- shapiro_test(M_animalweight$weight)
new_p <- p.adjust(this_shaptest$p.value, method = "bonferroni", num_pw_comparisons)
print(this_shaptest$p.value) #0.96
print(new_p) #1

#Next, run an F-test to determine the two datasets have similar variance
var.test(F_animalweight$weight, M_animalweight$weight, alternative = "two.sided")
  #pval = 0.3268 > 0.05
    # No significant difference between the two variances

#Then run an unpaired two sample ttest to determine if the means are different
t.test(F_animalweight$weight, M_animalweight$weight, alternative = "less", var.equal = TRUE)
  #two-sided: p=0.09323 > 0.05, weight are NOT significantly different

#### CALCULATE MEAN and SD for each dependent variable for each sex (M, F) and each experiment (BL, Hot Cold) and write out to file
dependent_variables = c("EE", "RQ", "VO2", "VCO2", "H2Omg")
MF_all_list = c("all_noOL_F", "all_noOL_M", "BL_noOL_F", "BL_noOL_M", "hot_noOL_F", "hot_noOL_M", "cold_noOL_F", "cold_noOL_M")

#Write header to mean/sd file
meanSD_header <- paste('DV', 'Dataset', 'mean', 'sd', sep=',')
write.table(meanSD_header, "mean_sd_eachTreatment_Xsex.csv", sep=',', col.names = FALSE, row.names = FALSE, quote = FALSE)

#write head to ttest file
t_header <- paste('Data1', 'Data2', 'DV', 'p-value', 'unadj. p', sep=',')
write.table(t_header, "ttest_results_Xsex.csv", sep=',', col.names = FALSE, row.names = FALSE, quote = FALSE)

#For each data frame (day and night of each treatment)
#Calcualte the average and SD of each depenedent variable
count = 1
for(df in MF_all_list){
  for (DV in dependent_variables){
    this_mean = mean(get(df)[[DV]])
    this_sd = sd(get(df)[[DV]])
    line = paste(DV, df, this_mean, this_sd, sep=',')
    write.table(line, "mean_sd_eachTreatment_Xsex.csv", sep = ',', append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
    templist <- list("all_noOL_F", "all_noOL_M",  "BL_noOL_F", "BL_noOL_M", "hot_noOL_F", "hot_noOL_M", "cold_noOL_F", "cold_noOL_M")
    k <- length(templist)
    num_pw_comparisons <- k*(k-1) / 2 
    for(data in templist){
      ts <- t.test(get(df)[[DV]], get(data)[[DV]], conf.level = 0.95)
      new_p <- p.adjust(ts$p.value, method = "bonferroni", num_pw_comparisons)
      this_line=paste(df, data, DV, new_p, ts$p.value, sep = ',')
      write.table(this_line, "ttest_results_Xsex.csv", sep=',', append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
    }
    count = count + 1
  }
}

# PLOTTING
# REPEAT FOR EACH VARIABLE: EE, RQ, H2Omg, VCO2, VO2 (and rename output variable accordingly)
ttest_xSex <- read.csv("ttest_results_Xsex.csv")
ttest_xSex$roundP <- round(ttest_xSex$p.value, 2)
ee_xSex = ttest_xSex[ttest_xSex$DV == 'EE', ] # RERUN FOR EACH VAR and rename outputs accordingly
ee_mat <- dcast(ee_xSex, Data1~Data2, value.var="roundP") # RERUN FOR EACH VAR and rename outputs accordingly
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
ee_tri_m <- melt(get_lower_tri(ee_mat)) # RERUN FOR EACH VAR and rename outputs accordingly


# Rename 'value' column to pvalue for labeling purposes
ee_tri_m <- ee_tri_m %>% rename(pvalue = value)

#Edit input data name for each variable
ggplot(data = ee_tri_m, aes(Data1, variable, fill = pvalue))+
  geom_tile(color = "white")+
  scale_fill_viridis(option="viridis", alpha = 0.75, limit = c(-1, 1), na.value="white") +
  labs(y="Dataset 1", x = "Dataset 2") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1), axis.text.y = element_text(size = 12),
        legend.justification = c(0.9, 0),legend.position = "none", legend.direction = "horizontal", legend.title = element_blank())+
  coord_fixed() +
  geom_text(aes(x=Data1,y=variable,label=pvalue),size=3,color="black") 

### THEN REPEAT FOR M (males) and F (females), DAY AND NIGHT WITHIN EACH EXP
#MF_dayNight_list <- c("BL_day_noOL_F", "BL_night_noOL_F",
#                      "hot_day_noOL_F", "hot_night_noOL_F",
#                      "cold_day_noOL_F", "cold_night_noOL_F",
#                      
#                      "BL_day_noOL_M", "BL_night_noOL_M", 
#                      "hot_day_noOL_M", "hot_night_noOL_M",
#                      "cold_day_noOL_M", "cold_night_noOL_M")

#Final figure tweaks were done in Inkscape
