### Test for a relationship between weight and each response variable: EE, RQ, H2O, VO2, VCO2
### Run remove_outliers.R and create_data_subsets.R first
### Author: Jocelyn P. Colella

# NOTE: If R starts plotting only a single point, restart R, recreate data subsets and rerun (then it will miraculously work again)
  # This is why we should use python instead of R...

# Load libraries
library(dplyr)
library(cowplot)
  
# Set working directory
setwd("~/Documents/Jocie/projects/Pero_respo/Analyses/data/")

######## FEMALES ###########
### BASELINE Females
# Group animals by ID, calculate mean EE for each animal, then create new dataset with weight (col1) and mean EE (col2)
BL_animalweight <- BL_noOL_F %>% group_by(Animal_ID) %>% summarise(weight = mean(weight)) %>% select(weight)
BL_meanEE <- BL_noOL_F %>% group_by(Animal_ID) %>% summarise(EE = mean(EE)) %>% select(EE)
BL_EEwt <- cbind(BL_animalweight, BL_meanEE)

# Test for a relationship between animal weight and EE (see: Pr(>|t|))
summary(lm(BL_meanEE[[1]] ~ BL_animalweight[[1]]))

# Plot relationship as a linear regression
BL_F_eeXwt_plot <- ggscatter(BL_EEwt, x = "weight", y = "EE", palette = "jco",add = "reg.line") +
  stat_cor(label.y = 0.37) +
  stat_regline_equation(label.y = 0.40) +
  theme(plot.title = element_text(size = 14, hjust=0.5)) +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "lines")) +
  labs(title="Baseline", x = "", y ="Female: EE")
BL_F_eeXwt_plot

### HOT Females
hot_animalweight <- hot_noOL_F %>% group_by(Animal_ID) %>% summarise(weight = mean(weight)) %>% select(weight)
hot_meanEE <- hot_noOL_F %>% group_by(Animal_ID) %>% summarise(EE = mean(EE)) %>% select(EE)
hot_EEwt <- cbind(hot_animalweight, hot_meanEE)
summary(lm(hot_meanEE[[1]] ~ hot_animalweight[[1]]))
hot_F_eeXwt_plot <- ggscatter(hot_EEwt, x = "weight", y = "EE", palette = "jco", add = "reg.line") +
  stat_cor(label.y = 0.37) +
  stat_regline_equation(label.y = 0.40) +
  theme(plot.title = element_text(size = 14, hjust=0.5)) +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "lines")) +
  labs(title="Hot", x = "", y ="")
hot_F_eeXwt_plot

### COLD Female
cold_animalweight <- cold_noOL_F %>% group_by(Animal_ID) %>% summarise(weight = mean(weight)) %>% select(weight)
cold_meanEE <- cold_noOL_F %>% group_by(Animal_ID) %>% summarise(EE = mean(EE)) %>% select(EE)
cold_EEwt <- cbind(cold_animalweight, cold_meanEE)
summary(lm(cold_meanEE[[1]] ~ cold_animalweight[[1]]))
cold_F_eeXwt_plot <- ggscatter(cold_EEwt, x = "weight", y = "EE", palette = "jco", add = "reg.line") +
  stat_cor(label.y = 0.37) +
  stat_regline_equation(label.y = 0.39) +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "lines")) +
  theme(plot.title = element_text(size = 14, hjust=0.5)) +
  labs(title="Cold", x = "", y ="")
cold_F_eeXwt_plot


######## MALES ###########
### BASELINE Males
BL_animalweight <- BL_noOL_M %>% group_by(Animal_ID) %>% summarise(weight = mean(weight)) %>% select(weight)
BL_meanEE <- BL_noOL_M %>% group_by(Animal_ID) %>% summarise(EE = mean(EE)) %>% select(EE)
BL_EEwt <- cbind(BL_animalweight, BL_meanEE)
summary(lm(BL_meanEE[[1]] ~ BL_animalweight[[1]]))
BL_M_eeXwt_plot <- ggscatter(BL_EEwt, x = "weight", y = "EE", palette = "jco", add = "reg.line") +
  stat_cor(label.y = 0.37) +
  stat_regline_equation(label.y = 0.40) +
  #theme(plot.title = element_text(size = 14, hjust=0.5)) +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "lines")) +
  labs(title="", x = "", y ="Male: EE")
BL_M_eeXwt_plot


### HOT Males
hot_animalweight <- hot_noOL_M %>% group_by(Animal_ID) %>% summarise(weight = mean(weight)) %>% select(weight)
hot_meanEE <- hot_noOL_M %>% group_by(Animal_ID) %>% summarise(EE = mean(EE)) %>% select(EE)
hot_EEwt <- cbind(hot_animalweight, hot_meanEE)
summary(lm(hot_meanEE[[1]] ~ hot_animalweight[[1]]))
hot_M_eeXwt_plot <- ggscatter(hot_EEwt, x = "weight", y = "EE", palette = "jco", add = "reg.line") +
  stat_cor(label.y = 0.365) +
  stat_regline_equation(label.y = 0.39) +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "lines")) +
  labs(title="", x = "Weight (g)", y ="")
hot_M_eeXwt_plot


### COLD Males
cold_animalweight <- cold_noOL_M %>% group_by(Animal_ID) %>% summarise(weight = mean(weight)) %>% select(weight)
cold_meanEE <- cold_noOL_M %>% group_by(Animal_ID) %>% summarise(EE = mean(EE)) %>% select(EE)
cold_EEwt <- cbind(cold_animalweight, cold_meanEE)
summary(lm(cold_meanEE[[1]] ~ cold_animalweight[[1]]))
cold_M_eeXwt_plot <- ggscatter(cold_EEwt, x = "weight", y = "EE", palette = "jco",add = "reg.line") +
  stat_cor(label.y = 0.375) +
  stat_regline_equation(label.y = 0.39) +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "lines")) +
  labs(title="", x = "", y ="")
cold_M_eeXwt_plot


### CREATE PANELD PLOT IN R
ggdraw() +
  draw_plot(BL_F_eeXwt_plot,  x = 0, y = 0.5, width = .33, height = .5) +
  draw_plot(hot_F_eeXwt_plot, x = 0.33, y = 0.5, width = .33, height = .5) +
  draw_plot(cold_F_eeXwt_plot, x = 0.66, y = 0.5, width = .33, height = .5) +
  
  draw_plot(BL_M_eeXwt_plot,  x = 0,    y = 0.02, width = .33, height = .5) +
  draw_plot(hot_M_eeXwt_plot,  x = 0.33, y = 0.02, width = .33, height = .5) +
  draw_plot(cold_M_eeXwt_plot, x = 0.66, y = 0.02, width = .33, height = .5)

### REPEAT for each dependent variable (EE, RQ, H2Omg, VCO2, VO2)
### Can also repeat for _day_ and _night_ data subsets for each experiment (BL, Hot, Cold), as desired
