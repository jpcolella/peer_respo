# R script to generate paneled time series plots (24-hr) for respirometry variables (EE, RQ, H2O, VCO2, VO2)
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

### BASELINE FEMALES
### GENERATE PLOT FOR EACH RESPONSE VARIABLE: EE, RQ, H2Omg, VCO2, VO2
### AND REPEAT EACH SEX (F = Female, M = Male)
BL_F_EE_plot <- ggplot(BL_noOL_F,aes(x = time_in_S, y = EE))+#, color = experiment, fill = experiment)) +
  geom_point(alpha = 0.5, cex = 0.5, color = "#E7B800") +
  #Add dashed temp line, may need to alter scaling
  geom_smooth(aes(x = time_in_S, y = Deg_C*0.018), se = FALSE, method= "loess", span = 0.45, lwd = 0.75, lty="dashed", color = 'grey25')+
  geom_smooth( method='loess', span=.45, color = "white") +
  theme(plot.title = element_text(size = 10, hjust=0.5)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  scale_y_continuous(limits = c(0.0, 0.6), breaks=c(0.0, 0.2, 0.4, 0.6)) + # may need to alter scape
  theme(axis.text.x = element_blank())+#,axis.text.y = element_blank()) +
  labs(title = "", x ="", y = "BASELINE")
BL_F_EE_plot 
    # Resulting plots:
    #BL_F_EE_plot, BL_F_RQ_plot, BL_F_H2O_plot, BL_F_VO2_plot, BL_F_VCO2_plot
    #BL_M_EE_plot, BL_M_RQ_plot, BL_M_H2O_plot, BL_M_VO2_plot, BL_M_VCO2_plot


#HOT FEMALES
### GENERATE PLOT FOR EACH RESPONSE VARIABLE: EE, RQ, H2Omg, VCO2, VO2
### AND REPEAT EACH SEX (F = Female, M = Male)
hot_F_EE_plot <- ggplot(hot_noOL_F,aes(x = time_in_S, y = EE))+
  geom_point(alpha = 0.5, cex = 0.5, color = "#FC4E07") +
  #Add dashed temp line to HOT plots, may need to adjust scaling
  geom_hline(yintercept=0.45, linetype="dashed", color = "grey25", lwd=1) +
  geom_smooth( method='loess', span=.45, color = "white") +
  theme(plot.title = element_text(size = 10)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  scale_y_continuous(limits = c(0.0, 0.6), breaks=c(0.0, 0.2, 0.4, 0.6)) +
  theme(axis.text.x = element_blank())+#,axis.text.y = element_blank()) +
  labs(title = "", x ="", y = "HOT")
hot_F_EE_plot
    # Resulting plots
    #hot_F_EE_plot, hot_F_RQ_plot, hot_F_H2O_plot, hot_F_VO2_plot, hot_F_VCO2_plot
    #hot_M_EE_plot, hot_M_RQ_plot, hot_M_H2O_plot, hot_M_VO2_plot, hot_M_VCO2_plot
    
#COLD FEMALES
### GENERATE PLOT FOR EACH RESPONSE VARIABLE: EE, RQ, H2Omg, VCO2, VO2
### AND REPEAT EACH SEX (F = Female, M = Male)
cold_F_EE_plot <- ggplot(cold_noOL_F,aes(x = time_in_S, y = EE))+
  geom_point(alpha = 0.5, cex = 0.5, color = "#00AFBB") +
  #Add dashed temp line to COLD plots, may need to alter scale
  geom_hline(yintercept=0.1, linetype="dashed", color = "grey25", lwd=1) +#geom_smooth(method="lm", color = "black", fill = "grey25") +
  geom_smooth( method='loess', span=.45, color = "white") +
  theme(plot.title = element_text(size = 10)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  scale_y_continuous(limits = c(0.0, 0.6), breaks=c(0.0, 0.2, 0.4, 0.6)) +
  scale_x_continuous(limits = c(0, 86400), breaks = c(0, 43200, 86400), labels = c('0', '12', '24'))+
  labs(title = "", x = "", y="COLD")
cold_F_EE_plot
    #Resulting plots:
    #cold_F_EE_plot, cold_F_RQ_plot, cold_F_H2O_plot, cold_F_VO2_plot, cold_F_VCO2_plot
    #cold_M_EE_plot, cold_M_RQ_plot, cold_M_H2O_plot, cold_M_VO2_plot, cold_M_VCO2_plot


# Generate paneled plot of EE, RQ, and H2O for females and males
ggdraw() +
  draw_plot(BL_F_EE_plot,  x = 0,     y = 0.52, width = .18, height = .28) +
  draw_plot(BL_M_EE_plot,  x = 0.17,    y = 0.52, width = .18, height = .28) +
  draw_plot(BL_F_RQ_plot,  x = 0.32, y = 0.52, width = .18, height = .28) +
  draw_plot(BL_M_RQ_plot,  x = 0.48, y = 0.52, width = .18, height = .28) +
  draw_plot(BL_F_h2o_plot, x = 0.65, y = 0.52, width = .18, height = .28) +
  draw_plot(BL_M_h2o_plot, x = 0.85, y = 0.52, width = .18, height = .28) +
  draw_plot(hot_F_EE_plot,  x = 0,    y = 0.4, width = .17, height = .28) +
  draw_plot(hot_M_EE_plot,  x = 0,    y = 0.4, width = .17, height = .28) +
  draw_plot(hot_F_RQ_plot,  x = 0.33, y = 0.4, width = .17, height = .28) +
  draw_plot(hot_M_RQ_plot,  x = 0.33, y = 0.4, width = .17, height = .28) +
  draw_plot(hot_F_h2o_plot, x = 0.66, y = 0.4, width = .17, height = .28) +
  draw_plot(hot_M_h2o_plot, x = 0.66, y = 0.4, width = .17, height = .28) +
  draw_plot(cold_F_EE_plot,  x = 0,    y = 0.2, width = .33, height = .3) +
  draw_plot(cold_M_EE_plot,  x = 0,    y = 0.2, width = .33, height = .3) +
  draw_plot(cold_F_RQ_plot,  x = 0.33, y = 0.2, width = .33, height = .3) +
  draw_plot(cold_M_RQ_plot,  x = 0.33, y = 0.2, width = .33, height = .3) +
  draw_plot(cold_F_h2o_plot,  x = 0.66, y = 0.2, width = .33, height = .3) +
  draw_plot(cold_M_h2o_plot, x = 0.66, y = 0.2, width = .33, height = .3)

  
# To create supplementary figure for VO2 and VCO2  
  ggdraw() +
    draw_plot(BL_F_VCO2_plot,  x = 0,     y = 0.7, width = .2, height = .3) +
    draw_plot(BL_M_VCO2_plot,  x = 0.18,    y = 0.7, width = .2, height = .3) +
    draw_plot(BL_F_VO2_plot,  x = 0.36, y = 0.7, width = .2, height = .3) +
    draw_plot(BL_M_VO2_plot,  x = 0.54, y = 0.7, width = .2, height = .3) +
    draw_plot(hot_F_VCO2_plot,  x = 0,  y = 0.45, width = .2, height = .3) +
    draw_plot(hot_M_VCO2_plot,  x = 0.18,  y = 0.45, width = .2, height = .3) +
    draw_plot(hot_F_VO2_plot,  x = 0.36, y = 0.45, width = .2, height = .3) +
    draw_plot(hot_M_VO2_plot,  x = 0.54, y = 0.45, width = .2, height = .3) +
    draw_plot(cold_F_VCO2_plot,  x = 0,    y = 0.15, width = .2, height = .33) +
    draw_plot(cold_M_VCO2_plot,  x = 0.18, y = 0.15, width = .2, height = .33) +
    draw_plot(cold_F_VO2_plot,  x = 0.36, y = 0.15, width = .2, height = .33) +
    draw_plot(cold_M_VO2_plot,  x = 0.54, y = 0.15, width = .2, height = .33)
    

