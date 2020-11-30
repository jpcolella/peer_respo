# R script to generate paneled violin plots for respirometry variables (E, RQ, H2O, VCO2, VO2)
# Author: Jocelyn P Colella

library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(hrbrthemes)
library(viridis)
library(cowplot)
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

# Function to calculate mean, median, min and max
data_summary <- function(x) {
  med <- median(x)
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=med,ymin=ymin,ymax=ymax))
}

#### VIOLIN PLOTS
# ALL female (F) data (day and night)
    # Run for each variable: EE, RQ, H2Omg, VCO2, VO2
    # Re-run for each data set: all_, all_day, all_night
    # then run for Males (F = Female, M = Male), for each variable and dataset
F_all_pEE <- all_noOL_F %>%
  ggplot( aes(x=experiment, y=EE, fill=experiment, color=experiment)) +
  geom_jitter(color = "darkgrey", alpha = 0.5, cex = 0.5) +
  geom_violin(width=1, size=0.5, alpha = 0.8) +
  scale_color_manual(values=c("#E7B800", "#00AFBB", "#FC4E07")) +
  scale_fill_manual(values=c("#E7B800", "#00AFBB", "#FC4E07")) +
  stat_summary(fun.data=mean_sdl, mult=1, geom="pointrange", color="black") +
  theme_ipsum() + theme(legend.position="none") +
  theme(plot.title = element_text(size=14)) + theme(plot.margin = unit(c(0,0,0,0), "lines")) + ylim(0,0.6)+
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  labs(title="EE", x = "Females", y ="")
F_all_pEE 
    # Resulting plots:
    #Female plots
    #F_all_pEE, F_all_pRQ, F_all_pH2O, F_all_VO2, F_all_VCO2
    #F_day_pEE, F_day_pRQ, F_day_pH2O, F_day_VO2, F_day_VCO2
    #F_night_pEE, F_night_pRQ, F_night_pH2O, F_night_VO2, F_night_VCO2
    
    #Male plots:
    #M_all_pEE, M_all_pRQ, M_all_pH2O, M_all_VO2, M_all_VCO2
    #M_day_pEE, M_day_pRQ, M_day_pH2O, M_day_VO2, M_day_VCO2
    #M_night_pEE, M_night_pRQ, M_night_pH2O, M_night_VO2, M_night_VCO2

# Paneled plot for EE, RQ, and H2O for both males and females
plot_grid(F_all_pEE, F_day_pEE, F_night_pEE, M_all_pEE, M_day_pEE, M_night_pEE,
          F_all_pRQ, F_day_pRQ, F_night_pRQ, M_all_pRQ, M_day_pRQ, M_night_pRQ,
          F_all_pH2O, F_day_pH2O, F_night_pH2O, M_all_pH2O, M_day_pH2O, M_night_pH2O,
          ncol = 3, labels = c('All', 'Day', 'Night'), label_x = c(0.3, 0.3, 0.3), label_size = 12)

# Paneled plot for VCO2 and VO2
plot_grid(F_all_pVCO2, F_day_pVCO2, F_night_pVCO2, M_all_pVCO2, M_day_pVCO2, M_night_pVCO2,
          F_all_pVO2, F_day_pVO2, F_night_pVO2,M_all_pVO2, M_day_pVO2, M_night_pVO2,
          ncol = 3, labels = c('All', 'Day', 'Night'), label_size = 12)
