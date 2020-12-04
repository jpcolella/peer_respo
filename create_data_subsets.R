### R script Creating data subsets
### Author: Jocelyn P. Colella
### Input data files made via remove_outliers.R

# Load libraries
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

### Female data subsets
all_noOL_F <- read_csv("all_noOL_F.csv", 
                       col_types = cols(Sex = col_character(),
                                        EE = col_double(),H2Omg = col_double(), RQ = col_double(),
                                        Animal_ID = col_character(),Deg_C = col_double(),
                                        weight = col_double(), experiment = col_character(),
                                        StartTime = col_character(), 
                                        SD_VCO2 = col_double(), SD_VO2 = col_double(), SD_H2Omg = col_double(),
                                        VO2 = col_double(), VCO2 = col_double(),
                                        StartDate = col_date(format = "%Y-%m-%d"), hour = col_integer()))

# Add a column of the total time in SECONDS to reach each sampling point (24:00 clock)
all_noOL_F$time_in_S <- period_to_seconds(hms(all_noOL_F$StartTime))

### Repeat for MALES
all_noOL_M <- read_csv("all_noOL_M.csv", 
                       col_types = cols(Sex = col_character(),
                                        EE = col_double(),H2Omg = col_double(), RQ = col_double(),
                                        Animal_ID = col_character(),Deg_C = col_double(),
                                        weight = col_double(), experiment = col_character(),
                                        StartTime = col_character(), 
                                        SD_VCO2 = col_double(), SD_VO2 = col_double(), SD_H2Omg = col_double(),
                                        VO2 = col_double(), VCO2 = col_double(),
                                        StartDate = col_date(format = "%Y-%m-%d"), hour = col_integer()))

### Establish when each interval/transition starts and stops IN SECONDS
# Daytime interval: hrs:8:00-21:00
daytime_interval <- period_to_seconds(hms("09:00:00")):period_to_seconds(hms("20:00:00"))

# Night time: hrs 22:00-5:00 (do NOT do a 22:5 sequence as it will count backwards from 22 to 5...)
nighttime_interval <- c((period_to_seconds(hms("21:00:01")):period_to_seconds(hms("24:59:59"))), #evening portion of 'nighttime'
                        (period_to_seconds(hms("00:00:00")):period_to_seconds(hms("06:00:00")))) #morning portion of 'nightitme'
# Morning transition (t1): 6:00-9:00
t1_interval <- period_to_seconds(hms("06:00:00")):period_to_seconds(hms("09:00:00"))

# Evening transition (t2): 20-21:00
t2_interval <- period_to_seconds(hms("20:00:00")):period_to_seconds(hms("21:00:00"))



### ==============================
### Create data subsets for males and females EXCLUDING OUTLIERS
# Subset each experiment (all, BL, Hot, Cold) and each time block [e.g., t1/transition1, daytime, t2/transistion2, nighttime]) within each experiment
# ALL males and females across all 3 experiments (BL, hot, cold)

# ALL (all experiments combined, no outliers)
# MALES
all_day_noOL_M = all_noOL_M[all_noOL_M$time_in_S %in% daytime_interval, ]
  all_night_noOL_M = all_noOL_M[all_noOL_M$time_in_S %in% nighttime_interval, ]
  all_t1_noOL_M = all_noOL_M[all_noOL_M$time_in_S %in% t1_interval, ]
  all_t2_noOL_M = all_noOL_M[all_noOL_M$time_in_S %in% t2_interval, ]
# FEMALES
all_day_noOL_F = all_noOL_F[all_noOL_F$time_in_S %in% daytime_interval, ]
  all_night_noOL_F = all_noOL_F[all_noOL_F$time_in_S %in% nighttime_interval, ]
  all_t1_noOL_F = all_noOL_F[all_noOL_F$time_in_S %in% t1_interval, ]
  all_t2_noOL_F = all_noOL_F[all_noOL_F$time_in_S %in% t2_interval, ]

#BASELINE EXPERIMENTAL SUBSETS
# MALES
BL_noOL_M = all_noOL_M[all_noOL_M$experiment == 'baseline', ]  
  BL_day_noOL_M = all_noOL_M[all_noOL_M$experiment == 'baseline' & all_noOL_M$time_in_S %in% daytime_interval, ]
  BL_night_noOL_M = all_noOL_M[all_noOL_M$experiment == 'baseline' & all_noOL_M$time_in_S %in% nighttime_interval, ]
  BL_t1_noOL_M = all_noOL_M[all_noOL_M$experiment == 'baseline' & all_noOL_M$time_in_S %in% t1_interval, ]
  BL_t2_noOL_M = all_noOL_M[all_noOL_M$experiment == 'baseline' & all_noOL_M$time_in_S %in% t2_interval, ]
# FEMALES
BL_noOL_F = all_noOL_F[all_noOL_F$experiment == 'baseline', ] 
  BL_day_noOL_F = all_noOL_F[all_noOL_F$experiment == 'baseline' & all_noOL_F$time_in_S %in% daytime_interval, ]
  BL_night_noOL_F = all_noOL_F[all_noOL_F$experiment == 'baseline' & all_noOL_F$time_in_S %in% nighttime_interval, ]
  BL_t1_noOL_F = all_noOL_F[all_noOL_F$experiment == 'baseline' & all_noOL_F$time_in_S %in% t1_interval, ]
  BL_t2_noOL_F = all_noOL_F[all_noOL_F$experiment == 'baseline' & all_noOL_F$time_in_S %in% t2_interval, ]

# HOT EXPERIMENTAL SUBSETS
# MALES
hot_noOL_M = all_noOL_M[all_noOL_M$experiment == 'hot', ] 
  hot_day_noOL_M = all_noOL_M[all_noOL_M$experiment == 'hot' & all_noOL_M$time_in_S %in% daytime_interval,] 
  hot_night_noOL_M = all_noOL_M[all_noOL_M$experiment == 'hot' & all_noOL_M$time_in_S %in% nighttime_interval,] 
  hot_t1_noOL_M = all_noOL_M[all_noOL_M$experiment == 'hot' & all_noOL_M$time_in_S %in% t1_interval,] 
  hot_t2_noOL_M = all_noOL_M[all_noOL_M$experiment == 'hot' & all_noOL_M$time_in_S %in% t2_interval,] 
# FEMALES
hot_noOL_F = all_noOL_F[all_noOL_F$experiment == 'hot', ]
  hot_day_noOL_F = all_noOL_F[all_noOL_F$experiment == 'hot' & all_noOL_F$time_in_S %in% daytime_interval,]
  hot_night_noOL_F = all_noOL_F[all_noOL_F$experiment == 'hot' & all_noOL_F$time_in_S %in% nighttime_interval,] 
  hot_t1_noOL_F = all_noOL_F[all_noOL_F$experiment == 'hot' & all_noOL_F$time_in_S %in% t1_interval,] 
  hot_t2_noOL_F = all_noOL_F[all_noOL_F$experiment == 'hot' & all_noOL_F$time_in_S %in% t2_interval,] 

# COLD EXPERIMENTAL SUBSETS
# MALES
cold_noOL_M = all_noOL_M[all_noOL_M$experiment == 'cold', ] 
  cold_night_noOL_M = all_noOL_M[all_noOL_M$experiment == 'cold' & all_noOL_M$time_in_S %in% nighttime_interval, ]
  cold_t1_noOL_M = all_noOL_M[all_noOL_M$experiment == 'cold' & all_noOL_M$time_in_S %in% nighttime_interval, ]
  cold_t2_noOL_M = all_noOL_M[all_noOL_M$experiment == 'cold' & all_noOL_M$time_in_S %in% nighttime_interval, ]
# FEMALES
cold_noOL_F = all_noOL_F[all_noOL_F$experiment == 'cold', ] 
  cold_day_noOL_F = all_noOL_F[all_noOL_F$experiment == 'cold' & all_noOL_F$time_in_S %in% daytime_interval, ]
  cold_night_noOL_F = all_noOL_F[all_noOL_F$experiment == 'cold' & all_noOL_F$time_in_S %in% nighttime_interval, ]
  cold_t1_noOL_F = all_noOL_F[all_noOL_F$experiment == 'cold' & all_noOL_F$time_in_S %in% nighttime_interval, ]
  cold_t2_noOL_F = all_noOL_F[all_noOL_F$experiment == 'cold' & all_noOL_F$time_in_S %in% nighttime_interval, ]
  
### Run this script after remove_outlier.R but before the other scripts in this directory, as downstream analyses uses these data subsets
