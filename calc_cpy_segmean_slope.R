library(changepoint)
library(mcp)
library(EnvCpt)
library(ggplot2)
library(lubridate)
require(grDevices)
library(ggfortify)
library(tsbox)
library(zoo)
library(bcp)

#### FEMALE BASELINE EE
### Based on the AIC optimal number of changepoints identified via maximum-likelihood (changepoint_est_ML.R)
BL_noOL_F_sort <- BL_noOL_F %>% arrange(time_in_S)
ts.plot(BL_noOL_F_sort)
xmax <- length(BL_noOL_M_sort$EE)

# Multiple penalty values were already tested in changepoint_est_ML.R, we just want to use those changepoints and est segment means and slopes
# Re-run test for ML changepoints, as before
out=cpt.meanvar(BL_noOL_F_sort$EE,pen.value=c(20, 500), Q=4, penalty="CROPS",method="PELT") 
plot(out,diagnostic=TRUE) # 6 changepoints optimal
pen.value.full(out) # 6 changepoints 
plot(out,ncpts=6) # 6 changepoints = optimal, BUT many are during environmental transitions (e.g, overfit)
plot(out,ncpts=4) # More likely 2-4 ncpts (visualize various ncpts values to thoroughly explore the data)

#Raw locations of changepoints:
cpts.full(out)
cpts.full(out)[2,] # row with 6 changepoints in it
cpts.full(out)[4,] # row with 4 changepoints in it

# Rescale arbitrary 'changepoints units' to time (s) for plotting
intercepts_s = c()
rescaled_intercepts_hr_min = c()

#For each changepoint 1:whatever#
for(i in 1:6){ ### UPDATE based on the number of changepoints detected (1:6 = six change points)
  this_intercept <- cpts.full(out)[2,][[i]] #UPDATE to the row of our matrix that contains the locations of each  change point (e.g,  cpts.full(out)[2,])
  print(this_intercept)
  # Because seconds measurements may not be regularly spaced, look up changepoint indx in time_in_S to find the closest time point to that cpt
  secs <- BL_noOL_F_sort$time_in_S[this_intercept] ### UPDATE dataset being examined
  intercepts_s <- c(intercepts_s, secs)
  hrmin <- as.character(seconds_to_period(secs))
  hrmin_parts <- strsplit(hrmin, " ")
  hr <-  hrmin_parts[[1]][1]
  hr_num <- as.numeric(gsub("([0-9]+).*$", "\\1", hr))
  min <- hrmin_parts[[1]][2]
  min_num <- as.numeric(gsub("([0-9]+).*$", "\\1", min))
  this_hrmin <- paste0(hr_num,":", min_num) 
  print(this_hrmin) 
  rescaled_intercepts_hr_min <- c(rescaled_intercepts_hr_min, this_hrmin) 
}
intercepts_s # list of seconds where change points occur
rescaled_intercepts_hr_min # list of times (hr:min) where change points occur - for use in plotting

# If necessary, manually add a zero to any rescaled_intercepts_hr_min times that are missing the 0 in front of the minutes (will not be necessary for all/most cpts)
# EXAMPLE: rescaled_intercepts_hr_min[1] <- "5:08"

# FOR AIC OPTIMAL CHANGEPOINTS
# Create 2 lists: one with start positions and one with stop positions for each segment and wrie to file
header1 <- paste('dataset', 'adj or not', 'DV', 'segnum', 'mean', 'slope', sep=',')
write.table(header1, "cpt_mean_slope.csv", sep=',', col.names = FALSE, row.names = FALSE, quote = FALSE)

start <- 0
starts_list <- c()
stops_list <- c()
for(this_s in intercepts_s){
  starts_list <- c(starts_list, start)
  stops_list<- c(stops_list, this_s)
  start <- this_s + 1
}

# Iterate through both lists to calc mean and slope (this_data) is the same for all tests in this file)
this_data <- BL_noOL_F_sort
loop_seg_num <- 1
for (i in seq_along(starts_list)){
  start <- starts_list[i]
  stop <- stops_list[i]
  this_interval <- start:stop
  subset_data = this_data[this_data$time_in_S %in% this_interval, ]
  segmean <- mean(subset_data$EE) ### UPDATE for each dependent variable you're testing (EE, RQ, H2Omg, VCO2, VO2) 
  this_lm <- lm(subset_data$EE ~ subset_data$time_in_S) ### UPDATE for each dependent variable you're testing (EE, RQ, H2Omg, VCO2, VO2) 
  slope <- as.numeric(coef(this_lm)[2])
  
  ### UPDATE outputs for each dataset and dependent variable you're analyzing
  line <- paste("BL_noOL_F_sort", 'N', 'EE', loop_seg_num, round(segmean,3), round(slope,3), sep=',') #prints: dataset, N [for not-altered = AIC optimized changepoints], segment number, segment mean, segment slope
  write.table(line, "cpt_mean_slope.csv", sep=',', append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
  loop_seg_num <- loop_seg_num + 1 
}

###############################
# Now do the same for the MANULLY ADJUSTED NUMBER OF CHANGEPOINTS (based on visual inspect of various ncpts values in: plot(out,ncpts=4))
adj_intercepts_s <- intercepts_s[-c(2)]
start <- 0
starts_list <- c()
stops_list <- c()
for(this_s in adj_intercepts_s){
  starts_list <- c(starts_list, start)
  stops_list<- c(stops_list, this_s)
  start <- this_s + 1
}

# Iterate through both lists to calc mean and slope (this_data) is the same for all tests in this file)
loop_seg_num <- 1
for (i in seq_along(starts_list)){
  start <- starts_list[i]
  stop <- stops_list[i]
  this_interval <- start:stop
  subset_data = this_data[this_data$time_in_S %in% this_interval, ]
  segmean <- mean(subset_data$EE) ### UPDATE for each dependent variable you're testing (EE, RQ, H2Omg, VCO2, VO2) 
  this_lm <- lm(subset_data$EE ~ subset_data$time_in_S) ### UPDATE for each dependent variable you're testing (EE, RQ, H2Omg, VCO2, VO2) 
  slope <- as.numeric(coef(this_lm)[2])
  
  ### UPDATE outputs for each dataset and dependent variable you're analyzing
  line <- paste("BL_noOL_F_sort", 'A', 'EE', loop_seg_num, round(segmean,3), round(slope,3), sep=',') #prints: dataset, A [for altered = NOT AIC optimized changepoints, but more realistic cpts], segment number, segment mean, segment slope
  write.table(line, "cpt_mean_slope.csv", sep=',', append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
  loop_seg_num <- loop_seg_num + 1 
}

### Repeat for each dependent variable: EE, RQ, H2Omg, VCO2, VO2)
### Repeat for each experiment: BL=baseline, Hot, Cold
