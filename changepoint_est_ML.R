### Maximum likelihood (ML) changepoint estimation to detect shifts in mean and variance (e.g., changepoints [cpts]) in continuous respirometry data, and plotting
### Author: Jocelyn P. Colella
### Run remove_outliers.R and create_data_subsets.R first, as the datasubsets produced by those scripts are used herein

# Load libraries
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

#### FEMALE BASELINE EE (will repeat this for each dependent variable: EE, RQ, H2Omg, VCO2, VO2)
### Maximum Likelihood changepoint estimation
BL_noOL_F_sort <- BL_noOL_F %>% arrange(time_in_S)

#Are the data continuous?
ts.plot(BL_noOL_F_sort) # yes

# ML test of a range of changepoints
set.seed(1)
out=cpt.meanvar(BL_noOL_F_sort$EE,pen.value=c(5, 500), Q=4, penalty="CROPS",method="PELT")

# Determine how many segments/changepoints are optimal?
plot(out,diagnostic=TRUE) # Must reach asymptote
cpts.full(out) # All changepoints
pen.value.full(out) # Optimal nuumber of changepoints

# Now, rerun with adjusted penalty values
out=cpt.meanvar(BL_noOL_F_sort$EE,pen.value=c(20, 500), Q=4, penalty="CROPS",method="PELT")
plot(out,diagnostic=TRUE) # 6 cpts = optimal
pen.value.full(out) # Yup, 6 cpts
plot(out,ncpts=6) # Plot cpt segments and data (visualize various ncpt values to thoroughly explore the data)

#Raw locations of changepoints:
cpts.full(out)
cpts.full(out)[2,] # here's the line that contains 6 cpts (the optimal based on the data)

# Rescale arbitrary 'changepoints units' to time(s) for plotting
intercepts_s = c()
rescaled_intercepts_hr_min = c()

#For each changepoint 1:N
for(i in 1:6){
  this_intercept <- cpts.full(out)[2,][[i]] 
  print(this_intercept)
  # Because seconds measurements may not be regularly spaced, look up changepoint indx in time_in_S to find the closest time point to that cpt
  secs <- BL_noOL_F_sort$time_in_S[this_intercept]
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
rescaled_intercepts_hr_min # list of times (hr:min) where change points occur

# Manually add a zero to any rescaled_intercepts_hr_min times that are missing the 0 in front of the minutes (will not be necessary for all/most cpts)
rescaled_intercepts_hr_min[1] <- "5:08"

# Scale plot by seconds in a day (only do this once for all cpt plots)
locations <- c()
sec_in_day <- 86400
changept_scale <- length(BL_noOL_F_sort$EE) 
locations <- c(floor(0),
               floor(as.numeric((changept_scale*21600)/sec_in_day)), 
               floor(as.numeric((changept_scale * 43200)/sec_in_day)),
               floor(as.numeric((changept_scale * 64800)/sec_in_day)),
               floor(changept_scale))

# Plot data and changepoints 
plot(out, ncpts = 6, type = "p", pch = 16, cex = 0.7,
     col = alpha("#E7B800", 0.5), cpt.width = 3, xaxt='n',
     xlab="Time (s)", ylab="EE",
     frame.plot =  FALSE, xlim=c(0, 1900), ylim=c(0, 0.5))
axis(1, at = locations, labels = c("0", "21600", "43200", "64800", "86400")) 
abline(v = c(406, 551, 612, 1441, 1506, 1597), col="grey31", lty=2, lwd =2)
text(c(420, 580, 800, 1450, 1620, 1800), 
     c(rep(0.5, length(rescaled_intercepts_hr_min))),
     rescaled_intercepts_hr_min, pos=2)
### Export as pdf

### Now repeat for baseline female: EE (done!), RQ, H2Omg, VO2, and VCO2
### Then repeat for each experiment (baseline = BL, Hot, Cole)
