### Bayesian (B) changepoint estimation to detect shifts in mean and variance (e.g., changepoints [cpts]) in continuous respirometry data, and plotting
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

### BASELINE FEMALE EE (will repeat this for each dependent variable: EE, RQ, H2Omg, VCO2, VO2)
# Sort data by time
BL_noOL_F_sort <- BL_noOL_F %>% arrange(time_in_S)

# Check if the data are continuous?
ts.plot(BL_noOL_F_sort) # yes

# Perform a Bayesian analysis of change points
fit_bcp = bcp(BL_noOL_F_sort$EE, mcmc = 10000, burnin = 1000)
plot(fit_bcp)

# Identify indicies for changepoints with a posterior probability > 0.2
which(fit_bcp$posterior.prob > 0.20)

# RAW changepoint locations output from the above command
B_changepoints = c(406,  430,  432, 1583) # These values will change for each variable and experiment

#rescale changepoints to time:
B_intercepts_s = c()
B_rescaled_intercepts_hr_min = c()
for(i in 1:4){ ### UPDATE the length of the sequence based on how many change points were identifid above (PP > 0.2). 4 changepoints = 1:4, 5 changepoints = 1:5
  this_intercept <- B_changepoints[i]
  print(this_intercept)
  secs <- BL_noOL_F_sort$time_in_S[this_intercept]
  B_intercepts_s <- c(B_intercepts_s, secs)
  hrmin <- as.character(seconds_to_period(secs))
  hrmin_parts <- strsplit(hrmin, " ")
  hr <-  hrmin_parts[[1]][1]
  hr_num <- as.numeric(gsub("([0-9]+).*$", "\\1", hr))
  min <- hrmin_parts[[1]][2]
  min_num <- as.numeric(gsub("([0-9]+).*$", "\\1", min))
  this_hrmin <- paste0(hr_num,":", min_num)
  print(this_hrmin)
  B_rescaled_intercepts_hr_min <- c(B_rescaled_intercepts_hr_min, this_hrmin)
}
B_intercepts_s
B_rescaled_intercepts_hr_min

# add a zero to any rescaled-intercepts_hr_min times missing the 0 in front of the minutes (will not need to do this for most changepoints)
B_rescaled_intercepts_hr_min[1] <- "5:08"

# PLOT (function code bades on: https://rdrr.io/cran/bcp/src/R/output.R)
make_a_plot <- function(x, ...) { 
  if ((is.matrix(x$data) && ncol(x$data)>2 ) && 
      !(attr(x, "model") == "multivariate" || attr(x, "structure") == "series"))
    stop("Legacy bcp plot invalid for multivariate bcp object or graph bcp object.")
  posterior.prob <- x$posterior.prob
  posterior.prob[length(posterior.prob)] <- 0
  op <- par(mfrow=c(2,1),col.lab="black",col.main="black")
  op2 <- par(mar=c(0,4,4,2),xaxt="n", cex.axis=0.75)
  #UPPER PLOT
  plot(1:nrow(x$data), x$data[,2], col=alpha("grey", 0.7), 
       pch=20, cex = 1, cex.axis = 1, xlab="", ylab="Posterior Mean")#,
  lines(x$posterior.mean, lwd=2, col = "black")
  par(op2)
  op3 <- par(mar=c(5,4,0,2), xaxt="s", cex.axis=0.75)
  #LOWER PLOT
  plot(1:length(x$posterior.mean), posterior.prob, 
       yaxt="n", xaxt='n', type="l", ylim=c(0,1),lwd = 2, 
       xlab="Time (24-hour)", ylab="Posterior Probability", main="")
  
  ### RAW locations of Bayesian change points
  abline(v = c(406,  430,  432, 1583), col="red", lty=2, lwd =2) ### UPDATE these values for each variable/experiement (same values as in: B_changepoints) 
  
  ### TIME locations of changepoints relative to X axis scale - adjust for each plot
  text(c(18501, 19618, 19701, 72725), ### UPDATE for each variable/experiemnt (same values as in: B_intercepts_s)
       c(rep(0.95, length(B_rescaled_intercepts_hr_min))), B_rescaled_intercepts_hr_min,
       pos=2, cex.axis = 1)
  axis(1, at = locations, labels = c("0", "6", "12", "18", "24"), cex.axis = 1) 
  axis(2, yaxp=c(0, 0.8, 2), cex.axis = 1)
  par(op3)
  par(op)
}

make_a_plot(fit_bcp)

# Export plot as pdf
pdf("F_BL_EE_cptPlot.pdf", width = 5, height = 6)
make_a_plot(fit_bcp)
dev.off()

### Rerun for each dependent variable: EE, RQ, H2O, VCO2, VO2
### Rerun for males (M)
### Rerun for each experimental group: BL, Hot, Cold
