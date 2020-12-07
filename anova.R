### Running ANOVAs for each dependent variable (EE, RQ, H2Omg, VCO2, VO2) and correcting for multiple comparisons
### Jocelyn P. Colella

# Load libraries
library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)
library(agricolae)
library(plyr)

# Create list of dataframes on which we want to run an ANOVA
allexp_df_list = c("all_noOL_F", "all_day_noOL_F", "all_night_noOL_F", "all_noOL_M", "all_day_noOL_M", "all_night_noOL_M")
# Can also test log transformmed datasets 

# Run an ANOVA on EE for each dataset 
for(i in allexp_df_list){ ### Update list of datasets used depending on what dataset you want to analyze
  print(i)
  print(get(i) %>% anova_test(EE ~ experiment)) ### UPDATE dependent variable to test each (EE, RQ, H2Omg, VO2, VCO2)
}

### Run an ANOVA corrected for sample sizes: Tukey post-hoc tst
for(i in l_allexp_df_list){ ### Update list of datasets used depending on what dataset you want to analyze
  print(i)
  print(get(i) %>% tukey_hsd(EE ~ experiment)) ### UPDATE dependent variable to test each (EE, RQ, H2Omg, VO2, VCO2)
}

# Determine the length, mean, sd, and se for each dependent variable and each dataset
for(i in allexp_df_list){# ## Update list of datasets used depending on what dataset you want to analyze
  cdata <- ddply(get(i), c("experiment"), summarise, N = length(EE), mean = mean(EE), sd   = sd(EE), se   = sd / sqrt(N)) ### UPDATE dependent variable to test each (EE, RQ, H2Omg, VO2, VCO2)
  print(cdata)
}
