### Looped Krustal-Wallis tests (non parametric alternative to one-way ANOVA test when thre are >2 group) for each dependent variable (EE, RQ, H2Omg, VCO2, VO2)
### Corrected for multiple comparisons and plotted
### Jocelyn P. Colella

# Load libraries
library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)
library(agricolae)
library(plyr)

#  Box plot function 
myboxplot <- function(data, DV, wc){
  ggboxplot(data, x = "experiment", y = DV, 
            color = "experiment", palette = c("#E7B800","#00AFBB", "#FC4E07"),
            order = c("baseline", "cold", "hot"),
            ylab = as.character(DV), xlab = "Experiment") +
    stat_pvalue_manual(wc, hide.ns = TRUE) +
    labs(caption = get_pwc_label(wc))
}

#Looped Kruskal-Wallis and Wilcox tests on all datasets of interest (all, baseline=BL, Hot, Cold)
dependent_variables = c("EE", "RQ", "VO2", "VCO2", "H2Omg")
# use "all_" datasets for KW tests because all experimental treatments must be included in the same file
allexp_df_list = c("all_noOL_F", "all_day_noOL_F", "all_night_noOL_F", "all_noOL_M", "all_day_noOL_M", "all_night_noOL_M")

#write head to t test results file
t_header <- paste('Data', 'DV', 'p.adj(BL-cold)', 'p.adj(BL-hot)', 'p.adj(cold-hot)', 'eff. size', 'magnitude', sep=',')
write.table(t_header, "kruskalwallis_results.csv", sep=',', col.names = FALSE, row.names = FALSE, quote = FALSE)

# Loop through dependent variables, run KW test, run wilcox test and write results to output file
for(DV in dependent_variables){
  print(DV)
  for(i in allexp_df_list){
    print(i)
    #res.kw <- get(i) %>% kruskal.test(DV ~ experiment)
    this_effsize <- get(i) %>% kruskal_effsize(get(DV) ~ experiment)
    wc <- wilcox_test(data = get(i), as.formula(paste(DV, "~ experiment")), p.adjust.method = "bonferroni")
    line = paste(i, DV,  wc[1,]$p.adj, wc[2,]$p.adj, wc[3,]$p.adj, this_effsize$effsize, this_effsize$magnitude, sep=',')
    print(line)
    print('\n')
    write.table(line, "kruskalwallis_results.csv", sep = ',', append=TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
  }
} 

# Plotting loop for KW tests
plt_list <- c()
count = 1
for(DV in dependent_variables){
  for(i in allexp_df_list){
    print(i)
    print(DV)
    wc <- wilcox_test(data = get(i), as.formula(paste(DV, "~ experiment")), p.adjust.method = "bonferroni")
    wc <- wc %>% add_xy_position(x = "experiment")
    plot.new()
    plt_list[[count]] <-  myboxplot(get(i), DV, wc)
    count = count + 1
  }
}

# Create paneled plot for each dependent variable (EE, RQ, H2Omg, VO2, VCO2)
# Top row: FEMALES. all X exp, daytime X exp, nighttime X exp
# Bottom row: MALES. all X exp, daytime X exp, nighttime X exp
# all M, day M, night
pdf("plot_EExExp_KW.pdf", width = 8.5, height = 6)
grid.arrange(plt_list[[1]], plt_list[[2]], plt_list[[3]],
             plt_list[[4]], plt_list[[5]], plt_list[[6]],
             top = "", nrow = 2)
dev.off()

pdf("plot_RQxExp_KW.pdf", width = 8.5, height = 6)
grid.arrange(plt_list[[7]], plt_list[[8]], plt_list[[9]],
             plt_list[[10]], plt_list[[11]], plt_list[[12]],
             top = "", nrow = 2)
dev.off()

pdf("plot_h2oxExp_KW.pdf", width = 8.5, height = 6)
grid.arrange(plt_list[[13]], plt_list[[14]], plt_list[[15]],
             plt_list[[16]], plt_list[[17]], plt_list[[18]],
             top = "", nrow = 2)
dev.off()

pdf("plot_vco2xExp_KW.pdf", width = 8.5, height = 6)
grid.arrange(plt_list[[19]], plt_list[[20]], plt_list[[21]],
             plt_list[[22]], plt_list[[23]], plt_list[[24]],
             top = "", nrow = 2)
dev.off()

pdf("plot_vo2xExp_KW.pdf", width = 8.5, height = 6)
grid.arrange(plt_list[[25]], plt_list[[26]], plt_list[[27]],
             plt_list[[28]], plt_list[[29]], plt_list[[30]],
             top = "", nrow = 2)
dev.off()

# Finalized edits in InkScape as desired.
