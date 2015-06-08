#!/usr/bin/R
#author = "Michael Gruenstaeudl, PhD"
#copyright = "Copyright (C) 2014 Michael Gruenstaeudl"
#contributors = c("Michael Gruenstaeudl")
#email = "gruenstaeudl.1@osu.edu"
#version = "2015.01.15.2200"

################################
# STEP1. Set Globals Variables #
################################
library(ggplot2)
library(grid)
setwd("/home/michael/Desktop/")
sim_nums = c(1,2,11,14,18,19)
n_reps = c(2,10,100)
## CHANGE HERE!
indir = paste("/home/michael/research/analyses/03_analyses_P2C2M/",
            "REVISION_SimRepli/03_extracted/01_wModel/", sep="")

###########################
# STEP2. Helper Functions #
###########################
do_stacking = function(data_fn, n_reps){
  indata = get(data_fn)
  ## Change here, if so desired
  sub_data = indata$rawStats$RAY$dif
  handle = stack(as.data.frame(sub_data))
  handle[,3] = n_reps
  colnames(handle)= c("value", "gene", "n_reps")
return(handle)
}

make_name = function(n_rep, sim_num) {
## CHANGE HERE!
  return(paste("RS2.wModel.Hi.", sim_num, ".nReps", n_rep, sep=""))
}

rm_outliers = function(indata) {
  indata = sample(indata[indata>quantile(indata, probs = seq(0, 1, 0.1))[2]], length(indata), replace=TRUE)
  indata = sample(indata[indata<quantile(indata, probs = seq(0, 1, 0.1))[10]], length(indata), replace=TRUE)
  outdata = sample(indata[!indata %in% boxplot.stats(indata)$out], length(indata), replace=TRUE)
  return(outdata)
}

####################
# STEP2. Load data #
####################
for (sim_num in sim_nums) {
  indata = unlist(lapply(n_reps, make_name, sim_num))
  fns = unlist(lapply(indir, paste0, lapply(indata, paste0, ".rda")))
  lapply(fns, load, .GlobalEnv)
}

########################################
# STEP3. Transform data and make plots #
########################################

for (sim_num in sim_nums) {

  ## Parse data
  indata = unlist(lapply(n_reps, make_name, sim_num))
  nRep1_data = do_stacking(indata[1], n_reps[1])
  ## BELOW LINE FOR TESTING
  #nRep1_data[,1] = rm_outliers(nRep1_data[,1])
  outdata = nRep1_data  
  for (i in 2:length(indata)) {
    outdata = rbind(outdata, do_stacking(indata[i], n_reps[i]))
  }

#  ## Make plots - Facet type 1
#  plots[[sim_num]] = ggplot(data=outdata, aes(x=value, group=n_reps)) +
#    #geom_density(aes(linetype=factor(n_reps))) +
#    geom_density(aes(colour=factor(n_reps))) +
#    facet_grid(gene ~ .) +
#    geom_vline(xintercept=0, linetype="dashed") +
#    #scale_x_discrete(breaks=c(nums), labels=c(nums)) +
#    labs(x="Difference Values") +
#    xlim(-30, 10) +
#    #scale_y_discrete(limits=c("sum", c(n_loci:1))) +
#    #ylim(100, 0) +
#    labs(y="") +
#    theme_bw() +
#    theme(axis.text.y = element_blank(),
#          axis.ticks.y = element_blank(),
#          strip.text.y = element_text(angle=0),
#          strip.background = element_rect(fill="white"),
#          plot.title = element_text(face="bold", size=rel(1.5), vjust=-1)) +
#    ggtitle(expression(atop("Distribution of Differences",
#    atop(italic("Descriptive Statistic: RAY") , ""))))

  ## Make plots - Facet type 2
  ggplot(data=outdata, aes(x=value)) +
    #geom_density(aes(linetype=factor(n_reps))) +
    geom_density() +
    facet_grid(gene ~ n_reps, scales = "free_y") +
    geom_vline(xintercept=0, linetype="dashed") +
    #scale_x_discrete(breaks=c(nums), labels=c(nums)) +
    labs(x="Difference Values") +
    xlim(-30, 10) +
    #scale_y_discrete(limits=c("sum", c(n_loci:1))) +
    #ylim(100, 0) +
    labs(y="") +
    ggtitle(expression(atop("Distributions under different N of replicates",
                            atop(italic("Descriptive Statistic: RAY"), "")))) +
    theme_bw() +
    theme(axis.text.y=element_text(colour="grey"),
          axis.ticks.y=element_line(colour="grey"),
          axis.title.y=element_blank(),
          strip.text.y=element_text(angle=0),
          panel.grid.major.x=element_blank(),
          panel.grid.major.y=element_blank(),
          strip.background = element_rect(fill="white"), 
          panel.margin = unit(0.5, "lines"),
          plot.title = element_text(face="bold", size=rel(1.5), vjust=-1))

  ## Save plots
  ggsave(paste("~/Desktop/Figure4_nRepsDistr_Sim.", sim_num, ".svg", sep=""),
         width=10, height=8)
}
