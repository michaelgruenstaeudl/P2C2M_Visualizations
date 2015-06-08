#!/usr/bin/R
#author = "Michael Gruenstaeudl, PhD"
#copyright = "Copyright (C) 2014-2015 Michael Gruenstaeudl"
#contributors = c("Michael Gruenstaeudl")
#email = "gruenstaeudl.1@osu.edu"
#version = "2015.03.17.2100"

################################
# STEP1. Set Globals Variables #
################################
library(ggplot2)
library(grid)
setwd("/home/michael/Desktop/")
#model_type = "wModel"
model_type = "wMig"
sim_nums = c(1,2)
## CHANGE HERE!
in_dir = paste("/home/michael/research/manuscripts/03_P2C2M/",
               "11_second_revision/SupplFile4_GTPvsRAY/01_input/", sep="")

####################
# STEP2. Load data #
####################
in_fns = list()
counter = 1
for (sim_num in sim_nums) {
    infilename = paste("RS2.", model_type, ".Hi.", sim_num, ".nReps100", sep="")
    in_fns[[counter]] = infilename
    load(paste(in_dir, infilename, ".rda", sep=""))
    counter = counter + 1
}

###########################
# STEP 3. Helper Function #
###########################
do_stacking = function(data_fn){
  indata = get(data_fn)
  ## Change here, if so desired
  sub_data = indata$rawStats$RAY$dif
  handle_RAY = stack(as.data.frame(sub_data))
  handle_RAY[,3] = NA
  name_list = unlist(strsplit(data_fn, split="\\."))
  if ("1" %in% name_list) {handle_RAY[,3] = "sim.01"}
  if ("2" %in% name_list) {handle_RAY[,3] = "sim.02"}
  handle_RAY[,4] = "RAY"
  ##
  sub_data = indata$rawStats$GTP$dif
  handle_GTP = stack(as.data.frame(sub_data))
  handle_GTP[,3] = NA
  name_list = unlist(strsplit(data_fn, split="\\."))
  if ("1" %in% name_list) {handle_GTP[,3] = "sim.01"}
  if ("2" %in% name_list) {handle_GTP[,3] = "sim.02"}
  handle_GTP[,4] = "GTP"
  ##
  handle = rbind(handle_RAY, handle_GTP)
  colnames(handle)= c("value", "gene", "sim_num", "stat")

return(handle)
}

calc_means = function(data_fn){
  indata = get(data_fn)
  ##
  sub_data = indata$rawStats$RAY$dif
  handle_RAY = stack(colMeans(sub_data))
  handle_RAY[,3] = NA
  name_list = unlist(strsplit(data_fn, split="\\."))
  if ("1" %in% name_list) {handle_RAY[,3] = "sim.01"}
  if ("2" %in% name_list) {handle_RAY[,3] = "sim.02"}
  handle_RAY[,4] = "RAY"
  ##
  sub_data = indata$rawStats$GTP$dif
  handle_GTP = stack(colMeans(sub_data))
  handle_GTP[,3] = NA
  name_list = unlist(strsplit(data_fn, split="\\."))
  if ("1" %in% name_list) {handle_GTP[,3] = "sim.01"}
  if ("2" %in% name_list) {handle_GTP[,3] = "sim.02"}
  handle_GTP[,4] = "GTP"
  ##
  handle = rbind(handle_RAY, handle_GTP)
  colnames(handle)= c("meanval", "gene", "sim_num", "stat")

return(handle)
}

############################
# STEP 4. Prepare the data #
############################

data_handle = lapply(in_fns, do_stacking)
data_list = do.call(rbind, data_handle)

means_handle = lapply(in_fns, calc_means)
means_list = do.call(rbind, means_handle)

##########################
# STEP 5. Make the plots #
##########################

my_plot = ggplot(data=data_list, aes(x=value, group=stat)) +
  geom_density(aes(linetype=factor(stat))) +
  facet_grid(gene ~ sim_num, scales = "free_y") +
  #facet_grid(gene ~ sim_num) +
  geom_vline(xintercept=0, colour="grey", size=1.5, alpha=0.5) +
  geom_vline(aes(xintercept=meanval, linetype=factor(stat)), data=means_list) +
  labs(x="Difference Values") +
  xlim(-50, 50) +
  labs(y="") +
  ggtitle(paste(model_type, "\n", sep="")) +
  theme_bw() +
  theme(axis.text.y=element_text(colour="grey"),
        axis.ticks.y=element_line(colour="grey"),
        axis.title.y=element_blank(),
        strip.text.y=element_text(angle=0),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        strip.background = element_rect(fill="white"), 
        panel.margin = unit(0.5, "lines"))

## Save plots
ggsave(filename=paste("~/Desktop/SupplFile4_GTPvsRAY_", "RS2.", model_type, ".nReps100", ".svg", sep=""), plot=my_plot, width=10, height=8)
