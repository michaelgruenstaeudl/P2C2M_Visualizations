#!/usr/bin/R
#author = "Michael Gruenstaeudl, PhD"
#copyright = "Copyright (C) 2014-2015 Michael Gruenstaeudl"
#contributors = c("Michael Gruenstaeudl")
#email = "gruenstaeudl.1@osu.edu"
#version = "2015.03.18.2000"

# SETTING GLOBAL VARIABLES
library(ggplot2)
#library(grid)
in_dir = "/home/michael/research/manuscripts/03_P2C2M/11_second_revision/SupplFile5_GTPvsRAY/02_posteriorDistribs/06_output_from_P2C2M/old/"
out_dir = "/home/michael/research/manuscripts/03_P2C2M/11_second_revision/SupplFile5_GTPvsRAY/02_posteriorDistribs/07_visualization/"
in_fn = "sim.E.011"

# LOADING DATA
load(paste(in_dir, in_fn, ".rda", sep=""))
indata = get(in_fn)

table_raw = read.table("/home/michael/research/manuscripts/03_P2C2M/11_second_revision/SupplFile5_GTPvsRAY/02_posteriorDistribs/03_output_from_BEAST_starBEAST/sim.E.11.SpeciesTree.log.reduced", sep="\t", header=T)
species.coalescent = table_raw[,"species.coalescent"]

# STACKING DATA
#sub_data = indata$rawStats$GTP$emp
sub_data = indata$rawStats$GTP$emp$unsort
handle_GTP = stack(as.data.frame(sub_data))
handle_GTP[,3] = "GTP"
#sub_data = indata$rawStats$RAY$emp
sub_data = indata$rawStats$RAY$emp$unsort
handle_RAY = stack(as.data.frame(sub_data))
handle_RAY[,3] = "RAY"

handle_starBEAST = handle_RAY
handle_starBEAST[,1] = rep(species.coalescent/10, 10)
handle_starBEAST[,3] = "starBEAST"


handle = rbind(handle_RAY, handle_GTP, handle_starBEAST)
colnames(handle)= c("value", "gene", "stat")

# GENERATING PLOT
my_plot = ggplot(data=handle, aes(x=value, group=stat)) +
  geom_density(aes(linetype=factor(stat))) +
  facet_grid(gene ~ .) +
  labs(x="Coalescent likelihood") +
  #xlim(-50, 50) +
  labs(y="") +
  #ggtitle(paste(model_type, "\n", sep="")) +
  theme_bw() +
  theme(axis.text.y=element_text(colour="grey"),
        axis.ticks.y=element_line(colour="grey"),
        axis.title.y=element_blank(),
        strip.text.y=element_text(angle=0),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        strip.background = element_rect(fill="white")#, 
        #panel.margin = unit(0.5, "lines")
        )

# SAVING PLOT
ggsave(filename=paste(out_dir, "SupplFile5_GTPvsRAY_", in_fn, ".svg", sep=""), plot=my_plot, width=10, height=8)
