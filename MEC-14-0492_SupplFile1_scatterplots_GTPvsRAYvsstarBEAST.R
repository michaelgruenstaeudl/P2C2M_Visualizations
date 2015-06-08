#!/usr/bin/R
#author = "Michael Gruenstaeudl, PhD"
#copyright = "Copyright (C) 2014-2015 Michael Gruenstaeudl"
#contributors = c("Michael Gruenstaeudl")
#email = "gruenstaeudl.1@osu.edu"
#version = "2015.03.27.1200"

#current_var = "coal"
#current_label = "\nCoalescent likelihood (Liu & Yu 2010)"

current_var = "lcwt"
current_label = "\nLikelihood of coalescent waiting times (Reid et al. 2014)"

# SETTING GLOBAL VARIABLES
library(ggplot2)
#library(grid)
in_dir = "/home/michael/research/analyses/03_analyses_P2C2M/SupplFile5_GTPvsRAY/02_posteriorDistribs/06_output_from_P2C2M/old/"
out_dir = "/home/michael/research/analyses/03_analyses_P2C2M/SupplFile5_GTPvsRAY/03_scatterplots/"
in_fn = "sim.E.011"

# LOADING DATA
load(paste(in_dir, in_fn, ".rda", sep=""))
indata = get(in_fn)

starBEAST_logFile = read.table("/home/michael/research/analyses/03_analyses_P2C2M/SupplFile5_GTPvsRAY/02_posteriorDistribs/03_output_from_BEAST_starBEAST/sim.E.11.SpeciesTree.log.reduced", sep="\t", header=T)
species.coalescent = starBEAST_logFile[,"species.coalescent"]

# STACKING DATA
sub_data = indata$rawStats$RAY$emp$unsort
handle_RAY = stack(as.data.frame(sub_data))

sub_data = indata$rawStats$GTP$emp$unsort
handle_GTP = stack(as.data.frame(sub_data))

handle_starBEAST = handle_RAY
#handle_starBEAST[,1] = rep(species.coalescent/10, 10)
handle_starBEAST[,1] = rep(species.coalescent, 10)

handle = as.data.frame(cbind(handle_RAY[,2], handle_RAY[,1],
                    handle_GTP[,1], handle_starBEAST[,1]))
# handle_RAY[,2] not stored proberly in TLA, hence:
handle[,1] = handle_RAY[,2]
colnames(handle)= c("gene", "coal", "lcwt", "species.coalescent")

handle_new = rbind(handle[which(handle[,1]=="gene003"),],
                   handle[which(handle[,1]=="gene004"),],
                   handle[which(handle[,1]=="gene005"),])


corcofs = c()
for (i in 3:5) {
    temp = handle[which(handle[,1]==paste("gene00", i, sep="")),]
    cc = round(cor(temp[,"species.coalescent"], temp[, current_var]),4)
    corcofs = c(corcofs, cc)
}


# GENERATING PLOT 1
plot_handle = ggplot(data=handle_new, aes(x=species.coalescent, y=get(current_var))) +
  geom_point(shape=1,color="darkgrey") +
  geom_smooth(method="lm", fill=NA, color="black") +
  facet_grid(gene ~ .) +
  labs(x=current_label) +
  labs(y="Species coalescent likelihood (*BEAST)\n") +
  #ggtitle(paste(model_type, "\n", sep="")) +
  theme_bw() +
  theme(#axis.text.y=element_text(colour="grey"),
        #axis.ticks.y=element_line(colour="grey"),
        #axis.title.y=element_blank(),
        #strip.text.y=element_text(angle=0),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        strip.background = element_rect(fill="white")#, 
        #panel.margin = unit(0.5, "lines")
    )
    
for (i in 3:5) {
    plot_handle = plot_handle + annotate("text", x=max(handle_new[,"species.coalescent"])-50, y=max(handle_new[ current_var]), label=paste("R=", paste(corcofs, collapse="_"), sep=""), size=3)
}

ggsave(filename=paste(out_dir, "SupplFile1_Scatterplot_part2.svg", sep=""), plot=plot_handle, width=4, height=8)
