#!/usr/bin/R
#author = "Michael Gruenstaeudl, PhD"
#copyright = "Copyright (C) 2014 Michael Gruenstaeudl"
#contributors = c("Michael Gruenstaeudl")
#email = "gruenstaeudl.1@osu.edu"
#version = "2015.01.13.1100"

## Load .rda-files
setwd("/home/michael/Desktop/")
figData = read.csv(file.choose())
# /home/michael/research/manuscripts/03_P2C2M/02_figures/Figure3_MPI/2015.01.14_ComputationTime _vs_nReps.csv

################################################################################

## The distribution of false positive result values is visualized as 
## presence/absence plot.
library(ggplot2)

# Separating the data
woMPI = figData[which(figData[,"mpiBool"]=="n"),]
MPI = figData[which(figData[,"mpiBool"]=="y"),]

# Labels to the very right-most data points
woMPI_labels = woMPI[which(woMPI[,"nreps"]==100),]
MPI_labels = MPI[which(MPI[,"nreps"]==100),]

plot = ggplot(data=woMPI, aes(x=nreps, y=time_h, group=sim, linetype=mpiBool)) +
    geom_point() +
    geom_line() +
    geom_text(data=woMPI_labels, aes(label=sim), size=3, vjust=-0.5, hjust=1.0) +
    geom_point(data=MPI) +
    geom_line(data=MPI) +
    geom_text(data=MPI_labels, aes(label=sim), size=3, vjust=-0.5, hjust=1.0) +
    scale_x_discrete(breaks=c(1,5,10,20,50,100), labels=c(1,5,10,20,50,100)) +
    scale_y_discrete(breaks=c(1, seq(0,75,5)[2:16]), labels=c(1, seq(0,75,5)[2:16])) +
    theme_bw() +
    xlab("\nNumber of Replicates") + 
    ylab("Computation Time (hours)\n") +
    ggtitle("Computation Time by Number of Replicates and\nApplication of OpenMPI\n") +
    scale_linetype_manual(values = c("dotted", "solid"))

################################################################################

svg("/home/michael/Desktop/Figure5_MPITime.svg", width=6, height=5)
plot
dev.off()

