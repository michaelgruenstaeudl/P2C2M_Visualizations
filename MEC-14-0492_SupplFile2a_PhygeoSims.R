#!/usr/bin/R
#author = "Michael Gruenstaeudl, PhD"
#copyright = "Copyright (C) 2014 Michael Gruenstaeudl"
#contributors = c("Michael Gruenstaeudl")
#email = "gruenstaeudl.1@osu.edu"
#version = "2015.01.21.1400"

## Load .rda-files
setwd("/home/michael/research/analyses/03_analyses_P2C2M/Figure1_FalsePositives/sim.NoMigration_COMPLETE/06_output_from_P2C2M/06b_starBEASTgeneTrees_COMPLETE/")
for (i in c(1:9)) {
  load(paste("sim.E.00",i,".rda",sep=""))
}
for (i in c(10:20)) {
  load(paste("sim.E.0",i,".rda",sep=""))
}
for (i in c(1:9)) {
  load(paste("sim.F.00",i,".rda",sep=""))
}
for (i in c(10:20)) {
  load(paste("sim.F.0",i,".rda",sep=""))
}


################################################################################

## Load perGene data
# Save into lists "High" and "Low", and with different alpha values,
# respectively
High_a0.01_perGene = list()
High_a0.01_perGene$`01` = sim.E.001$results$alpha0.01$perGene
High_a0.01_perGene$`02` = sim.E.002$results$alpha0.01$perGene
High_a0.01_perGene$`03` = sim.E.003$results$alpha0.01$perGene
High_a0.01_perGene$`04` = sim.E.004$results$alpha0.01$perGene
High_a0.01_perGene$`05` = sim.E.005$results$alpha0.01$perGene
High_a0.01_perGene$`06` = sim.E.006$results$alpha0.01$perGene
High_a0.01_perGene$`07` = sim.E.007$results$alpha0.01$perGene
High_a0.01_perGene$`08` = sim.E.008$results$alpha0.01$perGene
High_a0.01_perGene$`09` = sim.E.009$results$alpha0.01$perGene
High_a0.01_perGene$`10` = sim.E.010$results$alpha0.01$perGene
High_a0.01_perGene$`12` = sim.E.012$results$alpha0.01$perGene
High_a0.01_perGene$`13` = sim.E.013$results$alpha0.01$perGene
High_a0.01_perGene$`14` = sim.E.014$results$alpha0.01$perGene
High_a0.01_perGene$`15` = sim.E.015$results$alpha0.01$perGene
High_a0.01_perGene$`16` = sim.E.016$results$alpha0.01$perGene
High_a0.01_perGene$`17` = sim.E.017$results$alpha0.01$perGene
High_a0.01_perGene$`18` = sim.E.018$results$alpha0.01$perGene
High_a0.01_perGene$`19` = sim.E.019$results$alpha0.01$perGene
High_a0.01_perGene$`20` = sim.E.020$results$alpha0.01$perGene

High_a0.05_perGene = list()
High_a0.05_perGene$`01` = sim.E.001$results$alpha0.05$perGene
High_a0.05_perGene$`02` = sim.E.002$results$alpha0.05$perGene
High_a0.05_perGene$`03` = sim.E.003$results$alpha0.05$perGene
High_a0.05_perGene$`04` = sim.E.004$results$alpha0.05$perGene
High_a0.05_perGene$`05` = sim.E.005$results$alpha0.05$perGene
High_a0.05_perGene$`06` = sim.E.006$results$alpha0.05$perGene
High_a0.05_perGene$`07` = sim.E.007$results$alpha0.05$perGene
High_a0.05_perGene$`08` = sim.E.008$results$alpha0.05$perGene
High_a0.05_perGene$`09` = sim.E.009$results$alpha0.05$perGene
High_a0.05_perGene$`10` = sim.E.010$results$alpha0.05$perGene
High_a0.05_perGene$`12` = sim.E.012$results$alpha0.05$perGene
High_a0.05_perGene$`13` = sim.E.013$results$alpha0.05$perGene
High_a0.05_perGene$`14` = sim.E.014$results$alpha0.05$perGene
High_a0.05_perGene$`15` = sim.E.015$results$alpha0.05$perGene
High_a0.05_perGene$`16` = sim.E.016$results$alpha0.05$perGene
High_a0.05_perGene$`17` = sim.E.017$results$alpha0.05$perGene
High_a0.05_perGene$`18` = sim.E.018$results$alpha0.05$perGene
High_a0.05_perGene$`19` = sim.E.019$results$alpha0.05$perGene
High_a0.05_perGene$`20` = sim.E.020$results$alpha0.05$perGene

Low_a0.01_perGene = list()
Low_a0.01_perGene$`01` = sim.F.001$results$alpha0.01$perGene
Low_a0.01_perGene$`02` = sim.F.002$results$alpha0.01$perGene
Low_a0.01_perGene$`03` = sim.F.003$results$alpha0.01$perGene
Low_a0.01_perGene$`04` = sim.F.004$results$alpha0.01$perGene
Low_a0.01_perGene$`05` = sim.F.005$results$alpha0.01$perGene
Low_a0.01_perGene$`06` = sim.F.006$results$alpha0.01$perGene
Low_a0.01_perGene$`07` = sim.F.007$results$alpha0.01$perGene
Low_a0.01_perGene$`08` = sim.F.008$results$alpha0.01$perGene
Low_a0.01_perGene$`09` = sim.F.009$results$alpha0.01$perGene
Low_a0.01_perGene$`10` = sim.F.010$results$alpha0.01$perGene
Low_a0.01_perGene$`12` = sim.F.012$results$alpha0.01$perGene
Low_a0.01_perGene$`13` = sim.F.013$results$alpha0.01$perGene
Low_a0.01_perGene$`14` = sim.F.014$results$alpha0.01$perGene
Low_a0.01_perGene$`15` = sim.F.015$results$alpha0.01$perGene
Low_a0.01_perGene$`16` = sim.F.016$results$alpha0.01$perGene
Low_a0.01_perGene$`17` = sim.F.017$results$alpha0.01$perGene
Low_a0.01_perGene$`18` = sim.F.018$results$alpha0.01$perGene
Low_a0.01_perGene$`19` = sim.F.019$results$alpha0.01$perGene
Low_a0.01_perGene$`20` = sim.F.020$results$alpha0.01$perGene

Low_a0.05_perGene = list()
Low_a0.05_perGene$`01` = sim.F.001$results$alpha0.05$perGene
Low_a0.05_perGene$`02` = sim.F.002$results$alpha0.05$perGene
Low_a0.05_perGene$`03` = sim.F.003$results$alpha0.05$perGene
Low_a0.05_perGene$`04` = sim.F.004$results$alpha0.05$perGene
Low_a0.05_perGene$`05` = sim.F.005$results$alpha0.05$perGene
Low_a0.05_perGene$`06` = sim.F.006$results$alpha0.05$perGene
Low_a0.05_perGene$`07` = sim.F.007$results$alpha0.05$perGene
Low_a0.05_perGene$`08` = sim.F.008$results$alpha0.05$perGene
Low_a0.05_perGene$`09` = sim.F.009$results$alpha0.05$perGene
Low_a0.05_perGene$`10` = sim.F.010$results$alpha0.05$perGene
Low_a0.05_perGene$`12` = sim.F.012$results$alpha0.05$perGene
Low_a0.05_perGene$`13` = sim.F.013$results$alpha0.05$perGene
Low_a0.05_perGene$`14` = sim.F.014$results$alpha0.05$perGene
Low_a0.05_perGene$`15` = sim.F.015$results$alpha0.05$perGene
Low_a0.05_perGene$`16` = sim.F.016$results$alpha0.05$perGene
Low_a0.05_perGene$`17` = sim.F.017$results$alpha0.05$perGene
Low_a0.05_perGene$`18` = sim.F.018$results$alpha0.05$perGene
Low_a0.05_perGene$`19` = sim.F.019$results$alpha0.05$perGene
Low_a0.05_perGene$`20` = sim.F.020$results$alpha0.05$perGene

################################################################################

## Load acrGene data
High_a0.01_acrGene = list()
High_a0.01_acrGene$`01` = sim.E.001$results$alpha0.01$acrGene[1,]
High_a0.01_acrGene$`02` = sim.E.002$results$alpha0.01$acrGene[1,]
High_a0.01_acrGene$`03` = sim.E.003$results$alpha0.01$acrGene[1,]
High_a0.01_acrGene$`04` = sim.E.004$results$alpha0.01$acrGene[1,]
High_a0.01_acrGene$`05` = sim.E.005$results$alpha0.01$acrGene[1,]
High_a0.01_acrGene$`06` = sim.E.006$results$alpha0.01$acrGene[1,]
High_a0.01_acrGene$`07` = sim.E.007$results$alpha0.01$acrGene[1,]
High_a0.01_acrGene$`08` = sim.E.008$results$alpha0.01$acrGene[1,]
High_a0.01_acrGene$`09` = sim.E.009$results$alpha0.01$acrGene[1,]
High_a0.01_acrGene$`10` = sim.E.010$results$alpha0.01$acrGene[1,]
High_a0.01_acrGene$`12` = sim.E.012$results$alpha0.01$acrGene[1,]
High_a0.01_acrGene$`13` = sim.E.013$results$alpha0.01$acrGene[1,]
High_a0.01_acrGene$`14` = sim.E.014$results$alpha0.01$acrGene[1,]
High_a0.01_acrGene$`15` = sim.E.015$results$alpha0.01$acrGene[1,]
High_a0.01_acrGene$`16` = sim.E.016$results$alpha0.01$acrGene[1,]
High_a0.01_acrGene$`17` = sim.E.017$results$alpha0.01$acrGene[1,]
High_a0.01_acrGene$`18` = sim.E.018$results$alpha0.01$acrGene[1,]
High_a0.01_acrGene$`19` = sim.E.019$results$alpha0.01$acrGene[1,]
High_a0.01_acrGene$`20` = sim.E.020$results$alpha0.01$acrGene[1,]

High_a0.05_acrGene = list()
High_a0.05_acrGene$`01` = sim.E.001$results$alpha0.05$acrGene[1,]
High_a0.05_acrGene$`02` = sim.E.002$results$alpha0.05$acrGene[1,]
High_a0.05_acrGene$`03` = sim.E.003$results$alpha0.05$acrGene[1,]
High_a0.05_acrGene$`04` = sim.E.004$results$alpha0.05$acrGene[1,]
High_a0.05_acrGene$`05` = sim.E.005$results$alpha0.05$acrGene[1,]
High_a0.05_acrGene$`06` = sim.E.006$results$alpha0.05$acrGene[1,]
High_a0.05_acrGene$`07` = sim.E.007$results$alpha0.05$acrGene[1,]
High_a0.05_acrGene$`08` = sim.E.008$results$alpha0.05$acrGene[1,]
High_a0.05_acrGene$`09` = sim.E.009$results$alpha0.05$acrGene[1,]
High_a0.05_acrGene$`10` = sim.E.010$results$alpha0.05$acrGene[1,]
High_a0.05_acrGene$`12` = sim.E.012$results$alpha0.05$acrGene[1,]
High_a0.05_acrGene$`13` = sim.E.013$results$alpha0.05$acrGene[1,]
High_a0.05_acrGene$`14` = sim.E.014$results$alpha0.05$acrGene[1,]
High_a0.05_acrGene$`15` = sim.E.015$results$alpha0.05$acrGene[1,]
High_a0.05_acrGene$`16` = sim.E.016$results$alpha0.05$acrGene[1,]
High_a0.05_acrGene$`17` = sim.E.017$results$alpha0.05$acrGene[1,]
High_a0.05_acrGene$`18` = sim.E.018$results$alpha0.05$acrGene[1,]
High_a0.05_acrGene$`19` = sim.E.019$results$alpha0.05$acrGene[1,]
High_a0.05_acrGene$`20` = sim.E.020$results$alpha0.05$acrGene[1,]

Low_a0.01_acrGene = list()
Low_a0.01_acrGene$`01` = sim.F.001$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGene$`02` = sim.F.002$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGene$`03` = sim.F.003$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGene$`04` = sim.F.004$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGene$`05` = sim.F.005$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGene$`06` = sim.F.006$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGene$`07` = sim.F.007$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGene$`08` = sim.F.008$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGene$`09` = sim.F.009$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGene$`10` = sim.F.010$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGene$`12` = sim.F.012$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGene$`13` = sim.F.013$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGene$`14` = sim.F.014$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGene$`15` = sim.F.015$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGene$`16` = sim.F.016$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGene$`17` = sim.F.017$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGene$`18` = sim.F.018$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGene$`19` = sim.F.019$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGene$`20` = sim.F.020$results$alpha0.01$acrGene[1,]

Low_a0.05_acrGene = list()
Low_a0.05_acrGene$`01` = sim.F.001$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGene$`02` = sim.F.002$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGene$`03` = sim.F.003$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGene$`04` = sim.F.004$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGene$`05` = sim.F.005$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGene$`06` = sim.F.006$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGene$`07` = sim.F.007$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGene$`08` = sim.F.008$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGene$`09` = sim.F.009$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGene$`10` = sim.F.010$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGene$`12` = sim.F.012$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGene$`13` = sim.F.013$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGene$`14` = sim.F.014$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGene$`15` = sim.F.015$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGene$`16` = sim.F.016$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGene$`17` = sim.F.017$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGene$`18` = sim.F.018$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGene$`19` = sim.F.019$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGene$`20` = sim.F.020$results$alpha0.05$acrGene[1,]

################################################################################

## A custom function is specified which converts results matrices into 
## presence/absence matrices, stacks the matrix columns and adds identifier 
## information.
myfunc1 = function(inData, simNum){
  handle = inData
  # It is CRITICAL that the order of stats is alphabetic, because 
  # sim.E.001$results$alpha0.01$perGene is also sorted alphabetically
  colnames(handle) = c("gsi", "gtp", "ndc", "ray")
  handle[grepl("\\*", handle)] = 1
  handle[grepl("n.s.", handle)] = 0
  handle[grepl(" 0", handle)] = 0
  handle = stack(data.frame(handle ,stringsAsFactors = FALSE))
  colnames(handle)[1] = "value"
  colnames(handle)[2] = "stat"
  handle[,3] = rep(c(1:10), 4)
  colnames(handle)[3] = "gene"
  handle[,4] = simNum
  colnames(handle)[4] = "sim"
  return(handle)
}

myfunc2 = function(inData, simNum){
  #handle = inData
  handle = t(as.matrix(inData))
  # It is CRITICAL that the order of stats is alphabetic, because 
  # sim.E.001$results$alpha0.01$acrGene is also sorted alphabetically
  colnames(handle) = c("gsi", "gtp", "ndc", "ray")
  handle[grepl("\\*", handle)] = 1
  handle[grepl("n.s.", handle)] = 0
  handle[grepl(" 0", handle)] = 0
  handle = stack(data.frame(handle ,stringsAsFactors = FALSE))
  colnames(handle)[1] = "value"
  colnames(handle)[2] = "stat"
  handle[,3] = rep("sum", 4)
  colnames(handle)[3] = "gene"
  handle[,4] = simNum
  colnames(handle)[4] = "sim"
  return(handle)
}

################################################################################

## The custom function is executed on the example data, which consists of two 
## subsets that are characterized by different substitution rates.
highL = list()
sims = as.numeric(names(High_a0.01_perGene))
for (i in 1:length(High_a0.01_perGene)) {
  highL[[i]] = myfunc1(High_a0.01_perGene[[i]], sims[i])
}
High_a0.01 = do.call("rbind", highL)
High_a0.01[,ncol(High_a0.01)+1] = "High_Subst_Rate"
colnames(High_a0.01)[ncol(High_a0.01)] = "ratetype"


lowL = list()
sims = as.numeric(names(Low_a0.01_perGene))
for (i in 1:length(Low_a0.01_perGene)) {
  lowL[[i]] = myfunc1(Low_a0.01_perGene[[i]], sims[i])
}
Low_a0.01 = do.call("rbind", lowL)
Low_a0.01[,ncol(Low_a0.01)+1] = "Low_Subst_Rate"
colnames(Low_a0.01)[ncol(Low_a0.01)] = "ratetype"


highL = list()
sims = as.numeric(names(High_a0.05_perGene))
for (i in 1:length(High_a0.05_perGene)) {
  highL[[i]] = myfunc1(High_a0.05_perGene[[i]], sims[i])
}
High_a0.05 = do.call("rbind", highL)
High_a0.05[,ncol(High_a0.05)+1] = "High_Subst_Rate"
colnames(High_a0.05)[ncol(High_a0.05)] = "ratetype"


lowL = list()
sims = as.numeric(names(Low_a0.05_perGene))
for (i in 1:length(Low_a0.05_perGene)) {
  lowL[[i]] = myfunc1(Low_a0.05_perGene[[i]], sims[i])
}
Low_a0.05 = do.call("rbind", lowL)
Low_a0.05[,ncol(Low_a0.05)+1] = "Low_Subst_Rate"
colnames(Low_a0.05)[ncol(Low_a0.05)] = "ratetype"

data_perGene_a0.01 = rbind(High_a0.01, Low_a0.01)
data_perGene_a0.05 = rbind(High_a0.05, Low_a0.05)

#data_perGene_a0.01[,ncol(data_perGene_a0.01)+1] = "0"
#colnames(data_perGene_a0.01)[ncol(data_perGene_a0.01)] = "migration"

#data_perGene_a0.05[,ncol(data_perGene_a0.05)+1] = "0"
#colnames(data_perGene_a0.05)[ncol(data_perGene_a0.05)] = "migration"

################################################################################

highL = list()
sims = as.numeric(names(High_a0.01_acrGene))
for (i in 1:length(High_a0.01_acrGene)) {
  highL[[i]] = myfunc2(High_a0.01_acrGene[[i]], sims[i])
}
High_a0.01 = do.call("rbind", highL)
High_a0.01[,ncol(High_a0.01)+1] = "High_Subst_Rate"
colnames(High_a0.01)[ncol(High_a0.01)] = "ratetype"


lowL = list()
sims = as.numeric(names(Low_a0.01_acrGene))
for (i in 1:length(Low_a0.01_acrGene)) {
  lowL[[i]] = myfunc2(Low_a0.01_acrGene[[i]], sims[i])
}
Low_a0.01 = do.call("rbind", lowL)
Low_a0.01[,ncol(Low_a0.01)+1] = "Low_Subst_Rate"
colnames(Low_a0.01)[ncol(Low_a0.01)] = "ratetype"


highL = list()
sims = as.numeric(names(High_a0.05_acrGene))
for (i in 1:length(High_a0.05_acrGene)) {
  highL[[i]] = myfunc2(High_a0.05_acrGene[[i]], sims[i])
}
High_a0.05 = do.call("rbind", highL)
High_a0.05[,ncol(High_a0.05)+1] = "High_Subst_Rate"
colnames(High_a0.05)[ncol(High_a0.05)] = "ratetype"


lowL = list()
sims = as.numeric(names(Low_a0.05_acrGene))
for (i in 1:length(Low_a0.05_acrGene)) {
  lowL[[i]] = myfunc2(Low_a0.05_acrGene[[i]], sims[i])
}
Low_a0.05 = do.call("rbind", lowL)
Low_a0.05[,ncol(Low_a0.05)+1] = "Low_Subst_Rate"
colnames(Low_a0.05)[ncol(Low_a0.05)] = "ratetype"

data_acrGene_a0.01 = rbind(High_a0.01, Low_a0.01)
data_acrGene_a0.05 = rbind(High_a0.05, Low_a0.05)

#data_acrGene_a0.01[,ncol(data_acrGene_a0.01)+1] = "0"
#colnames(data_acrGene_a0.01)[ncol(data_acrGene_a0.01)] = "migration"

#data_acrGene_a0.05[,ncol(data_acrGene_a0.05)+1] = "0"
#colnames(data_acrGene_a0.05)[ncol(data_acrGene_a0.05)] = "migration"

################################################################################

data_a0.01 = rbind(data_perGene_a0.01, data_acrGene_a0.01)
data_a0.05 = rbind(data_perGene_a0.05, data_acrGene_a0.05)

################################################################################

## The distribution of false positive result values is visualized as 
## presence/absence plot.
library(ggplot2)
plot = ggplot(data=data_a0.05, aes(x=sim, y=gene)) +
    geom_point(aes(colour=value), size=3, alpha=0.25) +
    scale_colour_manual(values=c(NA, 'black')) +
    facet_grid(stat ~ ratetype) +
#    ggtitle("Distribution of False Positives\n") +
    theme_bw() +
    scale_x_discrete(breaks=c(1:20), labels=c(1:20)) +
    #scale_y_discrete(breaks=c(10:1), labels=c(10:1)) +
    scale_y_discrete(limits=c("sum", c(10:1))) +
    theme(axis.text = element_text(size=5),
          strip.background=element_rect(fill="white")) +
    xlab("\nSimulation ID") + 
    ylab("Genes\n") +
    geom_point(data=data_a0.01, aes(x=sim, y=gene, colour=value), size=1.5, alpha=1.0)


################################################################################

svg("/home/michael/Desktop/SupplFile2a_PhygeoSims.svg", width=6, height=6)
plot
dev.off()

