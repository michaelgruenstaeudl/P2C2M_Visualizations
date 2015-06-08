#!/usr/bin/R
#author = "Michael Gruenstaeudl, PhD"
#copyright = "Copyright (C) 2014 Michael Gruenstaeudl"
#contributors = c("Michael Gruenstaeudl")
#email = "gruenstaeudl.1@osu.edu"
#version = "2015.01.21.1400"

## Load .rda-files
setwd("/home/michael/research/analyses/03_analyses_P2C2M/Figure1_FalsePositives/sim.WithMigration_COMPLETE/06_output_from_P2C2M/06a_BEASTgeneTrees_COMPLETE/")

for (i in c(1:20)) {
  load(paste("sim.GeneFlow.Hi.",i,".rda",sep=""))
  load(paste("sim.GeneFlow.Lo.",i,".rda",sep=""))
}

################################################################################

## Load perGene data
## Save into lists "High" and "Low", and with different alpha values,
#  respectively
High_a0.01_perGene = list()
High_a0.01_perGene$`01` = sim.GeneFlow.Hi.1$results$alpha0.01$perGene
High_a0.01_perGene$`02` = sim.GeneFlow.Hi.2$results$alpha0.01$perGene
High_a0.01_perGene$`03` = sim.GeneFlow.Hi.3$results$alpha0.01$perGene
High_a0.01_perGene$`04` = sim.GeneFlow.Hi.4$results$alpha0.01$perGene
High_a0.01_perGene$`05` = sim.GeneFlow.Hi.5$results$alpha0.01$perGene
High_a0.01_perGene$`06` = sim.GeneFlow.Hi.6$results$alpha0.01$perGene
High_a0.01_perGene$`07` = sim.GeneFlow.Hi.7$results$alpha0.01$perGene
High_a0.01_perGene$`08` = sim.GeneFlow.Hi.8$results$alpha0.01$perGene
High_a0.01_perGene$`09` = sim.GeneFlow.Hi.9$results$alpha0.01$perGene
High_a0.01_perGene$`10` = sim.GeneFlow.Hi.10$results$alpha0.01$perGene
High_a0.01_perGene$`11` = sim.GeneFlow.Hi.11$results$alpha0.01$perGene
High_a0.01_perGene$`12` = sim.GeneFlow.Hi.12$results$alpha0.01$perGene
High_a0.01_perGene$`13` = sim.GeneFlow.Hi.13$results$alpha0.01$perGene
High_a0.01_perGene$`14` = sim.GeneFlow.Hi.14$results$alpha0.01$perGene
High_a0.01_perGene$`15` = sim.GeneFlow.Hi.15$results$alpha0.01$perGene
High_a0.01_perGene$`16` = sim.GeneFlow.Hi.16$results$alpha0.01$perGene
High_a0.01_perGene$`17` = sim.GeneFlow.Hi.17$results$alpha0.01$perGene
High_a0.01_perGene$`18` = sim.GeneFlow.Hi.18$results$alpha0.01$perGene
High_a0.01_perGene$`19` = sim.GeneFlow.Hi.19$results$alpha0.01$perGene
High_a0.01_perGene$`20` = sim.GeneFlow.Hi.20$results$alpha0.01$perGene

High_a0.05_perGene = list()
High_a0.05_perGene$`01` = sim.GeneFlow.Hi.1$results$alpha0.05$perGene
High_a0.05_perGene$`02` = sim.GeneFlow.Hi.2$results$alpha0.05$perGene
High_a0.05_perGene$`03` = sim.GeneFlow.Hi.3$results$alpha0.05$perGene
High_a0.05_perGene$`04` = sim.GeneFlow.Hi.4$results$alpha0.05$perGene
High_a0.05_perGene$`05` = sim.GeneFlow.Hi.5$results$alpha0.05$perGene
High_a0.05_perGene$`06` = sim.GeneFlow.Hi.6$results$alpha0.05$perGene
High_a0.05_perGene$`07` = sim.GeneFlow.Hi.7$results$alpha0.05$perGene
High_a0.05_perGene$`08` = sim.GeneFlow.Hi.8$results$alpha0.05$perGene
High_a0.05_perGene$`09` = sim.GeneFlow.Hi.9$results$alpha0.05$perGene
High_a0.05_perGene$`10` = sim.GeneFlow.Hi.10$results$alpha0.05$perGene
High_a0.05_perGene$`11` = sim.GeneFlow.Hi.11$results$alpha0.05$perGene
High_a0.05_perGene$`12` = sim.GeneFlow.Hi.12$results$alpha0.05$perGene
High_a0.05_perGene$`13` = sim.GeneFlow.Hi.13$results$alpha0.05$perGene
High_a0.05_perGene$`14` = sim.GeneFlow.Hi.14$results$alpha0.05$perGene
High_a0.05_perGene$`15` = sim.GeneFlow.Hi.15$results$alpha0.05$perGene
High_a0.05_perGene$`16` = sim.GeneFlow.Hi.16$results$alpha0.05$perGene
High_a0.05_perGene$`17` = sim.GeneFlow.Hi.17$results$alpha0.05$perGene
High_a0.05_perGene$`18` = sim.GeneFlow.Hi.18$results$alpha0.05$perGene
High_a0.05_perGene$`19` = sim.GeneFlow.Hi.19$results$alpha0.05$perGene
High_a0.05_perGene$`20` = sim.GeneFlow.Hi.20$results$alpha0.05$perGene

Low_a0.01_perGene = list()
Low_a0.01_perGene$`01` = sim.GeneFlow.Lo.1$results$alpha0.01$perGene
Low_a0.01_perGene$`02` = sim.GeneFlow.Lo.2$results$alpha0.01$perGene
Low_a0.01_perGene$`03` = sim.GeneFlow.Lo.3$results$alpha0.01$perGene
Low_a0.01_perGene$`04` = sim.GeneFlow.Lo.4$results$alpha0.01$perGene
Low_a0.01_perGene$`05` = sim.GeneFlow.Lo.5$results$alpha0.01$perGene
Low_a0.01_perGene$`06` = sim.GeneFlow.Lo.6$results$alpha0.01$perGene
Low_a0.01_perGene$`07` = sim.GeneFlow.Lo.7$results$alpha0.01$perGene
Low_a0.01_perGene$`08` = sim.GeneFlow.Lo.8$results$alpha0.01$perGene
Low_a0.01_perGene$`09` = sim.GeneFlow.Lo.9$results$alpha0.01$perGene
Low_a0.01_perGene$`10` = sim.GeneFlow.Lo.10$results$alpha0.01$perGene
Low_a0.01_perGene$`11` = sim.GeneFlow.Lo.11$results$alpha0.01$perGene
Low_a0.01_perGene$`12` = sim.GeneFlow.Lo.12$results$alpha0.01$perGene
Low_a0.01_perGene$`13` = sim.GeneFlow.Lo.13$results$alpha0.01$perGene
Low_a0.01_perGene$`14` = sim.GeneFlow.Lo.14$results$alpha0.01$perGene
Low_a0.01_perGene$`15` = sim.GeneFlow.Lo.15$results$alpha0.01$perGene
Low_a0.01_perGene$`16` = sim.GeneFlow.Lo.16$results$alpha0.01$perGene
Low_a0.01_perGene$`17` = sim.GeneFlow.Lo.17$results$alpha0.01$perGene
Low_a0.01_perGene$`18` = sim.GeneFlow.Lo.18$results$alpha0.01$perGene
Low_a0.01_perGene$`19` = sim.GeneFlow.Lo.19$results$alpha0.01$perGene
Low_a0.01_perGene$`20` = sim.GeneFlow.Lo.20$results$alpha0.01$perGene

Low_a0.05_perGene = list()
Low_a0.05_perGene$`01` = sim.GeneFlow.Lo.1$results$alpha0.05$perGene
Low_a0.05_perGene$`02` = sim.GeneFlow.Lo.2$results$alpha0.05$perGene
Low_a0.05_perGene$`03` = sim.GeneFlow.Lo.3$results$alpha0.05$perGene
Low_a0.05_perGene$`04` = sim.GeneFlow.Lo.4$results$alpha0.05$perGene
Low_a0.05_perGene$`05` = sim.GeneFlow.Lo.5$results$alpha0.05$perGene
Low_a0.05_perGene$`06` = sim.GeneFlow.Lo.6$results$alpha0.05$perGene
Low_a0.05_perGene$`07` = sim.GeneFlow.Lo.7$results$alpha0.05$perGene
Low_a0.05_perGene$`08` = sim.GeneFlow.Lo.8$results$alpha0.05$perGene
Low_a0.05_perGene$`09` = sim.GeneFlow.Lo.9$results$alpha0.05$perGene
Low_a0.05_perGene$`10` = sim.GeneFlow.Lo.10$results$alpha0.05$perGene
Low_a0.05_perGene$`11` = sim.GeneFlow.Lo.11$results$alpha0.05$perGene
Low_a0.05_perGene$`12` = sim.GeneFlow.Lo.12$results$alpha0.05$perGene
Low_a0.05_perGene$`13` = sim.GeneFlow.Lo.13$results$alpha0.05$perGene
Low_a0.05_perGene$`14` = sim.GeneFlow.Lo.14$results$alpha0.05$perGene
Low_a0.05_perGene$`15` = sim.GeneFlow.Lo.15$results$alpha0.05$perGene
Low_a0.05_perGene$`16` = sim.GeneFlow.Lo.16$results$alpha0.05$perGene
Low_a0.05_perGene$`17` = sim.GeneFlow.Lo.17$results$alpha0.05$perGene
Low_a0.05_perGene$`18` = sim.GeneFlow.Lo.18$results$alpha0.05$perGene
Low_a0.05_perGene$`19` = sim.GeneFlow.Lo.19$results$alpha0.05$perGene
Low_a0.05_perGene$`20` = sim.GeneFlow.Lo.20$results$alpha0.05$perGene

################################################################################

## Load acrGene data
High_a0.01_acrGenes = list()
High_a0.01_acrGenes$`01` = sim.GeneFlow.Hi.1$results$alpha0.01$acrGene[1,]
High_a0.01_acrGenes$`02` = sim.GeneFlow.Hi.2$results$alpha0.01$acrGene[1,]
High_a0.01_acrGenes$`03` = sim.GeneFlow.Hi.3$results$alpha0.01$acrGene[1,]
High_a0.01_acrGenes$`04` = sim.GeneFlow.Hi.4$results$alpha0.01$acrGene[1,]
High_a0.01_acrGenes$`05` = sim.GeneFlow.Hi.5$results$alpha0.01$acrGene[1,]
High_a0.01_acrGenes$`06` = sim.GeneFlow.Hi.6$results$alpha0.01$acrGene[1,]
High_a0.01_acrGenes$`07` = sim.GeneFlow.Hi.7$results$alpha0.01$acrGene[1,]
High_a0.01_acrGenes$`08` = sim.GeneFlow.Hi.8$results$alpha0.01$acrGene[1,]
High_a0.01_acrGenes$`09` = sim.GeneFlow.Hi.9$results$alpha0.01$acrGene[1,]
High_a0.01_acrGenes$`10` = sim.GeneFlow.Hi.10$results$alpha0.01$acrGene[1,]
High_a0.01_acrGenes$`11` = sim.GeneFlow.Hi.11$results$alpha0.01$acrGene[1,]
High_a0.01_acrGenes$`12` = sim.GeneFlow.Hi.12$results$alpha0.01$acrGene[1,]
High_a0.01_acrGenes$`13` = sim.GeneFlow.Hi.13$results$alpha0.01$acrGene[1,]
High_a0.01_acrGenes$`14` = sim.GeneFlow.Hi.14$results$alpha0.01$acrGene[1,]
High_a0.01_acrGenes$`15` = sim.GeneFlow.Hi.15$results$alpha0.01$acrGene[1,]
High_a0.01_acrGenes$`16` = sim.GeneFlow.Hi.16$results$alpha0.01$acrGene[1,]
High_a0.01_acrGenes$`17` = sim.GeneFlow.Hi.17$results$alpha0.01$acrGene[1,]
High_a0.01_acrGenes$`18` = sim.GeneFlow.Hi.18$results$alpha0.01$acrGene[1,]
High_a0.01_acrGenes$`19` = sim.GeneFlow.Hi.19$results$alpha0.01$acrGene[1,]
High_a0.01_acrGenes$`20` = sim.GeneFlow.Hi.20$results$alpha0.01$acrGene[1,]

High_a0.05_acrGenes = list()
High_a0.05_acrGenes$`01` = sim.GeneFlow.Hi.1$results$alpha0.05$acrGene[1,]
High_a0.05_acrGenes$`02` = sim.GeneFlow.Hi.2$results$alpha0.05$acrGene[1,]
High_a0.05_acrGenes$`03` = sim.GeneFlow.Hi.3$results$alpha0.05$acrGene[1,]
High_a0.05_acrGenes$`04` = sim.GeneFlow.Hi.4$results$alpha0.05$acrGene[1,]
High_a0.05_acrGenes$`05` = sim.GeneFlow.Hi.5$results$alpha0.05$acrGene[1,]
High_a0.05_acrGenes$`06` = sim.GeneFlow.Hi.6$results$alpha0.05$acrGene[1,]
High_a0.05_acrGenes$`07` = sim.GeneFlow.Hi.7$results$alpha0.05$acrGene[1,]
High_a0.05_acrGenes$`08` = sim.GeneFlow.Hi.8$results$alpha0.05$acrGene[1,]
High_a0.05_acrGenes$`09` = sim.GeneFlow.Hi.9$results$alpha0.05$acrGene[1,]
High_a0.05_acrGenes$`10` = sim.GeneFlow.Hi.10$results$alpha0.05$acrGene[1,]
High_a0.05_acrGenes$`11` = sim.GeneFlow.Hi.11$results$alpha0.05$acrGene[1,]
High_a0.05_acrGenes$`12` = sim.GeneFlow.Hi.12$results$alpha0.05$acrGene[1,]
High_a0.05_acrGenes$`13` = sim.GeneFlow.Hi.13$results$alpha0.05$acrGene[1,]
High_a0.05_acrGenes$`14` = sim.GeneFlow.Hi.14$results$alpha0.05$acrGene[1,]
High_a0.05_acrGenes$`15` = sim.GeneFlow.Hi.15$results$alpha0.05$acrGene[1,]
High_a0.05_acrGenes$`16` = sim.GeneFlow.Hi.16$results$alpha0.05$acrGene[1,]
High_a0.05_acrGenes$`17` = sim.GeneFlow.Hi.17$results$alpha0.05$acrGene[1,]
High_a0.05_acrGenes$`18` = sim.GeneFlow.Hi.18$results$alpha0.05$acrGene[1,]
High_a0.05_acrGenes$`19` = sim.GeneFlow.Hi.19$results$alpha0.05$acrGene[1,]
High_a0.05_acrGenes$`20` = sim.GeneFlow.Hi.20$results$alpha0.05$acrGene[1,]

Low_a0.01_acrGenes = list()
Low_a0.01_acrGenes$`01` = sim.GeneFlow.Lo.1$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGenes$`02` = sim.GeneFlow.Lo.2$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGenes$`03` = sim.GeneFlow.Lo.3$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGenes$`04` = sim.GeneFlow.Lo.4$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGenes$`05` = sim.GeneFlow.Lo.5$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGenes$`06` = sim.GeneFlow.Lo.6$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGenes$`07` = sim.GeneFlow.Lo.7$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGenes$`08` = sim.GeneFlow.Lo.8$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGenes$`09` = sim.GeneFlow.Lo.9$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGenes$`10` = sim.GeneFlow.Lo.10$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGenes$`11` = sim.GeneFlow.Lo.11$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGenes$`12` = sim.GeneFlow.Lo.12$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGenes$`13` = sim.GeneFlow.Lo.13$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGenes$`14` = sim.GeneFlow.Lo.14$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGenes$`15` = sim.GeneFlow.Lo.15$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGenes$`16` = sim.GeneFlow.Lo.16$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGenes$`17` = sim.GeneFlow.Lo.17$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGenes$`18` = sim.GeneFlow.Lo.18$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGenes$`19` = sim.GeneFlow.Lo.19$results$alpha0.01$acrGene[1,]
Low_a0.01_acrGenes$`20` = sim.GeneFlow.Lo.20$results$alpha0.01$acrGene[1,]

Low_a0.05_acrGenes = list()
Low_a0.05_acrGenes$`01` = sim.GeneFlow.Lo.1$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGenes$`02` = sim.GeneFlow.Lo.2$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGenes$`03` = sim.GeneFlow.Lo.3$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGenes$`04` = sim.GeneFlow.Lo.4$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGenes$`05` = sim.GeneFlow.Lo.5$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGenes$`06` = sim.GeneFlow.Lo.6$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGenes$`07` = sim.GeneFlow.Lo.7$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGenes$`08` = sim.GeneFlow.Lo.8$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGenes$`09` = sim.GeneFlow.Lo.9$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGenes$`10` = sim.GeneFlow.Lo.10$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGenes$`11` = sim.GeneFlow.Lo.11$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGenes$`12` = sim.GeneFlow.Lo.12$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGenes$`13` = sim.GeneFlow.Lo.13$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGenes$`14` = sim.GeneFlow.Lo.14$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGenes$`15` = sim.GeneFlow.Lo.15$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGenes$`16` = sim.GeneFlow.Lo.16$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGenes$`17` = sim.GeneFlow.Lo.17$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGenes$`18` = sim.GeneFlow.Lo.18$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGenes$`19` = sim.GeneFlow.Lo.19$results$alpha0.05$acrGene[1,]
Low_a0.05_acrGenes$`20` = sim.GeneFlow.Lo.20$results$alpha0.05$acrGene[1,]

################################################################################

## A custom function is specified which converts results matrices into 
## presence/absence matrices, stacks the matrix columns and adds identifier 
## information.
myfunc1 = function(inData, simNum){
  handle = inData
  # It is CRITICAL that the order of stats is alphabetic, because 
  # sim.E.001$results$alpha0.01$perGene is also sorted alphabetically
  colnames(handle) = c("gsi", "gtp", "ndc", "ray")
  #handle[grepl("\\*", handle)] = 0
  #handle[grepl("n.s.", handle)] = 1
  #handle[grepl(" 0", handle)] = 1
  handle[grepl("\\*", handle)] = 1
  handle[grepl("n.s.", handle)] = 0
  handle[grepl(" 0", handle)] = 0
  handle = stack(data.frame(handle, stringsAsFactors = FALSE))
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
  #handle[grepl("\\*", handle)] = 0
  #handle[grepl("n.s.", handle)] = 1
  #handle[grepl(" 0", handle)] = 1
  handle[grepl("\\*", handle)] = 1
  handle[grepl("n.s.", handle)] = 0
  handle[grepl(" 0", handle)] = 0
  handle = stack(data.frame(handle, stringsAsFactors = FALSE))
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

#data_a0.01[,ncol(data_a0.01)+1] = "1"
#colnames(data_a0.01)[ncol(data_a0.01)] = "migration"

#data_a0.05[,ncol(data_a0.05)+1] = "1"
#colnames(data_a0.05)[ncol(data_a0.05)] = "migration"

################################################################################

highL = list()
sims = as.numeric(names(High_a0.01_acrGenes))
for (i in 1:length(High_a0.01_acrGenes)) {
  highL[[i]] = myfunc2(High_a0.01_acrGenes[[i]], sims[i])
}
High_a0.01 = do.call("rbind", highL)
High_a0.01[,ncol(High_a0.01)+1] = "High_Subst_Rate"
colnames(High_a0.01)[ncol(High_a0.01)] = "ratetype"


lowL = list()
sims = as.numeric(names(Low_a0.01_acrGenes))
for (i in 1:length(Low_a0.01_acrGenes)) {
  lowL[[i]] = myfunc2(Low_a0.01_acrGenes[[i]], sims[i])
}
Low_a0.01 = do.call("rbind", lowL)
Low_a0.01[,ncol(Low_a0.01)+1] = "Low_Subst_Rate"
colnames(Low_a0.01)[ncol(Low_a0.01)] = "ratetype"


highL = list()
sims = as.numeric(names(High_a0.05_acrGenes))
for (i in 1:length(High_a0.05_acrGenes)) {
  highL[[i]] = myfunc2(High_a0.05_acrGenes[[i]], sims[i])
}
High_a0.05 = do.call("rbind", highL)
High_a0.05[,ncol(High_a0.05)+1] = "High_Subst_Rate"
colnames(High_a0.05)[ncol(High_a0.05)] = "ratetype"


lowL = list()
sims = as.numeric(names(Low_a0.05_acrGenes))
for (i in 1:length(Low_a0.05_acrGenes)) {
  lowL[[i]] = myfunc2(Low_a0.05_acrGenes[[i]], sims[i])
}
Low_a0.05 = do.call("rbind", lowL)
Low_a0.05[,ncol(Low_a0.05)+1] = "Low_Subst_Rate"
colnames(Low_a0.05)[ncol(Low_a0.05)] = "ratetype"

data_acrGene_a0.01 = rbind(High_a0.01, Low_a0.01)
data_acrGene_a0.05 = rbind(High_a0.05, Low_a0.05)

#data_a0.01[,ncol(data_a0.01)+1] = "1"
#colnames(data_a0.01)[ncol(data_a0.01)] = "migration"

#data_a0.05[,ncol(data_a0.05)+1] = "1"
#colnames(data_a0.05)[ncol(data_a0.05)] = "migration"

################################################################################

data_a0.01 = rbind(data_perGene_a0.01, data_acrGene_a0.01)
data_a0.05 = rbind(data_perGene_a0.05, data_acrGene_a0.05)

################################################################################

## The distribution of false positive result values is visualized as 
## presence/absence plot.
library(ggplot2)
plot = ggplot(data=data_a0.01, aes(x=sim, y=gene)) +
    geom_point(aes(colour=value), size=3, alpha=1.0) +
#plot = ggplot(data=data_a0.05, aes(x=sim, y=gene)) +
#    geom_point(aes(colour=value), size=4, alpha=0.25) +
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
    ylab("Genes\n")
#    ylab("Genes\n") +
#    geom_point(data=data_a0.01, aes(x=sim, y=gene, colour=value), size=2, alpha=1.0)


################################################################################

svg("/home/michael/Desktop/Figure2b_PhygeoSims.svg", width=6, height=6)
plot
dev.off()

