#!/usr/bin/R
#author = "Michael Gruenstaeudl, PhD"
#copyright = "Copyright (C) 2014 Michael Gruenstaeudl"
#contributors = c("Michael Gruenstaeudl")
#email = "gruenstaeudl.1@osu.edu"
#version = "2015.01.12.1400"

## Global Variables
setwd(paste("/home/michael/research/analyses/03_analyses_P2C2M/",
            "Revision_RS1/08_for_visualization/", sep=""))

n_sims = 25
nums = sprintf("%02d", c(1:n_sims))
fn_prefixes = c("RS1.5loci.sim.", "RS1.10loci.sim.",
                "RS1.15loci.sim.", "RS1.20loci.sim.")

## Load .rda-files
for (fns in fn_prefixes) {
    for (num in nums) {
      load(paste(fns, num, ".rda", sep=""))
    }
}

####################
# Helper Functions #
####################

customStack = function(inData, simNum, nLoci, special=FALSE) {
# Custom function which 
# (a) converts results matrices into presence/absence matrices,
# (b) stacks the matrix columns,
# (c) adds identifier information

  if (special) {handle = t(as.matrix(inData))}
  else {handle = inData}
  # Order of stats must be alphabetic because input data
  # also sorted alphabetically
  colnames(handle) = c("gsi", "gtp", "ndc", "ray")
  handle[grepl("\\*", handle)] = 1
  handle[grepl("n.s.", handle)] = 0
  handle[grepl(" 0", handle)] = 0
  handle = stack(data.frame(handle, stringsAsFactors=FALSE))
  colnames(handle)[1] = "value"
  colnames(handle)[2] = "stat"  
  if (special) {handle[,3] = rep("sum", 4)}
  else {handle[,3] = rep(c(1:nLoci), 4)}
  colnames(handle)[3] = "gene"
  handle[,4] = simNum
  colnames(handle)[4] = "sim"
  return(handle)
}

wrapper = function(inData, nums, nLoci, special=FALSE) {
  out_list = list()
  for (num in nums) {
    out_list[[num]] = customStack(inData[[num]], num, nLoci, special)
  }
  return(out_list)
}


################################################
# STEP1. Load perGene data and save into lists #
################################################ 

#### perGene - 5 loci ###
RS1.5loci_a0.01_perGene = list()
RS1.5loci_a0.05_perGene = list()
for (num in nums) {
  varN_perGene_0.01 = paste(fn_prefixes[[1]], num, 
                            "$results$alpha0.01$perGene", sep="")
  varN_perGene_0.05 = paste(fn_prefixes[[1]], num,
                            "$results$alpha0.05$perGene", sep="")
  RS1.5loci_a0.01_perGene[[num]] = eval(parse(text = varN_perGene_0.01))
  RS1.5loci_a0.05_perGene[[num]] = eval(parse(text = varN_perGene_0.05))
}

#### perGene - 10 loci ###
RS1.10loci_a0.01_perGene = list()
RS1.10loci_a0.05_perGene = list()
for (num in nums) {
  varN_perGene_0.01 = paste(fn_prefixes[[2]], num, 
                            "$results$alpha0.01$perGene", sep="")
  varN_perGene_0.05 = paste(fn_prefixes[[2]], num,
                            "$results$alpha0.05$perGene", sep="")
  RS1.10loci_a0.01_perGene[[num]] = eval(parse(text = varN_perGene_0.01))
  RS1.10loci_a0.05_perGene[[num]] = eval(parse(text = varN_perGene_0.05))
}

#### perGene - 15 loci ###
RS1.15loci_a0.01_perGene = list()
RS1.15loci_a0.05_perGene = list()
for (num in nums) {
  varN_perGene_0.01 = paste(fn_prefixes[[3]], num, 
                            "$results$alpha0.01$perGene", sep="")
  varN_perGene_0.05 = paste(fn_prefixes[[3]], num,
                            "$results$alpha0.05$perGene", sep="")
  RS1.15loci_a0.01_perGene[[num]] = eval(parse(text = varN_perGene_0.01))
  RS1.15loci_a0.05_perGene[[num]] = eval(parse(text = varN_perGene_0.05))
}

#### perGene - 20 loci ###
RS1.20loci_a0.01_perGene = list()
RS1.20loci_a0.05_perGene = list()
for (num in nums) {
  varN_perGene_0.01 = paste(fn_prefixes[[4]], num, 
                            "$results$alpha0.01$perGene", sep="")
  varN_perGene_0.05 = paste(fn_prefixes[[4]], num,
                            "$results$alpha0.05$perGene", sep="")
  RS1.20loci_a0.01_perGene[[num]] = eval(parse(text = varN_perGene_0.01))
  RS1.20loci_a0.05_perGene[[num]] = eval(parse(text = varN_perGene_0.05))
}

################################################
# STEP2. Load acrGenes data and save into lists #
################################################

#### acrGenes - 5 loci ###
RS1.5loci_a0.01_acrGene = list()
RS1.5loci_a0.05_acrGene = list()
for (num in nums) {
  varN_acrGene_0.01 = paste(fn_prefixes[[1]], num,
                            "$results$alpha0.01$acrGene[1,]", sep="")
  varN_acrGene_0.05 = paste(fn_prefixes[[1]], num,
                            "$results$alpha0.05$acrGene[1,]", sep="")
  RS1.5loci_a0.01_acrGene[[num]] = eval(parse(text = varN_acrGene_0.01))
  RS1.5loci_a0.05_acrGene[[num]] = eval(parse(text = varN_acrGene_0.05))
}

#### acrGenes - 10 loci ###
RS1.10loci_a0.01_acrGene = list()
RS1.10loci_a0.05_acrGene = list()
for (num in nums) {
  varN_acrGene_0.01 = paste(fn_prefixes[[2]], num,
                            "$results$alpha0.01$acrGene[1,]", sep="")
  varN_acrGene_0.05 = paste(fn_prefixes[[2]], num,
                            "$results$alpha0.05$acrGene[1,]", sep="")
  RS1.10loci_a0.01_acrGene[[num]] = eval(parse(text = varN_acrGene_0.01))
  RS1.10loci_a0.05_acrGene[[num]] = eval(parse(text = varN_acrGene_0.05))
}

#### acrGenes - 15 loci ###
RS1.15loci_a0.01_acrGene = list()
RS1.15loci_a0.05_acrGene = list()
for (num in nums) {
  varN_acrGene_0.01 = paste(fn_prefixes[[3]], num,
                            "$results$alpha0.01$acrGene[1,]", sep="")
  varN_acrGene_0.05 = paste(fn_prefixes[[3]], num,
                            "$results$alpha0.05$acrGene[1,]", sep="")
  RS1.15loci_a0.01_acrGene[[num]] = eval(parse(text = varN_acrGene_0.01))
  RS1.15loci_a0.05_acrGene[[num]] = eval(parse(text = varN_acrGene_0.05))
}

#### acrGenes - 20 loci ###
RS1.20loci_a0.01_acrGene = list()
RS1.20loci_a0.05_acrGene = list()
for (num in nums) {
  varN_acrGene_0.01 = paste(fn_prefixes[[4]], num,
                            "$results$alpha0.01$acrGene[1,]", sep="")
  varN_acrGene_0.05 = paste(fn_prefixes[[4]], num,
                            "$results$alpha0.05$acrGene[1,]", sep="")
  RS1.20loci_a0.01_acrGene[[num]] = eval(parse(text = varN_acrGene_0.01))
  RS1.20loci_a0.05_acrGene[[num]] = eval(parse(text = varN_acrGene_0.05))
}

####################################
# STEP3. Format data for "perGene" #
####################################

#### perGene - 5 loci at alpha=0.01 ####
RS1_5l_pG_0.01 = do.call("rbind", wrapper(RS1.5loci_a0.01_perGene, nums, 5))
RS1_5l_pG_0.01[, ncol(RS1_5l_pG_0.01)+1] = "5"
colnames(RS1_5l_pG_0.01)[ncol(RS1_5l_pG_0.01)] = "num_loci"

#### perGene - 5 loci at alpha=0.05 ####
RS1_5l_pG_0.05 = do.call("rbind", wrapper(RS1.5loci_a0.05_perGene, nums, 5))
RS1_5l_pG_0.05[, ncol(RS1_5l_pG_0.05)+1] = "5"
colnames(RS1_5l_pG_0.05)[ncol(RS1_5l_pG_0.05)] = "num_loci"

#### perGene - 10 loci at alpha=0.01 ####
RS1_10l_pG_0.01 = do.call("rbind", wrapper(RS1.10loci_a0.01_perGene, nums, 10))
RS1_10l_pG_0.01[, ncol(RS1_10l_pG_0.01)+1] = "10"
colnames(RS1_10l_pG_0.01)[ncol(RS1_10l_pG_0.01)] = "num_loci"

#### perGene - 10 loci at alpha=0.05 ####
RS1_10l_pG_0.05 = do.call("rbind", wrapper(RS1.10loci_a0.05_perGene, nums, 10))
RS1_10l_pG_0.05[, ncol(RS1_10l_pG_0.05)+1] = "10"
colnames(RS1_10l_pG_0.05)[ncol(RS1_10l_pG_0.05)] = "num_loci"

#### perGene - 15 loci at alpha=0.01 ####
RS1_15l_pG_0.01 = do.call("rbind", wrapper(RS1.15loci_a0.01_perGene, nums, 15))
RS1_15l_pG_0.01[, ncol(RS1_15l_pG_0.01)+1] = "15"
colnames(RS1_15l_pG_0.01)[ncol(RS1_15l_pG_0.01)] = "num_loci"

#### perGene - 15 loci at alpha=0.05 ####
RS1_15l_pG_0.05 = do.call("rbind", wrapper(RS1.15loci_a0.05_perGene, nums, 15))
RS1_15l_pG_0.05[, ncol(RS1_15l_pG_0.05)+1] = "15"
colnames(RS1_15l_pG_0.05)[ncol(RS1_15l_pG_0.05)] = "num_loci"

#### perGene - 20 loci at alpha=0.01 ####
RS1_20l_pG_0.01 = do.call("rbind", wrapper(RS1.20loci_a0.01_perGene, nums, 20))
RS1_20l_pG_0.01[, ncol(RS1_20l_pG_0.01)+1] = "20"
colnames(RS1_20l_pG_0.01)[ncol(RS1_20l_pG_0.01)] = "num_loci"

#### perGene - 20 loci at alpha=0.05 ####
RS1_20l_pG_0.05 = do.call("rbind", wrapper(RS1.20loci_a0.05_perGene, nums, 20))
RS1_20l_pG_0.05[, ncol(RS1_20l_pG_0.05)+1] = "20"
colnames(RS1_20l_pG_0.05)[ncol(RS1_20l_pG_0.05)] = "num_loci"


########################################
# STEP4. Format data for "acrossGenes" #
########################################

#### acrGenes - 5 loci at alpha=0.01 ####
RS1_5l_aG_0.01 = do.call("rbind", wrapper(RS1.5loci_a0.01_acrGene, nums, 5, special=T))
RS1_5l_aG_0.01[, ncol(RS1_5l_aG_0.01)+1] = "5"
colnames(RS1_5l_aG_0.01)[ncol(RS1_5l_aG_0.01)] = "num_loci"

#### acrGenes - 5 loci at alpha=0.05 ####
RS1_5l_aG_0.05 = do.call("rbind", wrapper(RS1.5loci_a0.05_acrGene, nums, 5, special=T))
RS1_5l_aG_0.05[, ncol(RS1_5l_aG_0.05)+1] = "5"
colnames(RS1_5l_aG_0.05)[ncol(RS1_5l_aG_0.05)] = "num_loci"

#### acrGenes - 10 loci at alpha=0.01 ####
RS1_10l_aG_0.01 = do.call("rbind", wrapper(RS1.10loci_a0.01_acrGene, nums, 10, special=T))
RS1_10l_aG_0.01[, ncol(RS1_10l_aG_0.01)+1] = "10"
colnames(RS1_10l_aG_0.01)[ncol(RS1_10l_aG_0.01)] = "num_loci"

#### acrGenes - 10 loci at alpha=0.05 ####
RS1_10l_aG_0.05 = do.call("rbind", wrapper(RS1.10loci_a0.05_acrGene, nums, 10, special=T))
RS1_10l_aG_0.05[, ncol(RS1_10l_aG_0.05)+1] = "10"
colnames(RS1_10l_aG_0.05)[ncol(RS1_10l_aG_0.05)] = "num_loci"

#### acrGenes - 15 loci at alpha=0.01 ####
RS1_15l_aG_0.01 = do.call("rbind", wrapper(RS1.15loci_a0.01_acrGene, nums, 15, special=T))
RS1_15l_aG_0.01[, ncol(RS1_15l_aG_0.01)+1] = "15"
colnames(RS1_15l_aG_0.01)[ncol(RS1_15l_aG_0.01)] = "num_loci"

#### acrGenes - 15 loci at alpha=0.05 ####
RS1_15l_aG_0.05 = do.call("rbind", wrapper(RS1.15loci_a0.05_acrGene, nums, 15, special=T))
RS1_15l_aG_0.05[, ncol(RS1_15l_aG_0.05)+1] = "15"
colnames(RS1_15l_aG_0.05)[ncol(RS1_15l_aG_0.05)] = "num_loci"

#### acrGenes - 20 loci at alpha=0.01 ####
RS1_20l_aG_0.01 = do.call("rbind", wrapper(RS1.20loci_a0.01_acrGene, nums, 20, special=T))
RS1_20l_aG_0.01[, ncol(RS1_20l_aG_0.01)+1] = "20"
colnames(RS1_20l_aG_0.01)[ncol(RS1_20l_aG_0.01)] = "num_loci"

#### acrGenes - 20 loci at alpha=0.05 ####
RS1_20l_aG_0.05 = do.call("rbind", wrapper(RS1.20loci_a0.05_acrGene, nums, 20, special=T))
RS1_20l_aG_0.05[, ncol(RS1_20l_aG_0.05)+1] = "20"
colnames(RS1_20l_aG_0.05)[ncol(RS1_20l_aG_0.05)] = "num_loci"


#######################################
# STEP5. Combine perGene and acrGenes #
#######################################

RS1_5l_0.01 = rbind(RS1_5l_pG_0.01, RS1_5l_aG_0.01)
RS1_5l_0.05 = rbind(RS1_5l_pG_0.05, RS1_5l_aG_0.05)

RS1_10l_0.01 = rbind(RS1_10l_pG_0.01, RS1_10l_aG_0.01)
RS1_10l_0.05 = rbind(RS1_10l_pG_0.05, RS1_10l_aG_0.05)

RS1_15l_0.01 = rbind(RS1_15l_pG_0.01, RS1_15l_aG_0.01)
RS1_15l_0.05 = rbind(RS1_15l_pG_0.05, RS1_15l_aG_0.05)

RS1_20l_0.01 = rbind(RS1_20l_pG_0.01, RS1_20l_aG_0.01)
RS1_20l_0.05 = rbind(RS1_20l_pG_0.05, RS1_20l_aG_0.05)

#############################
# STEP6. Give order to plot #
#############################
RS1_5l_0.01$num_loci = factor(RS1_5l_0.01$num_loci, levels = c(5, 10, 15, 20))
RS1_5l_0.05$num_loci = factor(RS1_5l_0.05$num_loci, levels = c(5, 10, 15, 20))

RS1_10l_0.01$num_loci = factor(RS1_10l_0.01$num_loci, levels = c(5, 10, 15, 20))
RS1_10l_0.05$num_loci = factor(RS1_10l_0.05$num_loci, levels = c(5, 10, 15, 20))

RS1_15l_0.01$num_loci = factor(RS1_15l_0.01$num_loci, levels = c(5, 10, 15, 20))
RS1_15l_0.05$num_loci = factor(RS1_15l_0.05$num_loci, levels = c(5, 10, 15, 20))

RS1_20l_0.01$num_loci = factor(RS1_20l_0.01$num_loci, levels = c(5, 10, 15, 20))
RS1_20l_0.05$num_loci = factor(RS1_20l_0.05$num_loci, levels = c(5, 10, 15, 20))

#####################
# STEP7. Make plots #
#####################
library(ggplot2)

# N of loci to be specified in eight different positions
n_loci = 5

plot_RS1_5l_0.01and0.05 = ggplot(data=RS1_5l_0.05, aes(x=sim, y=gene)) +
    geom_point(aes(colour=value), size=4, alpha=0.25) +
    scale_colour_manual(values=c(NA, 'black')) +
    facet_grid(stat ~ num_loci) +
    ggtitle(paste("Phylogenetically-inspired Simulations",
                  "Distribution of False Positives",
                  "N of loci: 5",
                  sep="\n")) +
    theme_bw() +
    scale_x_discrete(breaks=c(nums), labels=c(nums)) +
    scale_y_discrete(limits=c("sum", c(n_loci:1))) +
    theme(axis.text = element_text(size=6),
          strip.background=element_rect(fill="white")) +
    xlab("\nSimulations") + 
    ylab("Genes\n") +
    geom_point(data=RS1_5l_0.01, aes(x=sim, y=gene, colour=value),
               size=2, alpha=1.0)

plot_RS1_5l_0.01only = ggplot(data=RS1_5l_0.01, aes(x=sim, y=gene)) +
    geom_point(aes(colour=value), size=2, alpha=1.0) +
    scale_colour_manual(values=c(NA, 'black')) +
    facet_grid(stat ~ num_loci) +
    ggtitle(paste("Phylogenetically-inspired Simulations",
                  "Distribution of False Positives",
                  "N of loci: 5",
                  sep="\n")) +
    theme_bw() +
    scale_x_discrete(breaks=c(nums), labels=c(nums)) +
    scale_y_discrete(limits=c("sum", c(n_loci:1))) +
    theme(axis.text = element_text(size=6),
          strip.background=element_rect(fill="white")) +
    xlab("\nSimulations") + 
    ylab("Genes\n")

svg("/home/michael/Desktop/Figure3_PhygenSims_5loci_0.01and0.05.svg", width=5, height=5)
plot_RS1_5l_0.01and0.05
dev.off()

svg("/home/michael/Desktop/Figure3_PhygenSims_5loci_0.01only.svg", width=5, height=5)
plot_RS1_5l_0.01only
dev.off()


n_loci = 10
plot_RS1_10l_0.01and0.05 = ggplot(data=RS1_10l_0.05, aes(x=sim, y=gene)) +
    geom_point(aes(colour=value), size=4, alpha=0.25) +
    scale_colour_manual(values=c(NA, 'black')) +
    facet_grid(stat ~ num_loci) +
    ggtitle(paste("Phylogenetically-inspired Simulations",
                  "Distribution of False Positives",
                  "N of loci: 10",
                  sep="\n")) +
    theme_bw() +
    scale_x_discrete(breaks=c(nums), labels=c(nums)) +
    scale_y_discrete(limits=c("sum", c(n_loci:1))) +
    theme(axis.text = element_text(size=6),
          strip.background=element_rect(fill="white")) +
    xlab("\nSimulations") + 
    ylab("Genes\n") +
    geom_point(data=RS1_10l_0.01, aes(x=sim, y=gene, colour=value),
               size=2, alpha=1.0)

plot_RS1_10l_0.01only = ggplot(data=RS1_10l_0.01, aes(x=sim, y=gene)) +
    geom_point(aes(colour=value), size=2, alpha=1.0) +
    scale_colour_manual(values=c(NA, 'black')) +
    facet_grid(stat ~ num_loci) +
    ggtitle(paste("Phylogenetically-inspired Simulations",
                  "Distribution of False Positives",
                  "N of loci: 10",
                  sep="\n")) +
    theme_bw() +
    scale_x_discrete(breaks=c(nums), labels=c(nums)) +
    scale_y_discrete(limits=c("sum", c(n_loci:1))) +
    theme(axis.text = element_text(size=6),
          strip.background=element_rect(fill="white")) +
    xlab("\nSimulations") + 
    ylab("Genes\n")

svg("/home/michael/Desktop/Figure3_PhygenSims_10loci_0.01and0.05.svg", width=5, height=7.5)
plot_RS1_10l_0.01and0.05
dev.off()

svg("/home/michael/Desktop/Figure3_PhygenSims_10loci_0.01only.svg", width=5, height=7.5)
plot_RS1_10l_0.01only
dev.off()


n_loci = 15
plot_RS1_15l_0.01and0.05 = ggplot(data=RS1_15l_0.05, aes(x=sim, y=gene)) +
    geom_point(aes(colour=value), size=4, alpha=0.25) +
    scale_colour_manual(values=c(NA, 'black')) +
    facet_grid(stat ~ num_loci) +
    ggtitle(paste("Phylogenetically-inspired Simulations",
                  "Distribution of False Positives",
                  "N of loci: 15",
                  sep="\n")) +
    theme_bw() +
    scale_x_discrete(breaks=c(nums), labels=c(nums)) +
    scale_y_discrete(limits=c("sum", c(n_loci:1))) +
    theme(axis.text = element_text(size=6),
          strip.background=element_rect(fill="white")) +
    xlab("\nSimulations") + 
    ylab("Genes\n") +
    geom_point(data=RS1_15l_0.01, aes(x=sim, y=gene, colour=value),
               size=2, alpha=1.0)

plot_RS1_15l_0.01only = ggplot(data=RS1_15l_0.01, aes(x=sim, y=gene)) +
    geom_point(aes(colour=value), size=2, alpha=1.0) +
    scale_colour_manual(values=c(NA, 'black')) +
    facet_grid(stat ~ num_loci) +
    ggtitle(paste("Phylogenetically-inspired Simulations",
                  "Distribution of False Positives",
                  "N of loci: 15",
                  sep="\n")) +
    theme_bw() +
    scale_x_discrete(breaks=c(nums), labels=c(nums)) +
    scale_y_discrete(limits=c("sum", c(n_loci:1))) +
    theme(axis.text = element_text(size=6),
          strip.background=element_rect(fill="white")) +
    xlab("\nSimulations") + 
    ylab("Genes\n")

svg("/home/michael/Desktop/Figure3_PhygenSims_15loci_0.01and0.05.svg", width=5, height=10)
plot_RS1_15l_0.01and0.05
dev.off()

svg("/home/michael/Desktop/Figure3_PhygenSims_15loci_0.01only.svg", width=5, height=10)
plot_RS1_15l_0.01only
dev.off()


n_loci = 20
plot_RS1_20l_0.01and0.05 = ggplot(data=RS1_20l_0.05, aes(x=sim, y=gene)) +
    geom_point(aes(colour=value), size=4, alpha=0.25) +
    scale_colour_manual(values=c(NA, 'black')) +
    facet_grid(stat ~ num_loci) +
    ggtitle(paste("Phylogenetically-inspired Simulations",
                  "Distribution of False Positives",
                  "N of loci: 20",
                  sep="\n")) +
    theme_bw() +
    scale_x_discrete(breaks=c(nums), labels=c(nums)) +
    scale_y_discrete(limits=c("sum", c(n_loci:1))) +
    theme(axis.text = element_text(size=6),
          strip.background=element_rect(fill="white")) +
    xlab("\nSimulations") + 
    ylab("Genes\n") +
    geom_point(data=RS1_20l_0.01, aes(x=sim, y=gene, colour=value),
               size=2, alpha=1.0)

plot_RS1_20l_0.01only = ggplot(data=RS1_20l_0.01, aes(x=sim, y=gene)) +
    geom_point(aes(colour=value), size=2, alpha=1.0) +
    scale_colour_manual(values=c(NA, 'black')) +
    facet_grid(stat ~ num_loci) +
    ggtitle(paste("Phylogenetically-inspired Simulations",
                  "Distribution of False Positives",
                  "N of loci: 20",
                  sep="\n")) +
    theme_bw() +
    scale_x_discrete(breaks=c(nums), labels=c(nums)) +
    scale_y_discrete(limits=c("sum", c(n_loci:1))) +
    theme(axis.text = element_text(size=6),
          strip.background=element_rect(fill="white")) +
    xlab("\nSimulations") + 
    ylab("Genes\n")    

svg("/home/michael/Desktop/Figure3_PhygenSims_20loci_0.01and0.05.svg", width=5, height=12.5)
plot_RS1_20l_0.01and0.05
dev.off()

svg("/home/michael/Desktop/Figure3_PhygenSims_20loci_0.01only.svg", width=5, height=12.5)
plot_RS1_20l_0.01only
dev.off()
