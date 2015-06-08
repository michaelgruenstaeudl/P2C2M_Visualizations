#!/usr/bin/R
#author = "Michael Gruenstaeudl, PhD"
#copyright = "Copyright (C) 2014 Michael Gruenstaeudl"
#contributors = c("Michael Gruenstaeudl","Noah Reid")
#email = "gruenstaeudl.1@osu.edu"
#version = "2014.12.15.2100"

setwd("/home/michael/research/analyses/03_analyses_P2C2M/Figure1_FalsePositives/sim.NoMigration_COMPLETE/06_output_from_P2C2M/06b_starBEASTgeneTrees/")

for (i in c(1:9)) {
  load(paste("sim.E.00",i,".rda",sep=""))
}
for (i in c(10:20)) {
  load(paste("sim.E.0",i,".rda",sep=""))
}

################################################################################

## Recalculating CV for descriptive statistics "ndc"

myfunc1 = function(inFn) {
  load(paste(inFn, ".rda", sep=''))
  diff = eval(parse(text=paste(inFn, "$rawStats$NDC$dif", sep='')))
  Stdv = apply(diff, MARGIN=1, sd, na.rm=T)
  Mean = rowMeans(diff)
  cv = Stdv/Mean
  is.na(cv) <- do.call(cbind,lapply(cv, is.infinite))
  outD = mean(cv, na.rm=T)
return(outD)
}

high_gsi = list()
for (i in 1:9) {
  varName = paste("sim.E.00", i, sep='')
  high_gsi[[i]] = myfunc1(varName)
}

for (i in 10:20) {
  varName = paste("sim.E.0", i, sep='')
  high_gsi[[i]] = myfunc1(varName)
}

high_gsi = as.data.frame(as.matrix(high_gsi))
high_gsi[,2] = "ndc"
high_gsi[,3] = rownames(high_gsi)
colnames(high_gsi) = c("cv", "stat", "sim")
rownames(high_gsi) = NULL

################################################################################

## Recalculating CV for descriptive statistics "gsi"

myfunc1 = function(inFn) {
  load(paste(inFn, ".rda", sep=''))
  diff = eval(parse(text=paste(inFn, "$rawStats$GSI$dif", sep='')))
  Stdv = apply(diff, MARGIN=1, sd, na.rm=T)
  Mean = rowMeans(diff)
  cv = Stdv/Mean
  is.na(cv) <- do.call(cbind,lapply(cv, is.infinite))
  outD = mean(cv, na.rm=T)
return(outD)
}

high_ndc = list()
for (i in 1:9) {
  varName = paste("sim.E.00", i, sep='')
  high_ndc[[i]] = myfunc1(varName)
}

for (i in 10:20) {
  varName = paste("sim.E.0", i, sep='')
  high_ndc[[i]] = myfunc1(varName)
}

high_ndc = as.data.frame(as.matrix(high_ndc))
high_ndc[,2] = "gsi"
high_ndc[,3] = rownames(high_ndc)
colnames(high_ndc) = c("cv", "stat", "sim")
rownames(high_ndc) = NULL

################################################################################

## Extracting CV for descriptive statistics "gsi", "gtp" and "ray"
myfunc2 = function(inFn) {
  raw = eval(parse(text=paste(inFn, "$results$alpha0.01$acrGenes[5,]", sep='')))
  cv = as.data.frame(as.matrix(raw))
  cv[,2] = tolower(rownames(cv))

  # Extract strings of importance
  cv[,1] = unlist(lapply(cv[,1], function(x) unlist(strsplit(as.character(x), split=" "))[1]))
  cv[,2] = unlist(lapply(cv[,2], function(x) unlist(strsplit(x, split="\\["))[1]))

  # Remove all ndc and gsi entries
  cv = cv[which(cv[,2]!="ndc"),]
  cv = cv[which(cv[,2]!="gsi"),]

return(cv)
}

high_oth = list()
for (i in 1:9) {
  varName = paste("sim.E.00", i, sep='')
  tmpD = myfunc2(varName)
  tmpD[,ncol(tmpD)+1] = i
  colnames(tmpD) = c("cv", "stat", "sim")
  rownames(tmpD) = NULL
  high_oth[[i]] = tmpD
}

for (i in 10:20) {
  varName = paste("sim.E.0", i, sep='')
  tmpD = myfunc2(varName)
  tmpD[,ncol(tmpD)+1] = i
  colnames(tmpD) = c("cv", "stat", "sim")
  rownames(tmpD) = NULL
  high_oth[[i]] = tmpD
}

high_oth = do.call("rbind", high_oth)

################################################################################

frmtMntse = function (inNum, mntse) {
  # function to control the significands of a number
return(as.numeric(format(round(inNum, mntse), nsmall=mntse)))
}

high_cv = rbind(high_oth, high_ndc, high_gsi)
# TFL is necessary, because high_cv$cv is a list, not a vector at this point
high_cv$cv = unlist(high_cv$cv)

# Constrain values to 4 significands after comma, in cases where more than that
high_cv$cv = unlist(lapply(as.numeric(high_cv$cv), frmtMntse, 4))

# Remove outliers
outl = boxplot.stats(as.numeric(high_cv$cv))$out
high_cv = high_cv[which(!high_cv$cv %in% outl),]

#
high_cv$sim = as.numeric(high_cv$sim)


################################################################################

# EXECUTE ADD-ON HERE

rownames(high_cv) = NULL
rownames(low_cv) = NULL
data_cv = rbind(high_cv, low_cv)

################################################################################

## The distribution of false positive result values is visualized as 
## presence/absence plot.
library(ggplot2)
library(grid)

plot = ggplot(data=data_cv, aes(x=sim, y=cv, group=stat)) +
    geom_hline(yintercept=0, size=1, colour="grey") +
    geom_point(size=2.5) +
    facet_grid(. ~ stat) +
    xlab("\nSimulations") + 
    ylab("Mean CV\n") +
    scale_x_discrete(breaks=c(1,5,10,15,20), labels=c(1,5,10,15,20)) +
    ggtitle(expression(atop("Comparison of Mean Coefficients of Variation (CV), with outlier values removed",
            atop("Data of low and high substitution rates were combined for better visualization\n")))) +
    theme_bw() +
    theme(axis.text = element_text(size=8),
          strip.background=element_rect(fill="white"),
          panel.margin = unit(0.5, "lines")) 


################################################################################

svg("/home/michael/Desktop/SupplFile3_MeanCVs.svg", height=5, width=6)
plot
dev.off()

