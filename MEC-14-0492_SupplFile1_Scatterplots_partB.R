#!/usr/bin/R
#author = "Michael Gruenstaeudl, PhD"
#copyright = "Copyright (C) 2014-2015 Michael Gruenstaeudl"
#contributors = c("Michael Gruenstaeudl")
#email = "mi.gruenstaeudl@gmail.com"
#version = "2015.05.05.1900"

## HELPER FUNCTION
lmp <- function (modelobject) {
# Source: http://www.gettinggeneticsdone.com/2011/01/rstats-function-for-extracting-f-test-p.html
	if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
	f <- summary(modelobject)$fstatistic
	p <- pf(f[1],f[2],f[3],lower.tail=F)
	attributes(p) <- NULL
	return(p)
}

## SETTING GLOBAL VARIABLES
library(ggplot2)
in_dir = "/home/lenovox200/research/analyses/03_analyses_P2C2M/SupplFile5_GTPvsRAY/02_posteriorDistribs/06_output_from_P2C2M/old/"
#out_dir = "/home/lenovox200/research/analyses/03_analyses_P2C2M/SupplFile5_GTPvsRAY/03_scatterplots/test/"
out_dir = "/home/lenovox200/Desktop/"
in_fn = "sim.E.011"

## LOADING DATA
load(paste(in_dir, in_fn, ".rda", sep=""))
indata = get(in_fn)

starBEAST_logFile = read.table("/home/lenovox200/research/analyses/03_analyses_P2C2M/SupplFile5_GTPvsRAY/02_posteriorDistribs/03_output_from_BEAST_starBEAST/sim.E.11.SpeciesTree.log.reduced", sep="\t", header=T)
species.coalescent = starBEAST_logFile[,"species.coalescent"]

## STACKING DATA
sub_data = indata$rawStats$RAY$emp$unsort
handle_RAY = stack(as.data.frame(sub_data))

sub_data = indata$rawStats$GTP$emp$unsort
handle_GTP = stack(as.data.frame(sub_data))

handle_starBEAST = handle_RAY
handle_starBEAST[,1] = rep(species.coalescent, 10)

handle = as.data.frame(cbind(handle_RAY[,2], handle_RAY[,1],
                    handle_GTP[,1], handle_starBEAST[,1]))
# handle_RAY[,2] not stored proberly in TLA, hence:
handle[,1] = handle_RAY[,2]
colnames(handle)= c("gene", "coal", "lcwt", "species.coalescent")

handle_new = rbind(handle[which(handle[,1]=="gene003"),],
                   handle[which(handle[,1]=="gene004"),],
                   handle[which(handle[,1]=="gene005"),])


## Generating plots

my_function = function(indata, out_dir, current_var, current_label) {

    # Conduct additional calculations
    corcofs = c()
    lrgm_fit = c()

    for (i in 3:5) {
        temp = handle[which(handle[,1]==paste("gene00", i, sep="")),]

        # Calculate correlation coefficients
        cc = cor(temp[,"species.coalescent"], temp[, current_var])
        corcofs = c(corcofs, round(cc, 4))

        # Testing fit of the data to a linear regression model
        f = as.formula(paste("species.coalescent", "~", current_var, sep=""))
        pval = lmp(lm(f, data=temp))
        pval_pretty = format(pval, digits=4, width=7)
        lrgm_fit = c(lrgm_fit, pval_pretty)
    }

    # Generate raw plot
    plot_handle = ggplot(data=indata, aes(x=species.coalescent, y=get(current_var))) +
      geom_point(shape=1,color="darkgrey") +
      geom_smooth(method="lm", fill=NA, color="black") +
      facet_grid(gene ~ ., scale="free_x") +
      labs(x="\nSpecies coalescent likelihood (*BEAST)") +
      labs(y=paste(current_label, "\n", sep="")) +
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

    # Add labels to plot
    for (i in 3:5) {
        plot_handle = plot_handle + annotate("text",
                                             x=max(indata[,"species.coalescent"])-50,
                                             y=max(indata[current_var]),
                                        # Add correlation coefficients to plot
                                             label=paste("R", "=", paste(corcofs, collapse="_"), "\n",
                                        # Add linear regression fit to plot
                                             "p-val:", paste(lrgm_fit, collapse="_"),
                                             sep=" "),
                                             size=3)
    }

    # Save the plot
    ggsave(filename=paste(out_dir, "SupplFile1_Scatterplot_", current_var, ".svg", sep=""),
           plot=plot_handle, width=4, height=8)
}

## EXECUTE SCRIPT
current_var = "coal"
current_label = "\nCoalescent likelihood (Liu & Yu 2010)"
my_function(handle_new, out_dir, current_var, current_label)

current_var = "lcwt"
current_label = "\nLikelihood of coalescent waiting times (Reid et al. 2014)"
my_function(handle_new, out_dir, current_var, current_label)



