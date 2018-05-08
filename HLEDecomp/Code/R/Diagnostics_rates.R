
# diagnostics on rates: mspecs 2 and 3 for now. 
# 2 is age + age^2
# 3 is splines, afraid might be out of control

# prior: prevalence-weighted avg of death rates in 3 should abide by
# mort laws, but state-specific ones are not bound by mort laws
# ---------------------------------------------------


# mortality:
mortrates <- c("m14","m24","m34")

me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
	read.path <- "N:\\dcs\\proj\\hledecomp\\results"
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
	read.path <- "/home/tim/Data/hledecomp/results"
}

library(reshape2)
source("Code/R/Functions.R")
















