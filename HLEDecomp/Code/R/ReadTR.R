
# Author: tim
###############################################################################

# a script to import all available transition rates to R binary format,
# also modifying format in some useful ways (state1, state2, for example)
me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
	read.path <- "N:\\dcs\\proj\\hledecomp\\results"
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
	read.path <- "/home/tim/Data/hledecomp/results"
}
source("Code/R/Functions.R")
source("Code/R/Preamble.R")
#
names(edus) <- edusl
sexes
for (i in length(versions)){
	TR.i <- get_rates_all(
			path = read.path, 
			version = versions[i],
			self = TRUE)
	TR.i$edu <- edus[TR.i$educlevel]
}

