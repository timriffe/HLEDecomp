
# just to make a transition matrix for Transient Symmetry

me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
}
source("Code/R/Functions.R")


dat <- get_data()
str(dat)
U <- data_2_U(dat)

citation("DemoDecomp")
