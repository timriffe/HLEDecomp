
# for the most recent transition probabilities, which transition probability
# has the biggest leverage?

me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
}
source("Code/R/Functions.R")
library(popbio)

# work with mspec 06
# how much LE could come from everyone being healthy?
# how much LE could come from everyone being high edu?
# how much LE could come from everyone reaching age 50 in good health?








#m2014      <- get_data(time = 2014)
#U2014      <- data_2_U(m2014)
#U2004      <- data_2_U(get_data(time = 2004))
#
#U2014[rowSums(U2014) == 0, ] <- .005
#sum(sensitivity(U2014))
#
#LTRE(U2014,U2004,TRUE)

