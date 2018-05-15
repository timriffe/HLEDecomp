
# Author: tim
###############################################################################

version <- "02"

me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
}
source("Code/R/Functions.R")
source("Code/R/Preamble.R")

version      <- "02"
le.path      <- file.path("Data","Results",paste0("mspec",version),"le")
read.these   <- dir(le.path)
x <- read.these[1]
leAll <- lapply(read.these,function(x,le.path){
			X <- local(get(load(file.path(le.path,x))))
			X[,4]<- c(1996,2006,2014)
			X
		},le.path=le.path)

names(leAll) <- gsub("_20.Rdata","",read.these)

x <- names(leAll)[1]

X <- leAll[[1]]
pdf(file.path("Figures","le","barplotsAll.pdf"))
lapply(names(leAll), function(x, leAll){
			X <- leAll[[x]]
			barplot(t(X[,1:3]),main=x,ylim=c(0,17))
		},leAll=leAll)
dev.off()




