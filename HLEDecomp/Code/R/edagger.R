
# Author: tim
###############################################################################

# who wants to decompose a change in edagger due to transition probabilities,
# potentially reduced to 'edagger of the healthy and sick'

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
library(data.table)
source("Code/R/Functions.R")
source("Code/R/Preamble.R")

version    <- "06"
#mspec      <- paste0("mspec", version)
#path       <- file.path("Data", "Results", mspec, "dec")
#if (!dir.exists(path)){
#	dir.create(path, recursive = TRUE)
#	dir.create(file.path("Data", "Results", mspec, "prev"), recursive = TRUE)
#	dir.create(file.path("Data", "Results", mspec, "le"), recursive = TRUE)
#}

TR2 <- readRDS(file.path("Data", "Transitions", "Collapsed", paste0("TR_v", version, "_collapsed.rds")))
# what is prev again?
PR2 <- TR2[ , get_prev_dt(.SD, ntrans = 2), by = list(sex, edu, time)]
DAT <- TR2[sex == "f" & time == 1996 & edu == "all_edu"]

string <- c("m14","m13")
# detect death nr:
detect_dead_int <- function(string){
	ids <- nchar(string) == 3
	if (any(ids)){
		dead <- max(
				suppressWarnings(as.integer(unlist(lapply(strsplit(string[ids],split=""),"[[",3)))),
				na.rm=TRUE)
		return(dead)
	}
	NULL
}

get_prev2_dt <- function(DAT, to, prop, deduct = TRUE, ntrans = 3){
	DAT <- as.data.frame(DAT)
	
	deathcols <- paste0("m",1:ntrans,detect_dead_int(colnames(DAT)))
	DeathCols <- DAT[,deathcols]
	if (missing(prop)){
		pnames <- paste0("s", 1:ntrans, "_prop")
		prop   <- unlist(DAT[1, pnames])
	}
	age       <- DAT$age
	cols      <- getcols(ntrans, self = TRUE)
	
	DAT       <- DAT[, cols]
	U         <- data_2_U(DAT, ntrans = ntrans)
	N         <- U2N(U, interval = 2)
	cind      <- rep(seq(50,112,by=2), ntrans) == 50
	prev      <- N[,cind] %*% prop
	
	dim(prev) <- c(32,ntrans)
	prev      <- prev / 2
	colnames(prev) <- paste0("pi",1:ntrans)
	# verify with DCS re age groups. Why exclude age 50?
	prev      <- prev[-nrow(prev), ]
	
	DF        <- as.data.frame(prev)
	DF$age    <- age
	
	# potentially fragile
	DF <- cbind(DF,DeathCols )
	DF
}
PR2 <- TR2[ , get_prev2_dt(.SD, ntrans = 2), by = list(sex, edu, time)]

plot(rowSums(PR2[,c("pi1","pi2")]))