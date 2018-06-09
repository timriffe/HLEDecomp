# Script to get episode duration distributions
# Author: tim
###############################################################################
#

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
# -1 is subdiag, 1 is superdiag
sdiag <- function(x,X,offset=0){
	n      <- length(x)
	d      <- n + abs(offset)
	if (missing(X)){
		X  <- matrix(0,nrow = d, ncol = d)
	}
	# needs to be appropriate size.
	# i.e. intentionally resize x as appropriate
	stopifnot(all(dim(X) == d))
	
	#
	Rows   <- row(X)
	Cols   <- col(X)
	
	ind    <- (Rows - Cols) == -offset
	X[ind] <- x
	X
}


# construct episode matrix, for a single direction (m12 or m21 then stay)
mii <- c(.1,.08,.06,.04)

# just feed this self vectors
conditional_episodes <- function(mii){
	n <- length(mii)
	ce <- matrix(0,n,n)
	for (i in 1:n){
		ce[1:(n+1-i),i] <- cumprod(mii[i:n])
	}
	ce
}
# try matrix algebra: but matrix not organized so friendly...
# want easy row or col summing.
#conditional_episodes2 <- function(mii){
#	U  <- sdiag(mii,offset = -1)
#	ce <- U2N(U, interval = 1)
#	ce
#}

stationary_episodes <- function(
		mii, # state i selfs
		mji, # probability of moving j to i
		pj,  # statoonary structure of j
		scale = FALSE){
	ce <- conditional_episodes(mii)
	
	epi <- rowSums(t(ce) * mji * pi)
	if (scale){
		epi <- epi / sum(epi)
	}
	epi
}

TR <- readRDS(file.path("Data", "Transitions", "Collapsed", paste0("TR_v", "06", "_collapsed.rds")))

chunk <- subset(TR, time == 1996 & sex == "f" & edu == "all_edu")
mii   <- chunk$m11
mji   <- chunk$m21
PR    <- get_prev_dt(chunk, ntrans = 2)
pj    <- PR$pi2
# still an alignment issue:
# want probability of being in state j at start of interval times moving to j
# for 1, then cumprod of staying for 1 and higher. Figure out in next sitting.
stationary_episodes(mii,mji,pj)


