
# Author: tim
###############################################################################

# procedure was developed here and moved to Functions.R

# two ways: either collapse before getting results or after
# make redundant collapsed rates I guess.
#me <- system("whoami",intern=TRUE)
#if (me == "mpidr_d\\riffe"){
#	setwd("U:/git/HLEDecomp/HLEDecomp")
#	read.path <- "N:\\dcs\\proj\\hledecomp\\results"
#}
#if (me == "tim"){
#	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
#	read.path <- "/home/tim/Data/hledecomp/results"
#}
#
#library(reshape2)
#library(data.table)
#source("Code/R/Functions.R")
#source("Code/R/Preamble.R")
#
## 2 state model
#
#version    <- "02"
#mspec      <- paste0("mspec", version)
#
#TR         <- get_TR(version=version)
#
#collapseTR   <- function(TR, PREV, version = "02"){
#	# stationary collapse of states 2 and 3 into state 2
#	
#	if (missing(TR)){
#		TR     <- get_TR(version = version)
#	}
#	TR         <- data.table(TR)
#	if (missing(PREV)){
#		PREV   <- TR[ , get_prev_dt(.SD), by = list(sex, edu, time)]
#	}
## rates and prev
#	RP         <- merge(TR, PREV)
## reorder RP
#	RP         <- setorder(RP, time, sex, edu, age)
## m11 <- m11
#	m11        <- RP$m11
## m14 <- m14 # keep 4 as dead
#	m14        <- RP$m14
## m12 <- m12 + m13
#	m12        <- RP$m12 + RP$m13
## m22 <- (m22 * pi2 + m33 * pi3) / (pi2 + pi3)
#	m21        <- (RP$m21 * RP$pi2 + RP$m31 * RP$pi3) / (RP$pi2 + RP$pi3)
#	m24        <- (RP$m24 * RP$pi2 + RP$m34 * RP$pi3) / (RP$pi2 + RP$pi3)
#	m22        <- 1 - m21 - m24
#	m22i       <- ( (RP$m22 + RP$m23) * RP$pi2 + (RP$m33 + RP$m32) * RP$pi3) / (RP$pi2 + RP$pi3)
#	
#	stopifnot(all(abs(m22 - m22i) < 1e-7)) # sanity check
#	
#	RP         <- data.table(RP)
#	s2_prop    <- RP$s2_prop + RP$s3_prop
#	RP         <- cbind(RP[, c("time", "sex", "edu", "age", "s1_prop")], 
#			            s2_prop, m11, m12, m14, m22, m21, m24)
#	RP
#}

# check if transition selection.
#TR.prev <- TR.i[,do_prev_chunk(.SD)]
# now conundrum, need a good way to attach the initprop info. When in chunks can save as metadata,
# but how to attach metadata to a .SD chunk?
#head(sex)
#Mi <- acast(RP[sex == "f", ], age~time~edu,value.var = "m24")
#M2 <- acast(TR[TR$sex == "f", ], age~time~edu,value.var = "m24")
#M3 <- acast(TR[TR$sex == "f", ], age~time~edu,value.var = "m34")
#
#
#matplot(as.integer(rownames(Mi)),Mi[,j,i],type='l',log='y',
#		lty=1,col=gray(seq(.7,0,length=3)),lwd=seq(3,1,length=3))
#matplot(as.integer(rownames(M2)),M2[,j,i],type='l',log='y',
#		lty=2,col=gray(seq(.7,0,length=3)),lwd=seq(3,1,length=3),add=TRUE)
#matplot(as.integer(rownames(M3)),M3[,j,i],type='l',log='y',
#		lty=3,col=gray(seq(.7,0,length=3)),lwd=seq(3,1,length=3),add=TRUE)

