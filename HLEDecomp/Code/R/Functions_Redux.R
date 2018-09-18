setwd("/home/tim/git/HLEDecomp/HLEDecomp")
source("Code/R/Functions.R")
getwd()
# Author: tim
###############################################################################
# for a single year-sex-edu
wmean <- function(x,w){
	sum(x*w) / sum(w)
}

# stack trans,frac,trans,frac,trans,frac

dec_fun_redux <- function(datoutvec, to, age = 50, n=31, ntrans = 3, deduct = TRUE){
	
	# first get into 3 column matrix:
	dim(datoutvec) <- c(ntrans^2 * n+ntrans,3)
	
	# now ciphen off fractions
	fracs     <- datoutvec[(ntrans^2 * n + 1):(ntrans^2 * n + ntrans),]
	datoutvec <- datoutvec[1:(ntrans^2 * n), ]
	# requires input as vector, first reshape to matrix or df
	dc        <- rep(0,3)
	for (i in 1:3){
		datouti    <- v2m(datoutvec[,i], ntrans = ntrans)
		# remove death rates and replace with self-arrows, needed to make
		# transition matrices
		datselfi   <- out2self(datouti, ntrans = ntrans)
		# append fractions: normalize so group expectancies look right, then reweight
        propi      <- fracs[, i] / sum(fracs[,i ])
		# then compute e50 using the correct transition rates, and relevant fractions
		dc[i]      <- e50(datselfi, prop = propi, to = to, age = age, ntrans = ntrans, deduct = deduct)
	}
	# radix-weighted mean
	wmean(dc,colSums(fracs))
}
library(data.table)
#TR.2    <- local(get(load("/home/tim/git/HLEDecomp/HLEDecomp/Data/Transitions/DCS/initdetail/TR_v06.Rdata")))
#TR2     <- data.table(TR.2)
#LE2     <- TR2[ , e50_dt(.SD), by = list(sex, edu, time)]
#LE2$LE  <- rowSums(LE2[,4:6])
#
#TR.1    <- local(get(load("/home/tim/git/HLEDecomp/HLEDecomp/Data/Transitions/DCS/TR_v06.Rdata")))
#TR.1    <- TR.1[TR.1$edu != "all_edu",]
#TR1     <- data.table(TR.1)
#LE1     <- TR1[ , e50_dt(.SD), by = list(sex, edu, time)]
#LE1$LE  <- rowSums(LE1[,4:6]) # identical
#PREV    <- TR1[ , get_prev_dt(.SD), by = list(sex, edu, time)]
## PREVs all identical after one time step
#
#LE1 <- data.frame(LE1)
#LE2 <- data.frame(LE2)
#
#
#
#head(LE1)
#write.csv(LE1,file="/home/tim/Desktop/LEspec16.csv")
#write.csv(LE2,file="/home/tim/Desktop/LEspec18.csv")


TR    <- local(get(load("/home/tim/git/HLEDecomp/HLEDecomp/Data/Transitions/DCS/initdetail/TR_v06.Rdata")))

eduvec <- function(TR,.sex="m",.time=1996){
	outcols <- getcols(3,self=FALSE)
	
	pri <- subset(TR,sex == .sex & time == .time & edu == "primary")
	sec <- subset(TR,sex == .sex & time == .time & edu == "secondary")
	uni <- subset(TR,sex == .sex & time == .time & edu == "terciary")
			
	priout <- as.matrix(pri[,outcols])
	secout <- as.matrix(sec[,outcols])
	uniout <- as.matrix(uni[,outcols])
	
	priv <- c(priout)
	secv <- c(secout)
	univ <- c(uniout)
	
	pcols <- c("s1_prop","s2_prop","s3_prop")
	prifrac <- unlist(pri[1,pcols])
	secfrac <- unlist(sec[1,pcols])
	unifrac <- unlist(uni[1,pcols])
	
	c(priv, prifrac, secv, secfrac, univ, unifrac)
	
}


mhrs <-  c(dec_fun_redux(eduvec(TR,"m",1996),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE),
dec_fun_redux(eduvec(TR,"m",2006),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE),
dec_fun_redux(eduvec(TR,"m",2014),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE))

fhrs <- c(dec_fun_redux(eduvec(TR,"f",1996),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE),
dec_fun_redux(eduvec(TR,"f",2006),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE),
dec_fun_redux(eduvec(TR,"f",2014),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE))


# data for Fig 5
# get fig 4 

library(DemoDecomp)

# expected to be slow
deci <- horiuchi(dec_fun_redux, eduvec(TR,"f",2006),eduvec(TR,"f",2014),5)
sum(deci); fhrs[3]-fhrs[2]

dim(deci) <- c(length(deci)/3,3)

fracs <- deci[(nrow(deci)-2):nrow(deci), ]
sum(fracs)
rowSums(fracs)
DL <- lapply(as.data.frame(deci[1:(nrow(deci)-3), ]),v2m)
getcols(3,self=FALSE)
colSums(DL[[1]]) + colSums(DL[[2]]) + colSums(DL[[3]])




HLEDecomp_redux <- function(datout1, datout2, N = 10, ntrans = 3, prop, to, deduct = TRUE){
	
	# TR: change this to reflect due data dimensions
	if (missing(prop)){
		# this is OK, but would need separate by edu,
		# also separate from datout2 and 1
		pnames <- paste0("s",1:ntrans,"_prop")
		prop   <- unlist(datout1[1, pnames])
	}
	prop   <- prop / sum(prop)
	# just to be sure we're rodered correctly.
	datout1            <- datout1[order(datout1$age), ]
	datout2            <- datout2[order(datout2$age), ]
	
	age                <- datout1$age
	rownames(datout1)  <- age
	rownames(datout2)  <- age
	cols               <- getcols(ntrans = ntrans, self = FALSE)
	datout1            <- datout1[, cols]
	datout2            <- datout2[, cols]
	
	datout1vec         <- c(as.matrix(datout1))
	datout2vec         <- c(as.matrix(datout2))
	# arrow decomposition:
	dec      <- DecompHoriuchi::DecompContinuousOrig(
			func = dec_fun, 
			rates1 = datout1vec, 
			rates2 = datout2vec, 
			N = N, 
			prop = prop,
			ntrans = ntrans,
			to = to,
			deduct = deduct)
	dim(dec)      <- dim(datout1)
	dimnames(dec) <- dimnames(datout1)
	# no dim reduction here
	dec
}


le <- readRDS("/home/tim/git/HLEDecomp/HLEDecomp/Data/Results/mspec06/le/le_2.rds")
le$LE <- rowSums(le[,c("1","2")])
le[sex=="m"]
#rowSums(datselfi)