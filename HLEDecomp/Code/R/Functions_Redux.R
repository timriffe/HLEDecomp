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

TR.2    <- local(get(load("/home/tim/git/HLEDecomp/HLEDecomp/Data/Transitions/DCS/initdetail/TR_v06.Rdata")))
TR2     <- data.table(TR.2)
LE2     <- TR2[ , e50_dt(.SD), by = list(sex, edu, time)]
LE2$LE  <- rowSums(LE2[,4:6])

TR.1    <- local(get(load("/home/tim/git/HLEDecomp/HLEDecomp/Data/Transitions/DCS/TR_v06.Rdata")))
TR.1    <- TR.1[TR.1$edu != "all_edu",]
TR1     <- data.table(TR.1)
LE1     <- TR1[ , e50_dt(.SD), by = list(sex, edu, time)]
LE1$LE  <- rowSums(LE1[,4:6]) # identical
PREV    <- TR1[ , get_prev_dt(.SD), by = list(sex, edu, time)]
# PREVs all identical after one time step


library(data.table)
TR <- data.table(TR.i)
LE                <- TR[ , e50_dt(.SD), by = list(sex, edu, time)]

LE$LE <- rowSums(LE[,4:6])
LE
#matplot(seq(50,110,by=2),datselfi,type='l')
# example of stacked vector:
unique(TR.i$sex)
outcols <- getcols(3,self=FALSE)

priout <- as.matrix(subset(TR.i,sex == "m" & time == 1996 & edu == "primary")[,outcols])
secout <- as.matrix(subset(TR.i,sex == "m" & time == 1996 & edu == "secondary")[,outcols])
uniout <- as.matrix(subset(TR.i,sex == "m" & time == 1996 & edu == "terciary")[,outcols])
# vec operator
pri <- c(priout)
sec <- c(secout)
uni <- c(uniout)

prifrac <- unlist(TR.i[TR.i$sex == "m" & TR.i$time == 1996 & TR.i$edu == "primary" & TR.i$age == 50,
				c("s1_prop","s2_prop","s3_prop")])
secfrac <- c(sec,unlist(TR.i[TR.i$sex == "m" & TR.i$time == 1996 & TR.i$edu == "secondary" & TR.i$age == 50,
				c("s1_prop","s2_prop","s3_prop")]))
unifrac <- c(uni,unlist(TR.i[TR.i$sex == "m" & TR.i$time == 1996 & TR.i$edu == "terciary" & TR.i$age == 50,
				c("s1_prop","s2_prop","s3_prop")]))

DAT <- as.matrix(subset(TR.i,sex == "m" & time == 1996 & edu == "primary")[,getcols(3,self=TRUE)])
TR <- get_TR(version = "06")
TR <- data.frame(TR)
fracsold <- TR[TR$age == 50 & TR$sex == "m" & TR$edu == "primary" & TR$time == 1996,c("s1_prop","s2_prop","s3_prop")]
fracsold /sum(fracsold)

e50(DAT,to, age = 50, prop = c(.9091997,0.0465915, 0.04420884), ntrans = 3, deduct = TRUE, interval = 2)
datout <- as.matrix(subset(TR,sex == "m" & time == 1996 & edu == "primary")[,getcols(3,self=FALSE)])

e50(out2self(priout,3), prop =  prifrac/sum(prifrac),to=5,age=50)
e50(out2self(priout,3), prop =  fracsold /sum(fracsold),to=5,age=50)

e50(out2self(priout,3), prop =  c(1,0,0),to=5,age=50)
e50(out2self(priout,3), prop =  c(0,1,0),to=5,age=50)
e50(out2self(priout,3), prop =  c(0,0,1),to=5,age=50)













datself <- out2self(priout,3)
datoutvec <- c(pri,sec,uni)
e50(datselfi, prop = propi, to = 3, age = age, ntrans = ntrans, deduct = deduct)
e50_dt(subset(TR.i,sex == "m" & time == 1996 & edu == "primary"), 
		age = 50, ntrans = 3, prop, deduct = TRUE)
prop <- unlist(TR.i[TR.i$sex == "m" & TR.i$time == 1996 & TR.i$edu == "primary" & TR.i$age == 50,
				c("s1_prop","s2_prop","s3_prop")])
getcols(3,self=FALSE)


HLEDecomp <- function(datout1, datout2, N = 10, ntrans = 3, prop, to, deduct = TRUE){
	
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