
# Author: tim
###############################################################################
# for a single year-sex-edu
wmean <- function(x,w){
	sum(x*w) / sum(w)
}

# stack trans,frac,trans,frac,trans,frac

dec_fun_redux <- function(datoutvec, to, age = 50, n=31, ntrans = 3, deduct = TRUE){
	
	# first get into 3 column matrix:
	dim(datoutvec) <- c(n+ntrans,3)
	
	# now ciphen off fractions
	fracs     <- datoutvec[(n + 1):(n + ntrans),]
	datoutvec <- datoutvec[1:n, ]
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

TR.i <- local(get(load("/home/tim/git/HLEDecomp/HLEDecomp/Data/Transitions/DCS/initdetail/TR_v06.Rdata")))
head(TR.i)


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


