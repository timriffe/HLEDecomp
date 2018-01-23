me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
}


getsub <- function(Mat){
	Mat[row(Mat) == (col(Mat) + 1)]
}

pi2u <- function(pivec){
	cbind(rbind(0,diag(pivec)),0)
}

fac2ch <- function(f){
	as.character(f)
}

# determine relevant column names, per DS's standard naming scheme
getcols <- function(ntrans = 3,self=TRUE){
	if (self){
		return(paste0("m",c(t(outer(1:ntrans,1:ntrans,paste0)))))
	} else {
		cols <- outer(1:ntrans,1:(ntrans+1),paste0)
		cols <- sort(cols[lower.tri(cols) | upper.tri(cols)])
		cols <- paste0("m",cols)
		return(cols)
	}
}

get_data <- function(path = "N:\\dcs\\proj\\hledecomp\\results\\margins", 
		version = "01",
		sex = "1.men",
		time = 2004,
		educlevel = "0.All edu",
        self = TRUE,
		matrix = TRUE){
	# this works on Tim's MPIDR PC, Windows machine...
	final_path    <- file.path(path, paste0("mspec", version), paste0("transp_m", version, ".dta"))
	Dat           <- foreign::read.dta(final_path)
	sprop         <- foreign::read.dta("N:\\dcs\\proj\\hledecomp\\results\\initprop\\initprop2.dta")
	# read.dta() has no stringsAsFactors argument...
	facs          <- sapply(Dat,class) == "factor"
    Dat[, facs]   <- lapply(Dat[,facs], fac2ch)
	facs          <- sapply(sprop,class) == "factor"
	sprop[, facs] <- lapply(sprop[,facs], fac2ch)
	
	# subset() or with() don't like it when your args have same names as cols, so
	# here's a verbose subset
	ind            <- Dat$sex == sex & Dat$educlevel == educlevel & Dat$time == time 
    DatS           <- Dat[ind, ]
	
	ind            <- sprop$sex == sex & sprop$educlevel == educlevel & sprop$propweighted == 1 & grepl(pattern = "age 50-54", sprop[,1])
	sprop          <- sprop[ind, c("s1_prop","s2_prop","s3_prop")]
	
	DatS           <- DatS[order(DatS$age), ]
	age            <- DatS$age
	cols           <- getcols(ntrans = 3, self = self)
	DatS           <- DatS[, cols]
	rownames(DatS) <- age
	
	sprop          <-  unlist(sprop)
	sprop          <- sprop / sum(sprop)
	attr(DatS, "initprop") <- sprop
    if (matrix){
		DatS <- DatS
	}
	DatS
}

out2self <- function(datout){
	colsself      <- getcols(ntrans = 3, self = TRUE)
		
	colsout       <- getcols(ntrans = 3, self = FALSE)
	dim(colsout)  <- c(3, 3)
    colnames(datout) <- colsout
	self1          <- matrix(1 - rowSums(datout[,colsout[,1]]), ncol = 1, dimnames = list(NULL, "m11"))
	self2          <- matrix(1 - rowSums(datout[,colsout[,2]]), ncol = 1, dimnames = list(NULL, "m22"))
	self3          <- matrix(1 - rowSums(datout[,colsout[,3]]), ncol = 1, dimnames = list(NULL, "m33"))
	
	out <- cbind(datout, self1, self2, self3)[, colsself]
	out
}

v2m <- function(vec, ntrans = 3){
	N <- length(vec)
	dim(vec) <- c(N / (ntrans^2), ntrans^2)
	vec
}

data_2_U <- function(dat){
	UL <- lapply(as.data.frame(dat),pi2u)
	U <- cbind(
					do.call("rbind",UL[c("m11","m12","m13")]),
					do.call("rbind",UL[c("m21","m22","m23")]),
					do.call("rbind",UL[c("m31","m32","m33")]))
	U
}


U2N <- function(U){
	I   <- diag(nrow(U))
	Nsx <- solve(I - U)
	Nsx 
}

e50 <- function(dat,to=1,age=50,prop=attr(dat,"initprop")){
	#prop <- attr(dat,"initprop")
	U    <- data_2_U(dat)
	N    <- U2N(U)
	rind <- 1:32 + (to-1) * 32
	cind <- rep(seq(50,112,by=2), 3) == age
	
	e50  <- colSums(N[rind,cind]) * 2
	# subtract half interval from self-state
	e50[to] <- e50[to] - 1 
	sum(e50 * prop)
}

e50dcs <- function(dat,age=50,prop=attr(dat,"initprop"),deduct=FALSE){
	#prop <- attr(dat,"initprop")
	U    <- data_2_U(dat)
	N    <- U2N(U)
	cind <- rep(seq(50,112,by=2), 3) == age
	N2   <- N[, cind]
	
	# get proportions in each 
	rgroups <- rep(1:3,each=32)
	Ntab <- matrix(0,3,3)
	for (i in 1:3){
		Ntab[,i] <- tapply(N2[,i], rgroups, sum)
	}
	Ntab  <- Ntab * 2
	
	# this is the dcs adjustment...
	if (deduct){
		Nprop <- t(t(Ntab) / colSums(Ntab))
		Ntab  <- Ntab - Nprop
	}

	sum(colSums(Ntab) * prop)
}

dec_fun <- function(datoutvec,to=1,age=52, prop){
	datout  <- v2m(datoutvec, 3)
	datself <- out2self(datout)
	e50(datself, to = to, age = age, prop = prop)
}

# preliminary results to cross-check w DCS
# AGREEMENT w DCS on 23-01-2018
#m1995 <- get_data(time = 1995)
#m2004 <- get_data(time = 2004)
#m2014 <- get_data(time = 2014)
#
#e50dcs(m1995,age=52) - 1
#e50dcs(m2004,age=52) - 1
#e50dcs(m2014,age=52) - 1
#
#e50dcs(m1995,age=52, deduct = TRUE) 
#e50dcs(m2004,age=52, deduct = TRUE) 
#e50dcs(m2014,age=52, deduct = TRUE) 
#
#e50(m1995,age=52, to = 1) + e50(m1995,age=52, to = 2) + e50(m1995,age=52, to = 3)
#e50(m2004,age=52) + e50(m2004,age=52, to = 2) + e50(m2004,age=52, to = 3)
#e50(m2014,age=52) + e50(m2014,age=52, to = 2) + e50(m2014,age=52, to = 3)

# state-specific expectancies will not agree until we decide where to deduct from.


HLEDecomp <- function(datout1, datout2, N = 10, prop = attr(datout1,"initprop"), to=1){
	
	datout1vec <- c(as.matrix(datout1))
	datout2vec <- c(as.matrix(datout2))
	# arrow decomposition:
	dec      <- DecompHoriuchi::DecompContinuousOrig(
			        func = dec_fun, 
			        rates1 = datout1vec, 
			        rates2 = datout2vec, 
			        N = N, 
			        prop = prop,
					to = to)
	dim(dec)      <- dim(datout1)
	dimnames(dec) <- dimnames(datout1)
	# no dim reduction here
	dec
}


m1995 <- get_data(time = 1995, self = FALSE)
m2004 <- get_data(time = 2004, self = FALSE)
m2014 <- get_data(time = 2014, self = FALSE)

library(devtools)


dec1.1 <- HLEDecomp(m1995, m2004, N = 20, to = 1)[-1, ]
dec1.2 <- HLEDecomp(m1995, m2004, N = 20, to = 2)[-1, ]
dec1.3 <- HLEDecomp(m1995, m2004, N = 20, to = 3)[-1, ]

a <- seq(52,110,by=2)

blend <- function(col1, col2){
	rgb1   <- col2rgb(col1)
	rgb2   <- col2rgb(col2)
	rgbnew <- sqrt((rgb1^2+rgb2^2)/2)
	rgb2hex(c(rgbnew))
}

RColorBrewer::display.brewer.all()
cols1 <- RColorBrewer::brewer.pal(9,"YlOrBr")[5:7]
cols2 <- RColorBrewer::brewer.pal(11,"RdYlBu")[c(9,3,1)]
cols3 <- RColorBrewer::brewer.pal(11,"RdYlGn")[c(10,9,1)]
cols1[3] <- blend(cols1[3],"#6E3A07")
cols2[3] <- blend(cols2[3],"#6E3A07")
cols3[3] <- blend(cols3[3],"#6E3A07")

dec1.tot <- dec1.1 + dec1.2 + dec1.3


cols <- c(cols1,cols2,cols3)

png("Figures/dec1hle.png")
matplot(a,dec1.1, type = 'l', xlab = "age", xlim = c(52,100), col = cols, lty = 1, lwd = 2, 
		ylab = "contribution to difference in HLE", main = "males all edu HLE 1995 vs 2004 (+1.47 yrs)")
text(60,dec1.1["60", ], colnames(dec1), font = 2, col = cols, pos = c(3,1,1,2,1,3,3,3,1))
dev.off()

png("Figures/dec1adl1.png")
matplot(a,dec1.2, type = 'l', xlab = "age", xlim = c(52,100), col = cols, lty = 1, lwd = 2, 
		ylab = "contribution to difference in ADL1 LE", main = "males all edu ADL1 LE 1995 vs 2004 (trivial increase)")
text(60,dec1.2["60", ], colnames(dec1), font = 2, col = cols)
dev.off()

png("Figures/dec1adl2.png")
matplot(a,dec1.3, type = 'l', xlab = "age", xlim = c(52,100), col = cols, lty = 1, lwd = 2, 
		ylab = "contribution to difference in ADL2+ LE", main = "males all edu ADL2+ LE 1995 vs 2004 (-1 months)")
text(60,dec1.3["60", ], colnames(dec1), font = 2, col = cols)
dev.off()

png("Figures/dec1le.png")
matplot(a,dec1.tot, type = 'l', xlab = "age", xlim = c(52,100), col = cols, lty = 1, lwd = 2, 
		ylab = "contribution to difference in LE", main = "males all edu LE 1995 vs 2004 (+1.36 yrs)")
text(60,dec1.tot["60", ], colnames(dec1), font = 2, col = cols)
dev.off()

matplot(m2004-m2014,type='l')
dec2.1 <- HLEDecomp(m2004, m2014, N = 20, to = 1)[-1, ]
dec2.2 <- HLEDecomp(m2004, m2014, N = 20, to = 2)[-1, ]
dec2.3 <- HLEDecomp(m2004, m2014, N = 20, to = 3)[-1, ]

dec2.tot <- dec2.1 + dec2.2 + dec2.3

dec3.1 <- HLEDecomp(m2014, m1995, N = 10, to = 1)[-1, ]
dec3.2 <- HLEDecomp(m1995, m2014, N = 10, to = 2)[-1, ]
dec3.3 <- HLEDecomp(m1995, m2014, N = 10, to = 3)[-1, ]

dec3.tot <- dec3.1 + dec3.2 + dec3.3

matplot(m2014, type ='l',col=cols)

matplot(a,dec3.1, type = 'l', xlab = "age", xlim = c(52,100), col = cols, lty = 1, lwd = 2, 
		ylab = "contribution to difference in LE", main = "males all edu LE 1995 vs 2014")

matplot(a,dec3.tot, type = 'l', xlab = "age", xlim = c(52,100), col = cols, lty = 1, lwd = 2, 
		ylab = "contribution to difference in LE", main = "males all edu LE 1995 vs 2014")
text(60,dec2.tot["60", ], colnames(dec1), font = 2, col = cols)


matplot(a,dec2.1, type = 'l', xlab = "age", xlim = c(52,100), col = cols, lty = 1, lwd = 2, 
		ylab = "contribution to difference in HLE", main = "males all edu HLE 2004 vs 2014")
text(60,dec2.1["60", ], colnames(dec1), font = 2, col = cols)
sum(dec2.tot)
png("Figures/dec2le.png")
matplot(a,dec2.tot, type = 'l', xlab = "age", xlim = c(52,100), col = cols, lty = 1, lwd = 2, 
		ylab = "contribution to difference in LE", main = "males all edu LE 2004 vs 2014 (+1.42 yrs)")
text(60,dec2.tot["60", ], colnames(dec1), font = 2, col = cols)
dev.off()

#matplot(out2self(m2004)-out2self(m2014),type='l')

#
#m1995 <- get_data(time = 1995)
#m2004 <- get_data(time = 2004)
#m2014 <- get_data(time = 2014)
#
## hle
#hle95 <- e50(m1995, to = 1, age = 52)
#hle04 <- e50(m2004, to = 1, age = 52)
#hle14 <- e50(m2014, to = 1, age = 52)
#
## adl1
#a195 <- e50(m1995, to = 2, age = 52)
#a104 <- e50(m2004, to = 2, age = 52)
#a114 <- e50(m2014, to = 2, age = 52)
#
## adl2
#a295 <- e50(m1995, to = 3, age = 52)
#a204 <- e50(m2004, to = 3, age = 52)
#a214 <- e50(m2014, to = 3, age = 52)
#
## tot
#tot95 <- hle95 + a195 + a295
#tot04 <- hle04 + a104 + a204
#tot14 <- hle14 + a114 + a214
#
#print(data.frame(hle = c(hle95,hle04,hle14),
#		adl1 = c(a195,a104,a114),
#		adl2p = c(a295,a204,a214),
#		tot = c(tot95,tot04,tot14)),digits=3)
