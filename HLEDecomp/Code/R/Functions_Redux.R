setwd("/home/tim/git/HLEDecomp/HLEDecomp")
source("Code/R/Functions.R")
library(data.table)
library(DemoDecomp)
getwd()
# Author: tim
###############################################################################
# for a single year-sex-edu
wmean <- function(x,w){
	sum(x*w) / sum(w)
}

# stack trans,frac,trans,frac,trans,frac (where frac is 4 pieces)
eduvec <- function(TR,.sex="m",.time=1996,ntrans=3){
	outcols <- getcols(ntrans,self=FALSE)
	TR  <- as.data.frame(TR, stringsAsFactors = FALSE)
	pri <- subset(TR,sex == .sex & time == .time & edu == "primary")
	sec <- subset(TR,sex == .sex & time == .time & edu == "secondary")
	uni <- subset(TR,sex == .sex & time == .time & edu == "terciary")
	
	priout <- as.matrix(pri[,outcols])
	secout <- as.matrix(sec[,outcols])
	uniout <- as.matrix(uni[,outcols])
	
	priv <- c(priout)
	secv <- c(secout)
	univ <- c(uniout)
	
	pcols <- paste0("s", 1:ntrans, "_prop")
	
	prifrac <- unlist(pri[1,pcols])
	secfrac <- unlist(sec[1,pcols])
	unifrac <- unlist(uni[1,pcols])
	
	fracs    <- cbind(prifrac, secfrac, unifrac)
	edufracs <- colSums(fracs)
	pifracs  <- rbind(t(t(fracs) / edufracs),edufracs)
	
	# now separate pi and edu fracs
	c(priv, pifracs[,1], secv, pifracs[,2], univ, pifracs[,3])
	
}

dec_fun_redux <- function(datoutvec, to, age = 50, n=31, ntrans = 3, deduct = TRUE){
	
	# first get into 3 edu column matrix:
	dim(datoutvec) <- c(ntrans^2 * n+ntrans+1,3)

	# now ciphen off fractions
	fracs     <- tail(datoutvec, ntrans+1)
	datoutvec <- datoutvec[1:(ntrans^2 * n), ]
	# requires input as vector, first reshape to matrix or df
	dc        <- rep(0,3)
	for (i in 1:3){
		datouti    <- v2m(datoutvec[,i], ntrans = ntrans)
		# remove death rates and replace with self-arrows, needed to make
		# transition matrices
		datselfi   <- out2self(datouti, ntrans = ntrans)
		# append fractions: normalize so group expectancies look right, then reweight
        propi      <- fracs[1:ntrans, i] 
		# then compute e50 using the correct transition rates, and relevant fractions
		dc[i]      <- e50(datselfi, prop = propi, to = to, age = age, ntrans = ntrans, deduct = deduct)
	}
	# radix-weighted mean
	wmean(dc,fracs[ntrans+1,])
}

# this changes the way fractions are decomposed.
# stack trans,frac,trans,frac,trans,frac (where frac is 4 pieces)
eduvec_leaveout <- function(TR,.sex="m",.time=1996,ntrans=3){
	outcols <- getcols(ntrans,self=FALSE)
	TR  <- as.data.frame(TR, stringsAsFactors = FALSE)
	pri <- subset(TR,sex == .sex & time == .time & edu == "primary")
	sec <- subset(TR,sex == .sex & time == .time & edu == "secondary")
	uni <- subset(TR,sex == .sex & time == .time & edu == "terciary")
	
	priout <- as.matrix(pri[,outcols])
	secout <- as.matrix(sec[,outcols])
	uniout <- as.matrix(uni[,outcols])
	
	priv <- c(priout)
	secv <- c(secout)
	univ <- c(uniout)
	
	pcols <- paste0("s", 1:ntrans, "_prop")
	
	prifrac  <- unlist(pri[1,pcols])
	secfrac  <- unlist(sec[1,pcols])
	unifrac  <- unlist(uni[1,pcols])
	
	fracs    <- cbind(prifrac, secfrac, unifrac)
	edufracs <- colSums(fracs)
	
	pifracs <- t(t(fracs) / edufracs)
	#pifracs[-1, ]
	
	#edufracs
	# leave university out
	# now separate pi and edu fracs
	c(priv, secv, univ, c(pifracs[-1, ]), edufracs[-1])
}

anti_leaveout <- function(vec,ntrans=3,n=31){
	enames       <- c("primary","secondary","terciary")
	rvec         <- rev(vec)
	edufracs     <- rev(rvec[1:2])
	edufracs     <- c( 1 - sum(edufracs), edufracs)
	names(edufracs) <- enames
	rvec         <- rvec[-c(1:2)]
	
	pifracs      <- rev(rvec[1:((ntrans-1)*3)])
	dim(pifracs) <- c(ntrans-1,3)
	pifracs      <- rbind(1-colSums(pifracs),pifracs)
	
	rownames(pifracs) <- paste0("s",1:ntrans,"_prop")
	colnames(pifracs) <- enames
	
	rvec         <- rvec[-c(1:((ntrans-1)*3))]
	vec          <- rev(rvec)
	dim(vec)     <- c(n*ntrans^2,3)
	colnames(vec) <- enames
	list(outmat = vec, pifracs = pifracs, edufracs = edufracs)
}

dec_fun_redux_leaveout <- function(datoutvec, to, age = 50, n=31, ntrans = 3, deduct = TRUE){
	
	parts <- anti_leaveout(datoutvec, ntrans = ntrans, n = n)
	
	# requires input as vector, first reshape to matrix or df
	dc        <- rep(0,3)
	
	# this is an education loop
	for (i in 1:3){
		datouti    <- v2m(parts$outmat[,i], ntrans = ntrans)
		# remove death rates and replace with self-arrows, needed to make
		# transition matrices
		datselfi   <- out2self(datouti, ntrans = ntrans)
		# append fractions: normalize so group expectancies look right, then reweight
		propi      <- parts$pifracs[,i] 
		# then compute e50 using the correct transition rates, and relevant fractions
		dc[i]      <- e50(datselfi, prop = propi, to = to, age = age, ntrans = ntrans, deduct = deduct)
	}
	# radix-weighted mean
	wmean(dc,parts$edufracs)
}

anti_leaveout_decomp <- function(vec,ntrans=3,n=31){
	enames       <- c("primary","secondary","terciary")
	rvec         <- rev(vec)
	edufracs     <- rev(rvec[1:2])
	
	rvec         <- rvec[-c(1:2)]
	
	pifracs      <- rev(rvec[1:((ntrans-1)*3)])
		
	rvec         <- rvec[-c(1:((ntrans-1)*3))]
	vec          <- rev(rvec)
	dim(vec)     <- c(n*ntrans^2,3)
	colnames(vec) <- enames
	list(outmat = vec, picomp = sum(pifracs), educomp = sum(edufracs))
}

datoutvec <- eduvec_leaveout(TR2,.sex="m",.time=1996,ntrans=2)
dec_fun_redux_leaveout(datoutvec, ntrans=2)


deci <- horiuchi(dec_fun_redux_leaveout, 
		eduvec_leaveout(TR2,"f",2006,ntrans=2),
		eduvec_leaveout(TR2,"f",2014,ntrans=2),
		N = 5,ntrans=2)
vec <- deci
deci <- anti_leaveout_decomp(deci,ntrans=2)

colSums(v2m(rowSums(deci$outmat),ntrans=2))

decomp_redux_leaveout_wrapper <- function(
		TR,
		time1=2006,
		time2=2014,
		sex="f",
		ntrans=2,
		to=5,
		deduct = TRUE,
		n=31,
		N=20){
	outvec1 <- eduvec_leaveout(TR=TR,.sex=sex,.time=time1,ntrans=ntrans)
	outvec2 <- eduvec_leaveout(TR=TR,.sex=sex,.time=time2,ntrans=ntrans)
	deci <- horiuchi(dec_fun_redux_leaveout, 
			outvec1,
			outvec2,
			N = N,
			ntrans=ntrans,
			deduct=deduct,
			to=to)
	
	deci <- anti_leaveout_decomp(deci,ntrans=2)
	
	out <- c(colSums(v2m(rowSums(deci$outmat),ntrans=ntrans)),deci$picomp,deci$educomp)
	names(out) <- c(getcols(ntrans=ntrans,self=FALSE),"Health 50","Education 50")
	out
}

# table 1a (males 1996-2006)

Tab1a <- cbind(
		decomp_redux_leaveout_wrapper(TR2,time1=1996,time2=2006,sex="m",ntrans=2,to=1),
		decomp_redux_leaveout_wrapper(TR2,time1=1996,time2=2006,sex="m",ntrans=2,to=2),
		decomp_redux_leaveout_wrapper(TR2,time1=1996,time2=2006,sex="m",ntrans=2,to=5))

Tab1b <- cbind(
		decomp_redux_leaveout_wrapper(TR2,time1=2006,time2=2014,sex="m",ntrans=2,to=1),
		decomp_redux_leaveout_wrapper(TR2,time1=2006,time2=2014,sex="m",ntrans=2,to=2),
		decomp_redux_leaveout_wrapper(TR2,time1=2006,time2=2014,sex="m",ntrans=2,to=5))

Tab2a <- cbind(
		decomp_redux_leaveout_wrapper(TR2,time1=1996,time2=2006,sex="f",ntrans=2,to=1),
		decomp_redux_leaveout_wrapper(TR2,time1=1996,time2=2006,sex="f",ntrans=2,to=2),
		decomp_redux_leaveout_wrapper(TR2,time1=1996,time2=2006,sex="f",ntrans=2,to=5))
Tab2b <- cbind(
		decomp_redux_leaveout_wrapper(TR2,time1=2006,time2=2014,sex="f",ntrans=2,to=1),
		decomp_redux_leaveout_wrapper(TR2,time1=2006,time2=2014,sex="f",ntrans=2,to=2),
		decomp_redux_leaveout_wrapper(TR2,time1=2006,time2=2014,sex="f",ntrans=2,to=5))






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
TR    <- data.table(TR)
PREV  <- TR[ , get_prev_dt(.SD), by = list(sex, edu, time)]
TR2   <- collapseTR(TR = TR, PREV = PREV)

mhrs <-  c(dec_fun_redux(eduvec(TR,"m",1996),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE),
		dec_fun_redux(eduvec(TR,"m",2006),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE),
		dec_fun_redux(eduvec(TR,"m",2014),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE))
fhrs <- c(dec_fun_redux(eduvec(TR,"f",1996),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE),
		dec_fun_redux(eduvec(TR,"f",2006),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE),
		dec_fun_redux(eduvec(TR,"f",2014),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE))

# for Fig 4
mhrs2 <-  c(dec_fun_redux(eduvec(TR=TR2,"m",1996,ntrans=2),to=5, age = 50, n=31, ntrans = 2, deduct = TRUE),
		dec_fun_redux(eduvec(TR=TR2,"m",2006,ntrans=2),to=5, age = 50, n=31, ntrans = 2, deduct = TRUE),
		dec_fun_redux(eduvec(TR=TR2,"m",2014,ntrans=2),to=5, age = 50, n=31, ntrans = 2, deduct = TRUE))
fhrs2 <- c(dec_fun_redux(eduvec(TR=TR2,"f",1996,ntrans=2),to=5, age = 50, n=31, ntrans = 2, deduct = TRUE),
		dec_fun_redux(eduvec(TR=TR2,"f",2006,ntrans=2),to=5, age = 50, n=31, ntrans = 2, deduct = TRUE),
		dec_fun_redux(eduvec(TR=TR2,"f",2014,ntrans=2),to=5, age = 50, n=31, ntrans = 2, deduct = TRUE))




matplot(subset(PREV,sex=="m"&edu=="secondary"& time==1996)[,c("pi1","pi2","pi3")],type='l')




mhrs <-  c(dec_fun_redux(eduvec(TR,"m",1996),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE),
dec_fun_redux(eduvec(TR,"m",2006),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE),
dec_fun_redux(eduvec(TR,"m",2014),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE))

fhrs <- c(dec_fun_redux(eduvec(TR,"f",1996),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE),
dec_fun_redux(eduvec(TR,"f",2006),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE),
dec_fun_redux(eduvec(TR,"f",2014),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE))

deci <- horiuchi(dec_fun_redux, 
		eduvec(TR2,"f",2006,ntrans=2),
		eduvec(TR2,"f",2014,ntrans=2),
		N = 5,ntrans=2)
dim(deci) <- c(length(deci)/3,3)

pifracs <- deci[(nrow(deci)-3):(nrow(deci)-1), ]
efracs  <- deci[nrow(deci), ]
sum(pifracs)


# data for Fig 5
# get fig 4 



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




le <- readRDS("/home/tim/git/HLEDecomp/HLEDecomp/Data/Results/mspec06/le/le_2.rds")
le$LE <- rowSums(le[,c("1","2")])
le[sex=="m"]
#rowSums(datselfi)