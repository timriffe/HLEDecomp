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

Trans <- outer(1:4,1:4,paste0)
OutTrans <- Trans[lower.tri(Trans) | upper.tri(Trans)]


Probs <- paste0("m",c(t(outer(1:3,1:3,paste0))))


fac2ch <- function(f){
	as.character(f)
}

get_data <- function(path = "N:\\dcs\\proj\\hledecomp\\results\\margins", 
		version = "01",
		sex = "1.men",
		time = 2004,
		educlevel = "0.All edu",
        self = TRUE){
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
	ind           <- Dat$sex == sex & Dat$educlevel == educlevel & Dat$time == time 
    DatS          <- Dat[ind, ]
	
	ind           <- sprop$sex == sex & sprop$educlevel == educlevel & sprop$propweighted == 1 & grepl(pattern = "age 50-54", sprop[,1])
	sprop         <- sprop[ind, c("s1_prop","s2_prop","s3_prop")]
	
	DatS          <- DatS[order(DatS$age), ]
	age           <- DatS$age
	if (self){
		cols <- paste0("m",c(t(outer(1:3,1:3,paste0))))
	} else {
		cols <- outer(1:4,1:4,paste0)
		cols <- sort(Trans[lower.tri(Trans) | upper.tri(Trans)])
	}
	DatS           <- DatS[, cols]
	rownames(DatS) <- age
	
	sprop          <-  unlist(sprop)
	sprop          <- sprop / sum(sprop)
	attr(DatS, "initprop") <- sprop

	DatS
}
#time: 1995, 2004, 2014
m1995 <- get_data(time = 1995)
m2004 <- get_data(time = 2004)


data_2_U <- function(dat){
	UL <- lapply(dat,pi2u)
	U <- cbind(
					do.call("rbind",UL[c("m11","m12","m13")]),
					do.call("rbind",UL[c("m21","m22","m23")]),
					do.call("rbind",UL[c("m31","m32","m33")]))
	U
}


# identical:
#max(abs(piout2U(extractfrom(A1)) -A[1:150,1:150]))

U2N <- function(U,comp = c(.7,.25,.05)){
	I   <- diag(nrow(U))
	Nsx <- solve(I - U)
	Nsx 
}

# 
e50 <- function(dat,to=1,age=50){
	prop <- attr(dat,"initprop")
	U    <- data_2_U(dat)
	N    <- U2N(U,2)
	rind <- 1:32 + (to-1) * 32
	cind <- rep(seq(50,112,by=2),3) == age
	
	e50  <- colSums(N[rind,cind]) * 2
	# subtract half interval from self-state
	e50[to] <-e50[to] - 1 
	sum(e50 * prop)
}

# preliminary results
m1995 <- get_data(time = 1995)
m2004 <- get_data(time = 2004)
m2014 <- get_data(time = 2014)

HLE95 <- e50(m1995,to=1)
HLE04 <- e50(m2004,to=1)
HLE14 <- e50(m2014,to=1)

LE295 <- e50(m1995,to=2)
LE204 <- e50(m2004,to=2)
LE214 <- e50(m2014,to=2)

LE395 <- e50(m1995,to=3)
LE304 <- e50(m2004,to=3)
LE314 <- e50(m2014,to=3)

HLE95 + LE295 + LE395
HLE04 + LE204 + LE304
HLE14 + LE214 + LE314

results <- data.frame(HLE = c(HLE95,HLE04, HLE14),
		   ADL1LE = c(LE295, LE204, LE214),
		   ADL2LE = c(LE395, LE304, LE314))
   results$e0 <- rowSums(results)

print(results,digits=3)





HLEDecomp <- function(A1,A2,N=10,comp= c(.7,.25,.05)){
	
	# arrow decomposition:
	deltaxs      <- DecompHoriuchi::DecompContinuousOrig(
			func = piout2WLE50, 
			rates1 = pi1, 
			rates2 = pi2, 
			N = N, 
			comp = comp)
	dim(deltaxs) <- c(length(deltaxs) / 9,9)
	deltaxs      <- colSums(deltaxs)
# from in columns, to in rows, in this case most arrows similar,
	wirnot  <- matrix(deltaxs, 3, 3, byrow = TRUE, dimnames = list(c("W", "I", "R"), c("W", "I", "R")))
	
	todeath <- diag(wirnot)
	diag(wirnot) <- NA
	rbind(wirnot, D = todeath)
}
