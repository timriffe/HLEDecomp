me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
}

source("Code/R/utils.R")




# deprecated?
get_rates_all <- function(path = "N:\\dcs\\proj\\hledecomp\\results", 
		version = "01"){
	final_path    <- file.path(path, "margins",paste0("mspec", version), paste0("transp_m", version, ".dta"))
	Dat           <- foreign::read.dta(final_path)
	# read.dta() has no stringsAsFactors argument...
	facs          <- sapply(Dat,class) == "factor"
	Dat[, facs]   <- lapply(Dat[,facs], fac2ch)
	Dat
}



# this is used, no arg yet to determine which version
# of initprop to use... This assumes an R version has already been
# created at the MPIDR machine

get_TR <- function(version = "02",home = getwd(),...){
	path <- file.path(home,"Data","Transitions","DCS",paste0("TR_v",version,".Rdata"))
	Dat           <- local(get(load(path)))
	Dat$educlevel <- NULL
	Dat$spec      <- NULL
	Dat$specnum   <- NULL
	Dat$specgr    <- NULL
	Dat$specnum2  <- NULL
	Dat$specgr    <- NULL
	subset(Dat, ...)
}
#DAT <- get_TR(version = "02", subset = sex == "f")
#DAT <- get_TR("02",subset = sex == "f" & edu == "all_edu" & time == 1996)

out2self <- function(datout, ntrans = 3){
	trans.self      <- getcols(ntrans = ntrans, self = TRUE)
	trans.out       <- getcols(ntrans = ntrans, self = FALSE)
	out.order       <- as.data.frame(matrix(trans.out, ntrans, byrow = TRUE),stringsAsFactors=FALSE)
	
	# TR: 25-05-2018 strong assumption of rate ordering!!
	# this line could be more robust. pragmatic fix now
	colnames(datout) <- trans.out 
	
	selfs           <- apply(out.order, 1, function(x, datout){
				            1 - rowSums(datout[, x])
			          }, datout = datout)
	colnames(selfs) <- paste0("m", 1:ntrans, 1:ntrans)
	
	out             <- cbind(datout, selfs)[, trans.self]
	out
}

# vector to matrix with rate schedules rbound together
# useful since decomp needs vectors
v2m <- function(vec, ntrans = 3){
	N <- length(vec)
	dim(vec) <- c(N / (ntrans^2), ntrans^2)
	vec
}

v2mall <- function(vecall,ntrans=2){
	# assume columns in order provided by
	# getcolsall(ntrans)
	N <- length(vecall)
	dim(vecall) <- c(N / (ntrans^2+ntrans), ntrans^2+ntrans)
	vecall
}

pi2u <- function(pivec){
	cbind(rbind(0,diag(pivec)),0)
}

#datself <- get_TR("02",subset = sex == "f" & edu == "all_edu" & time == 1996)
# just transition rates, excluding to death, single sex, edu, year
data_2_U <- function(datself, ntrans = 3){
	datself    <- as.data.frame(datself,stringsAsFactors=FALSE)
	# get the block order
	trans.self <- getcols(ntrans = ntrans, self = TRUE)
	this.order <- as.data.frame(matrix(trans.self, ntrans, byrow = TRUE),stringsAsFactors=FALSE)

	# take advantage of data.frame columns as list elements
	UL         <- lapply(as.data.frame(datself[,trans.self],stringsAsFactors=FALSE), pi2u)

	U          <- do.call("cbind", 
			              lapply(this.order, function(x, UL){
				             do.call("rbind", UL[x])
			              }, UL = UL)
                          )
						  
	U
}


U2N <- function(U, interval = 2){
	I   <- diag(nrow(U))
	Nsx <- solve(I - U) * interval
	Nsx 
}

#dat <- get_TR_all(version = "02",subset = sex == "f" & edu == "all_edu" & time == 2006)
# TR: added deduct switch to test for differences re DCS email
#dat <- A1

# for a single year-sex-edu
e50 <- function(DAT, to, age = 50, prop, ntrans, deduct = TRUE, interval = 2, dead = "4"){
	DAT <- as.data.frame(DAT, stringsAsFactors = FALSE)
	n   <- nrow(DAT)
	if (missing(ntrans)){
		ntrans <- guess_ntrans(DAT)
	}
	
	if (missing(prop)){
		pnames <- paste0("s", 1:ntrans, "_prop")
        prop   <- unlist(DAT[1, pnames])
	}
	prop <- prop / sum(prop)
	
	selfcols <- getcols(ntrans, self = TRUE, dead = dead)
	U    <- data_2_U(DAT[, selfcols], ntrans = ntrans)
	N    <- U2N(U, interval = interval)
	
	cind <- rep(seq(50, 112, by = 2), ntrans) == age
	
	# subtract half interval from self-state
    # we do so in the block subdiagonal. Now
	# all self-arrows are deducted. This subtotals
	# code is R-esoteric. Basically take colsums for 
	# 3 block rows.
	e.50 <- do.call("rbind",
			lapply(
					split(as.data.frame(N[, cind],stringsAsFactors=FALSE), 
							rep(1:ntrans, each = (n + 1))),
					colSums)
	)
	# this is the Dudel deduction.
	if (deduct){
	  e.50 <- e.50 - diag(ntrans) # because 1 is half an interval width
    }
	# each to state weighted because person years can originate
	# in any from state.
	# Note age 50 props used even if age selection isn't 50
	e50all <- colSums(e.50 * prop)

    # this replaces, possibly erroneously, e.50 %*% prop
	# it's likely that whatever is happening here is the achilles heal
	
	# this if we only want a destination state subset.
	if (!missing(to)){
		if (to <= ntrans){
			return(e50all[to])
		}
	}
	# otherwise return LE
	sum(e50all)
}

#DAT <- get_TR(version = "02",subset = edu == "all_edu" & sex == "f" & time == 1996)
# this one is just for generating e0 descriptive results
# for a single year-sex-edu

e50_dt <- function(DAT, age = 50, ntrans, prop, deduct = TRUE, interval = 2, dead = "4"){
	DAT <- as.data.frame(DAT, stringsAsFactors = FALSE)
	n   <- nrow(DAT)
	
	if (missing(ntrans)){
		ntrans <- guess_ntrans(DAT)
	}
	
	if (missing(prop)){
		pnames <- paste0("s", 1:ntrans, "_prop")
		prop   <- unlist(DAT[1, pnames])
	}
	# TR: new Sept 18
	prop <- prop / sum(prop)
	selfcols <- getcols(ntrans, self = TRUE, dead = dead)
	U    <- data_2_U(DAT[, selfcols], ntrans = ntrans)
	
	N    <- U2N(U, interval = interval)
	
	cind <- rep(seq(50, 112, by = 2), ntrans) == age
	
	# subtract half interval from self-state
	# we do so in the block subdiagonal. Now
	# all self-arrows are deducted. This subtotals
	# code is R-esoteric. Basically take colsums for 
	# 3 block rows.
	e.50 <- do.call("rbind",
			lapply(
					split(
							as.data.frame(N[, cind], stringsAsFactors = FALSE), 
					        rep(1:ntrans, each = n + 1 )),
					colSums)
	)
	# this is the Dudel deduction.
	if (deduct){
		e.50 <- e.50 - diag(ntrans) # because 1 is half an interval width
	}
	# each to state weighted because person years can originate
	# in any from state.
	e50all     <- colSums(e.50 * prop)
	# TR identical:
	# e50all <- t(prop) %*% e.50 
    out        <- as.data.frame(t(e50all),stringsAsFactors=FALSE)
	out
}


dec_fun <- function(datoutvec, to, age = 50, prop, ntrans = 3, deduct = TRUE){
	# requires input as vector, first reshape to matrix or df
	datout  <- v2m(datoutvec, ntrans = ntrans)
	# remove death rates and replace with self-arrows, needed to make
	# transition matrices
	datself <- out2self(datout, ntrans = ntrans)
	# then compute e50 using the correct transition rates
	dc      <- e50(datself, to = to, age = age, prop = prop, ntrans = ntrans, deduct = deduct)

	dc 
}

HLEDecomp <- function(datout1, datout2, N = 10, ntrans = 3, prop, to, deduct = TRUE){
	
	# TR: we don't decompose wrt differences in initial proportions
	# so we can siphen prop from the first data object, using for both.
	if (missing(prop)){
		pnames <- paste0("s",1:ntrans,"_prop")
		prop   <- unlist(datout1[1, pnames])
	}
	# TR: new Sept 18
	prop <- prop / sum(prop)
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

# not implemented for logit transform. Could add in logical switch tho
#dec<- do_decomp(years = c(2006,2014),version = "02",sex = "f", edu = "all_edu")
# This function needs to be reworked to work w new HLEDECOMP output. Simplify ugly inner functions too.
# just make it do a single comparison.

# TODO: make this select an arbitrary two populations to compare. Could be over time or between pops (within time)
# TODO: try initial differences vs trend decomp version. Both at same time maybe?
do_decomp <- function(
		years = c(1996,2014), 
		ntrans,        # number of states
		version = "02",    # character 2 digits
		sex = "f",       
		edu = "all_edu", 
		to, # i.e. do we decompose wrt HLE, ADL1, ADL2p, or LE. leave missing for LE
		N = 20, 
		deduct = TRUE){
	years <- years[1:2]
	# if it's missing then it means LE, so make LE beyond the state count.
	if (missing(to)){
		to <- 5
	}
	# it'd get confused otherwise and not down-select as needed
	.edu <- edu
	.sex <- sex
	TR          <- get_TR(version = version, 
			              subset = sex == .sex & edu == .edu & time %in% years)
	if (missing(ntrans)){
			ntrans <- guess_ntrans(TR)
	}
	DatL        <- split(TR, TR$time)
	names(DatL) <- years
	
	dec <- HLEDecomp(DatL[[1]],
			DatL[[2]],
			N = N, 
			to = to, 
			deduct = deduct)#[-1, ]
	
	dec  <- melt(dec, varnames = c("age", "transition"))
		
	dec$statedec  <- to
	dec$year1     <- years[1]
	dec$year2     <- years[2]
	
	# add metadata
	dec$sex       <- sex
	dec$edu       <- edu
	dec$version   <- version
	dec$N         <- N
	dec$state1    <- substr(dec$transition,2,2)
	dec$state2    <- substr(dec$transition,3,3)
	
	dec
}

do_decomp_dt <- function( DAT,
		ntrans,        # number of states
		to, # i.e. do we decompose wrt HLE, ADL1, ADL2p, or LE. leave missing for LE
		N = 20, 
		deduct = TRUE){
	DAT   <- as.data.frame(DAT,stringsAsFactors=FALSE)
	
	if (missing(ntrans)){
		ntrans <- guess_ntrans(DAT)
	}
	
	years <- sort(unique(DAT$time))
	years <- years[1:2]
	
	# if it's missing then it means LE, so make LE beyond the state count.
	if (missing(to)){
		to <- ntrans + 5
	}
	edu  <- unique(DAT$edu)
	sex  <- unique(DAT$sex)
	# it'd get confused otherwise and not down-select as needed
	.edu <- edu
	.sex <- sex

	DatL        <- split(DAT, DAT$time)
	names(DatL) <- years
#	datout1 <- DatL[[1]]
#	datout2 <- DatL[[2]]
	dec <- HLEDecomp(DatL[[1]],
			DatL[[2]],
			N = N, 
			to = to, 
			deduct = deduct,
			ntrans = ntrans)#[-1, ]
	
	dec  <- melt(dec, varnames = c("age", "transition"))
	
	dec$statedec  <- to
	dec$year1     <- years[1]
	dec$year2     <- years[2]
	
	# add metadata
	dec$sex       <- sex
	dec$edu       <- edu
	dec$version   <- version
	dec$N         <- N
	dec$state1    <- substr(dec$transition,2,2)
	dec$state2    <- substr(dec$transition,3,3)
	
	data.table(dec)
}


#dec <- do_decomp(to=1)
#head(dec)
#LE1 <- e50(DatL[[1]],to=1)
#LE2 <- e50(DatL[[2]],to=1)
#LE2 - LE1
#sum(dec$value)
#do_le <- function(years = c(1995,2004,2014),age = 50, version, sex, 
#		educlevel, deduct = TRUE, dcs = FALSE, path = "N:\\dcs\\proj\\hledecomp\\results\\margins"){
#	DatL   <- lapply(years, 
#			get_data, 
#			self = TRUE, 
#			version = version, 
#			sex = Sex, 
#			educlevel = educlevel,
#			path = path)
#	
#	names(DatL) <- years
#
#	do.call("rbind",lapply(DatL, function(X,deduct,dcs){
#				prop <- attr(X, "initprop")
#				U    <- data_2_U(X)
#				N    <- U2N(U, interval = 2)
#				cind <- rep(seq(50,112,by=2), 3) == age
#				
#				rgroups <- rep(1:3, each = 32)
#				Ntab <- apply(N[, cind], 2, function(x, rgroups){
#							tapply(x, rgroups, sum) #* 2
#						}, rgroups = rgroups)
#				if (deduct & !dcs){
#					Ntab <- Ntab - diag(3)
#				}
#				if (deduct & dcs){
#			
#					Nprop <- t(t(Ntab) / colSums(Ntab))
#					Ntab  <- Ntab - Nprop
#				}
#				rowSums(Ntab  %*% diag(prop))
#			}, deduct = deduct, dcs = dcs))
#	
#}


# Calculate a prevalence object in a data.table structure, data presumed
# to refer to a single subset. Radix proportions either given or siphened
# from columns s1_prop, s2_prop (s3_prop). transition probabilities labeled
# m11, m12, m13, etc
# deduct toggles the dudel deduction. ntrans is the number of transient states,
# important. Detected, but not fallible.

#DAT <- get_TR(version = "06",subset = edu == "primary" & sex == "m" & time == 1996)
get_prev_dt <- function(DAT, prop, deduct = TRUE, ntrans, interval = 2, age = 50){
	
	DAT <- as.data.frame(DAT,stringsAsFactors=FALSE)
	
	if (missing(ntrans)){
		ntrans <- guess_ntrans(DAT)
	}

	if (missing(prop)){
		pnames <- paste0("s", 1:ntrans, "_prop")
		prop   <- unlist(DAT[1, pnames])
	}
	# TR: new Sept 18
	prop      <- prop / sum(prop)
	
	Age       <- DAT$age
	cols      <- getcols(ntrans, self = TRUE)
	
	DAT       <- DAT[, cols]
	U         <- data_2_U(DAT, ntrans = ntrans)
	N         <- U2N(U, interval = interval)
	
	ages      <- rep(seq(50,112, by = interval), ntrans)
	cind      <- ages == 50
	
	# replaces: N[,cind]) %*% prop
	prev      <- N[, cind] * rep(prop, each=32)
	
	prev      <- as.matrix(aggregate(x=prev,by=list(ages), sum)[,-1])
	
	#dim(prev) <- c(32,ntrans)
	prev      <- prev / interval
	colnames(prev) <- paste0("pi",1:ntrans)
	# verify with DCS re age groups. Why exclude age 50?
	prev      <- prev[-nrow(prev), ]
	
	DF        <- as.data.frame(prev,stringsAsFactors=FALSE)
    DF$age    <- Age
	
	DF[DF$Age >= age,]
	
	DF
}

# collapse transition rates, this is indeed presently used,
# and it should be verified at some point. PREV can be calculated
# on the fly. It is presumed that TR contains 3 living states:
# 1,2,3, and that the 4th state is dead. transition proabbility
# columns must be labelled with the convention m11, m12, m13, etc.
# radix proportions stored in columns s1_prop, s2_prop, s3_prop.
# siphened from the first row of each subset (presumed age-sorted).

collapseTR   <- function(TR, PREV, version = "02"){
	# stationary collapse of states 2 and 3 into state 2
	# TR: new Sept 18
	#pcols       <- c("s1_prop","s2_prop","s3_prop")
	#TR[, pcols] <- TR[, pcols] / rowSums(TR[, pcols])
	
	if (missing(TR)){
		TR     <- get_TR(version = version)
	}
	TR         <- data.table(TR)
	if (missing(PREV)){
		PREV   <- TR[ , get_prev_dt(.SD), by = list(sex, edu, time)]
	}
# rates and prev
	RP         <- merge(TR, PREV)
# reorder RP
	RP         <- setorder(RP, time, sex, edu, age)
# m11 <- m11
	m11        <- RP$m11
# m14 <- m14 # keep 4 as dead
	m14        <- RP$m14
# m12 <- m12 + m13
	m12        <- RP$m12 + RP$m13
# m22 <- (m22 * pi2 + m33 * pi3) / (pi2 + pi3)
	m21        <- (RP$m21 * RP$pi2 + RP$m31 * RP$pi3) / (RP$pi2 + RP$pi3)
	m24        <- (RP$m24 * RP$pi2 + RP$m34 * RP$pi3) / (RP$pi2 + RP$pi3)
	m22        <- 1 - m21 - m24
	m22i       <- ( (RP$m22 + RP$m23) * RP$pi2 + (RP$m33 + RP$m32) * RP$pi3) / (RP$pi2 + RP$pi3)
	
	stopifnot(all(abs(m22 - m22i) < 1e-7)) # sanity check
	
	RP         <- data.table(RP)
	s2_prop    <- RP$s2_prop + RP$s3_prop
	RP         <- cbind(RP[, c("time", "sex", "edu", "age", "s1_prop")], 
			s2_prop, m11, m12, m14, m22, m21, m24)
	RP
}

#do_prev <- function(
#		years = c(1995,2004,2014),
#		age = 52, 
#		version, 
#		sex, 
#		educlevel, 
#		deduct = TRUE, 
#		dcs = FALSE, 
#		path = "N:\\dcs\\proj\\hledecomp\\results\\margins"){
#	DatL   <- lapply(years, 
#			get_data, 
#			self = TRUE, 
#			version = version, 
#			sex = sex, 
#			educlevel = educlevel,
#			path = path)
#	
#	names(DatL) <- years
#	
#	prev <- do.call("rbind",lapply(DatL, function(X,deduct,dcs){
#						prop    <- attr(X, "initprop")
#						year    <- attr(X, "time")
#						U       <- data_2_U(X)
#						N       <- U2N(U, interval = 2)
#						cind    <- rep(seq(50,112,by=2), 3) == age
#						DF      <- as.data.frame(matrix(rowSums(N[,cind] %*% diag(prop)),ncol=3,dimnames=list(NULL,1:3)))
#						DF$time <- year
#						DF
#					}, deduct = deduct, dcs = dcs))
#	prev
#}
#

#logit <- function(x){
#	log(x / (1 - x))
#}
#expit <- function(x){
#	exp(x )/ (1 + exp(x))
#}
#dec_fun_logit <- function(datoutvec,to=1,age=52, prop){
#	datoutvec <- expit(datoutvec)
#	datout    <- v2m(datoutvec, 3)
#	
#	datself   <- out2self(datout,)
#	e50(datself, to = to, age = age, prop = prop) * 2 # age interval
#}
#HLEDecomp_logit <- function(datout1, datout2, N = 10, prop, to){
#	# TR: we don't decompose wrt differences in initial proportions
#	# so we can siphen prop from the first data object, using for both.
#	if (missing(prop)){
#		if ("initprop" %in% names(attributes(dat))){
#			prop <- attr(datout1,"initprop")
#		} else {
#			prop <- unlist(datout1[1,c("s1_prop","s2_prop","s3_prop")])
#		}
#	}
#	
#	datout1vec <- c(as.matrix(datout1))
#	datout2vec <- c(as.matrix(datout2))
#	
#	# impute 0s (none are structural
#	datout1vec[datout1vec == 0] <- 1e-7
#	datout2vec[datout2vec == 0] <- 1e-7
#	
#	# logit transform
#	datout1vec <- logit(datout1vec)
#	datout2vec <- logit(datout2vec)
#	# arrow decomposition:
#	dec      <- DecompHoriuchi::DecompContinuousOrig(
#			func = dec_fun_logit, 
#			rates1 = datout1vec, 
#			rates2 = datout2vec, 
#			N = N, 
#			prop = prop,
#			to = to)
#	dim(dec)      <- dim(datout1)
#	dimnames(dec) <- dimnames(datout1)
#	# no dim reduction here
#	dec
#}



#sprintf("%.2f", round(Tab.i[,1],2))
#sprintf("%03",as.character(round(Tab.i[,1],2)))
# deprecated as of 10-May 2018. Functions were revamped to account for different
# input data format, and allow for more general deduction procedure.

#e50dcs <- function(dat,age=50,prop,deduct=FALSE,to = 4){
#	
#	if (missing(prop)){
#		if ("initprop" %in% names(attributes(dat))){
#			prop <- attr(dat,"initprop")
#		} else {
#			prop <- unlist(dat[1,c("s1_prop","s2_prop","s3_prop")])
#		}
#	}
#	
#	#prop <- attr(dat,"initprop")
#	U    <- data_2_U(dat)
#	N    <- U2N(U, interval = 2)
#	cind <- rep(seq(50,112,by=2), 3) == age
#	N2   <- N[, cind]
#	
#	# get proportions in each 
#	rgroups <- rep(1:3,each=32)
#	Ntab <- matrix(0,3,3)
#	for (i in 1:3){
#		Ntab[,i] <- tapply(N2[,i], rgroups, sum)
#	}
#	
#	# this should adjust for age groups already...
#	Ntab  <- Ntab #* 2
#	
#	# this is the dcs adjustment...
#	if (deduct){
#		Nprop <- t(t(Ntab) / colSums(Ntab))
#		Ntab  <- Ntab - Nprop
#	}
#	ei <- rowSums(Ntab %*% diag(prop))
#    if (is.integer(to) & to <= 3){
#		out <- ei[to]
#	} else {
#		out <- sum(ei)
#	}
#	out
#}

# deprecated
#dec_fun <- function(datoutvec,to=1,age=52, prop, deduct = TRUE, dcs = FALSE){
#	datout  <- v2m(datoutvec, 3)
#	datself <- out2self(datout)
#	if (!dcs){
#		dc <- e50(datself, to = to, age = age, prop = prop)
#	} else {
#		dc <- e50dcs(datself, to = to, age = age, prop = prop, deduct = deduct)
#	}
#	dc 
#}

#get_data <- function(path = "N:\\dcs\\proj\\hledecomp\\results", 
#		version = "01",
#		sex = "1.men",
#		year = 2004,
#		educlevel = "0.All edu",
#        self = TRUE){
#	# this works on Tim's MPIDR PC, Windows machine...
#	Dat           <- get_rates_all(path = path, version = version, self = self)
#
#	sprop         <- foreign::read.dta(file.path(path,"initprop","initprop2.dta"))
#	
#	facs          <- sapply(sprop,class) == "factor"
#	sprop[, facs] <- lapply(sprop[,facs], fac2ch)
#	
#	# subset() or with() don't like it when your args have same names as cols, so
#	# here's a verbose subset
#	ind            <- Dat$sex == sex & Dat$educlevel == educlevel & Dat$time == year 
#    Dat            <- Dat[ind, ]
#	Dat            <- Dat[order(Dat$age), ]
#
#	ind            <- sprop$sex == sex & sprop$educlevel == educlevel & sprop$propweighted == 1 & grepl(pattern = "age 50-54", sprop[,1])
#	sprop          <- sprop[ind, c("s1_prop","s2_prop","s3_prop")]
#	
#	Dat            <- Dat[order(Dat$age), ]
#	age            <- Dat$age
#	cols           <- getcols(ntrans = 3, self = self)
#	Dat            <- Dat[, cols]
#	rownames(Dat)  <- age
#	
#	sprop          <- unlist(sprop)
#	sprop          <- sprop / sum(sprop)
#	attr(Dat, "initprop") <- sprop
#	attr(Dat, "time")     <- year
#	Dat
#}