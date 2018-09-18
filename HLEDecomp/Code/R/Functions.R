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
getcols <- function(ntrans = 3,self=TRUE,dead="4"){
	if (self){
		return(paste0("m",c(t(outer(1:ntrans,1:ntrans,paste0)))))
	} else {
		cols <- outer(1:ntrans,c(1:ntrans,dead),paste0)
		cols <- sort(cols[lower.tri(cols) | upper.tri(cols)])
		cols <- paste0("m",cols)
		return(cols)
	}
}

get_rates_all <- function(path = "N:\\dcs\\proj\\hledecomp\\results", 
		version = "01"){
	final_path    <- file.path(path, "margins",paste0("mspec", version), paste0("transp_m", version, ".dta"))
	Dat           <- foreign::read.dta(final_path)
	# read.dta() has no stringsAsFactors argument...
	facs          <- sapply(Dat,class) == "factor"
	Dat[, facs]   <- lapply(Dat[,facs], fac2ch)
	Dat
}

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
	out.order       <- as.data.frame(matrix(trans.out, ntrans, byrow = TRUE))
	
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

v2m <- function(vec, ntrans = 3){
	N <- length(vec)
	dim(vec) <- c(N / (ntrans^2), ntrans^2)
	vec
}


#datself <- get_TR("02",subset = sex == "f" & edu == "all_edu" & time == 1996)
# just transition rates, excluding to death, single sex, edu, year
data_2_U <- function(datself, ntrans = 3){
	# get the block order
	trans.self <- getcols(ntrans = ntrans, self = TRUE)
	this.order <- as.data.frame(matrix(trans.self, ntrans, byrow = TRUE))
	
	# take advantage of data.frame columns as list elements
	UL         <- lapply(as.data.frame(datself[,trans.self]), pi2u)
	
	U          <- do.call("rbind", 
			              lapply(this.order, function(x, UL){
				             do.call("cbind", UL[x])
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
e50 <- function(DAT, to, age = 50, prop, ntrans = 3, deduct = TRUE, interval = 2){
	if (missing(prop)){
		pnames <- paste0("s", 1:ntrans, "_prop")
        prop   <- unlist(DAT[1, pnames])
	}
	prop <- prop / sum(prop)
	n    <- nrow(DAT) + 1
	selfcols <- getcols(ntrans, self = TRUE)
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
					split(as.data.frame(N[, cind]), rep(1:ntrans, each = n )),
					colSums)
	)
	# this is the Dudel deduction.
	if (deduct){
	  e.50 <- e.50 - diag(ntrans) # because 1 is half an interval width
    }
	# each to state weighted because person years can originate
	# in any from state.
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

e50_dt <- function(DAT, age = 50, ntrans = 3, prop, deduct = TRUE, interval = 2){
	DAT <- as.data.frame(DAT)
	if (missing(prop)){
		pnames <- paste0("s",1:ntrans,"_prop")
		prop   <- unlist(DAT[1, pnames])
	}
	# TR: new Sept 18
	prop <- prop / sum(prop)
	
	U    <- data_2_U(DAT[, getcols(ntrans, self = TRUE)], ntrans = ntrans)
	N    <- U2N(U, interval = interval)
	
	cind <- rep(seq(50, 112, by = 2), ntrans) == age
	
	# subtract half interval from self-state
	# we do so in the block subdiagonal. Now
	# all self-arrows are deducted. This subtotals
	# code is R-esoteric. Basically take colsums for 
	# 3 block rows.
	e.50 <- do.call("rbind",
			lapply(
					split(as.data.frame(N[, cind]), rep(1:ntrans, each = 32)),
					colSums)
	)
	# this is the Dudel deduction.
	if (deduct){
		e.50 <- e.50 - diag(ntrans) # because 1 is half an interval width
	}
	# each to state weighted because person years can originate
	# in any from state.
	e50all     <- colSums(e.50 * prop)

    out        <- as.data.frame(t(e50all))
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
		ntrans = 3,        # number of states
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
		ntrans = 3,        # number of states
		to, # i.e. do we decompose wrt HLE, ADL1, ADL2p, or LE. leave missing for LE
		N = 20, 
		deduct = TRUE){
	DAT   <- as.data.frame(DAT)
	years <- sort(unique(DAT$time))
	years <- years[1:2]
	
	# if it's missing then it means LE, so make LE beyond the state count.
	if (missing(to)){
		to <- 5
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

# this runs on a chunk (sex,year,edu)
#DAT <- get_TR(version = "02",subset = edu == "all_edu" & sex == "f" & time == 1996)
get_prev_dt <- function(DAT, to, prop, deduct = TRUE, ntrans = 3){
	DAT <- as.data.frame(DAT)
	if (missing(prop)){
		pnames <- paste0("s", 1:ntrans, "_prop")
		prop   <- unlist(DAT[1, pnames])
	}
	# TR: new Sept 18
	prop      <- prop / sum(prop)
	
	age       <- DAT$age
	cols      <- getcols(ntrans, self = TRUE)
	
	DAT       <- DAT[, cols]
	U         <- data_2_U(DAT, ntrans = ntrans)
	N         <- U2N(U, interval = 2)
	cind      <- rep(seq(50,112,by=2), ntrans) == 50
	
	# replaces: N[,cind]) %*% prop
	prev      <- N[, cind] * rep(prop, each=32)
	stop("this should break")
	dim(prev) <- c(32,ntrans)
	prev      <- prev / 2
	colnames(prev) <- paste0("pi",1:ntrans)
	# verify with DCS re age groups. Why exclude age 50?
	prev      <- prev[-nrow(prev), ]
	
	DF        <- as.data.frame(prev)
    DF$age    <- age
	
	DF
}


collapseTR   <- function(TR, PREV, version = "02"){
	# stationary collapse of states 2 and 3 into state 2
	# TR: new Sept 18
	pcols       <- c("s1_prop","s2_prop","s3_prop")
	TR[, pcols] <- TR[, pcols] / rowSums(TR[, pcols])
	
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


# functions for figures. geometric color blending

blend <- function(col1, col2){
	rgb1   <- col2rgb(col1)
	rgb2   <- col2rgb(col2)
	rgbnew <- sqrt((rgb1^2+rgb2^2)/2)
	spatstat::rgb2hex(c(rgbnew))
}

get_colors <- function(){
	cols1    <- RColorBrewer::brewer.pal(9,"YlOrBr")[5:7]
	cols2    <- RColorBrewer::brewer.pal(11,"RdYlBu")[c(9,3,1)]
	cols3    <- RColorBrewer::brewer.pal(11,"RdYlGn")[c(10,9,1)]
	cols1[3] <- blend(cols1[3],"#6E3A07")
	cols2[3] <- blend(cols2[3],"#6E3A07")
	cols3[3] <- blend(cols3[3],"#6E3A07")
	cols     <- c(cols1,cols2,cols3)
	cols
}

plot_prev <- function(prev, type = "bar", scale = FALSE, time = 1995, to = 1,col,...){
	a <- seq(50,110,by=2)
	if (type == "bar"){
		prevm <- as.matrix(prev[prev$time == time, 1:3])[-1, ]
		rownames(prevm) <- NULL
		if (scale){
			prevm <- prevm / rowSums(prevm)
		}
		a10 <-seq(50,110,by=10) - 50
		p10 <- seq(.2,1,by=.2)
		barplot(t(prevm),axes=FALSE,space = 0,width=2,border=NA,xlab = "Age",ylab = ifelse(scale,"Proportion","Prevalence"),col=col,...)
		segments(a10,0,a10,1,col="#FFFFFF50",lwd=.5)
		segments(0,p10,nrow(prevm)*2,p10,col="#FFFFFF50",lwd=.5)
		text(a10,0,a10+50,pos=1,xpd=TRUE)
		text(0,seq(0,1,by=.2),seq(0,1,by=.2),pos=2,xpd=TRUE)
	}
	if (type == 'l'){
		prevt <- matrix(prev[, to], ncol = 3)[-1, ]
		ma    <- colSums(prevt * (a+1)) / colSums(prevt)
		matplot(a, prevt, type = 'l',col=col,xlab = "Age",ylab = "prevalence",...)
		segments(ma,0,ma,1,lwd=1,col=col)
	}
	
}

barmargins <- function(dec.i,ylim){
	
	sets          <- paste(dec.i$year1,"vs",dec.i$year2)
	code          <- unique(sets)
	recvec        <- 1:length(code)
	names(recvec) <- code
	dec.i$decnr   <- recvec[sets]
	
	sex           <- unique(dec.i$sex)
	sex           <- ifelse(sex == "1.men" ,"m", ifelse(sex == "2.wmn", "f", "b"))
	
	edus          <- c("all_edu", "primary" , "secondary", "terciary"  )
	edus1         <- c("0.All edu", "1.Less HS" , "4.HS/GED/Sm coll ex AA", "5.AA/BS/+"  )
	names(edus)   <- edus1
	edu           <- edus[unique(educlevel)]
			
			
	trmargins     <- acast(dec.i, transition ~ state ~ decnr, sum, value.var = "value")
	
	
	trp <- trn <-trmargins
	trp[trp < 0] <- NA
	trn[trn > 0] <- NA
	
	lab.extra <- rownames(trmargins)
	names(lab.extra) <- c("onset 1","onset 2", "mort 1", "rec 1", "onset 2", "mort 2", "rec 2", "rec part", "mort 3")
	
	if (missing(ylim)){
		ylim <- range(pretty(c(apply(trn,3,rowSums,na.rm=TRUE), apply(trp,3,rowSums,na.rm=TRUE))))	
	}
	
	barplot(t(trp[,,1]), ylim = ylim, legend.text=c("HLE","ADL1","ADL2p"), main = paste(code[1],sex,edu),
			ylab = "contribution to difference in e50", space = 0)
	text(1:length(lab.extra) - .5,ylim[1] - .25,names(lab.extra),srt=90,xpd=TRUE,cex = .8,pos=2)
	barplot(t(trn[,,1]),add=TRUE,space=0)
	
# 1995 vs 2014
	barplot(t(trp[,,2]), ylim = ylim, legend.text=c("HLE","ADL1","ADL2p"), main = paste(code[2],sex,edu),
			ylab = "contribution to difference in e50",space=0)
	text(1:length(lab.extra) - .5,ylim[1] - .25,names(lab.extra),srt=90,xpd=TRUE,cex = .8,pos=2)
	barplot(t(trn[,,2]),add=TRUE,space=0)
	
# 2004 vs 2014
	barplot(t(trp[,,3]), ylim = range(trmargins), legend.text=c("HLE","ADL1","ADL2p"), main = paste(code[3],sex,edu),
			ylab = "contribution to difference in e50",space=0)
	text(1:length(lab.extra) - .5,ylim[1] - .25,names(lab.extra),srt=90,xpd=TRUE,cex = .8,pos=2)
	barplot(t(trn[,,3]),add=TRUE,space=0)
}

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
