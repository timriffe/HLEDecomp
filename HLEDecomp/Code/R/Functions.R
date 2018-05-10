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

get_rates_all <- function(path = "N:\\dcs\\proj\\hledecomp\\results", 
		version = "01",
		self = TRUE){
	final_path    <- file.path(path, "margins",paste0("mspec", version), paste0("transp_m", version, ".dta"))
	Dat           <- foreign::read.dta(final_path)
	# read.dta() has no stringsAsFactors argument...
	facs          <- sapply(Dat,class) == "factor"
	Dat[, facs]   <- lapply(Dat[,facs], fac2ch)
	Dat
}

get_data <- function(path = "N:\\dcs\\proj\\hledecomp\\results", 
		version = "01",
		sex = "1.men",
		year = 2004,
		educlevel = "0.All edu",
        self = TRUE){
	# this works on Tim's MPIDR PC, Windows machine...
	Dat           <- get_rates_all(path = path, version = version, self = self)

	sprop         <- foreign::read.dta(file.path(path,"initprop","initprop2.dta"))
	
	facs          <- sapply(sprop,class) == "factor"
	sprop[, facs] <- lapply(sprop[,facs], fac2ch)
	
	# subset() or with() don't like it when your args have same names as cols, so
	# here's a verbose subset
	ind            <- Dat$sex == sex & Dat$educlevel == educlevel & Dat$time == year 
    Dat            <- Dat[ind, ]
	Dat            <- Dat[order(Dat$age), ]

	ind            <- sprop$sex == sex & sprop$educlevel == educlevel & sprop$propweighted == 1 & grepl(pattern = "age 50-54", sprop[,1])
	sprop          <- sprop[ind, c("s1_prop","s2_prop","s3_prop")]
	
	Dat            <- Dat[order(Dat$age), ]
	age            <- Dat$age
	cols           <- getcols(ntrans = 3, self = self)
	Dat            <- Dat[, cols]
	rownames(Dat)  <- age
	
	sprop          <-  unlist(sprop)
	sprop          <- sprop / sum(sprop)
	attr(Dat, "initprop") <- sprop
	attr(Dat, "time")     <- year
	Dat
}

get_TR_all <- function(version = "02",home = getwd(),...){
	path <- file.path(home,"Data","Transitions","DCS",paste0("TR_v",version,".Rdata"))
	Dat  <- local(get(load(path)))
	subset(Dat, ...)
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


U2N <- function(U, interval = 2){
	I   <- diag(nrow(U))
	Nsx <- solve(I - U) * interval
	Nsx 
}

# TR: added deduct switch to test for differences re DCS email
e50 <- function(dat, to = 1, age = 50, prop = attr(dat, "initprop"), deduct = TRUE){
	#prop <- attr(dat,"initprop")
	U    <- data_2_U(dat)
	N    <- U2N(U, interval = 2)
	rind <- 1:32 + (to-1) * 32
	cind <- rep(seq(50,112,by=2), 3) == age
	
	e50  <- colSums(N[rind,cind]) * 2
	# subtract half interval from self-state
	if (deduct){
	  e50[to] <- e50[to] - 1 
    }
	sum(e50 * prop)
}

e50dcs <- function(dat,age=50,prop=attr(dat,"initprop"),deduct=FALSE,to = 4){
	
	#prop <- attr(dat,"initprop")
	U    <- data_2_U(dat)
	N    <- U2N(U, interval = 2)
	cind <- rep(seq(50,112,by=2), 3) == age
	N2   <- N[, cind]
	
	# get proportions in each 
	rgroups <- rep(1:3,each=32)
	Ntab <- matrix(0,3,3)
	for (i in 1:3){
		Ntab[,i] <- tapply(N2[,i], rgroups, sum)
	}
	
	# this should adjust for age groups already...
	Ntab  <- Ntab #* 2
	
	# this is the dcs adjustment...
	if (deduct){
		Nprop <- t(t(Ntab) / colSums(Ntab))
		Ntab  <- Ntab - Nprop
	}
	ei <- rowSums(Ntab %*% diag(prop))
    if (is.integer(to) & to <= 3){
		out <- ei[to]
	} else {
		out <- sum(ei)
	}
	out
}

dec_fun <- function(datoutvec,to=1,age=52, prop, deduct = TRUE, dcs = FALSE){
	datout  <- v2m(datoutvec, 3)
	datself <- out2self(datout)
	if (!dcs){
		dc <- e50(datself, to = to, age = age, prop = prop)
	} else {
		dc <- e50dcs(datself, to = to, age = age, prop = prop, deduct = deduct)
	}
	dc 
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


HLEDecomp <- function(datout1, datout2, N = 10, prop = attr(datout1,"initprop"), to=1, deduct = TRUE, dcs = FALSE){
	
	datout1vec <- c(as.matrix(datout1))
	datout2vec <- c(as.matrix(datout2))
	# arrow decomposition:
	dec      <- DecompHoriuchi::DecompContinuousOrig(
			        func = dec_fun, 
			        rates1 = datout1vec, 
			        rates2 = datout2vec, 
			        N = N, 
			        prop = prop,
					to = to,
					deduct = deduct,
					dcs = dcs)
	dim(dec)      <- dim(datout1)
	dimnames(dec) <- dimnames(datout1)
	# no dim reduction here
	dec
}

logit <- function(x){
	log(x / (1 - x))
}
expit <- function(x){
	exp(x )/ (1 + exp(x))
}
dec_fun_logit <- function(datoutvec,to=1,age=52, prop){
	datoutvec <- expit(datoutvec)
	datout    <- v2m(datoutvec, 3)
	
	datself   <- out2self(datout)
	e50(datself, to = to, age = age, prop = prop) * 2 # age interval
}
HLEDecomp_logit <- function(datout1, datout2, N = 10, prop = attr(datout1,"initprop"), to=1){
	
	datout1vec <- c(as.matrix(datout1))
	datout2vec <- c(as.matrix(datout2))
	
	# impute 0s (none are structural
	datout1vec[datout1vec == 0] <- 1e-7
	datout2vec[datout2vec == 0] <- 1e-7
	
	# logit transform
	datout1vec <- logit(datout1vec)
	datout2vec <- logit(datout2vec)
	# arrow decomposition:
	dec      <- DecompHoriuchi::DecompContinuousOrig(
			      func = dec_fun_logit, 
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

# not implemented for logit transform. Could add in logical switch tho
do_decomp <- function(
		years = c(1995,2004,2014), 
		ntrans = 3, # number of states
		version,    # character 2 digits
		sex,       
		educlevel, 
		N = 20, 
		deduct = TRUE, 
		dcs = FALSE, 
		path = "N:\\dcs\\proj\\hledecomp\\results"){
	
	# get transition rate sets
	DatL   <- lapply(years, 
			get_data, 
			self = FALSE, 
			version = version, 
			sex = sex, 
			educlevel = educlevel,
			path = path)
	
	names(DatL) <- years
		
	# make from-to pairlist
	comparisons <- outer(years, years, '<')
	from        <- row(comparisons)[comparisons]
	to          <- col(comparisons)[comparisons]
	ft          <- mapply(c, from, to, SIMPLIFY = FALSE)
	
	# looping. Can be swapped w parallel decomp
	dec <- do.call("rbind",lapply(ft, function(fromto, DatL, ntrans, N, deduct, dcs){
						A1   <- DatL[[fromto[1]]]
						A2   <- DatL[[fromto[2]]]
						yrs  <- as.integer(names(DatL)[fromto])
						decx <- do.call("rbind", lapply(1:ntrans, function(sto, A1, A2, N = N, deduct, dcs){
											dec.i <- melt(HLEDecomp(A1, A2, N = N, to = sto, deduct = deduct, dcs = dcs)[-1, ],
													varnames = c("age","transition"))
											dec.i$state <- sto
											dec.i
										}, A1 = A1, A2 = A2, N = N, deduct = deduct, dcs = dcs))
						decx$year1 <- yrs[1]
						decx$year2 <- yrs[2]
						decx
					}, DatL = DatL, ntrans = ntrans, N = N, deduct = deduct, dcs = dcs))  
	
	# add metadata
	dec$sex       <- sex
	dec$educlevel <- educlevel
	dec$version   <- version
	dec$N         <- N
	
	dec
}


do_le <- function(years = c(1995,2004,2014),age = 52, version, sex, 
		educlevel, deduct = TRUE, dcs = FALSE, path = "N:\\dcs\\proj\\hledecomp\\results\\margins"){
	DatL   <- lapply(years, 
			get_data, 
			self = TRUE, 
			version = version, 
			sex = Sex, 
			educlevel = educlevel,
			path = path)
	
	names(DatL) <- years

	do.call("rbind",lapply(DatL, function(X,deduct,dcs){
				prop <- attr(X, "initprop")
				U    <- data_2_U(X)
				N    <- U2N(U, interval = 2)
				cind <- rep(seq(50,112,by=2), 3) == age
				
				rgroups <- rep(1:3, each = 32)
				Ntab <- apply(N[, cind], 2, function(x, rgroups){
							tapply(x, rgroups, sum) #* 2
						}, rgroups = rgroups)
				if (deduct & !dcs){
					Ntab <- Ntab - diag(3)
				}
				if (deduct & dcs){
			
					Nprop <- t(t(Ntab) / colSums(Ntab))
					Ntab  <- Ntab - Nprop
				}
				rowSums(Ntab  %*% diag(prop))
			}, deduct = deduct, dcs = dcs))
	
}

do_prev <- function(
		years = c(1995,2004,2014),
		age = 52, 
		version, 
		sex, 
		educlevel, 
		deduct = TRUE, 
		dcs = FALSE, 
		path = "N:\\dcs\\proj\\hledecomp\\results\\margins"){
	DatL   <- lapply(years, 
			get_data, 
			self = TRUE, 
			version = version, 
			sex = sex, 
			educlevel = educlevel,
			path = path)
	
	names(DatL) <- years
	
	prev <- do.call("rbind",lapply(DatL, function(X,deduct,dcs){
						prop    <- attr(X, "initprop")
						year    <- attr(X, "time")
						U       <- data_2_U(X)
						N       <- U2N(U, interval = 2)
						cind    <- rep(seq(50,112,by=2), 3) == age
						DF      <- as.data.frame(matrix(rowSums(N[,cind] %*% diag(prop)),ncol=3,dimnames=list(NULL,1:3)))
						DF$time <- year
						DF
					}, deduct = deduct, dcs = dcs))
	prev
}



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
