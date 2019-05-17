
# Author: tim
###############################################################################

library(data.table)
#library(DemoDecomp)
#library(RColorBrewer)
#library(xtable)
library(compositions)
me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
}

source("Code/R/Functions.R")
library(data.table)
library(DemoDecomp)
library(RColorBrewer)
library(xtable)
library(compositions)
me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
}

source("Code/R/Functions.R")
source("Code/R/Functions_ALR.R")

TR    <- local(get(load("Data/Transitions/DCS/initdetail/TR_v06.Rdata")))
TR    <- data.table(TR)
# TR: had to fix colnames problem
PREV  <- TR[ , get_prev_dt(.SD), by = list(sex, edu, time)]
# TR2 just means that there are now TWO living states,
# healthy and disabled.
TR2   <- collapseTR(TR = TR, PREV = PREV)
setnames(TR2,c("m14","m24"),c("m13","m23"))


####################################################################
# CLR functions for a single subset (one edu,sex,year)             #
####################################################################
# for a single population, produce clr vec
get_vec_clr <- function(.SD,ntrans=2){
	.SD       <- as.data.frame(.SD)
	propnames <- paste0("s",1:ntrans,"_prop")
	
	# need to rescale prop too
	prop      <- unlist(.SD[1,propnames])
	prop      <- prop / sum(prop)
	prop_clr  <- unclass(clr(prop))
	
	colsall   <- getcolsall(ntrans, vec = FALSE)
	datall    <- as.matrix(.SD[,colsall])
	
	# TR: now flexible to ntrans
	CLR <- c(apply(colsall, 1, function(x, datall){
				clr(datall[,x])
			}, datall = datall))
	
	vecall    <- c(CLR, prop_clr)
	vecall
}

# for a single population, convert an alr vec
# into datall, which can be used by e50() etc


vec_clr_2_datall <- function(vec_clr, ntrans = 2){
	
	# get s1_prop, s2_prop
	n         <- length(vec_clr)
	prop      <- vec_clr[(n-ntrans+1):n]
	prop      <- unclass(clrInv(prop))
	names(prop) <- paste0("s", 1:ntrans, "_prop")
	
	
	vec_clr   <- vec_clr[1:(n - ntrans)]
	n         <- length(vec_clr)

	nc           <- (ntrans + 1) * ntrans

	dim(vec_clr) <- c(n / nc, ntrans + 1, ntrans)
	# needs to be generalized
	datall <- c(apply(vec_clr,3,clrInv))
	dim(datall) <- c(n / nc,nc)
			   
	# match column order from vec function
	colnames(datall) <- getcolsall(ntrans, vec = TRUE)
	
	list(datall=datall,prop=unclass(prop))
}

# calculate e50 from clr vec, single pop.
e50_vec_clr <- function(
		vec_clr, 
		to = 1, 
		age = 50, 
		ntrans = 2, 
		deduct = TRUE, 
		interval = 2, 
		dead = ntrans + 1){
	datall <- vec_clr_2_datall(vec_clr = vec_clr, ntrans = ntrans)
	#DAT, to, age = 50, prop, ntrans, deduct = TRUE, interval = 2, dead = "4"
	e50(DAT=datall$datall, 
			to=to, 
			age=age, prop=datall$prop, 
			ntrans=ntrans, deduct=deduct, interval=interval, dead=ntrans+1)
	
}

#e50_vec_clr( get_vec_clr(.SD),to=5)
#e50_vec_alr(get_vec_alr(.SD),to=5)
#####################################################################
# now a function to get vecall for each edu, ordered like this:     #
#                                                                   #
# vec_clr_prim, vec_clr_sec, vec_clr_terc, clr_edu                  #
# i.e. the disability radix separate for each edu group,            #
# so that we can split structure into edu vs disability parts.      #
#####################################################################
get_vec_edu_clr <- function(.SD, ntrans = 2){
	pri_clr      <- get_vec_clr(subset(.SD, edu == "primary"), ntrans = ntrans)
	sec_clr      <- get_vec_clr(subset(.SD, edu == "secondary"), ntrans = ntrans)
	ter_clr      <- get_vec_clr(subset(.SD, edu == "terciary"), ntrans = ntrans)
	
	eduprop      <- rowSums(subset(.SD, age == 50,  paste0("s", 1:ntrans, "_prop")))
	eduprop      <- eduprop / sum(eduprop)
	edu_clr      <- unclass(clr(eduprop))
	
	c(pri_clr, sec_clr, ter_clr, edu_clr)
	
}


vec_edu_clr_2_datall <- function(vec_edu_clr, ntrans = 2){
	n                <- length(vec_edu_clr)
	edu_clr          <- vec_edu_clr[(n - 2):n]
	eduprop          <- unclass(clrInv(edu_clr))
	
	vec_edu_clr      <- vec_edu_clr[1:(n - 3)]
	# turn to col format
	dim(vec_edu_clr) <- c(length(vec_edu_clr) / 3, 3)
	# each col is an edu, in order prim, sec, ter
	
	pri_datall <- vec_clr_2_datall(vec_edu_clr[,1])
	sec_datall <- vec_clr_2_datall(vec_edu_clr[,2])
	ter_datall <- vec_clr_2_datall(vec_edu_clr[,3])
	
	list(pri_datall = pri_datall,
			sec_datall = sec_datall,
			ter_datall = ter_datall,
			eduprop = eduprop)
}

# calculate an e50 from vec_edu_alr 
e50_vec_edu_clr <- function(
		vec_edu_clr, 
		to = 1, 
		age = 50, 
		ntrans = 2, 
		deduct = TRUE, 
		interval = 2, 
		dead = ntrans + 1){
	
	datall     <- vec_edu_clr_2_datall(vec_edu_clr = vec_edu_clr, ntrans = ntrans)
	
	pri_datall <- datall$pri_datall
	sec_datall <- datall$sec_datall
	ter_datall <- datall$ter_datall
	
	eduprop    <- datall$eduprop
	
	
	#DAT, to, age = 50, prop, ntrans, deduct = TRUE, interval = 2, dead = "4"
	e_edu <- c(e50(DAT = pri_datall$datall, 
					to = to, 
					age = age, 
					prop = pri_datall$prop, 
					ntrans = ntrans, 
					deduct = deduct, 
					interval = interval, 
					dead = ntrans + 1),
			e50(DAT = sec_datall$datall, 
					to = to, 
					age = age, 
					prop = sec_datall$prop, 
					ntrans = ntrans, 
					deduct = deduct, 
					interval = interval, 
					dead = ntrans + 1),
			e50(DAT = ter_datall$datall, 
					to = to, 
					age = age, 
					prop = ter_datall$prop, 
					ntrans = ntrans, 
					deduct = deduct, 
					interval = interval, 
					dead = ntrans + 1)
	)
	
	# weight sum
	sum(e_edu * eduprop)
}

##########################################################################
# a function that takes the whole TR2 object and decomposes with specified comparison

decomp_edu_clr <- function(
		TR,
		time1 = 2006, 
		time2 = 2014,
		sex = "f", 
		ntrans = 2,
		age = 50, 
		to = 5, 
		deduct = TRUE, 
		N = 20){
	Sex   <- sex
	T1    <- subset(TR, time == time1 & sex == Sex)
	T2    <- subset(TR, time == time2 & sex == Sex)
	
	pars1 <- get_vec_edu_clr(T1, ntrans = ntrans)
	pars2 <- get_vec_edu_clr(T2, ntrans = ntrans)
	
	dec.i <- DemoDecomp::horiuchi(
			func = e50_vec_edu_clr,
			pars1 = pars1,
			pars2 = pars2,
			N = N,
			age = age,
			to = to,
			deduct = deduct)
	dec.i
}


summary_decomp_edu_clr <- function(dec.i, ntrans = 2){
	n                <- length(dec.i)
	
	# hard coded to 3 edu groups:
	edu_comp         <- sum(dec.i[(n - 2):n])
	# remove elements	
	dec.i            <- dec.i[1:(n - 3)]
	# redim for vec results per edu ( disability component at bottom)
	dim(dec.i)       <- c(length(dec.i) / 3, 3)
	
	# sum disability component, cut down again
	nr               <- nrow(dec.i)
	dis_comp         <- sum(dec.i[(nr - ntrans + 1):nr, ])
	dec.i            <- dec.i[1:(nr - ntrans), ]
	
	# redim to 3d array, age x trans x edu

	
	n                <- nrow(dec.i)
	nt               <- (ntrans+1)*ntrans
	colsi            <- getcolsall(ntrans, vec = TRUE)
	arrows           <- rep(0, nt)
	names(arrows)    <- colsi
    
	# edu loop
	for (i in 1:3){
		vec_i           <- dec.i[, i]
		dim(vec_i)      <- c(n / nt, nt)
		colnames(vec_i) <- colsi
		arrows          <- arrows + colSums(vec_i)
	}
	c(arrows, disab = dis_comp, edu = edu_comp)
}

# now do all decomps, 
wrapper_CLR <- function(TR, to = 5, sex = "m", time1 = 1996, time2 = 2006, age = 50, N = 20){
	dec.i <- decomp_edu_clr(TR, time1 = time1, time2 = time2, sex = sex,
			age = 50, to = to, deduct = TRUE, N = N, ntrans = 2)
	summary_decomp_edu_clr(dec.i, ntrans = 2)
}


