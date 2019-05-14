
# Author: tim
###############################################################################

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

####################################################################
# ILR functions for a single subset (one edu,sex,year)             #
####################################################################
# for a single population, produce an ilr vec
get_vec_ilr <- function(.SD,ntrans=2){
	.SD       <- as.data.frame(.SD)
	propnames <- paste0("s",1:ntrans,"_prop")
	
	# need to rescale prop too
	prop      <- unlist(.SD[1,propnames])
	prop      <- prop / sum(prop)
	prop_ilr  <- ilr(prop)
	
	colsall   <- getcolsall(ntrans)
	datall    <- as.matrix(.SD[,colsall])
	# column order doesn't matter
	ILR       <- cbind(ilr(datall[,c("m12","m13","m11")]),
			ilr(datall[,c("m21","m23","m22")]))
	
	vecall    <- c(ILR, prop_ilr)
	vecall
}

# for a single population, convert an ilr vec
# into datall, which can be used by e50() etc

vec_ilr_2_datall <- function(vec_ilr,ntrans=2){
	
	# get s1_prop, s2_prop
	n         <- length(vec_ilr)
	prop      <- vec_ilr[n]
	prop      <- ilrInv(prop)
	names(prop) <- paste0("s", 1:ntrans, "_prop")
	
	
	vec_ilr   <- vec_ilr[-n]
	n         <- n - 1
	
	# these are hard coded to be m12,m13,m11,m21,m23,m22
	dim(vec_ilr) <- c(n/(ntrans^2),ntrans^2)
	
	datall    <- cbind(ilrInv(vec_ilr[,1:2]),
			ilrInv(vec_ilr[,3:4]))
	colnames(datall) <- c("m12","m13","m11","m21","m23","m22")
	datall    <- datall[,getcolsall(ntrans)]
	list(datall=datall,prop=unclass(prop))
}

# calculate e50 from ilr vec, single pop.
e50_vec_ilr <- function(vec_ilr, to=1, age = 50, ntrans = 2, deduct = TRUE, interval = 2, dead = ntrans+1){
	datall <- vec_ilr_2_datall(vec_ilr = vec_ilr, ntrans=ntrans)
	#DAT, to, age = 50, prop, ntrans, deduct = TRUE, interval = 2, dead = "4"
	e50(DAT=datall$datall, 
			to=to, 
			age=age, prop=datall$prop, 
			ntrans=ntrans, deduct=deduct, interval=interval, dead=ntrans+1)
	
}

#####################################################################
# now a function to get vecall for each edu, ordered like this:     #
#                                                                   #
# vec_ilr_prim, vec_ilr_sec, vec_ilr_terc, ilr_edu                  #
# i.e. the disability radix separate for each edu group,            #
# so that we can split structure into edu vs disability parts.      #
#####################################################################
get_vec_edu_ilr <- function(.SD, ntrans=2){
	pri_ilr      <- get_vec_ilr(subset(.SD,edu == "primary"),ntrans=ntrans)
	sec_ilr      <- get_vec_ilr(subset(.SD,edu == "secondary"),ntrans=ntrans)
	ter_ilr      <- get_vec_ilr(subset(.SD,edu == "terciary"),ntrans=ntrans)
	
	eduprop      <- rowSums(.SD[age == 50, c("s1_prop", "s2_prop")])
	eduprop      <- eduprop / sum(eduprop)
	edu_ilr      <- ilr(eduprop)
	edu_ilr      <- unclass(edu_ilr)
	
	c(pri_ilr, sec_ilr, ter_ilr, edu_ilr)
	
}


vec_edu_ilr_2_datall <- function(vec_edu_ilr, ntrans=2){
	n                <- length(vec_edu_ilr)
	edu_ilr          <- vec_edu_ilr[(n-1):n]
	eduprop          <- unclass(ilrInv(edu_ilr))
	
	vec_edu_ilr      <- vec_edu_ilr[1:(n-2)]
	# turn to col format
	dim(vec_edu_ilr) <- c(length(vec_edu_ilr)/3,3)
	# each col is an edu, in order prim, sec, ter
	
	pri_datall <- vec_ilr_2_datall(vec_edu_ilr[,1])
	sec_datall <- vec_ilr_2_datall(vec_edu_ilr[,2])
	ter_datall <- vec_ilr_2_datall(vec_edu_ilr[,3])
	
	list(pri_datall = pri_datall,
			sec_datall = sec_datall,
			ter_datall = ter_datall,
			eduprop = eduprop)
}

# calculate an e50 from vec_edu_ilr 
e50_vec_edu_ilr <- function(vec_edu_ilr, to=1, age = 50, ntrans = 2, deduct = TRUE, interval = 2, dead = ntrans+1){
	datall     <- vec_edu_ilr_2_datall(vec_edu_ilr = vec_edu_ilr, ntrans = ntrans)
	
	pri_datall <- datall$pri_datall
	sec_datall <- datall$sec_datall
	ter_datall <- datall$ter_datall
	
	eduprop    <- datall$eduprop
	
	
	#DAT, to, age = 50, prop, ntrans, deduct = TRUE, interval = 2, dead = "4"
	e_edu <- c(e50(DAT=pri_datall$datall, 
					to=to, 
					age=age, 
					prop=pri_datall$prop, 
					ntrans=ntrans, deduct=deduct, interval=interval, dead=ntrans+1),
			e50(DAT=sec_datall$datall, 
					to=to, 
					age=age, 
					prop=sec_datall$prop, 
					ntrans=ntrans, deduct=deduct, interval=interval, dead=ntrans+1),
			e50(DAT=ter_datall$datall, 
					to=to, 
					age=age, 
					prop=ter_datall$prop, 
					ntrans=ntrans, deduct=deduct, interval=interval, dead=ntrans+1)
	)
	
	# weight sum
	sum(e_edu * eduprop)
}

##########################################################################
# a function that takes the whole TR2 object and decomposes with specified comparison

decomp_edu_ilr <- function(TR,
		time1=2006,time2=2014,
		sex="f",ntrans=2,
		age=50, to=5, 
		deduct = TRUE,N=20){
	Sex <- sex
	T1   <- subset(TR,time == time1 & sex == Sex)
	T2   <- subset(TR,time == time2 & sex == Sex)
	
	pars1 <- get_vec_edu_ilr(T1, ntrans=ntrans)
	pars2 <- get_vec_edu_ilr(T2, ntrans=ntrans)
	
	dec.i <- DemoDecomp::horiuchi(func=e50_vec_edu_ilr,
			pars1=pars1,
			pars2=pars2,
			N=N,
			age=age,to=to,deduct=deduct)
	dec.i
}


summary_decomp_edu_ilr <- function(dec.i,ntrans=2){
	n                <- length(dec.i)
	edu_comp         <- sum(dec.i[(n-1):n])
	
	dec.i            <- dec.i[1:(n-2)]
	# redim
	dim(dec.i)       <- c(length(dec.i)/3,3)
	
	dis_comp         <- sum(dec.i[nrow(dec.i)])
	
	dec.i            <- dec.i[-nrow(dec.i),]
	
	n                <- nrow(dec.i)
	
	arrows           <- rep(0,4)
	names(arrows)    <- c("m12","m13","m21","m23")
	for (i in 1:3){
		vec_i           <- dec.i[,i]
		dim(vec_i)      <- c(n/(ntrans^2),ntrans^2)
		colnames(vec_i) <- c("m12","m13","m21","m23")
		arrows          <- arrows + colSums(vec_i)
	}
	c(arrows, disab = dis_comp, edu = edu_comp)
}
