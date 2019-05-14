
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
# ALR functions for a single subset (one edu,sex,year)             #
####################################################################
# for a single population, produce an alr vec
get_vec_alr_d <- function(.SD,ntrans=2){
	.SD       <- as.data.frame(.SD)
	propnames <- paste0("s",1:ntrans,"_prop")
	
	# need to rescale prop too
	prop      <- unlist(.SD[1,propnames])
	prop      <- prop / sum(prop)
	prop_alr  <- alr(prop)
	
	colsall   <- getcolsall(ntrans)
	datall    <- as.matrix(.SD[,colsall])
	# TR changed so death in denom
	ALR       <- cbind(alr(datall[,c("m12","m11","m13")]),
			           alr(datall[,c("m21","m22","m23")]))
	
	vecall    <- c(ALR, prop_alr)
	vecall
}
# for a single population, convert an alr vec
# into datall, which can be used by e50() etc

vec_alr_2_datall_d <- function(vec_alr_d,ntrans=2){
	
	# get s1_prop, s2_prop
	n         <- length(vec_alr_d)
	prop      <- vec_alr_d[n]
	prop      <- alrInv(prop)
	names(prop) <- paste0("s", 1:ntrans, "_prop")
	
	
	vec_alr_d <- vec_alr_d[-n]
	n         <- n - 1
	
	# these are hard coded to be m12,m13,m21,m23
	dim(vec_alr_d) <- c(n/(ntrans^2),ntrans^2)
	
	datall    <- cbind(alrInv(vec_alr_d[,1:2]),
			alrInv(vec_alr_d[,3:4]))
	colnames(datall) <- c("m12","m11","m13","m21","m22","m23")
	datall    <- datall[,getcolsall(ntrans)]
	list(datall=datall,prop=unclass(prop))
}

# calculate e50 from alr vec, single pop.
 e50_vec_alr_d <- function(vec_alr_d, to=1, age = 50, ntrans = 2, deduct = TRUE, interval = 2, dead = ntrans+1){
	datall_d <- vec_alr_2_datall_d(vec_alr_d = vec_alr_d, ntrans=ntrans)
	#DAT, to, age = 50, prop, ntrans, deduct = TRUE, interval = 2, dead = "4"
	e50(DAT=datall_d$datall, 
			to=to, 
			age=age, prop=datall_d$prop, 
			ntrans=ntrans, deduct=deduct, interval=interval, dead=ntrans+1)
	
}

#####################################################################
# now a function to get vecall for each edu, ordered like this:     #
#                                                                   #
# vec_alr_prim, vec_alr_sec, vec_alr_terc, alr_edu                  #
# i.e. the disability radix separate for each edu group,            #
# so that we can split structure into edu vs disability parts.      #
#####################################################################
get_vec_edu_alr_d <- function(.SD, ntrans=2){
	pri_alr      <- get_vec_alr_d(subset(.SD,edu == "primary"),ntrans=ntrans)
	sec_alr      <- get_vec_alr_d(subset(.SD,edu == "secondary"),ntrans=ntrans)
	ter_alr      <- get_vec_alr_d(subset(.SD,edu == "terciary"),ntrans=ntrans)

	eduprop      <- rowSums(.SD[age == 50, c("s1_prop", "s2_prop")])
	eduprop      <- eduprop / sum(eduprop)
	edu_alr      <- alr(eduprop)
	edu_alr      <- unclass(edu_alr)
	
	c(pri_alr, sec_alr, ter_alr, edu_alr)
	
}


vec_edu_alr_2_datall_d <- function(vec_edu_alr_d, ntrans=2){
	n                <- length(vec_edu_alr_d)
	edu_alr          <- vec_edu_alr_d[(n-1):n]
	eduprop          <- unclass(alrInv(edu_alr))
	
	vec_edu_alr_d      <- vec_edu_alr_d[1:(n-2)]
	# turn to col format
	dim(vec_edu_alr_d) <- c(length(vec_edu_alr_d)/3,3)
	# each col is an edu, in order prim, sec, ter
	
	pri_datall <- vec_alr_2_datall_d(vec_edu_alr_d[,1])
	sec_datall <- vec_alr_2_datall_d(vec_edu_alr_d[,2])
	ter_datall <- vec_alr_2_datall_d(vec_edu_alr_d[,3])
	
	list(pri_datall = pri_datall,
			sec_datall = sec_datall,
			ter_datall = ter_datall,
			eduprop = eduprop)
}

# calculate an e50 from vec_edu_alr 
e50_vec_edu_alr_d <- function(vec_edu_alr_d, to=1, age = 50, ntrans = 2, deduct = TRUE, interval = 2, dead = ntrans+1){
	datall     <- vec_edu_alr_2_datall_d(vec_edu_alr_d = vec_edu_alr_d, ntrans = ntrans)
	
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

decomp_edu_alr_d <- function(TR,
		time1=2006,time2=2014,
		sex="f",ntrans=2,
		age=50, to=5, 
		deduct = TRUE,N=20){
    Sex <- sex
	T1   <- subset(TR,time == time1 & sex == Sex)
	T2   <- subset(TR,time == time2 & sex == Sex)
	
	pars1 <- get_vec_edu_alr_d(T1, ntrans=ntrans)
	pars2 <- get_vec_edu_alr_d(T2, ntrans=ntrans)
	                              
	dec.i <- DemoDecomp::horiuchi(func=e50_vec_edu_alr_d,
			                      pars1=pars1,
								  pars2=pars2,
								  N=N,
			                      age=age,to=to,deduct=deduct)
	dec.i
}


summary_decomp_edu_alr_d <- function(dec.i,ntrans=2){
	n                <- length(dec.i)
	edu_comp         <- sum(dec.i[(n-1):n])
	
	dec.i            <- dec.i[1:(n-2)]
	# redim
	dim(dec.i)       <- c(length(dec.i)/3,3)
	
	dis_comp         <- sum(dec.i[nrow(dec.i)])
	
	dec.i            <- dec.i[-nrow(dec.i),]
	
	n                <- nrow(dec.i)
	
	arrows           <- rep(0,4)
	names(arrows)    <- c("m12","m11","m21","m22")
	for (i in 1:3){
		vec_i           <- dec.i[,i]
		dim(vec_i)      <- c(n/(ntrans^2),ntrans^2)
		colnames(vec_i) <- c("m12","m11","m21","m22")
		arrows          <- arrows + colSums(vec_i)
	}
    c(arrows, disab = dis_comp, edu = edu_comp)
}

# now do all decomps, 
