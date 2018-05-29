
# Author: tim
###############################################################################

barposneg <- function(mat){
	pos <- neg <- mat
	pos[pos<0] <- 0
	neg[neg >0] <- 0
	list(pos=pos,neg=neg)
}

# this now does two time intervals, with total, state breakdown, and a decomp of each
dec_barplot_steps <- function(diffs, pnl, decbarst, decbars1,decbars2,
		statecols=c("palegreen2", "yellow1"),
		trcols = c("orange","red","forestgreen","purple"),...){
	
	leip            <- lein   <- matrix(0, nrow = 2, ncol = 11)
	leip[,c(2,8)]   <- pnl$pos
	lein[,c(2,8)]   <- pnl$neg
	
	led             <- rep(0, 11)
	led[c(1,7)]     <- colSums(diffs)
	
	decmp           <- decmn  <- matrix(0, nrow = 4, ncol = 11)
	decmp[,c(3,9)]  <- decbarst$pos
	decmn[,c(3,9)]  <- decbarst$neg
	decmp[,c(4,10)] <- decbars1$pos
	decmn[,c(4,10)] <- decbars1$neg
	decmp[,c(5,11)] <- decbars2$pos
	decmn[,c(5,11)] <- decbars2$neg
	
	barplot(led,space=0,width=1,las=1,...)
	barplot(leip,space=0,width=1,add=TRUE,axes=FALSE,col=statecols)
	barplot(lein,space=0,width=1,add=TRUE,axes=FALSE,col=statecols)
	barplot(decmp,space=0,width=1,add=TRUE,axes=FALSE,col=trcols)
	barplot(decmn,space=0,width=1,add=TRUE,axes=FALSE,col=trcols)
}

# now gets total, state1 and state2
dec_step_prep <- function(le2,dec2,.sex,.edu){
	lei       <- le2[edu==.edu & sex == .sex]
	# diffs
	ledi      <- rbind(diff(lei$'1'),diff(lei$'2'))	
	leipn     <- barposneg(ledi)
	# remove the full span decomp
	ind       <- dec2$year1 == 1996 & dec2$year2 == 2014
	dec2      <- dec2[!ind]
	dec2      <- dec2[,sum(value),by=list(sex,edu,transition,year1,statedec)]
	dec2t     <- dec2[edu == .edu & sex ==.sex & statedec == 5]
	dec21     <- dec2[edu == .edu & sex ==.sex & statedec == 1]
	dec22     <- dec2[edu == .edu & sex ==.sex & statedec == 2]
	decbarst  <- barposneg(acast(dec2t,transition~year1,value.var="V1"))
	decbars1  <- barposneg(acast(dec21,transition~year1,value.var="V1"))
	decbars2  <- barposneg(acast(dec22,transition~year1,value.var="V1"))
	list(diffs=ledi,pnl=leipn,decbarst=decbarst,decbars1=decbars1,decbars2=decbars2)
}

dec_barplot_full <- function(le2,dec2,sex,edu,
		statecols=c("palegreen2", "yellow1"),
		trcols = c("orange","red","forestgreen","purple"), ...){
	req <- dec_step_prep(le2,dec2,.sex = sex,.edu = edu)
	dec_barplot_steps(req$diffs, req$pnl, req$decbarst,req$decbars1,req$decbars2, statecols=statecols,trcols=trcols,...)
}


