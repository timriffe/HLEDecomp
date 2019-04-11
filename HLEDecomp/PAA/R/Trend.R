# Author: tim
###############################################################################

library(data.table)
library(DemoDecomp)
library(RColorBrewer)
library(xtable)
me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
}

source("Code/R/Functions.R")
source("PAA/R/FunctionsForPAA.R")

TR    <- local(get(load("Data/Transitions/DCS/initdetail/TR_v06.Rdata")))
TR    <- data.table(TR)
# TR: had to fix colnames problem
PREV  <- TR[ , get_prev_dt(.SD), by = list(sex, edu, time)]

# TR2 just means that there are now TWO living states,
# healthy and disabled.
TR2   <- collapseTR(TR = TR, PREV = PREV)
setnames(TR2,c("m14","m24"),c("m13","m23"))


LEs <- TR2[, list(DFLE = f_dec_rescale_all_edu_dt(.SD,age=50,to=1),
		   DLE = f_dec_rescale_all_edu_dt(.SD,age=50,to=2),
		   LE = f_dec_rescale_all_edu_dt(.SD,age=50,to=5)),by=list(sex,time)]

fle <- LEs[sex=="f"]
mle <- LEs[sex=="m"]
yrs <- c(1996,2006,2014)
plot(yrs, fle$LE, ylim = c(25, 35),type="o")
points(yrs, fle$DFLE)
lines(1996:2014,flt$ex[flt$Age==51 & flt$Year >= 1996 & flt$Year <= 2014])
lines(yrs, mle$LE)

#library(HMDHFDplus)
#flt <- readHMDweb("USA", "fltper_1x1", us, pw)
#mlt <- readHMDweb("USA", "mltper_1x1", us, pw)

barsubset <- function(chunk,ymax = 40,col=gray(c(.8,.4))){
	dat           <- t(as.matrix(chunk[,c("DFLE","DLE")]))
	colnames(dat) <-  c(1996,2006,2014)
	barplot(dat, col = col, ylim=c(0,ymax),border=NA,axes=FALSE)
	abline(h=seq(5,35,by=5),col="white")
	axis(2,las=1)
}

reds <- RColorBrewer::brewer.pal(3,"Reds")[2:3]
blues <- RColorBrewer::brewer.pal(3,"Blues")[2:3]

#
pdf("PAA/Pres/Figures/bar_males.pdf", height=5,width=2.2)
par(mai=c(.5,.5,.3,0))
barsubset(mle,col=blues)
dev.off()

pdf("PAA/Pres/Figures/bar_females.pdf", height=5,width=2.2)
par(mai=c(.5,.5,.3,0))
barsubset(fle,col=reds)
dev.off()
#lines(1996:2014,flt$ex[flt$Age==48 & flt$Year >= 1996 & flt$Year <=2014])


bardiffs <- function(chunk,ylim = c(-.5,2),col=gray(c(.8,.4))){
	chunk           <- as.matrix(chunk[,c("DFLE","DLE")])
	chunk           <- t(diff(chunk))
	colnames(chunk) <- c("1996-2006","2006-2014")
	
	neg <- pos    <- chunk
	pos[pos < 0]  <- 0
	neg[neg > 0]  <- 0
	
	barplot(pos, col = col, ylim=ylim,border=NA,axes=FALSE)
	barplot(neg, col = col, ylim=ylim,border=NA,axes=FALSE,add=TRUE)
	axis(2,las=1)
}

pdf("PAA/Pres/Figures/bar_diffs_males.pdf", height=4,width=2.2)
par(mai=c(.5,.5,.3,0))
bardiffs(mle,col=blues)
dev.off()


pdf("PAA/Pres/Figures/bar_diffs_females.pdf", height=4,width=2.2)
par(mai=c(.5,.5,.3,0))
bardiffs(fle,col=reds)
dev.off()

barplot(dat, col = col, ylim=c(0,ymax),border=NA,axes=FALSE)






