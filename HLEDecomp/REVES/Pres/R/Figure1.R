
# get new HMD data.

setwd("/home/tim/git/HLEDecomp/HLEDecomp")
library(reshape2)
library(data.table)
source("Code/R/Preamble.R")

#library(HMDHFDplus)
#mltper <- readHMDweb("USA","mltper_1x1",us,pw)
#fltper <- readHMDweb("USA","fltper_1x1",us,pw)
#mltper <- mltper[mltper$Age == 50 & mltper$Year >= 1992, ]
#fltper <- fltper[fltper$Age == 50 & fltper$Year >= 1992, ]
#hmdyrs <- unique(mltper$Year)
hmdyrs <- 1992:2016
#e50hmdm    <- mltper$ex
#e50hmdf    <- fltper$ex
e50hmdm    <- c(26.79, 26.68, 26.89, 27.01, 27.18, 27.39, 27.54, 27.61, 27.81, 
		28, 28.1, 28.29, 28.67, 28.72, 29.03, 29.25, 29.29, 29.59, 29.71, 
		29.8, 29.9, 29.89, 29.98, 29.95, 30.07)
e50hmdf    <- c(31.7, 31.48, 31.59, 31.58, 31.64, 31.73, 31.76, 31.66, 31.73, 
		31.84, 31.93, 32.05, 32.4, 32.42, 32.7, 32.92, 32.9, 33.24, 33.33, 
		33.36, 33.47, 33.48, 33.59, 33.53, 33.67)
# Author: tim
###############################################################################
#HMD <- local(get(load("/home/tim/git/DistributionTTD/DistributionTTD/Data/HMDltper.Rdata")))
#
#HMD <- HMD[HMD$CNTRY=="USA",]
#HMD <- HMD[HMD$Year >= 1992 & HMD$Age == 50, ]
# LE graph
#yrs <- 1992:2010

# LE HMD:

version <- "06"
mspec   <- paste0("mspec", version)
path1 <- file.path("Data","Results",mspec,"le","le.rds")
path2 <- file.path("Data","Results",mspec,"le","le_2.rds")
#hm <- get_rates_all(version="06",path="/home/tim/Data/hledecomp/results/")
le    <- readRDS(path1)
le2   <- readRDS(path2)

le$e50 <- rowSums(le[,c("1","2","3")])
le2$e50 <- rowSums(le2[,c("1","2")])

le2 <- data.table(le2)


em<-acast(le[le$sex == "m",],time~edu,value.var = "e50")
ef<-acast(le[le$sex == "f",],time~edu,value.var = "e50")


# need to wait to get HMD update before putting 
# this in a pdf and in the presentation
pdf("REVES/Pres/Figures/e50big.pdf")
par(mai = c(1, 1, 1, 2))
plot(NULL, type = "n", ylim = c(24, 38), xlim = c(1992, 2015), axes = FALSE, xlab = "", ylab = "")
lines(hmdyrs, e50hmdm, col = "blue",lwd=2,lty="9292")
lines(hmdyrs, e50hmdf, col = "red",lwd=2,lty="9292")
text(1993,e50hmdm[1],"HMD",pos=3,col="blue")
text(1993,e50hmdf[1],"HMD",pos=3,col="red")

#lines(c(1996, 2006, 2014), ef[, 1], lwd = 3, col = "red", lty = "8484")
#lines(c(1996, 2006, 2014), em[, 1], lwd = 3, col = "blue", lty = "8484")

axis(1)
axis(2, las = 1)

matplot(c(1996, 2006, 2014), em, add = TRUE, pch = 16, col = "blue", 
		type = "o", lty = "8484", lwd = c(3, 1, 1, 1), cex = c(1.5, 1, 1, 1))
matplot(c(1996, 2006, 2014), ef, add = TRUE, pch = 16, col = "red", 
		type = "o", lty = "8484", lwd = c(3, 1, 1, 1), cex = c(1.5, 1, 1, 1))

text(2014, ef[3, ], colnames(ef), pos = 4, col = "red", xpd = TRUE)
text(2014, em[3, ], colnames(em), pos = 4, col = "blue", xpd = TRUE)

rect(1996,ef[1,1],2006,ef[2,1],col="#FF000050",border="red")
rect(2006,ef[2,1],2014,ef[3,1],col="#FF000050",border="red")

rect(1996,em[1,1],2006,em[2,1],col="#0000FF50",border="blue")
rect(2006,em[2,1],2014,em[3,1],col="#0000FF50",border="blue")

text(c(1996,2006)+1,em[2:3,1]-.1,round(diff(em[,1]),2),pos=3,col = "blue",font=2)
text(c(1996,2006)+1,ef[2:3,1]-.1,round(diff(ef[,1]),2),pos=3,col = "red",font=2)
dev.off()

# strategy: layer plot into series of plots using foreground/background to tell the story.
# 1) HMD reference trend, then dim to back- mention gap decreasing
# 2) add all_edu lines (mention controls, etc, also that intermediate years and error bars forthcoming
# 3) add 3 more edu lines, dimming previous, point out gap larger and increasing
# 4) dim a lot, bringing all_edu to the fore as primary example.
# 5) switch to differences

# represent differences as jumps.

#fle <- le2[edu=="all_edu" & sex == "f"]
#mle <- le2[edu=="all_edu" & sex == "m"]

# simple depiction of difference.
# but ought to first show total bars.

barposneg <- function(mat){
	pos <- neg <- mat
	pos[pos<0] <- 0
	neg[neg >0] <- 0
	list(pos=pos,neg=neg)
}

years    <- c(1996,2006)
#path.i   <- file.path("Data", "Results", mspec, "dec", paste0("dec_", years[1],"_", years[2], ".rds"))
path2.i  <- file.path("Data", "Results", mspec, "dec", paste0("dec2_", "1996_2006.rds"))
path2.ii <- file.path("Data", "Results", mspec, "dec", paste0("dec2_","2006_2014.rds"))
dec2     <- rbind(readRDS(path2.i),readRDS(path2.ii))
dec2     <- data.table(dec2)
trcols   <- c("orange","red","forestgreen","purple")

dec_barplot_steps <- function(diffs, pnl, decbars, 
		statecols=c("palegreen2", "yellow1"),
		trcols = c("orange","red","forestgreen","purple"),...){
	
	leip            <- lein   <- matrix(0, nrow = 2, ncol = 7)
	leip[,c(2,6)]   <- pnl$pos
	lein[,c(2,6)]   <- pnl$neg
	
	led             <- rep(0, 7)
	led[c(1,5)]     <- colSums(diffs)
	
	decmp           <- decmn  <- matrix(0, nrow = 4, ncol = 7)
	decmp[,c(3,7)]  <- decbars$pos
	decmn[,c(3,7)]  <- decbars$neg
	
	barplot(led,space=0,width=1,las=1,...)
	barplot(leip,space=0,width=1,add=TRUE,axes=FALSE,col=statecols)
	barplot(lein,space=0,width=1,add=TRUE,axes=FALSE,col=statecols)
	barplot(decmp,space=0,width=1,add=TRUE,axes=FALSE,col=trcols)
	barplot(decmn,space=0,width=1,add=TRUE,axes=FALSE,col=trcols)
}

dec_step_prep <- function(le2,dec2,.sex,.edu){
	lei      <- le2[edu==.edu & sex == .sex]
	# diffs
	ledi     <- rbind(diff(lei$'1'),diff(lei$'2'))	
	leipn    <- barposneg(ledi)
	dec2     <- dec2[,sum(value),by=list(sex,edu,transition,year1)]
	dec2     <- dec2[edu == .edu & sex ==.sex]
	decbars  <- barposneg(acast(dec2,transition~year1,value.var="V1"))
	list(diffs=ledi,pnl=leipn,decbars=decbars)
}

dec_barplot_full <- function(le2,dec2,sex,edu,
		statecols=c("palegreen2", "yellow1"),
		trcols = c("orange","red","forestgreen","purple"), ...){
	req <- dec_step_prep(le2,dec2,.sex = sex,.edu = edu)
	dec_barplot_steps(req$diffs, req$pnl, req$decbars,statecols=statecols,trcols=trcols,...)
}

# these have already been a bit touched up. Maybe some more edits to do.
pdf("REVES/Pres/Figures/MalesDecAllEdu.pdf")
dec_barplot_full(le2,dec2,sex="m",edu="all_edu",ylim=c(-1,2))
title("males",cex=2)
text(c(1.5,5.5),-1,c("1996-2006","2006-2014"),xpd=TRUE,cex=1.5)
dev.off()
pdf("REVES/Pres/Figures/FemalesDecAllEdu.pdf")
dec_barplot_full(le2,dec2,sex="f",edu="all_edu",ylim=c(-1,2))
title("females",cex=2)
text(c(1.5,5.5),-1,c("1996-2006","2006-2014"),xpd=TRUE,cex=1.5)
dev.off()

pdf("REVES/Pres/Figures/MalesDec.pdf")
dec_barplot_full(le2,dec2,sex="m",edu="primary",ylim=c(-1,2))
title("males, low education",cex=2)
text(c(1.5,5.5),-1,c("1996-2006","2006-2014"),xpd=TRUE,cex=1.5)
dec_barplot_full(le2,dec2,sex="m",edu="secondary",ylim=c(-1,2))
title("males, middle education",cex=2)
text(c(1.5,5.5),-1,c("1996-2006","2006-2014"),xpd=TRUE,cex=1.5)
dec_barplot_full(le2,dec2,sex="m",edu="terciary",ylim=c(-1,2))
title("males, high education",cex=2)
text(c(1.5,5.5),-1,c("1996-2006","2006-2014"),xpd=TRUE,cex=1.5)
dev.off()

pdf("REVES/Pres/Figures/FemalesDec.pdf")
dec_barplot_full(le2,dec2,sex="f",edu="primary",ylim=c(-1,2))
title("females, low education",cex=2)
text(c(1.5,5.5),-1,c("1996-2006","2006-2014"),xpd=TRUE,cex=1.5)
dec_barplot_full(le2,dec2,sex="f",edu="secondary",ylim=c(-1,2))
title("females, medium education",cex=2)
text(c(1.5,5.5),-1,c("1996-2006","2006-2014"),xpd=TRUE,cex=1.5)
dec_barplot_full(le2,dec2,sex="f",edu="terciary",ylim=c(-1,2))
title("females, high education",cex=2)
text(c(1.5,5.5),-1,c("1996-2006","2006-2014"),xpd=TRUE,cex=1.5)
dev.off()



dec_barplot_steps(diffsf, pnf, debarsf,ylim=c(-1,2))

lei            <- matrix(0, nrow = 2, ncol = 7)
lei[,c(2,6)]   <- pnm$pos
led            <- rep(0, 7)
led[c(1,5)]    <- colSums(diffsm)
decmp          <- decmn  <- matrix(0, nrow = 4, ncol = 7)
decmp[,c(3,7)] <- decbars$pos[,,"m"]
decmn[,c(3,7)] <- decbars$neg[,,"m"]

barplot(led,space=0,width=1,ylim=c(-1,2),las=1)
barplot(lei,space=0,width=1,add=TRUE,axes=FALSE,col=statecols)
barplot(decmp,space=0,width=1,add=TRUE,axes=FALSE,col=trcols)
barplot(decmn,space=0,width=1,add=TRUE,axes=FALSE,col=trcols)

# combo bars


dec_bars <- function(le,dec,years=c(1996,2006,2014),.sex = "f",.edu = "all_edu"){
	cols <- c("orange","red","green","purple")
	names(cols)<- c("m12","m14","m21","m24")
	lekeep <- le2[sex==.sex & edu==.edu]
    decplot <- acast(dec[sex==.sex & edu==.edu],transition~year1,value.var="V1")
	# 3 time points, 2 spans.
	leplot <- as.matrix(lekeep[,c("1","2")])
	plot(NULL, type = "n", xlim = c(0, 6),ylim=c(0,35),axes=FALSE)
	barplot(t(leplot), beside = FALSE, add = TRUE, space = 1, width = 1)
	# figure out x spacing better.
	#barplot(decplot, beside = FALSE, add = TRUE, space = 1, width = 1)
	
}

dec2f <- dec2[sex=="f" & edu=="all_edu"]
head(dec2f)
sum(dec1$value)-sum(dec2$value)

# 1-2, 2-1
unique(dec.i$sex)
plot_dec2 <- function(dec2,
		.sex="f",
		.edu="all_edu",ylim = range(pretty(dec2$value)),...){
	dec2 <- as.data.frame(dec2)
	dec.i <- subset(dec2,sex==.sex & edu == .edu)

	ages <- unique(dec.i$age)
	yr1 <- unique(dec.i$year1)
	yr2 <- unique(dec.i$year2)
	main <- paste(sex, edu, paste0(yr1,"vs",yr2))
	plot(ages, dec.i$value[dec.i$transition == "m12"], ylim=ylim,type='l',col="orange",main=main)
	lines(ages, dec.i$value[dec.i$transition == "m14"],col="red")
	lines(ages, dec.i$value[dec.i$transition == "m21"],col="green")
	lines(ages, dec.i$value[dec.i$transition == "m24"],col="purple")
	legend("topright",lty=1,col=c("orange","red","green","purple"),legend=c("m12","m14","m21","m24"))
}

plot_dec2(dec2,
		.sex="m" , 
		.edu = "terciary")
