
# get new HMD data.

setwd("/home/tim/git/HLEDecomp/HLEDecomp")
library(reshape2)
library(data.table)
# Author: tim
###############################################################################
HMD <- local(get(load("/home/tim/git/DistributionTTD/DistributionTTD/Data/HMDltper.Rdata")))

HMD <- HMD[HMD$CNTRY=="USA",]
HMD <- HMD[HMD$Year >= 1992 & HMD$Age == 50, ]
# LE graph
yrs <- 1992:2010

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
par(mai = c(1, 1, 1, 2))
plot(NULL, type = "n", ylim = c(24, 38), xlim = c(1992, 2015), axes = FALSE, xlab = "", ylab = "")
lines(yrs, HMD$ex[HMD$Sex == "m"], col = "blue",lwd=2,lty="9292")
lines(yrs, HMD$ex[HMD$Sex == "f"], col = "red",lwd=2,lty="9292")
text(1993,HMD$ex[HMD$Sex == "m"][1],"HMD",pos=3,col="blue")
text(1993,HMD$ex[HMD$Sex == "f"][1],"HMD",pos=3,col="red")

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

# strategy: layer plot into series of plots using foreground/background to tell the story.
# 1) HMD reference trend, then dim to back- mention gap decreasing
# 2) add all_edu lines (mention controls, etc, also that intermediate years and error bars forthcoming
# 3) add 3 more edu lines, dimming previous, point out gap larger and increasing
# 4) dim a lot, bringing all_edu to the fore as primary example.
# 5) switch to differences

# represent differences as jumps.

fle <- le2[edu=="all_edu" & sex == "f"]
mle <- le2[edu=="all_edu" & sex == "m"]

# simple depiction of difference.
# but ought to first show total bars.

barposneg <- function(mat){
	pos <- neg <- mat
	pos[pos<0] <- 0
	neg[neg >0] <- 0
	list(pos=pos,neg=neg)
}

diffsm <- rbind(diff(mle$'1'),diff(mle$'2'))
diffsf <- rbind(diff(fle$'1'),diff(fle$'2'))

pnm <- barposneg(diffsm)
pnf <- barposneg(diffsf)

statecols <- c("palegreen2"  ,"yellow1")
# make border thicker
barplot(pnm$pos,ylim=c(-1,2),space=1,las=1,col=statecols)
rect(c(1,3),0,c(2,4),colSums(diffsm),lwd=2)

# make border thicker
barplot(pnf$pos,ylim=c(-1,2),space=1, las = 1)
barplot(pnf$neg,add=TRUE,space=1, axes=FALSE)
rect(c(1,3),0,c(2,4),colSums(diffsf),lwd=2)
#plot(c(1996,2006,2014),ef[,1],type="S")
# edu gap bigger than sex gap. (6-7 yrs vs 3.5-5), wow.
# also edu gap increasing vs sex gap decreasing.
#em[,"terciary"] - em[,"primary"]
#ef[,1]-em[,1]
#lelong   <- melt(le,id.vars=c("sex","edu","time"))
#ef       <- acast(lelong[lelong$sex == "f" & lelong$variable!="e50", ], edu~variable~time)

years    <- c(1996,2006)
path.i   <- file.path("Data", "Results", mspec, "dec", paste0("dec_", years[1],"_", years[2], ".rds"))
path2.i  <- file.path("Data", "Results", mspec, "dec", paste0("dec2_", years[1],"_", years[2], ".rds"))
path2.ii <- file.path("Data", "Results", mspec, "dec", paste0("dec2_","2006_2014.rds"))

dec1     <- readRDS(path.i)
dec2     <- rbind(readRDS(path2.i),readRDS(path2.ii))

# aggregate over age
dec2     <- data.table(dec2)
dec2     <- dec2[,sum(value),by=list(sex,edu,transition,year1)]
dec2     <- dec2[edu == "all_edu" & sex != "b"]
decbars  <- barposneg(acast(dec2,transition~year1~sex))
trcols   <- c("orange","red","forestgreen","purple")
# change these to final colors used in state space arrows.
# females
barplot(decbars$pos[,,1],ylim=c(-1,2),col=trcols,space=1,las=1)
barplot(decbars$neg[,,1],add=TRUE,col=trcols,space=1,axes=FALSE)
rect(c(1,3),0,c(2,4),colSums(diffsf),lwd=2)

# males
barplot(decbars$pos[,,2],ylim=c(-1,2),col=trcols,space=1,las=1)
barplot(decbars$neg[,,2],add=TRUE,col=trcols,space=1,axes=FALSE)
#rect(c(1,3),0,c(2,4),colSums(diffsm),lwd=2)

# to get them all together would have to interleave with 0s?

# function assumes two decomp periods only
debarsm <- list(pos = decbars$pos[,,"m"],
		neg = decbars$neg[,,"m"])
debarsf <- list(pos = decbars$pos[,,"f"],
		neg = decbars$neg[,,"f"])

# get this to include data prep?

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


dec_barplot_steps(diffsm, pnm, debarsm,ylim=c(-1,2))
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
