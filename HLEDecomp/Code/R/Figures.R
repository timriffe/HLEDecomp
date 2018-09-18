
# this script is for figures in abstract and or paper

me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
}
source("Code/R/Functions.R")
source("Code/R/Preamble.R")
#source("REVES/Pres/R/barplotfunctions.R")

library(reshape2)
library(data.table)

# colors
hrs_male_col <- "#053a8e"
hmd_male_col <- "#88aeea"

hrs_fem_col  <- "#a50847"
hmd_fem_col  <- "#ed9ebe"

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
hmdn <- length(hmdyrs)
hmdi <- 1:hmdn > 3

(maleinc <- e50hmdm[hmdn] - e50hmdm[hmdyrs==1996])
(femaleinc <- e50hmdf[hmdn] - e50hmdf[hmdyrs==1996])
(maleinc / e50hmdm[hmdyrs==1996] * 100)
(femaleinc / e50hmdf[hmdyrs==1996] * 100)

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
path1   <- file.path("Data","Results",mspec,"le","le.rds")
path2   <- file.path("Data","Results",mspec,"le","le_2.rds")
#hm <- get_rates_all(version="06",path="/home/tim/Data/hledecomp/results/")
le      <- readRDS(path1)
le2     <- readRDS(path2)

le$e50  <- rowSums(le[,c("1","2","3")])
le2$e50 <- rowSums(le2[,c("1","2")])

le2     <- data.table(le2)


em      <- acast(le[le$sex == "m",],time~edu,value.var = "e50")
ef      <- acast(le[le$sex == "f",],time~edu,value.var = "e50")

colnames(em) <- colnames(ef) <- c("HRS All edu","HRS low edu","high school","HRS high edu")

# need to wait to get HMD update before putting 
# this in a pdf and in the presentation
pdf("Figures/e50_R.pdf")
par(mai = c(1, 1, 1, 2),xpd=TRUE)
plot(NULL, type = "n", ylim = c(24, 38), xlim = c(1992, 2015), axes = FALSE, xlab = "", ylab = "")
lines(hmdyrs[hmdi], e50hmdm[hmdi], col = hmd_male_col,lwd=2,lty="9292")
lines(hmdyrs[hmdi], e50hmdf[hmdi], col = hmd_fem_col,lwd=2,lty="9292")
text(hmdyrs[hmdn],e50hmdm[hmdn],"HMD males",pos=4)
text(hmdyrs[hmdn],e50hmdf[hmdn],"HMD females",pos=4)

#lines(c(1996, 2006, 2014), ef[, 1], lwd = 3, col = "red", lty = "8484")
#lines(c(1996, 2006, 2014), em[, 1], lwd = 3, col = "blue", lty = "8484")

axis(1)
axis(2, las = 1)

#matplot(c(1996, 2006, 2014), em, add = TRUE, pch = 16, col = "blue", 
#		type = "o", lty = "8484", lwd = c(3, 1, 1, 1), cex = c(1.5, 1, 1, 1))
#matplot(c(1996, 2006, 2014), ef, add = TRUE, pch = 16, col = "red", 
#		type = "o", lty = "8484", lwd = c(3, 1, 1, 1), cex = c(1.5, 1, 1, 1))
matplot(c(1996, 2006, 2014), em[,-3], add = TRUE, pch = 16, col = hrs_male_col, 
		type = "o", lty = "8484", lwd = c(3, 1, 1), cex = c(1.5, 1, 1))
matplot(c(1996, 2006, 2014), ef[,-3], add = TRUE, pch = 16, col =hrs_fem_col, 
		type = "o", lty = "8484", lwd = c(3, 1, 1), cex = c(1.5, 1, 1))
text(2014, ef[3, -3], colnames(ef)[-3], pos = 4, col = hrs_fem_col, xpd = TRUE)
text(2014, em[3, -3], colnames(em)[-3], pos = 4, col = hrs_male_col, xpd = TRUE)

#rect(1996,ef[1,1],2006,ef[2,1],col="#FF000050",border="red")
#rect(2006,ef[2,1],2014,ef[3,1],col="#FF000050",border="red")

#rect(1996,em[1,1],2006,em[2,1],col="#0000FF50",border="blue")
#rect(2006,em[2,1],2014,em[3,1],col="#0000FF50",border="blue")

#text(c(1996,2006)+1,em[2:3,1]-.1,round(diff(em[,1]),2),pos=3,col = "blue",font=2)
#text(c(1996,2006)+1,ef[2:3,1]-.1,round(diff(ef[,1]),2),pos=3,col = "red",font=2)
dev.off()

le2 <- le2[le2$sex != "b",]
dflem      <- acast(le2[le2$sex == "m",],time~edu,value.var = "1")
dflef      <- acast(le2[le2$sex == "f",],time~edu,value.var = "1")

dlem       <- acast(le2[le2$sex == "m",],time~edu,value.var = "2")
dlef       <- acast(le2[le2$sex == "f",],time~edu,value.var = "2")

dflem <- cbind(dflem,state=1) 
dflef <- cbind(dflef,state=1) 
dlem  <- cbind(dlem,state=2) 
dlef  <- cbind(dlef,state=2) 
 

dlef[,1:4] / ef
#dflem / em
#dflef / ef
chunk <- le2[le2$sex == "f" & le2$edu == "all_edu", ]
barsubset <- function(chunk,ymax = 40){
	dat           <- t(as.matrix(chunk[,c("1","2")]))
	colnames(dat) <-  c(1996,2006,2014)
	barplot(dat, col = c(gray(c(.8,.4))), ylim=c(0,ymax),border=NA,axes=FALSE)
	abline(h=seq(5,35,by=5),col="white")
	axis(2,las=1)
}

pdf("Figures/bar_fem_all.pdf", height=5,width=2.2)
par(mai=c(.5,.5,.3,0))
barsubset(subset(le2,sex=="f" & edu == "all_edu"))
dev.off()

pdf("Figures/bar_fem_pri.pdf", height=5,width=2.2)
par(mai=c(.5,.5,.3,0))
barsubset(subset(le2,sex=="f" & edu == "primary"))
dev.off()

pdf("Figures/bar_fem_uni.pdf", height=5,width=2.2)
par(mai=c(.5,.5,.3,0))
barsubset(subset(le2,sex=="f" & edu == "terciary"))
dev.off()

pdf("Figures/bar_male_all.pdf", height=5,width=2.2)
par(mai=c(.5,.5,.3,0))
barsubset(subset(le2,sex=="m" & edu == "all_edu"))
dev.off()

pdf("Figures/bar_male_pri.pdf", height=5,width=2.2)
par(mai=c(.5,.5,.3,0))
barsubset(subset(le2,sex=="m" & edu == "primary"))
dev.off()

pdf("Figures/bar_male_uni.pdf", height=5,width=2.2)
par(mai=c(.5,.5,.3,0))
barsubset(subset(le2,sex=="m" & edu == "terciary"))
dev.off()

dflem      <- acast(le2[le2$sex == "m",],time~edu,value.var = "1")
dflef      <- acast(le2[le2$sex == "f",],time~edu,value.var = "1")

dlem       <- acast(le2[le2$sex == "m",],time~edu,value.var = "2")
dlef       <- acast(le2[le2$sex == "f",],time~edu,value.var = "2")

.91 / .21
.47/.11

