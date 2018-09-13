
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


# need to wait to get HMD update before putting 
# this in a pdf and in the presentation
pdf("Figures/e50big.pdf")
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

#matplot(c(1996, 2006, 2014), em, add = TRUE, pch = 16, col = "blue", 
#		type = "o", lty = "8484", lwd = c(3, 1, 1, 1), cex = c(1.5, 1, 1, 1))
#matplot(c(1996, 2006, 2014), ef, add = TRUE, pch = 16, col = "red", 
#		type = "o", lty = "8484", lwd = c(3, 1, 1, 1), cex = c(1.5, 1, 1, 1))
matplot(c(1996, 2006, 2014), em[,-3], add = TRUE, pch = 16, col = "blue", 
		type = "o", lty = "8484", lwd = c(3, 1, 1), cex = c(1.5, 1, 1))
matplot(c(1996, 2006, 2014), ef[,-3], add = TRUE, pch = 16, col = "red", 
		type = "o", lty = "8484", lwd = c(3, 1, 1), cex = c(1.5, 1, 1))
text(2014, ef[3, -3], colnames(ef)[-3], pos = 4, col = "red", xpd = TRUE)
text(2014, em[3, -3], colnames(em)[-3], pos = 4, col = "blue", xpd = TRUE)

#rect(1996,ef[1,1],2006,ef[2,1],col="#FF000050",border="red")
#rect(2006,ef[2,1],2014,ef[3,1],col="#FF000050",border="red")

#rect(1996,em[1,1],2006,em[2,1],col="#0000FF50",border="blue")
#rect(2006,em[2,1],2014,em[3,1],col="#0000FF50",border="blue")

text(c(1996,2006)+1,em[2:3,1]-.1,round(diff(em[,1]),2),pos=3,col = "blue",font=2)
text(c(1996,2006)+1,ef[2:3,1]-.1,round(diff(ef[,1]),2),pos=3,col = "red",font=2)
dev.off()