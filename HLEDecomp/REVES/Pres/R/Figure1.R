
# get new HMD data.

setwd("/home/tim/git/HLEDecomp/HLEDecomp")
# Author: tim
###############################################################################
HMD <- local(get(load("/home/tim/git/DistributionTTD/DistributionTTD/Data/HMDltper.Rdata")))

HMD <- HMD[HMD$CNTRY=="USA",]
HMD <- HMD[HMD$Year >= 1992 & HMD$Age == 50, ]
# LE graph
yrs <- 1992:2010

# LE HMD:
library(reshape2)
version <- "06"
mspec   <- paste0("mspec", version)
path1 <- file.path("Data","Results",mspec,"le","le.rds")
path2 <- file.path("Data","Results",mspec,"le","le_2.rds")
#hm <- get_rates_all(version="06",path="/home/tim/Data/hledecomp/results/")
le    <- readRDS(path1)
le2   <- readRDS(path2)

le$e50 <- rowSums(le[,c("1","2","3")])
le2$e50 <- rowSums(le2[,c("1","2")])

em<-acast(le[le$sex == "m",],time~edu,value.var = "e50")
ef<-acast(le[le$sex == "f",],time~edu,value.var = "e50")


plot(NULL, type = "n", ylim = c(24, 38), xlim = c(1992, 2015), axes = FALSE, xlab = "", ylab = "")
lines(yrs, HMD$ex[HMD$Sex == "m"], col = "blue")
lines(yrs, HMD$ex[HMD$Sex == "f"], col = "red")
axis(1)
axis(2, las = 1)

matplot(c(1996, 2006, 2014), em[, -1], add = TRUE, pch = 16:18, col = "blue")
matplot(c(1996, 2006, 2014), ef[, -1], add = TRUE, pch = 16:18, col = "red")

# edu gap bigger than sex gap. (6-7 yrs vs 3.5-5), wow.
# also edu gap increasing vs sex gap decreasing.
#em[,"terciary"] - em[,"primary"]
#ef[,1]-em[,1]
lelong <- melt(le,id.vars=c("sex","edu","time"))
ef <- acast(lelong[lelong$sex == "f" & lelong$variable!="e50", ], edu~variable~time)

years <- c(1996,2006)
path.i <- file.path("Data", "Results", mspec, "dec", paste0("dec_", years[1],"_", years[2], ".rds"))
path2.i <- file.path("Data", "Results", mspec, "dec", paste0("dec2_", years[1],"_", years[2], ".rds"))

dec1 <- readRDS(path.i)
dec2 <- readRDS(path2.i)

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
