# this script is to produce bulk figures for internal consumption

me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
}
source("Code/R/Functions.R")
version    <- "01"
sex        <- "m" # "m","f",or"b"
educlevel  <- "0.All edu"
# let sex recode

file.name  <- paste0(paste("dec",version,sex,educlevel,N,sep="_"),".Rdata")
path       <- file.path("Data","Results",paste0("mspec",version))

dec.i      <- local(get(load(file.path(path, file.name))))

# ------------------------------------------------------
# MARGINS PLOTS
sets          <- paste(dec.i$year1,dec.i$year2)
code          <- unique(sets)
recvec        <- 1:length(code)
names(recvec) <- code
dec.i$decnr   <- recvec[sets]
# transition margins
trmargins     <- acast(dec.i, transition ~ state ~ decnr, sum, value.var = "value")

barmargins <- function(dec.i){
	trmargins     <- acast(dec.i, transition ~ state ~ decnr, sum, value.var = "value")
	
	
	trp <- trn <-trmargins
	trp[trp < 0] <- NA
	trn[trn > 0] <- NA
	
	ylim <- c(min(apply(trn,3,rowSums,na.rm=TRUE)), max(apply(trp,3,rowSums,na.rm=TRUE)))
	barplot(t(trp[,,1]), ylim = ylim, legend.text=c("HLE","ADL1","ADL2p"), main = "1995 vs 2004, Males All edu",
			ylab = "contribution to difference in e50")
	barplot(t(trn[,,1]),add=TRUE)
	
# 1995 vs 2014
	barplot(t(trp[,,2]), ylim = ylim, legend.text=c("HLE","ADL1","ADL2p"), main = "1995 vs 2014, Males All edu",
			ylab = "contribution to difference in e50")
	barplot(t(trn[,,2]),add=TRUE)
	
# 2004 vs 2014
	barplot(t(trp[,,3]), ylim = range(trmargins), legend.text=c("HLE","ADL1","ADL2p"), main = "2004 vs 2014, Males All edu",
			ylab = "contribution to difference in e50")
	barplot(t(trn[,,3]),add=TRUE)
}


trp <- trn <-trmargins
trp[trp < 0] <- NA
trn[trn > 0] <- NA

ylim <- c(min(apply(trn,3,rowSums,na.rm=TRUE)), max(apply(trp,3,rowSums,na.rm=TRUE)))


figpath <-  file.path("Figures","margins",paste0("mspec",version))
fig.name <- gsub(".Rdata","",file.name)
fig.name <- paste0(fig.name,"trmargins.pdf")
pdf(file.path(figpath,fig.name))
# 1995 vs 2004
barplot(t(trp[,,1]), ylim = ylim, legend.text=c("HLE","ADL1","ADL2p"), main = "1995 vs 2004, Males All edu",
		ylab = "contribution to difference in e50")
barplot(t(trn[,,1]),add=TRUE)

# 1995 vs 2014
barplot(t(trp[,,2]), ylim = ylim, legend.text=c("HLE","ADL1","ADL2p"), main = "1995 vs 2014, Males All edu",
		ylab = "contribution to difference in e50")
barplot(t(trn[,,2]),add=TRUE)

# 2004 vs 2014
barplot(t(trp[,,3]), ylim = range(trmargins), legend.text=c("HLE","ADL1","ADL2p"), main = "2004 vs 2014, Males All edu",
		ylab = "contribution to difference in e50")
barplot(t(trn[,,3]),add=TRUE)
dev.off()

#----------------------------


library(plotrix)

cols     <- get_colors()
a        <- seq(52,110,by=2)


decomp_lines <- function(dec, a = as.integer(rownames(dec))-2, ...){
	plot(a,type = 'n', xlab = "age", xlim = c(52,100), ylim = range(dec), las = 1,
			ylab = "contribution to difference",sub = paste0("Total difference = ",round(sum(dec),3)),
			panel.first = list(abline(h=0,col="red",lwd=.5)),...)
	matplot(a,dec, type = 'l', col = cols, lty = 1, lwd = 2, add = TRUE)
	boxed.labels(60, dec["62", ], colnames(dec), font = 2, col = cols, bg = "white",border=FALSE)
}

pdf("Figures/Diag_DecompLines.pdf")
decomp_lines(dec1.1, main = "Males, all edu, HLE 1995 vs 2004")
decomp_lines(dec1.2, main = "Males, all edu, ADL1 1995 vs 2004")
decomp_lines(dec1.3, main = "Males, all edu, ADL2p 1995 vs 2004")
decomp_lines(dec1.tot, main = "Males, all edu, LE50 1995 vs 2004")

decomp_lines(dec2.1, main = "Males, all edu, HLE 2004 vs 2014")
decomp_lines(dec2.2, main = "Males, all edu, ADL1 2004 vs 2014")
decomp_lines(dec2.3, main = "Males, all edu, ADL2p 2004 vs 2014")
decomp_lines(dec2.tot, main = "Males, all edu, LE50 2004 vs 2014")

decomp_lines(dec3.1, main = "Males, all edu, HLE 1995 vs 2014")
decomp_lines(dec3.2, main = "Males, all edu, ADL1 1995 vs 2014")
decomp_lines(dec3.3, main = "Males, all edu, ADL2p 1995 vs 2014")
decomp_lines(dec3.tot, main = "Males, all edu, LE50 1995 vs 2014")
dev.off()

# notion of decomp error. Things should sum tho?
decomp_lines(dec1.1 + dec2.1 - dec3.1)
# decomp_lines(dec3.tot - dec3.tot.l)