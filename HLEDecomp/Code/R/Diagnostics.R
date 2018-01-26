# this script is to produce bulk figures for internal consumption

me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
}
source("Code/R/Functions.R")

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