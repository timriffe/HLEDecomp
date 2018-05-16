
# Author: tim
###############################################################################

# two ways: either collapse before getting results or after
# make redundant collapsed rates I guess.
me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
	read.path <- "N:\\dcs\\proj\\hledecomp\\results"
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
	read.path <- "/home/tim/Data/hledecomp/results"
}

library(reshape2)
library(data.table)
source("Code/R/Functions.R")
source("Code/R/Preamble.R")

# 2 state model

version <- "02"
mspec      <- paste0("mspec", version)

TR   <- get_TR(version)
TR   <- data.table(TR)

PREV <- readRDS(file.path("Data", "Results", mspec, "prev", "prev.rds"))

# rates and prev
RP   <- merge(TR,PREV)
setnames(RP,"1","pi1")
setnames(RP,"2","pi2")
setnames(RP,"3","pi3")

# reorder RP
RP <- setorder(RP,time,sex,edu,age)
colnames(PREV) %in% colnames(TR)
# m11 <- m11
m11 <- RP$m11
# m14 <- m14 # keep 4 as dead
m14  <- RP$m14
# m12 <- m12 + m13
m12  <- RP$m12 + RP$m13

# now check that these sum to 1
# m22 <- (m22 * pi2 + m33 * pi3) / (pi2 + pi3)
m21  <- (RP$m21 * RP$pi2 + RP$m31 * RP$pi3) / (RP$pi2 + RP$pi3)
m24  <- (RP$m24 * RP$pi2 + RP$m34 * RP$pi3) / (RP$pi2 + RP$pi3)
m22  <- 1 - m21 - m24
m22i <- ( (RP$m22 + RP$m23) * RP$pi2 + (RP$m33 + RP$m32) * RP$pi3) / (RP$pi2 + RP$pi3)
all(abs(m22 - m22i) < 1e-7) # sanity check

RP <- data.table(RP)
RP$s2_prop <- RP$s2_prop + RP$s3_prop
RP[,c("m11","m12","m13","m14","m21","m22","m23","m24","m31","m32","m33","m34","pi1","pi2","pi3","s3_prop"):=NULL]

RP <- cbind(RP, m11, m12, m14, m22, m21, m24)



# check if transition selection.
#TR.prev <- TR.i[,do_prev_chunk(.SD)]
# now conundrum, need a good way to attach the initprop info. When in chunks can save as metadata,
# but how to attach metadata to a .SD chunk?
head(sex)
Mi <- acast(RP[sex == "f", ], age~time~edu,value.var = "m24")
M2 <- acast(TR[TR$sex == "f", ], age~time~edu,value.var = "m24")
M3 <- acast(TR[TR$sex == "f", ], age~time~edu,value.var = "m34")


matplot(as.integer(rownames(Mi)),Mi[,j,i],type='l',log='y',
		lty=1,col=gray(seq(.7,0,length=3)),lwd=seq(3,1,length=3))
matplot(as.integer(rownames(M2)),M2[,j,i],type='l',log='y',
		lty=2,col=gray(seq(.7,0,length=3)),lwd=seq(3,1,length=3),add=TRUE)
matplot(as.integer(rownames(M3)),M3[,j,i],type='l',log='y',
		lty=3,col=gray(seq(.7,0,length=3)),lwd=seq(3,1,length=3),add=TRUE)

