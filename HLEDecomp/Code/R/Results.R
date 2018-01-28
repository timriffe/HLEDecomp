# TODO consider parallelsugar on Windows if this is too slow


me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
}
source("Code/R/Functions.R")
library(reshape2)
# set this to rerun
version    <- "01"
sex        <- "m" # "m","f",or"b"
educlevel  <- "0.All edu"
N <- 20
# let sex recode
Sex        <- ifelse(sex == "m", "1.men", ifelse(sex == "f", "2.wmn", "0.all"))

# define results container
# easier
dec.1 <- do_decomp(times = c(1995,2004,2014), ntrans = 3, version = version, sex = Sex, educlevel = educlevel, N = N, deduct = TRUE, dcs = FALSE)
dec.2 <- do_decomp(times = c(1995,2004,2014), ntrans = 3, version = version, sex = Sex, educlevel = educlevel, N = N, deduct = TRUE, dcs = TRUE)
dec.3 <- do_decomp(times = c(1995,2004,2014), ntrans = 3, version = version, sex = Sex, educlevel = educlevel, N = N, deduct = FALSE)

sets          <- paste(dec.1$year1,dec.1$year2)
code          <- unique(sets)
recvec        <- 1:length(code)
names(recvec) <- code
dec.1$decnr   <- recvec[sets]
dec.2$decnr   <- recvec[sets]
dec.3$decnr   <- recvec[sets]
path <- file.path("Data","Results",paste0("mspec",version))
save(dec.1, file = file.path(path, "dec1.Rdata"))
save(dec.2, file = file.path(path, "dec2.Rdata"))
save(dec.3, file = file.path(path, "dec3.Rdata"))

# save out results systematically
file.name <- paste0(paste("dec",version,sex,educlevel,N,sep="_"),".Rdata")
path <- file.path("Data","Results",paste0("mspec",version))
if (!dir.exists(path)){
	dir.create(path)
}
save(dec.i, file = file.path(path, file.name))

# once-off diagnostic:
figpath <-  file.path("Figures","margins",paste0("mspec",version))
pdf(file.path(figpath,"dec1margins.pdf"))
barmargins(dec.1)
dev.off()

pdf(file.path(figpath,"dec2margins.pdf"))
barmargins(dec.2)
dev.off()

pdf(file.path(figpath,"dec3margins.pdf"))
barmargins(dec.3)
dev.off()

# -------------------------------------------------------
# compare HLE with and without deduction.

e50m1 <- do_le(times = c(1995,2004,2014), version = version, sex = Sex, educlevel = educlevel, deduct = TRUE, dcs = FALSE) 
e50m2 <- do_le(times = c(1995,2004,2014), version = version, sex = Sex, educlevel = educlevel, deduct = TRUE, dcs = TRUE)
e50m3 <- do_le(times = c(1995,2004,2014), version = version, sex = Sex, educlevel = educlevel, deduct = FALSE)

e50m1 - e50m2

e50m2 - e50m3

e50m1 - e50m3






# ------------------------------------------------------
# repeat with logit decomp
do.this <- FALSE
if (do.this){
# decompose 1995 vs 2004
dec1.1.l   <- HLEDecomp_logit(m1995, m2004, N = 100, to = 1)[-1, ]
dec1.2.l   <- HLEDecomp_logit(m1995, m2004, N = 100, to = 2)[-1, ]
dec1.3.l   <- HLEDecomp_logit(m1995, m2004, N = 100, to = 3)[-1, ]
dec1.tot.l <- dec1.1.l + dec1.2.l + dec1.3.l

# decompose 2004 vs 2014
dec2.1.l   <- HLEDecomp_logit(m2004, m2014, N = 100, to = 1)[-1, ]
dec2.2.l   <- HLEDecomp_logit(m2004, m2014, N = 100, to = 2)[-1, ]
dec2.3.l   <- HLEDecomp_logit(m2004, m2014, N = 100, to = 3)[-1, ]
dec2.tot.l <- dec2.1.l + dec2.2.l + dec2.3.l

# decompose 1995 vs 2014
dec3.1.l   <- HLEDecomp_logit(m1995, m2014, N = 100, to = 1)[-1, ]
dec3.2.l   <- HLEDecomp_logit(m1995, m2014, N = 100, to = 2)[-1, ]
dec3.3.l   <- HLEDecomp_logit(m1995, m2014, N = 100, to = 3)[-1, ]
dec3.tot.l <- dec3.1.l + dec3.2.l + dec3.3.l

1+1
}

#matplot(out2self(m2004)-out2self(m2014),type='l')

#
#m1995 <- get_data(time = 1995)
#m2004 <- get_data(time = 2004)
#m2014 <- get_data(time = 2014)
#
## hle
#hle95 <- e50(m1995, to = 1, age = 52)
#hle04 <- e50(m2004, to = 1, age = 52)
#hle14 <- e50(m2014, to = 1, age = 52)
#
## adl1
#a195 <- e50(m1995, to = 2, age = 52)
#a104 <- e50(m2004, to = 2, age = 52)
#a114 <- e50(m2014, to = 2, age = 52)
#
## adl2
#a295 <- e50(m1995, to = 3, age = 52)
#a204 <- e50(m2004, to = 3, age = 52)
#a214 <- e50(m2014, to = 3, age = 52)
#
## tot
#tot95 <- hle95 + a195 + a295
#tot04 <- hle04 + a104 + a204
#tot14 <- hle14 + a114 + a214
#
#print(data.frame(hle = c(hle95,hle04,hle14),
#		adl1 = c(a195,a104,a114),
#		adl2p = c(a295,a204,a214),
#		tot = c(tot95,tot04,tot14)),digits=3)
