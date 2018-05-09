# Consider parallelsugar on Windows if this is too slow
# the decomps are slow but not too slow. Work fine on old laptop anyway.
# This script does decomposition, stationary prevalence, and state expectancies.
# -----------------------------------------

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
source("Code/R/Functions.R")
# set this to rerun

# The mpsec version, as defined by DCS. The early mspecs have rates on different scales
# that require some arguments to be toggled. In general, later mspecs require the arguments
# dcs = FALSE and deduct = TRUE (deduct half interval width)

#version    <- "01" # change this to run a single version and comment out decomp loop.

N            <- 20 # higher N = more precision
sexes        <- c("m", "f", "b")
edus         <- c("all_edu", "primary" , "secondary", "terciary"  )
edusl        <- c("0.All edu", "1.Less HS" , "4.HS/GED/Sm coll ex AA", "5.AA/BS/+"  )
names(edusl) <- edus

# version loop (temporary)-- long run times. Later once everything straightened out
# verify deduction procedures and each mspec def w DCS

# eliminate this loop once things are stable
for (version in c("01","02","03")){
# temporary until years straightened out:
	if (version == "01"){
		years  <-  c(1995,2004,2014)
		dcs    <- TRUE
		deduct <- FALSE
	} 
	if (version %in% c("02","03")){
		years  <- c(1996,2006,2014) # these may become single years for splines
		dcs    <- FALSE             # in future mspecs
		deduct <- TRUE
	}
	# path to place decomp results
	path       <- file.path("Data","Results",paste0("mspec",version),"dec")
	if (!dir.exists(path)){
		dir.create(path,recursive=TRUE)
	}
    # sex loop	
	for (sex in sexes){
		Sex        <- ifelse(sex == "m", "1.men", ifelse(sex == "f", "2.wmn", "0.all"))
		
		# education loop
		for (edu in edus){
			educlevel  <- edusl[edu]
			dec.i      <- do_decomp(years = years, 
					ntrans = 3, 
					version = version, 
					sex = Sex, 
					educlevel = educlevel, 
					N = N, 
					dcs = dcs,
					deduct = FALSE,
					path = read.path)
			file.name  <- paste0(paste("dec", version, sex, edu, N, sep = "_"), ".Rdata")
			
			save(dec.i, file = file.path(path, file.name))
		} # close education loop
	} # close sex loop
} # close version loop

# generate results for prevalence and LE (faster than the above loop)
for (version in c("01","02","03")){
# temporary until years straightened out:
	if (version == "01"){
		years  <-  c(1995,2004,2014)
		dcs    <- TRUE
		deduct <- FALSE
	} 
	if (version %in% c("02","03")){
		years  <- c(1996,2006,2014)
		dcs    <- FALSE
		deduct <- TRUE
	}
	# path for prev and le
	path       <- file.path("Data","Results",paste0("mspec",version),"prev")
	if (!dir.exists(path)){
		dir.create(path,recursive=TRUE)
	}
	pathe       <- file.path("Data","Results",paste0("mspec",version),"le")
	if (!dir.exists(pathe)){
		dir.create(pathe,recursive=TRUE)
	}
	# sex loop	
	for (sex in sexes){
		for (edu in edus){
			educlevel  <- edusl[edu]
# get prevalence saved for all combos:
			prev.i <- do_prev(years = years,
					age = 52, 
					version = version, 
					sex = sex, 
					educlevel = educlevel, 
					deduct = deduct, 
					dcs = dcs, 
					path = read.path)
			file.name  <- paste0(paste("prev", version, sex, edu, N, sep = "_"), ".Rdata")
			
			save(prev.i, file = file.path(path, file.name))
			
			prevL <- split(prev.i, list(prev.i$time))
			ex.i  <- do.call("rbind",lapply(prevL, colSums))
			ex.i$time <- years
			file.name.i  <- paste0(paste("le", version, sex, edu, N, sep = "_"), ".Rdata")
			save(ex.i, file = file.path(pathe, file.name.i))
		}
	}
}
# no visualizations done here. Scripts for diagnostic visualizations of results are
# found in R scripts whose file names start with Diagnostics_*.R

# ---------------------------------------------
# old code, deprecated
#
#
#dec.i <- do_decomp(times = c(1995,2004,2014), ntrans = 3, version = version, sex = Sex, educlevel = educlevel, N = N, deduct = TRUE, dcs = FALSE)
## save out results systematically
#file.name <- paste0(paste("dec", version, sex, edu, N, sep = "_"), ".Rdata")
#path <- file.path("Data","Results",paste0("mspec",version))
#if (!dir.exists(path)){
#	dir.create(path)
#}
#save(dec.i, file = file.path(path, file.name))

# define results container
# easier
#dec.1 <- do_decomp(times = c(1995,2004,2014), ntrans = 3, version = version, sex = Sex, educlevel = educlevel, N = N, deduct = TRUE, dcs = FALSE)
#dec.2 <- do_decomp(times = c(1995,2004,2014), ntrans = 3, version = version, sex = Sex, educlevel = educlevel, N = N, deduct = TRUE, dcs = TRUE)
#dec.3 <- do_decomp(times = c(1995,2004,2014), ntrans = 3, version = version, sex = Sex, educlevel = educlevel, N = N, deduct = FALSE)
#
#sets          <- paste(dec.1$year1,dec.1$year2)
#code          <- unique(sets)
#recvec        <- 1:length(code)
#names(recvec) <- code
#dec.1$decnr   <- recvec[sets]
#dec.2$decnr   <- recvec[sets]
#dec.3$decnr   <- recvec[sets]
#path <- file.path("Data","Results",paste0("mspec",version))
#save(dec.1, file = file.path(path, "dec1.Rdata"))
#save(dec.2, file = file.path(path, "dec2.Rdata"))
#save(dec.3, file = file.path(path, "dec3.Rdata"))


# once-off diagnostic:
#figpath <-  file.path("Figures","margins",paste0("mspec",version))
#pdf(file.path(figpath,"dec1margins.pdf"))
#barmargins(dec.1)
#dev.off()
#
#pdf(file.path(figpath,"dec2margins.pdf"))
#barmargins(dec.2)
#dev.off()
#
#pdf(file.path(figpath,"dec3margins.pdf"))
#barmargins(dec.3)
#dev.off()

# -------------------------------------------------------
# compare HLE with and without deduction.

# a once-off diagnostic.
#e50m1 <- do_le(times = c(1995,2004,2014), version = version, sex = Sex, educlevel = educlevel, deduct = TRUE, dcs = FALSE) 
#e50m2 <- do_le(times = c(1995,2004,2014), version = version, sex = Sex, educlevel = educlevel, deduct = TRUE, dcs = TRUE)
#e50m3 <- do_le(times = c(1995,2004,2014), version = version, sex = Sex, educlevel = educlevel, deduct = FALSE)
#
#e50m1 - e50m2
#
#e50m2 - e50m3
#
#e50m1 - e50m3
#
## -------------------------------------------------------
#
#
#
## ------------------------------------------------------
## repeat with logit decomp
#do.this <- FALSE
#if (do.this){
## decompose 1995 vs 2004
#dec1.1.l   <- HLEDecomp_logit(m1995, m2004, N = 100, to = 1)[-1, ]
#dec1.2.l   <- HLEDecomp_logit(m1995, m2004, N = 100, to = 2)[-1, ]
#dec1.3.l   <- HLEDecomp_logit(m1995, m2004, N = 100, to = 3)[-1, ]
#dec1.tot.l <- dec1.1.l + dec1.2.l + dec1.3.l
#
## decompose 2004 vs 2014
#dec2.1.l   <- HLEDecomp_logit(m2004, m2014, N = 100, to = 1)[-1, ]
#dec2.2.l   <- HLEDecomp_logit(m2004, m2014, N = 100, to = 2)[-1, ]
#dec2.3.l   <- HLEDecomp_logit(m2004, m2014, N = 100, to = 3)[-1, ]
#dec2.tot.l <- dec2.1.l + dec2.2.l + dec2.3.l
#
## decompose 1995 vs 2014
#dec3.1.l   <- HLEDecomp_logit(m1995, m2014, N = 100, to = 1)[-1, ]
#dec3.2.l   <- HLEDecomp_logit(m1995, m2014, N = 100, to = 2)[-1, ]
#dec3.3.l   <- HLEDecomp_logit(m1995, m2014, N = 100, to = 3)[-1, ]
#dec3.tot.l <- dec3.1.l + dec3.2.l + dec3.3.l
#
#1+1
#}

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
