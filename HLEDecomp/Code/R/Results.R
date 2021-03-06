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
library(data.table)
source("Code/R/Functions.R")
source("Code/R/Preamble.R")

# set this to rerun

# The mpsec version, as defined by DCS. The early mspecs have rates on different scales
# that require some arguments to be toggled. In general, later mspecs require the arguments
# dcs = FALSE and deduct = TRUE (deduct half interval width)

#version    <- "01" # change this to run a single version and comment out decomp loop.

version    <- "06"
mspec      <- paste0("mspec", version)
path       <- file.path("Data", "Results", mspec, "dec")
if (!dir.exists(path)){
	dir.create(path, recursive = TRUE)
	dir.create(file.path("Data", "Results", mspec, "prev"), recursive = TRUE)
	dir.create(file.path("Data", "Results", mspec, "le"), recursive = TRUE)
}


TR <- get_TR(version = version)
TR <- data.table(TR)
# decompositions (slow)
# 1996 vs 2006
years           <- c(1996, 2006)
TR_i            <- TR[time %in% years]
DEC_i           <- TR_i[ , do_decomp_dt(.SD), by = list(sex, edu)]
# by states
DEC_i_1         <- TR_i[ , do_decomp_dt(.SD,to=1), by = list(sex, edu)]
DEC_i_2         <- TR_i[ , do_decomp_dt(.SD,to=2), by = list(sex, edu)]
DEC_i_3         <- TR_i[ , do_decomp_dt(.SD,to=3), by = list(sex, edu)]
DEC_96_06       <- rbind(DEC_i,DEC_i_1,DEC_i_2,DEC_i_3)
saveRDS(DEC_96_06, file = file.path("Data", "Results", mspec, "dec", paste0("dec_", years[1],"_", years[2], ".rds")))

# 1996 vs 2014
years           <- c(1996, 2014)
TR_i            <- TR[time %in% years]
DEC_i           <- TR_i[ , do_decomp_dt(.SD), by = list(sex, edu)]
# by states
DEC_i_1         <- TR_i[ , do_decomp_dt(.SD,to=1), by = list(sex, edu)]
DEC_i_2         <- TR_i[ , do_decomp_dt(.SD,to=2), by = list(sex, edu)]
DEC_i_3         <- TR_i[ , do_decomp_dt(.SD,to=3), by = list(sex, edu)]
DEC_96_14       <- rbind(DEC_i,DEC_i_1,DEC_i_2,DEC_i_3)
saveRDS(DEC_96_14, file = file.path("Data", "Results", mspec, "dec", paste0("dec_", years[1], "_", years[2], ".rds")))

# 2006 vs 2014
years           <- c(2006, 2014)
TR_i            <- TR[time %in% years]
DEC_i           <- TR_i[ , do_decomp_dt(.SD), by = list(sex, edu)]
# by states
DEC_i_1         <- TR_i[ , do_decomp_dt(.SD,to=1), by = list(sex, edu)]
DEC_i_2         <- TR_i[ , do_decomp_dt(.SD,to=2), by = list(sex, edu)]
DEC_i_3         <- TR_i[ , do_decomp_dt(.SD,to=3), by = list(sex, edu)]
DEC_06_14       <- rbind(DEC_i,DEC_i_1,DEC_i_2,DEC_i_3)

saveRDS(DEC_06_14, file = file.path("Data", "Results", mspec, "dec", paste0("dec_", years[1], "_", years[2], ".rds")))

# finally group all of them together. Rather large data object I guess.
DEC.all <- rbind(DEC_96_06, DEC_96_14, DEC_06_14)
saveRDS(DEC.all, file = file.path("Data", "Results", mspec, "dec", paste0("dec_all.rds")))

# this is prevalence for all combos of year,sex,edu
# super quick
TR                <- get_TR(version = version)
TR                <- data.table(TR)
PREV              <- TR[ , get_prev_dt(.SD), by = list(sex, edu, time)]
saveRDS(PREV, file = file.path("Data", "Results", mspec, "prev", "prev.rds"))

# life expectancy at age 50 in each state.
LE                <- TR[ , e50_dt(.SD), by = list(sex, edu, time)]
saveRDS(LE, file = file.path("Data", "Results", mspec, "le", "le.rds"))

# now collapse states 2 and 3 into state 2 only.
TR2               <- collapseTR(TR = TR, PREV = PREV)
saveRDS(TR2, file = file.path("Data", "Transitions", "Collapsed", paste0("TR_v", version, "_collapsed.rds")))

LE2               <- TR2[ , e50_dt(.SD, ntrans = 2), by = list(sex, edu, time)] 
saveRDS(LE2, file = file.path("Data", "Results", mspec, "le", "le_2.rds"))

PREV2             <- TR2[ , get_prev_dt(.SD, ntrans = 2), by = list(sex, edu, time)]
saveRDS(PREV2, file = file.path("Data", "Results", mspec, "prev", "prev_2.rds"))


years             <- c(1996, 2006)
TR2.i             <- TR2[time%in%years]
DEC2.i            <- TR2.i[ , do_decomp_dt(.SD,ntrans=2), by = list(sex,edu)]
DEC2_1.i          <- TR2.i[ , do_decomp_dt(.SD,ntrans=2,to=1), by = list(sex,edu)]
DEC2_2.i          <- TR2.i[ , do_decomp_dt(.SD,ntrans=2,to=2), by = list(sex,edu)]
DEC2.1996_2006    <- rbind(DEC2.i, DEC2_1.i, DEC2_2.i)
saveRDS(DEC2.1996_2006, file = file.path("Data", "Results", mspec, "dec", paste0("dec2_", years[1],"_", years[2], ".rds")))

years             <- c(2006, 2014)
TR2.i             <- TR2[time%in%years]
DEC2_1.i          <- TR2.i[ , do_decomp_dt(.SD,ntrans=2,to=1), by = list(sex,edu)]
DEC2_2.i          <- TR2.i[ , do_decomp_dt(.SD,ntrans=2,to=2), by = list(sex,edu)]
DEC2.i            <- TR2.i[ , do_decomp_dt(.SD,ntrans=2), by = list(sex,edu)]
DEC2.2006_2014    <- rbind(DEC2.i, DEC2_1.i, DEC2_2.i)
saveRDS(DEC2.2006_2014, file = file.path("Data", "Results", mspec, "dec", paste0("dec2_", years[1],"_", years[2], ".rds")))

years             <- c(1996, 2014)
TR2.i             <- TR2[time%in%years]
DEC2_1.i          <- TR2.i[ , do_decomp_dt(.SD,ntrans=2,to=1), by = list(sex,edu)]
DEC2_2.i          <- TR2.i[ , do_decomp_dt(.SD,ntrans=2,to=2), by = list(sex,edu)]
DEC2.i            <- TR2.i[ , do_decomp_dt(.SD,ntrans=2), by = list(sex,edu)]
DEC2.1996_2014    <- rbind(DEC2.i, DEC2_1.i, DEC2_2.i)
saveRDS(DEC2.1996_2014, file = file.path("Data", "Results", mspec, "dec", paste0("dec2_", years[1],"_", years[2], ".rds")))

# bind all together for safe keeping
DEC2.all <- rbind(DEC2.1996_2006, DEC2.2006_2014, DEC2.1996_2014)

saveRDS(DEC2.all, file = file.path("Data", "Results", mspec, "dec", paste0("dec2_all.rds")))


###########################################################
# to health states?
# now rbinded in the above.
#years             <- c(1996, 2006)
#TR2.i             <- TR2[time%in%years]
#DEC2_1.i          <- TR2.i[ , do_decomp_dt(.SD,to=1,ntrans=2), by = list(sex, edu)]
#saveRDS(DEC2_1.i, file = file.path("Data", "Results", mspec, "dec", paste0("dec2_1_", years[1],"_", years[2], ".rds")))
#DEC2_2.i          <- TR2.i[ , do_decomp_dt(.SD,to=2,ntrans=2), by = list(sex, edu)]
#saveRDS(DEC2_2.i, file = file.path("Data", "Results", mspec, "dec", paste0("dec2_2_", years[1],"_", years[2], ".rds")))
#
#years             <- c(2006, 2014)
#TR2.i             <- TR2[time%in%years]
#DEC2_1.i          <- TR2.i[ , do_decomp_dt(.SD,to=1,ntrans=2), by = list(sex, edu)]
#saveRDS(DEC2_1.i, file = file.path("Data", "Results", mspec, "dec", paste0("dec2_1_", years[1],"_", years[2], ".rds")))
#DEC2_2.i          <- TR2.i[ , do_decomp_dt(.SD,to=2,ntrans=2), by = list(sex, edu)]
#saveRDS(DEC2_2.i, file = file.path("Data", "Results", mspec, "dec", paste0("dec2_2_", years[1],"_", years[2], ".rds")))


#
#for (sex in sexes){
#	for (edu in edus){
#		# years can be done more systematically as well, for all possible combos,
#		# but really we need to simplify rather than complicate things.
#		dec.i      <- do_decomp(years = c(1996,2014), # whole period
#				ntrans = 3, 
#				version = version, 
#				sex = sex, 
#				edu = edu, 
#				N = N, 
#				deduct = TRUE)
#		# now save year pair in name
#		file.name  <- paste0(paste("dec", version, sex, edu, N, years[1],years[2],sep = "_"), ".Rdata")
#		save(dec.i, file = file.path(path, file.name))
#	}
#}
#

# version loop (temporary)-- long run times. Later once everything straightened out
# verify deduction procedures and each mspec def w DCS

# eliminate this loop once things are stable
#for (version in versions){ # 1-3 now
## temporary until years straightened out:
#	if (version == "01"){
#		years  <-  c(1995,2004,2014)
#		dcs    <- TRUE
#		deduct <- FALSE
#	} 
#	if (version %in% c("02","03")){
#		years  <- c(1996,2006,2014) # these may become single years for splines
#		dcs    <- FALSE             # in future mspecs
#		deduct <- TRUE
#	}
#	# path to place decomp results
#	path       <- file.path("Data","Results",paste0("mspec",version),"dec")
#	if (!dir.exists(path)){
#		dir.create(path,recursive=TRUE)
#	}
#    # sex loop	
#	for (sex in sexes){
#		Sex        <- ifelse(sex == "m", "1.men", ifelse(sex == "f", "2.wmn", "0.all"))
#		
#		# education loop
#		for (edu in edus){
#			educlevel  <- edusl[edu]
#			dec.i      <- do_decomp(years = years, 
#					ntrans = 3, 
#					version = version, 
#					sex = Sex, 
#					educlevel = educlevel, 
#					N = N, 
#					dcs = dcs,
#					deduct = FALSE,
#					path = read.path)
#			file.name  <- paste0(paste("dec", version, sex, edu, N, sep = "_"), ".Rdata")
#			
#			save(dec.i, file = file.path(path, file.name))
#		} # close education loop
#	} # close sex loop
#} # close version loop

# version <- "02"; sex <- "m"; edu <- "all_edu"
# generate results for prevalence and LE (faster than the above loop)
#for (version in c("02","03")){
# temporary until years straightened out:
#	if (version == "01"){
#		years  <-  c(1995,2004,2014)
#		dcs    <- TRUE
#		deduct <- FALSE
#	} 
#	if (version %in% c("02","03")){
#		years  <- c(1996,2006,2014)
#		dcs    <- FALSE
#		deduct <- TRUE
#	}
#	# path for prev and le
#	path       <- file.path("Data","Results",paste0("mspec",version),"prev")
#	if (!dir.exists(path)){
#		dir.create(path,recursive=TRUE)
#	}
#	pathe       <- file.path("Data","Results",paste0("mspec",version),"le")
#	if (!dir.exists(pathe)){
#		dir.create(pathe,recursive=TRUE)
#	}
#	# sex loop	
#	for (sex in sexes){
#		Sex        <- ifelse(sex == "m", "1.men", ifelse(sex == "f", "2.wmn", "0.all"))
#		
#		for (edu in edus){
#			educlevel  <- edusl[edu]
## get prevalence saved for all combos:
#			prev.i <- do_prev(years = years,
#					age = 52, 
#					version = version, 
#					sex = Sex, 
#					educlevel = educlevel, 
#					deduct = deduct, 
#					dcs = dcs, 
#					path = read.path)
#			file.name  <- paste0(paste("prev", version, sex, edu, N, sep = "_"), ".Rdata")
#			
#			save(prev.i, file = file.path(path, file.name))
#			
#			prevL <- split(prev.i[,1:3], list(prev.i$time))
#			ex.i  <- do.call("rbind",lapply(prevL, colSums))
#			#ex.i$time <- years
#			file.name.i  <- paste0(paste("le", version, sex, edu, N, sep = "_"), ".Rdata")
#			save(ex.i, file = file.path(pathe, file.name.i))
#		}
#	}
#}
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

# test interp

#?DemoTools::interp()
#
#p1 <- runif(10)
#p2 <- runif(10)
#p1 <- p1/sum(p1)
#p2 <- p2/sum(p2)
#
#d1              <- "1980-01-01"
#d2              <- "1990-01-01"
#
#datesOut <- c("1980-01-01","1981-01-01","1982-01-01","1983-01-01","1984-01-01",
#				"1985-01-01","1986-01-01","1987-01-01","1988-01-01","1989-01-01",
#				"1990-01-01")
#(p_lin <- interp(cbind(p1,p2),c(d1,d2),datesOut,method = "linear"))
#(p_pow <- interp(cbind(p1,p2),c(d1,d2),datesOut,method = "power"))

#colSums(p_pow) # can't do power interpolation or exponential interpolation in horiuchi()
#colSums(p_lin)
