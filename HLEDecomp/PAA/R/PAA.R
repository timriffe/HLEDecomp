# Author: tim
###############################################################################

# just to get things rolling, rather unrealistically, this script will contain
# stuff used in the PAA presentation. But I can't help but want to decompose more
# to get a handle on differences.

library(data.table)
library(DemoDecomp)
library(RColorBrewer)
library(xtable)
me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
}

source("Code/R/Functions.R")
source("PAA/R/FunctionsForPAA.R")

TR    <- local(get(load("Data/Transitions/DCS/initdetail/TR_v06.Rdata")))
TR    <- data.table(TR)
# TR: had to fix colnames problem
PREV  <- TR[ , get_prev_dt(.SD), by = list(sex, edu, time)]

# TR2 just means that there are now TWO living states,
# healthy and disabled.
TR2   <- collapseTR(TR = TR, PREV = PREV)

setnames(TR2,c("m14","m24"),c("m13","m23"))

# now the decompositions: 
# strat by 2 sexes, 2 periods, 3 states (H,U,LE)

Tab1a <- cbind(f_dec_rescale_all_edu_decomp_dt(TR2,	to = 1,
				ntrans = 2,	deduct = TRUE, N = 20, sex1 = "m",
				time1 = 1996, time2 = 2006),
		f_dec_rescale_all_edu_decomp_dt(TR2, to = 2,
				ntrans = 2, deduct = TRUE, N = 20, sex1 = "m",
				time1 = 1996, time2 = 2006),
		f_dec_rescale_all_edu_decomp_dt(TR2, to = 5,
				ntrans = 2,	deduct = TRUE, N = 20,	sex1 = "m",
				time1 = 1996, time2 = 2006))

Tab1b <- cbind(f_dec_rescale_all_edu_decomp_dt(TR2,	to = 1,
				ntrans = 2,	deduct = TRUE, N = 20, sex1 = "m",
				time1 = 2006, time2 = 2014),
		f_dec_rescale_all_edu_decomp_dt(TR2, to = 2,
				ntrans = 2, deduct = TRUE, N = 20, sex1 = "m",
				time1 = 2006, time2 = 2014),
		f_dec_rescale_all_edu_decomp_dt(TR2, to = 5,
				ntrans = 2,	deduct = TRUE, N = 20,	sex1 = "m",
				time1 = 2006, time2 = 2014))

Tab2a <- cbind(f_dec_rescale_all_edu_decomp_dt(TR2,	to = 1,
				ntrans = 2,	deduct = TRUE, N = 20, sex1 = "f",
				time1 = 1996, time2 = 2006),
		f_dec_rescale_all_edu_decomp_dt(TR2, to = 2,
				ntrans = 2, deduct = TRUE, N = 20, sex1 = "f",
				time1 = 1996, time2 = 2006),
		f_dec_rescale_all_edu_decomp_dt(TR2, to = 5,
				ntrans = 2,	deduct = TRUE, N = 20,	sex1 = "f",
				time1 = 1996, time2 = 2006))

Tab2b <- cbind(f_dec_rescale_all_edu_decomp_dt(TR2,	to = 1,
				ntrans = 2,	deduct = TRUE, N = 20, sex1 = "f",
				time1 = 2006, time2 = 2014),
		f_dec_rescale_all_edu_decomp_dt(TR2, to = 2,
				ntrans = 2, deduct = TRUE, N = 20, sex1 = "f",
				time1 = 2006, time2 = 2014),
		f_dec_rescale_all_edu_decomp_dt(TR2, to = 5,
				ntrans = 2,	deduct = TRUE, N = 20,	sex1 = "f",
				time1 = 2006, time2 = 2014))

# collect together for easy manipulation in presentation
DecompResults <- list(Tab1a = Tab1a, Tab1b = Tab1b, Tab2a = Tab2a, Tab2b = Tab2b)

# this will need to change because we have added two self-arrows to the decomp

DecompResults <- lapply(DecompResults, function(x){
			x <- rbind(x,colSums(x))
			rownames(x) <- c("Stay DF","Onset","DF Mortality","Recovery","Stay Disab.",
					"Dis. Mortality","Age50 Disab.","Age 50 Educ.","Total")
			colnames(x) <- c("DFLE","DLE","LE")
			as.data.frame(x)
		})

saveRDS(DecompResults,file="Data/Results/mspec06/dec/TableDataStructRescaleAllPAA.rds")



# compare self-as-residual results with all-arrows-count results:
all <- readRDS("Data/Results/mspec06/dec/TableDataStructRescaleAllPAA.rds")
res <- readRDS("Data/Results/mspec06/dec/TableDataStruct.rds") 


all[[1]]
res[[1]]
f_dec_rescale_all_edu_dt(TR2[time==2006 & sex == "m"],to=5)-
f_dec_rescale_all_edu_dt(TR2[time==1996 & sex == "m"],to=5)

all[[2]]
res[[2]]
f_dec_rescale_all_edu_dt(TR2[time==2014 & sex == "m"],to=5)-
		f_dec_rescale_all_edu_dt(TR2[time==2006 & sex == "m"],to=5)


all[[3]]
res[[3]]
f_dec_rescale_all_edu_dt(TR2[time==2006 & sex == "f"],to=5)-
		f_dec_rescale_all_edu_dt(TR2[time==1996 & sex == "f"],to=5)

all[[4]]
res[[4]]
f_dec_rescale_all_edu_dt(TR2[time==2014 & sex == "f"],to=5)-
		f_dec_rescale_all_edu_dt(TR2[time==2006 & sex == "f"],to=5)

