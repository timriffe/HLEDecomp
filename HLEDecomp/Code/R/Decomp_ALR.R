
# Author: tim
###############################################################################

# uses functions in Functions_ALR.R

library(data.table)
library(DemoDecomp)
library(RColorBrewer)
library(xtable)
library(compositions)
me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
}

source("Code/R/Functions.R")
source("Code/R/Functions_ALR.R")

TR    <- local(get(load("Data/Transitions/DCS/initdetail/TR_v06.Rdata")))
TR    <- data.table(TR)
# TR: had to fix colnames problem
PREV  <- TR[ , get_prev_dt(.SD), by = list(sex, edu, time)]
# TR2 just means that there are now TWO living states,
# healthy and disabled.
TR2   <- collapseTR(TR = TR, PREV = PREV)
setnames(TR2,c("m14","m24"),c("m13","m23"))

# a quick wrapper

wrapper_ALR <- function(TR, to = 5, sex = "m", time1 = 1996, time2 = 2006, age = 50, N = 20){
	dec.i <- decomp_edu_alr(TR, time1 = time1, time2 = time2, sex = sex,
			age = 50, to = to, deduct = TRUE, N = N, ntrans = 2)
	summary_decomp_edu_alr(dec.i, ntrans = 2)
}


# ------------------------------------------------- #

Tab1a <- cbind(
		wrapper_ALR(TR2,to=1, sex = "m", time1 = 1996, time2 = 2006, age = 50, N = 20),
		wrapper_ALR(TR2,to=2, sex = "m", time1 = 1996, time2 = 2006, age = 50, N = 20),
		wrapper_ALR(TR2,to=5, sex = "m", time1 = 1996, time2 = 2006, age = 50, N = 20)
)
		
Tab1b <- cbind(
		wrapper_ALR(TR2,to=1, sex = "m", time1 = 2006, time2 = 2014, age = 50, N = 20),
		wrapper_ALR(TR2,to=2, sex = "m", time1 = 2006, time2 = 2014, age = 50, N = 20),
		wrapper_ALR(TR2,to=5, sex = "m", time1 = 2006, time2 = 2014, age = 50, N = 20)
)

Tab2a <- cbind(
		wrapper_ALR(TR2,to=1, sex = "f", time1 = 1996, time2 = 2006, age = 50, N = 20),
		wrapper_ALR(TR2,to=2, sex = "f", time1 = 1996, time2 = 2006, age = 50, N = 20),
		wrapper_ALR(TR2,to=5, sex = "f", time1 = 1996, time2 = 2006, age = 50, N = 20)
)

Tab2b <- cbind(
		wrapper_ALR(TR2,to=1, sex = "f", time1 = 2006, time2 = 2014, age = 50, N = 20),
		wrapper_ALR(TR2,to=2, sex = "f", time1 = 2006, time2 = 2014, age = 50, N = 20),
		wrapper_ALR(TR2,to=5, sex = "f", time1 = 2006, time2 = 2014, age = 50, N = 20)
)

# get totals
Tab1a <- rbind(Tab1a, colSums(Tab1a))
Tab1b <- rbind(Tab1b, colSums(Tab1b))
Tab2a <- rbind(Tab2a, colSums(Tab2a))
Tab2b <- rbind(Tab2b, colSums(Tab2b))

rnames <- c("Onset", "DF Mortality", "Recovery", "Dis. Mortality","Age 50 Disab.", "Age 50 Educ.", "Total")
cnames <- c("DFLE","DLE","LE")

dimnames(Tab1a) <- list(rnames,cnames)
dimnames(Tab1b) <- list(rnames,cnames)
dimnames(Tab2a) <- list(rnames,cnames)
dimnames(Tab2b) <- list(rnames,cnames)

Resutls <- list(Tab1a=Tab1a,Tab1b=Tab1b,Tab2a=Tab2a,Tab2b=Tab2b)
saveRDS(Resutls, file= "Data/Results/mspec06/dec/TableStruct_ALR.rds")

