
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

# ---------------------------------- #

#



Tab1a <- readRDS("Data/Results/mspec06/dec/TableStruct_ALR.rds")$Tab1a
saveRDS(Tab1a, file= "Data/Results/mspec06/dec/Tab1a_alr_selfdenom.rds")
round(Tab1a_clr,2)
round(Tab1a,2)

# checking margins
colSums(Tab1a_clr[1:3, ])
colSums(Tab1a[1:2, ])

# checking margins
colSums(Tab1a_clr[4:6, ])
colSums(Tab1a[3:4, ])

