# Author: tim
###############################################################################

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


TR    <- local(get(load("Data/Transitions/DCS/initdetail/TR_v06.Rdata")))
TR    <- data.table(TR)
# TR: had to fix colnames problem
PREV  <- TR[ , get_prev_dt(.SD), by = list(sex, edu, time)]
# TR2 just means that there are now TWO living states,
# healthy and disabled.
TR2   <- collapseTR(TR = TR, PREV = PREV)
setnames(TR2,c("m14","m24"),c("m13","m23"))

rnames <- c("Onset", "DF Mortality", "Recovery", "Dis. Mortality","Age 50 Disab.", "Age 50 Educ.", "Total")
cnames <- c("DFLE","DLE","LE")

# ALR self denom
source("Code/R/Functions_ALR.R")
Tab1a_alr <- cbind(
		wrapper_ALR(TR2,to=1, sex = "m", time1 = 1996, time2 = 2006, age = 50, N = 20),
		wrapper_ALR(TR2,to=2, sex = "m", time1 = 1996, time2 = 2006, age = 50, N = 20),
		wrapper_ALR(TR2,to=5, sex = "m", time1 = 1996, time2 = 2006, age = 50, N = 20)
)
Tab1a <- rbind(Tab1a, colSums(Tab1a))
dimnames(Tab1a) <- list(rnames,cnames)
saveRDS(Tab1a_alr_mortdenom, file= "Data/Results/mspec06/dec/Tab1a_alr_selfdenom.rds")
#------------------------------------------

# ALR mort denom
source("Code/R/Functions_ALR_Compare.R")
Tab1a_alr_mortdenom <- cbind(
		wrapper_ALR_d(TR2,to=1, sex = "m", time1 = 1996, time2 = 2006, age = 50, N = 20),
		wrapper_ALR_d(TR2,to=2, sex = "m", time1 = 1996, time2 = 2006, age = 50, N = 20),
		wrapper_ALR_d(TR2,to=5, sex = "m", time1 = 1996, time2 = 2006, age = 50, N = 20)
)

Tab1a_alr_mortdenom <- rbind(Tab1a_alr_mortdenom,colSums(Tab1a_alr_mortdenom))
rnames_d <- c("Onset","DF surv","Recovery","D surv","Age 50 Disab.","Age 50 Educ.", "Total")
rownames(Tab1a_alr_mortdenom) <- rnames_d
colnames(Tab1a_alr_mortdenom) <- cnames
saveRDS(Tab1a_alr_mortdenom, file= "Data/Results/mspec06/dec/Tab1a_alr_mortdenom.rds")
#------------------------------------------

# ILR
source("Code/R/Functions_ILR.R")
Tab1a_ilr <- cbind(
		wrapper_ILR(TR2,to=1, sex = "m", time1 = 1996, time2 = 2006, age = 50, N = 20),
		wrapper_ILR(TR2,to=2, sex = "m", time1 = 1996, time2 = 2006, age = 50, N = 20),
		wrapper_ILR(TR2,to=5, sex = "m", time1 = 1996, time2 = 2006, age = 50, N = 20)
)
Tab1a_ilr           <- rbind(Tab1a_ilr,colSums(Tab1a_ilr))
colnames(Tab1a_ilr) <- cnames
saveRDS(Tab1a_ilr, file= "Data/Results/mspec06/dec/Tab1a_ilr.rds")
#------------------------------------------

# CLR
source("Code/R/Functions_CLR.R")
Tab1a_clr <- cbind(
		wrapper_CLR(TR2,to=1, sex = "m", time1 = 1996, time2 = 2006, age = 50, N = 20),
		wrapper_CLR(TR2,to=2, sex = "m", time1 = 1996, time2 = 2006, age = 50, N = 20),
		wrapper_CLR(TR2,to=5, sex = "m", time1 = 1996, time2 = 2006, age = 50, N = 20)
)
Tab1a_clr <- rbind(Tab1a_clr,colSums(Tab1a_clr))
colnames(Tab1a_clr) <- cnames
rownames(Tab1a_clr) <- c("Stay DF", "Onset", "DF Mortality", "Recovery", "Stay Dis.", "Dis. Mortality","Age 50 Disab.", "Age 50 Educ.", "Total")
saveRDS(Tab1a_clr, file= "Data/Results/mspec06/dec/Tab1a_clr.rds")
#------------------------------------------

