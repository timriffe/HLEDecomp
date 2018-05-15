
# Author: tim
###############################################################################

# a script to import all available transition rates to R binary format,
# also modifying format in some useful ways (state1, state2, for example)
me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
	read.path <- "N:\\dcs\\proj\\hledecomp\\results"
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
	read.path <- "/home/tim/Data/hledecomp/results"
}
source("Code/R/Functions.R")
source("Code/R/Preamble.R")
#
sprop         <- foreign::read.dta(file.path(read.path,"initprop","initprop2.dta"))
facs          <- sapply(sprop,class) == "factor"
sprop[, facs] <- lapply(sprop[,facs], fac2ch)
sprop         <- sprop[sprop$propweighted == 1 & grepl(pattern = "age 50-54", sprop[,1]), ]

# we'll append to age 50 and pad with NAs in other ages.
# inefficient, but keeps things together.
sprop$age     <- 50

# now slim down to needed columns
pcols         <- c("s1_prop", "s2_prop", "s3_prop")
colskeep      <- c("sex", "educlevel","age", pcols)
sprop         <- sprop[,colskeep]
# now rescale to proportions rather than 10k radix
sprop[,pcols] <- sprop[,pcols] / rowSums(sprop[,pcols])

# now create version-specific data objects.
for (i in 1:length(versions)){
	TR.i <- get_rates_all(
			  path = read.path, 
			  version = versions[i],
			  self = TRUE)
	  
	TR.i     <-  merge(TR.i,sprop[,colskeep],all.x=TRUE,fill=NA)
	  
	TR.i$edu <- edus[TR.i$educlevel]
	TR.i$Sex <- TR.i$sex
	TR.i$sex <- sexes[TR.i$sex]
	
	save(TR.i, file = file.path("Data","Transitions","DCS",paste0("TR_v",versions[i],".Rdata")))
}
subset(TR.i, sex == "f" & edu == "terciary" & time == 2006)




