
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
# this changes: presently code for 3 states!!
versions      <- sprintf("%02d",c(1:14))
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
			  version = versions[i])
	  
	TR.i     <-  merge(TR.i,sprop[,colskeep],all.x=TRUE,fill=NA)
	  
	TR.i$edu <- edus[TR.i$educlevel]
	TR.i$Sex <- TR.i$sex
	TR.i$sex <- sexes[TR.i$sex]
	TR.i     <- TR.i[order(TR.i$time,TR.i$sex,TR.i$edu,TR.i$age),]
	save(TR.i, file = file.path("Data","Transitions","DCS",paste0("TR_v",versions[i],".Rdata")))
}
# -----------------------------------------------------------





# this space intentionally left blank





# -----------------------------------------------------------
# v2 redux to allow for time and edu-varying initprop:
sprop         <- foreign::read.dta(file.path(read.path,"initprop","initprop_detail.dta"))
head(sprop)
#facs          <- sapply(sprop,class) == "factor"
#sprop[, facs] <- lapply(sprop[,facs], fac2ch)

# change this to get modeled fractions.
sprop         <- sprop[sprop$propweighted == 1 & grepl(pattern = "age 50-54", sprop[,1]), ]

# we'll append to age 50 and pad with NAs in other ages.
# inefficient, but keeps things together.
sprop$age     <- 50

# now slim down to needed columns
pcols         <- c("e1_s1_prop_glmwgt", "e1_s2_prop_glmwgt", "e1_s3_prop_glmwgt", 
		"e4_s1_prop_glmwgt", "e4_s2_prop_glmwgt", "e4_s3_prop_glmwgt", 
		"e5_s1_prop_glmwgt", "e5_s2_prop_glmwgt", "e5_s3_prop_glmwgt")
colnames(sprop)

notp <-  c("sex","time", "age")
colskeep      <- c(notp, pcols)
sprop         <- sprop[,colskeep]

# get selected years for now
sprop <- sprop[sprop$time %in% c(1996,2006,2014), ]
# now rescale to proportions rather than 10k radix
sprop[,pcols] <- sprop[, pcols] / rowSums(sprop[,pcols])

# now stack for education:

sproppri <- spropsec <- spropuni <- sprop
sproppri <- sproppri[,c(notp,pcols[1:3])]
spropsec <- spropsec[,c(notp,pcols[4:6])]
spropuni <- spropuni[,c(notp,pcols[7:9])]


pnew     <- c("s1_prop", "s2_prop", "s3_prop")
colnames(sproppri) <- c(notp, pnew)
colnames(spropsec) <- c(notp, pnew)
colnames(spropuni) <- c(notp, pnew)

sproppri$educlevel <- Edus["primary"]
spropsec$educlevel <- Edus["secondary"]
spropuni$educlevel <- Edus["terciary"]

sprop <- rbind(sproppri, spropsec, spropuni)
# now create version-specific data objects.
for (i in 1:length(versions)){
	TR.i <- get_rates_all(
			path = read.path, 
			version = versions[i])
	
	TR.i     <- merge(TR.i,sprop,all.x=TRUE,fill=NA)
	
	TR.i$edu <- edus[TR.i$educlevel]
	TR.i$Sex <- TR.i$sex
	TR.i$sex <- sexes[TR.i$sex]
	TR.i     <- TR.i[order(TR.i$time,TR.i$sex,TR.i$edu,TR.i$age),]
	save(TR.i, file = file.path("Data","Transitions","DCS","initdetail",paste0("TR_v",versions[i],".Rdata")))
}
