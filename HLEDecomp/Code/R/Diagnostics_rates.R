
# diagnostics on rates: mspecs 2 and 3 for now. 
# 2 is age + age^2
# 3 is splines, afraid might be out of control

# prior: prevalence-weighted avg of death rates in 3 should abide by
# mort laws, but state-specific ones are not bound by mort laws
# ---------------------------------------------------

# note on hold. Lots of formats to make:
# 1) flip books where each page shows the 3 transitions originating in a state (or incl self arrows)  (all years together)
# 2) flip books where each page shows the 2-3 transitions into a state (or incl self arrows) (all years together)
# 3) show the state-specific and prevalence-weighted aggregate mortality rates. (all years together?)
# 4) all rates together for a particular strata and year.


# mortality:
#mortrates <- c("m14","m24","m34")

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
source("Code/R/Preamble.R")
#sex          <- "m" # "m","f",or"b"
sexes        <- c("m", "f", "b")
edus         <- c("all_edu", "primary" , "secondary", "terciary"  )
edusl        <- c("0.All edu", "1.Less HS" , "4.HS/GED/Sm coll ex AA", "5.AA/BS/+"  )
names(edusl) <- edus
N            <- 20
# version <- "03";sex <- "m";year <- 2006;edu <- "all_edu"

# ON HOLD need functions to do these things.
for (version in c("02","03")){
	figpath      <- file.path("Figures","rates",paste0("mspec",version))
	if (!dir.exists(figpath)){
		dir.create(figpath,recursive=TRUE)
	}
#	if (version == "01"){
#		years  <-  c(1995,2004,2014)
#	} 
	if (version %in% c("02","03")){
		years  <- c(1996,2006,2014) # these may become single years for splines
	}
	#Rates.v <- get_rates_all(path = read.path, version = version)
	for (sex in sexes){
		Sex        <- ifelse(sex == "m", "1.men", ifelse(sex == "f", "2.wmn", "0.all"))
		for (edu in edus){
			educlevel  <- edusl[edu]
#			Prev <- do_prev(years = years,
#					age = 52, 
#					version = version, 
#					sex = Sex, 
#					educlevel = educlevel, 
#					deduct = TRUE, 
#					dcs = FALSE, 
#					path = read.path)
			DatL   <- lapply(years, 
					get_data, 
					self = FALSE, 
					version = version, 
					sex = Sex, 
					educlevel = educlevel,
					path = read.path)
			
			age     <- as.integer(rownames(DatL[[1]]))
			Rates.i <- DatL[[1]]
			Prev.i  <- as.matrix(Prev[Prev$time == years[1], 1:3])
			
			Rw <- rowSums(Prev.i[-1,] * Rates.i[,c("m14","m24","m34")]) / rowSums(Prev.i[-1,]  )
			plot(Rw,type='l')
			matplot(age, Rates.i ,type='l', col = gray(seq(0,.7,length=9)),lty=1,lwd=seq(1,3,length=9))
			
			getcols(ntrans = 3, self = FALSE)
			
		}
	}
}
















