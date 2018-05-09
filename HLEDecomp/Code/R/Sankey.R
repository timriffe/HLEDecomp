
# Author: tim
###############################################################################

# idea to display stacked bars for expectancies.
# with decomp values as flows in series.
me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
}
source("Code/R/Functions.R")
source("Code/R/Preamble.R")

prev <- 

#sex          <- "m" # "m","f",or"b"
		sexes        <- c("m", "f", "b")
edus         <- c("all_edu", "primary" , "secondary", "terciary"  )
edusl        <- c("0.All edu", "1.Less HS" , "4.HS/GED/Sm coll ex AA", "5.AA/BS/+"  )
names(edusl) <- edus
N            <- 20
# version <- "03";sex <- "m";year <- 2006;edu <- "all_edu"

# ON HOLD need functions to do these things.
#for (version in c("02","03")){
version <- "02"
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
	pathe        <- file.path("Data","Results",paste0("mspec",version),"le")
	path         <- file.path("Data","Results",paste0("mspec",version),"dec")
	#Rates.v <- get_rates_all(path = read.path, version = version)
	for (sex in sexes){
		Sex        <- ifelse(sex == "m", "1.men", ifelse(sex == "f", "2.wmn", "0.all"))
		for (edu in edus){
			educlevel    <- edusl[edu]
			
			file.name.i  <- paste0(paste("le", version, sex, edu, N, sep = "_"), ".Rdata")
			ex.i         <- local(get(load(file.path(pathe,file.name.i))))
			file.name.i  <- paste0(paste("dec", version, sex, edu, N, sep = "_"), ".Rdata")
			dec.i        <- local(get(load(file.path(path,file.name.i))))
		    
			
		}
	}

	
	barplot(t(ex.i[,1:3]))
	le1   <- ex.i[1,1:3]
	le2   <- ex.i[2,1:3]
		
	dec.i$state1 <- substr(dec.i$transition,2,2)
	dec.i$state2 <- substr(dec.i$transition,3,3)
	
	dec12 <- dec.i[dec.i$year1 == 1996 & dec.i$year2 == 2006, ]
	# need separate from and to states
    
#	draw_Sankey <- function(le1,le2,dec12){
#		
#	}

#library(riverplot)
#
## you'll have your own state labels
#states <- LETTERS[1:5]
#
## the states should be factors, with all 5 levels. In this
## example we're using character, but you'll want a factor,
## because that way if a given level is absent it'll still
## get tabulated later as a 0
#Dat <- data.frame(id = 1:1000, 
#		state1 = sample(states,size=1000,replace = TRUE), 
#		state2 = sample(states,size=1000,replace = TRUE))
#
## create a node object
#nodes <- data.frame(ID = paste0(rep(states,each=2),rep(1:2,5)), x = rep(1:2,5))
#
##tablulate edge weights (transition counts) somehow
#trans1 <- table(paste(Dat$state1,Dat$state2))
#
## starting and ending nodes for each edge (flow)
#N1 <- paste0(rep(states,each=5),1)
#N2 <- paste0(rep(states,5),2)
## create edge object
#edges <- data.frame(N1 = N1, N2 = N2, Value = as.vector(trans1))
#
## this is key, you want a riverplot object, this makes it for you given
## an edges and nodes data.frame.
##?makeRiver
#Dat2 <- makeRiver(nodes, edges)
#plot(Dat2)