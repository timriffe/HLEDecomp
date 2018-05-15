
me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
}
source("Code/R/Functions.R")
source("Code/R/Preamble.R")


for (version in c("01","02","03")){
	figpath      <- file.path("Figures","margins",paste0("mspec",version))
	if (!dir.exists(figpath)){
		dir.create(figpath,recursive=TRUE)
	}
	
	
	for (sex in sexes){
		Sex        <- ifelse(sex == "m", "1.men", ifelse(sex == "f", "2.wmn", "0.all"))
		for (edu in edus){
			educlevel  <- edusl[edu]
			
			decpath    <- file.path("Data","Results",paste0("mspec",version),"dec")
			file.name  <- paste0(paste("dec",version,sex,edu,N,sep="_"),".Rdata")
			dec.i      <- local(get(load(file.path(decpath,file.name))))
			
			fig.name   <- gsub(".Rdata","",file.name)
			fig.name   <- paste0(fig.name,"_trmargins.pdf")
			
			pdf(file.path(figpath,fig.name))
			barmargins(dec.i, ylim=c(-1,3))
			dev.off()
		}
	}
}

figpath <-  file.path("Figures","margins",paste0("mspec",version))
fig.name <- gsub(".Rdata","",file.name)
fig.name <- paste0(fig.name,"trmargins.pdf")
pdf(file.path(figpath,fig.name))
barmargins(dec.i)
dev.off()