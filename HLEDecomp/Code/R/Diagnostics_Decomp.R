
me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
}
source("Code/R/Functions.R")
version      <- "01"

#sex          <- "m" # "m","f",or"b"
sexes        <- c("m", "f", "b")
edus         <- c("all_edu", "primary" , "secondary", "terciary"  )
edusl        <- c("0.All edu", "1.Less HS" , "4.HS/GED/Sm coll ex AA", "5.AA/BS/+"  )
names(edusl) <- edus
N            <- 20
figpath      <- file.path("Figures","margins",paste0("mspec",version))

for (sex in sexes){
	Sex        <- ifelse(sex == "m", "1.men", ifelse(sex == "f", "2.wmn", "0.all"))
	for (edu in edus){
		educlevel  <- edusl[edu]
		
		file.name  <- paste0(paste("dec",version,sex,edu,N,sep="_"),".Rdata")
		dec.i      <- local(get(load(file.path(decpath,file.name))))
		
		fig.name   <- gsub(".Rdata","",file.name)
		fig.name   <- paste0(fig.name,"trmargins.pdf")
		
		pdf(file.path(figpath,fig.name))
		barmargins(dec.i)
		dev.off()
	}
}


figpath <-  file.path("Figures","margins",paste0("mspec",version))
fig.name <- gsub(".Rdata","",file.name)
fig.name <- paste0(fig.name,"trmargins.pdf")
pdf(file.path(figpath,fig.name))
barmargins(dec.i)
dev.off()