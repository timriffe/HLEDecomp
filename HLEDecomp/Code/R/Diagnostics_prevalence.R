# this script is to produce bulk figures for internal consumption

me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
}
source("Code/R/Functions.R")
version    <- "01"

#sex        <- "m" # "m","f",or"b"
sexes        <- c("m", "f", "b")
edus         <- c("all_edu", "primary" , "secondary", "terciary"  )
edusl        <- c("0.All edu", "1.Less HS" , "4.HS/GED/Sm coll ex AA", "5.AA/BS/+"  )
names(edusl) <- edus

for (sex in sexes){
	Sex        <- ifelse(sex == "m", "1.men", ifelse(sex == "f", "2.wmn", "0.all"))
	for (edu in edus){
		educlevel  <- edusl[edu]
		
		file.name  <- paste0(paste("prev",version,sex,edu,sep="_"),".pdf")
		path <- file.path("Figures",file.name)
		
# plot prevalence
		prev       <- do_prev(times = c(1995,2004,2014), version = version, sex = Sex, educlevel = educlevel, deduct = FALSE)
		
		colss      <- c("#1B9E77", "#E6AB02", "#D95F02")
		colst      <- c("#6BAED6", "#3182BD", "#08519C")
		
		this.sex <- ifelse(sex == "m","males",ifelse(sex == "f","females","both sexes"))
		
		pdf(path)
		
		# absolute prevalence over time
		plot_prev(prev, type = "l", to = 1, lty = 1, lwd = 2, col = colst, main = paste("Health,",this.sex,"1995,2004,2014",edu))
		plot_prev(prev, type = "l", to = 2, lty = 1, lwd = 2, col = colst, main = paste("ADL1,",this.sex,"1995,2004,2014",edu))
		plot_prev(prev, type = "l", to = 3, lty = 1, lwd = 2, col = colst, main = paste("ADL2p,",this.sex,"1995,2004,2014",edu))
		
		
		# prevalence in survival curve
		plot_prev(prev, type = "bar", time = 1995, scale = FALSE, col = colss,
				main =paste( "Prevalence break down (Health, ADL1, ADL2p)",this.sex,", 1995,",edu))
		plot_prev(prev, type = "bar", time = 2004, scale = FALSE, col = colss,
				main = paste( "Prevalence break down (Health, ADL1, ADL2p)",this.sex,", 2004,",edu))
		plot_prev(prev, type = "bar", time = 2014, scale = FALSE, col = colss,
				main = paste( "Prevalence break down (Health, ADL1, ADL2p)",this.sex,", 2014,",edu))
		
		# prevalence conditioned on survival to age
		plot_prev(prev, type = "bar", time = 1995, scale = TRUE, col = colss,
				main =paste( "Prevalence break down (Health, ADL1, ADL2p)",this.sex,", 1995,",edu))
		plot_prev(prev, type = "bar", time = 2004, scale = TRUE, col = colss,
				main = paste( "Prevalence break down (Health, ADL1, ADL2p)",this.sex,", 2004,",edu))
		plot_prev(prev, type = "bar", time = 2014, scale = TRUE, col = colss,
				main = paste( "Prevalence break down (Health, ADL1, ADL2p)",this.sex,", 2014,",edu))
		
		dev.off()
		
	}
}



