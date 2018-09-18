
# Author: tim
###############################################################################

library(HMDHFDplus)

CNTRS <- getHMDcountries()

CNTRIES_USE <- c("AUS", "AUT", "BEL",  "CAN", "CHE", 
		"DEUTNP",  "DNK", "ESP",  "FIN", 
		"FRATNP","GRC", "IRL", "ISL", "ISR", "ITA", 
		"JPN", "KOR",  "LUX", "NLD", "NOR", "NZL_NP",
		"PRT", "SWE", "TWN", "GBR_NP", "USA")

e50L <- lapply(CNTRIES_USE,function(cntry,us,pw){
			mlt   <- readHMDweb(cntry,"mltper_1x1",username=us,password=pw)
			flt   <- readHMDweb(cntry,"fltper_1x1",username=us,password=pw)
			yrind <- sort(unique(mlt$Year[mlt$Year >= 1990]))
			e50m  <- mlt$ex[mlt$Age == 50 & mlt$Year >= 1990]
			e50f  <- flt$ex[flt$Age == 50 & flt$Year >= 1990]
			data.frame(CNTRY = cntry, Year = yrind, Male = e50m, Female = e50f)
		},us=us,pw=pw)
e50 <- do.call("rbind",e50L)
e50 <- e50[e50$CNTRY != "UKR", ]
e50 <- e50[e50$CNTRY != "KOR", ]
library(reshape2)
e50m <- acast(e50, Year~CNTRY, value.var = "Male")
e50f <- acast(e50, Year~CNTRY, value.var = "Female")

keep <- !is.na(e50m["2014",]) & !is.na(e50m["1990",])
e50m <- e50m[,keep]
e50f <- e50f[,keep]


hrs_male_col <- "#053a8e"
hmd_male_col <- "#88aeea"

hrs_fem_col  <- "#a50847"
hmd_fem_col  <- "#ed9ebe"

pdf("/home/tim/git/HLEDecomp/HLEDecomp/Figures/USAvsOthers_R.pdf")
plot(NULL, type = 'n',xlim = c(1990,2017),ylim=c(24,38), ann = FALSE,las=1, axes=FALSE)
matplot(1990:2017, e50m, type = 'l', col = hmd_male_col, lty = 1, add =TRUE)
lines(1990:2017, e50m[,"USA"],col = hrs_male_col, lwd = 2)

#plot(NULL, type = 'n',xlim = c(1990,2017),ylim=c(24,40), ann = FALSE,)
matplot(1990:2017, e50f, type = 'l', col = hmd_fem_col, lty = 1, add =TRUE)
lines(1990:2017, e50f[,"USA"],col = hrs_fem_col, lwd = 2)

axis(1)
axis(2,las=1)

points(c(1996,2014),e50m[c("1996","2014"),"USA"],pch=16,col=hrs_male_col,cex=1.2)
points(c(1996,2014),e50f[c("1996","2014"),"USA"],pch=16,col=hrs_fem_col,cex=1.2)
dev.off()


sort(23-rank(e50f["1990", ], na.last = NA)) # 12
sort(23-rank(e50f["1996", ], na.last = NA)) # 17 out of 23
sort(23-rank(e50f["2014", ], na.last = NA)) # last 23 - 17 = 6

sort(23-rank(e50m["1990", ], na.last = NA)) # 12
sort(23-rank(e50m["1996", ], na.last = NA)) # 11 out of 23
sort(23-rank(e50m["2014", ], na.last = NA)) # penultimate after TWN (21)

#colnames(e50m)
#"AUS"    
#"AUT"   
#"BEL"    
#"CHE"    
#"DEUTNP" 
#"DNK"    
#"ESP"    
#"FIN"   
# "FRATNP" 
# "IRL"    
# "ISL"   
# "ISR"    
# "ITA"    
# "JPN"    
# "LUX"    
# "NLD"   
# "NOR"   
# "PRT"    
# "SWE"    
# "TWN"    
# "GBR_NP" 
# "USA"  