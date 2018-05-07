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
sex        <- "m" # "m","f",or"b"
educlevel  <- "0.All edu"
edus       <- c("0.All edu", "1.Less HS" , "4.HS/GED/Sm coll ex AA", "5.AA/BS/+"  )
N          <- 20
Sex        <- ifelse(sex == "m", "1.men", ifelse(sex == "f", "2.wmn", "0.all"))

# let sex recode

file.name  <- paste0(paste("dec",version,sex,educlevel,N,sep="_"),".Rdata")
path       <- file.path("Data","Results",paste0("mspec",version))

dec.i      <- local(get(load(file.path(path, file.name))))

file.name  <- paste0(paste("prev",version,sex,educlevel,N,sep="_"),".pdf")
fig.path       <- file.path("Figures",file.name)


## TR: now in Diagnostics_prevalence.R
## plot prevalence
#prev       <- do_prev(times = c(1995,2004,2014), version = version, sex = Sex, educlevel = educlevel, deduct = FALSE)
#
#colss      <- c("#1B9E77", "#E6AB02", "#D95F02")
#colst      <- c("#6BAED6", "#3182BD", "#08519C")
#
#pdf(path)
#
#plot_prev(prev, type = "l", to = 1, lty = 1, lwd = 2, col = colst, main = "Health, males 1995,2004,2014, all edu")
#plot_prev(prev, type = "l", to = 2, lty = 1, lwd = 2, col = colst, main = "ADL1, males 1995,2004,2014, all edu")
#plot_prev(prev, type = "l", to = 3, lty = 1, lwd = 2, col = colst, main = "ADL2p, males 1995,2004,2014, all edu")
#
#plot_prev(prev, type = "bar", time = 1995, scale = FALSE, col = colss,main = "Prevalence break down (Health, ADL1, ADL2p) males, 1995, all edu")
#plot_prev(prev, type = "bar", time = 2004, scale = FALSE, col = colss,main = "Prevalence break down (Health, ADL1, ADL2p) males, 2004, all edu")
#plot_prev(prev, type = "bar", time = 2014, scale = FALSE, col = colss,main = "Prevalence break down (Health, ADL1, ADL2p) males, 2014, all edu")
#
#plot_prev(prev, type = "bar", time = 1995, scale = TRUE, col = colss,main = "Prevalence break down (Health, ADL1, ADL2p) males, 1995, all edu")
#plot_prev(prev, type = "bar", time = 2004, scale = TRUE, col = colss,main = "Prevalence break down (Health, ADL1, ADL2p) males, 2004, all edu")
#plot_prev(prev, type = "bar", time = 2014, scale = TRUE, col = colss,main = "Prevalence break down (Health, ADL1, ADL2p) males, 2014, all edu")
#
#dev.off()


# ------------------------------------------------------
# MARGINS PLOTS
sets          <- paste(dec.i$year1,dec.i$year2)
code          <- unique(sets)
recvec        <- 1:length(code)
names(recvec) <- code
dec.i$decnr   <- recvec[sets]
# transition margins
trmargins     <- acast(dec.i, transition ~ state ~ decnr, sum, value.var = "value")



trp <- trn <-trmargins
trp[trp < 0] <- NA
trn[trn > 0] <- NA

ylim <- c(min(apply(trn,3,rowSums,na.rm=TRUE)), max(apply(trp,3,rowSums,na.rm=TRUE)))


figpath <-  file.path("Figures","margins",paste0("mspec",version))
fig.name <- gsub(".Rdata","",file.name)
fig.name <- paste0(fig.name,"trmargins.pdf")
pdf(file.path(figpath,fig.name))
barmargins(dec.i)
dev.off()

#----------------------------


library(plotrix)

cols     <- get_colors()
a        <- seq(52,110,by=2)


decomp_lines <- function(dec, a = as.integer(rownames(dec))-2, ...){
	plot(a,type = 'n', xlab = "age", xlim = c(52,100), ylim = range(dec), las = 1,
			ylab = "contribution to difference",sub = paste0("Total difference = ",round(sum(dec),3)),
			panel.first = list(abline(h=0,col="red",lwd=.5)),...)
	matplot(a,dec, type = 'l', col = cols, lty = 1, lwd = 2, add = TRUE)
	boxed.labels(60, dec["62", ], colnames(dec), font = 2, col = cols, bg = "white",border=FALSE)
}

pdf("Figures/Diag_DecompLines.pdf")
decomp_lines(dec1.1, main = "Males, all edu, HLE 1995 vs 2004")
decomp_lines(dec1.2, main = "Males, all edu, ADL1 1995 vs 2004")
decomp_lines(dec1.3, main = "Males, all edu, ADL2p 1995 vs 2004")
decomp_lines(dec1.tot, main = "Males, all edu, LE50 1995 vs 2004")

decomp_lines(dec2.1, main = "Males, all edu, HLE 2004 vs 2014")
decomp_lines(dec2.2, main = "Males, all edu, ADL1 2004 vs 2014")
decomp_lines(dec2.3, main = "Males, all edu, ADL2p 2004 vs 2014")
decomp_lines(dec2.tot, main = "Males, all edu, LE50 2004 vs 2014")

decomp_lines(dec3.1, main = "Males, all edu, HLE 1995 vs 2014")
decomp_lines(dec3.2, main = "Males, all edu, ADL1 1995 vs 2014")
decomp_lines(dec3.3, main = "Males, all edu, ADL2p 1995 vs 2014")
decomp_lines(dec3.tot, main = "Males, all edu, LE50 1995 vs 2014")
dev.off()

# notion of decomp error. Things should sum tho?
decomp_lines(dec1.1 + dec2.1 - dec3.1)
# decomp_lines(dec3.tot - dec3.tot.l)