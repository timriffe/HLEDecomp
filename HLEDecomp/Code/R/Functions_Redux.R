
library(data.table)
library(DemoDecomp)
library(RColorBrewer)
library(xtable)
me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
}

source("Code/R/Functions.R")

# Author: tim
###############################################################################

# this changes the way fractions are decomposed.
# stack trans,frac,trans,frac,trans,frac (where frac is 4 pieces)
eduvec_leaveout <- function(TR,.sex="m",.time=1996,ntrans=3){
	outcols <- getcols(ntrans,self=FALSE)
	TR       <- as.data.frame(TR, stringsAsFactors = FALSE)
	pri      <- subset(TR,sex == .sex & time == .time & edu == "primary")
	sec      <- subset(TR,sex == .sex & time == .time & edu == "secondary")
	uni      <- subset(TR,sex == .sex & time == .time & edu == "terciary")
	
	priout   <- as.matrix(pri[, outcols])
	secout   <- as.matrix(sec[, outcols])
	uniout   <- as.matrix(uni[, outcols])
	
	priv     <- c(priout)
	secv     <- c(secout)
	univ     <- c(uniout)
	
	pcols    <- paste0("s", 1:ntrans, "_prop")
	
	prifrac  <- unlist(pri[1,pcols])
	secfrac  <- unlist(sec[1,pcols])
	unifrac  <- unlist(uni[1,pcols])
	
	fracs    <- cbind(prifrac, secfrac, unifrac)
	edufracs <- colSums(fracs)
	
	pifracs <- t(t(fracs) / edufracs)
	#pifracs[-1, ]
	
	#edufracs
	# leave university out
	# now separate pi and edu fracs
	c(priv, secv, univ, c(pifracs[-1, ]), edufracs[-1])
}

anti_leaveout <- function(vec, ntrans = 3, n = 31){
	enames            <- c("primary", "secondary", "terciary")
	rvec              <- rev(vec)
	edufracs          <- rev(rvec[1:2])
	edufracs          <- c( 1 - sum(edufracs), edufracs)
	names(edufracs)   <- enames
	rvec              <- rvec[-c(1:2)]
	
	pifracs           <- rev(rvec[1:((ntrans - 1) * 3)])
	dim(pifracs)      <- c(ntrans - 1, 3)
	pifracs           <- rbind(1-colSums(pifracs),pifracs)
	
	rownames(pifracs) <- paste0("s", 1:ntrans, "_prop")
	colnames(pifracs) <- enames
	
	rvec              <- rvec[-c(1:((ntrans - 1)*3))]
	vec               <- rev(rvec)
	dim(vec)          <- c(n * ntrans^2 ,3)
	colnames(vec)     <- enames
	list(outmat = vec, pifracs = pifracs, edufracs = edufracs)
}

dec_fun_redux_leaveout <- function(datoutvec, to, age = 50, n=31, ntrans = 3, deduct = TRUE){
	
	parts     <- anti_leaveout(datoutvec, ntrans = ntrans, n = n)
	
	# requires input as vector, first reshape to matrix or df
	dc        <- rep(0,3)
	
	# this is an education loop
	for (i in 1:3){
		datouti    <- v2m(parts$outmat[,i], ntrans = ntrans)
		# remove death rates and replace with self-arrows, needed to make
		# transition matrices
		datselfi   <- out2self(datouti, ntrans = ntrans)
		# append fractions: normalize so group expectancies look right, then reweight
		propi      <- parts$pifracs[,i] 
		# then compute e50 using the correct transition rates, and relevant fractions
		dc[i]      <- e50(datselfi, prop = propi, to = to, age = age, ntrans = ntrans, deduct = deduct)
	}
	# radix-weighted mean
	wmean(dc,parts$edufracs)
}

anti_leaveout_decomp <- function(vec,ntrans=3,n=31){
	enames       <- c("primary","secondary","terciary")
	rvec         <- rev(vec)
	edufracs     <- rev(rvec[1:2])
	
	rvec         <- rvec[-c(1:2)]
	
	pifracs      <- rev(rvec[1:((ntrans-1)*3)])
		
	rvec         <- rvec[-c(1:((ntrans-1)*3))]
	vec          <- rev(rvec)
	dim(vec)     <- c(n*ntrans^2,3)
	colnames(vec) <- enames
	list(outmat = vec, picomp = sum(pifracs), educomp = sum(edufracs))
}

decomp_redux_leaveout_wrapper <- function(
		TR,
		time1=2006,
		time2=2014,
		sex="f",
		ntrans=2,
		to=5,
		deduct = TRUE,
		n=31,
		N=20,
		age=50){
	outvec1 <- eduvec_leaveout(TR=TR,.sex=sex,.time=time1,ntrans=ntrans)
	outvec2 <- eduvec_leaveout(TR=TR,.sex=sex,.time=time2,ntrans=ntrans)
	deci <- horiuchi(dec_fun_redux_leaveout, 
			outvec1,
			outvec2,
			N = N,
			ntrans=ntrans,
			deduct=deduct,
			to=to,age=age)
	
	deci <- anti_leaveout_decomp(deci,ntrans=2)
	
	out <- c(colSums(v2m(rowSums(deci$outmat),ntrans=ntrans)),deci$picomp,deci$educomp)
	names(out) <- c(getcols(ntrans=ntrans,self=FALSE),"Health 50","Education 50")
	out
}
make_xtable_redux <- function(Tab.i,.sex="f", .deci="1996-2006",version="06",ramp,breaks=seq(-1.5,1.5,by=.1)){
	mspec   <- paste0("mspec", version)
	Tab.i <- rbind(Tab.i,colSums(Tab.i))
	
	rn <- c("Onset","DF Mortality", "Recovery","Dis. Mortality","Age 50 Disab.","Age 50 Educ.","Total")
	Tabi <- assign_colors(Tab.i,ramp=ramp,breaks=breaks)
	rownames(Tabi) <- rn
	colnames(Tabi) <- c("DFLE","DLE","LE")
	name <- paste0(paste(.sex,.deci,"struct",sep="-"),".tex")
	path <- file.path("Data","Tables",mspec)
	
	print(xtable(Tabi),
			hline.after = c(-1,0,4,6),
			only.contents = TRUE,
			file = file.path(path,name),
			booktabs = TRUE,
			sanitize.text.function = identity)
}
# table 1a (males 1996-2006)

Tab1a <- cbind(
		decomp_redux_leaveout_wrapper(TR2,time1=1996,time2=2006,sex="m",ntrans=2,to=1),
		decomp_redux_leaveout_wrapper(TR2,time1=1996,time2=2006,sex="m",ntrans=2,to=2),
		decomp_redux_leaveout_wrapper(TR2,time1=1996,time2=2006,sex="m",ntrans=2,to=5))

Tab1b <- cbind(
		decomp_redux_leaveout_wrapper(TR2,time1=2006,time2=2014,sex="m",ntrans=2,to=1),
		decomp_redux_leaveout_wrapper(TR2,time1=2006,time2=2014,sex="m",ntrans=2,to=2),
		decomp_redux_leaveout_wrapper(TR2,time1=2006,time2=2014,sex="m",ntrans=2,to=5))

Tab2a <- cbind(
		decomp_redux_leaveout_wrapper(TR2,time1=1996,time2=2006,sex="f",ntrans=2,to=1),
		decomp_redux_leaveout_wrapper(TR2,time1=1996,time2=2006,sex="f",ntrans=2,to=2),
		decomp_redux_leaveout_wrapper(TR2,time1=1996,time2=2006,sex="f",ntrans=2,to=5))


Tab2b <- cbind(
		decomp_redux_leaveout_wrapper(TR2,time1=2006,time2=2014,sex="f",ntrans=2,to=1),
		decomp_redux_leaveout_wrapper(TR2,time1=2006,time2=2014,sex="f",ntrans=2,to=2),
		decomp_redux_leaveout_wrapper(TR2,time1=2006,time2=2014,sex="f",ntrans=2,to=5))

# Test of DCS's suscipcion that decomp results
# sensitive to age definitions. Still need work on
# everything age related.
#Tab2b_test <- cbind(
#		decomp_redux_leaveout_wrapper(TR2,time1=2006,time2=2014,sex="f",ntrans=2,to=1,age=52),
#		decomp_redux_leaveout_wrapper(TR2,time1=2006,time2=2014,sex="f",ntrans=2,to=2,age=52),
#		decomp_redux_leaveout_wrapper(TR2,time1=2006,time2=2014,sex="f",ntrans=2,to=5,age=52))


ramp <- colorRampPalette(brewer.pal(11,"PRGn"),space = "Lab")
make_xtable_redux(Tab2a,.sex="f", .deci="1996-2006",version="06",ramp=ramp )
make_xtable_redux(Tab2b,.sex="f", .deci="2006-2014",version="06",ramp=ramp )
make_xtable_redux(Tab1a,.sex="m", .deci="1996-2006",version="06",ramp=ramp )
make_xtable_redux(Tab1b,.sex="m", .deci="2006-2014",version="06",ramp=ramp )

getwd()



#TR.2    <- local(get(load("/home/tim/git/HLEDecomp/HLEDecomp/Data/Transitions/DCS/initdetail/TR_v06.Rdata")))
#TR2     <- data.table(TR.2)
#LE2     <- TR2[ , e50_dt(.SD), by = list(sex, edu, time)]
#LE2$LE  <- rowSums(LE2[,4:6])
#
#TR.1    <- local(get(load("/home/tim/git/HLEDecomp/HLEDecomp/Data/Transitions/DCS/TR_v06.Rdata")))
#TR.1    <- TR.1[TR.1$edu != "all_edu",]
#TR1     <- data.table(TR.1)
#LE1     <- TR1[ , e50_dt(.SD), by = list(sex, edu, time)]
#LE1$LE  <- rowSums(LE1[,4:6]) # identical
#PREV    <- TR1[ , get_prev_dt(.SD), by = list(sex, edu, time)]
## PREVs all identical after one time step
#
#LE1 <- data.frame(LE1)
#LE2 <- data.frame(LE2)
#
#
#
#head(LE1)
#write.csv(LE1,file="/home/tim/Desktop/LEspec16.csv")
#write.csv(LE2,file="/home/tim/Desktop/LEspec18.csv")


TR    <- local(get(load("/home/tim/git/HLEDecomp/HLEDecomp/Data/Transitions/DCS/initdetail/TR_v06.Rdata")))
TR    <- data.table(TR)
PREV  <- TR[ , get_prev_dt(.SD), by = list(sex, edu, time)]
TR2   <- collapseTR(TR = TR, PREV = PREV)


matplot(PREV[PREV$time == 1996 & PREV$edu == "primary" & PREV$sex == "m",c("pi1", "pi2", "pi3")],type='l')


mhrs52 <-  c(dec_fun_redux(eduvec(TR,"m",1996),to=5, age = 52, n=31, ntrans = 3, deduct = TRUE),
		dec_fun_redux(eduvec(TR,"m",2006),to=5, age = 52, n=31, ntrans = 3, deduct = TRUE),
		dec_fun_redux(eduvec(TR,"m",2014),to=5, age = 52, n=31, ntrans = 3, deduct = TRUE))
mhrs50 <-  c(dec_fun_redux(eduvec(TR,"m",1996),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE),
		dec_fun_redux(eduvec(TR,"m",2006),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE),
		dec_fun_redux(eduvec(TR,"m",2014),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE))

mhrs50 - mhrs52

fhrs <- c(dec_fun_redux(eduvec(TR,"f",1996),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE),
		dec_fun_redux(eduvec(TR,"f",2006),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE),
		dec_fun_redux(eduvec(TR,"f",2014),to=5, age = 50, n=31, ntrans = 3, deduct = TRUE))

# for Fig 4
mhrs2 <-  c(dec_fun_redux(eduvec(TR=TR2,"m",1996,ntrans=2),to=5, age = 50, n=31, ntrans = 2, deduct = TRUE),
		dec_fun_redux(eduvec(TR=TR2,"m",2006,ntrans=2),to=5, age = 50, n=31, ntrans = 2, deduct = TRUE),
		dec_fun_redux(eduvec(TR=TR2,"m",2014,ntrans=2),to=5, age = 50, n=31, ntrans = 2, deduct = TRUE))
fhrs2 <- c(dec_fun_redux(eduvec(TR=TR2,"f",1996,ntrans=2),to=5, age = 50, n=31, ntrans = 2, deduct = TRUE),
		dec_fun_redux(eduvec(TR=TR2,"f",2006,ntrans=2),to=5, age = 50, n=31, ntrans = 2, deduct = TRUE),
		dec_fun_redux(eduvec(TR=TR2,"f",2014,ntrans=2),to=5, age = 50, n=31, ntrans = 2, deduct = TRUE))


LE2 <- TR2[ , e50_dt(.SD, ntrans = 2), by = list(sex, edu, time)] 
le2 <- as.data.frame(LE2,stringsAsFactors=FALSE)
le2$LE <- rowSums(le2[,c("V1","V2")])
mDFLE <- c(dec_fun_redux(eduvec(TR=TR2,"m",1996,ntrans=2),to=1, age = 50, n=31, ntrans = 2, deduct = TRUE),
		dec_fun_redux(eduvec(TR=TR2,"m",2006,ntrans=2),to=1, age = 50, n=31, ntrans = 2, deduct = TRUE),
		dec_fun_redux(eduvec(TR=TR2,"m",2014,ntrans=2),to=1, age = 50, n=31, ntrans = 2, deduct = TRUE))
mDLE <- c(dec_fun_redux(eduvec(TR=TR2,"m",1996,ntrans=2),to=2, age = 50, n=31, ntrans = 2, deduct = TRUE),
		dec_fun_redux(eduvec(TR=TR2,"m",2006,ntrans=2),to=2, age = 50, n=31, ntrans = 2, deduct = TRUE),
		dec_fun_redux(eduvec(TR=TR2,"m",2014,ntrans=2),to=2, age = 50, n=31, ntrans = 2, deduct = TRUE))

fDFLE <- c(dec_fun_redux(eduvec(TR=TR2,"f",1996,ntrans=2),to=1, age = 50, n=31, ntrans = 2, deduct = TRUE),
		dec_fun_redux(eduvec(TR=TR2,"f",2006,ntrans=2),to=1, age = 50, n=31, ntrans = 2, deduct = TRUE),
		dec_fun_redux(eduvec(TR=TR2,"f",2014,ntrans=2),to=1, age = 50, n=31, ntrans = 2, deduct = TRUE))
fDLE <- c(dec_fun_redux(eduvec(TR=TR2,"f",1996,ntrans=2),to=2, age = 50, n=31, ntrans = 2, deduct = TRUE),
		dec_fun_redux(eduvec(TR=TR2,"f",2006,ntrans=2),to=2, age = 50, n=31, ntrans = 2, deduct = TRUE),
		dec_fun_redux(eduvec(TR=TR2,"f",2014,ntrans=2),to=2, age = 50, n=31, ntrans = 2, deduct = TRUE))




barsubset <- function(chunk,ymax = 40){
	dat           <- t(as.matrix(chunk[,c("V1","V2")]))
	colnames(dat) <-  c(1996,2006,2014)
	barplot(dat, col = c(gray(c(.8,.4))), ylim=c(0,ymax),border=NA,axes=FALSE)
	abline(h=seq(5,35,by=5),col="white")
	axis(2,las=1)
}


pdf("Figures/bar_fem_all.pdf", height=5,width=2.2)
par(mai=c(.5,.5,.3,0))
barsubset(data.frame(V1 = fDFLE, V2 = fDLE))
dev.off()

pdf("Figures/bar_fem_pri.pdf", height=5,width=2.2)
par(mai=c(.5,.5,.3,0))
barsubset(subset(le2,sex=="f" & edu == "primary"))
dev.off()

pdf("Figures/bar_fem_uni.pdf", height=5,width=2.2)
par(mai=c(.5,.5,.3,0))
barsubset(subset(le2,sex=="f" & edu == "terciary"))
dev.off()

pdf("Figures/bar_male_all.pdf", height=5,width=2.2)
par(mai=c(.5,.5,.3,0))
barsubset(data.frame(V1 = mDFLE, V2 = mDLE))
dev.off()

pdf("Figures/bar_male_pri.pdf", height=5,width=2.2)
par(mai=c(.5,.5,.3,0))
barsubset(subset(le2,sex=="m" & edu == "primary"))
dev.off()

pdf("Figures/bar_male_uni.pdf", height=5,width=2.2)
par(mai=c(.5,.5,.3,0))
barsubset(subset(le2,sex=="m" & edu == "terciary"))
dev.off()

library(reshape2)
dflef <- acast(le2[le2$sex=="f",],edu~time,value.var = "V1")
dflem <- acast(le2[le2$sex=="m",],edu~time,value.var = "V1")
dlef <- acast(le2[le2$sex=="f",],edu~time,value.var = "V2")
dlem <- acast(le2[le2$sex=="m",],edu~time,value.var = "V2")
lem <- dflem+dlem
lef <- dflef+dlef
dlef / lef
dlem / lem

dflef / dflem
dlef / dlem
fDLE[3] - fDLE[2]

# counterfactual:
mhrs2 <-  c(dec_fun_redux(eduvec(TR=TR2,"m",1996,ntrans=2),to=5, age = 50, n=31, ntrans = 2, deduct = TRUE),
		dec_fun_redux(eduvec(TR=TR2,"m",2006,ntrans=2),to=5, age = 50, n=31, ntrans = 2, deduct = TRUE),
		dec_fun_redux(eduvec(TR=TR2,"m",2014,ntrans=2),to=5, age = 50, n=31, ntrans = 2, deduct = TRUE))

datoutvec2006 <- eduvec_leaveout(TR=TR2,"f",2006,ntrans=2)
parts2006     <- anti_leaveout(datoutvec2006, ntrans = ntrans, n = n)
datoutvec2014 <- eduvec_leaveout(TR=TR2,"f",2014,ntrans=2)
parts2014     <- anti_leaveout(datoutvec2014, ntrans = ntrans, n = n)

outmat1       <- v2m(parts2014$outmat[,1],ntrans=2)
outmat2       <- v2m(parts2014$outmat[,2],ntrans=2)
outmat3       <- v2m(parts2014$outmat[,3],ntrans=2)

outmat12      <- v2m(parts2006$outmat[,1],ntrans=2)
outmat22      <- v2m(parts2006$outmat[,2],ntrans=2)
outmat32      <- v2m(parts2006$outmat[,3],ntrans=2)

outmat1[,3]   <- outmat12[,3]
outmat2[,3]   <- outmat22[,3]
outmat3[,3]   <- outmat32[,3]

dataoutveccounter <- c(c(outmat1),c(outmat2),c(outmat3),c(parts2014$pifracs[-1,]),c(parts2014$edufracs[-1]))
dec_fun_redux(eduvec(TR=TR2,"f",2014,ntrans=2),to=5, age = 50, n=31, ntrans = 2, deduct = TRUE)


anti_leaveout(dataoutveccounter,ntrans=2)
dec_fun_redux_leaveout(datoutvec2014,to=1, age = 50, n=31, ntrans = 2, deduct = TRUE)-
dec_fun_redux_leaveout(dataoutveccounter,to=1, age = 50, n=31, ntrans = 2, deduct = TRUE)

# and if 50% sec 50% uni?

tr2014m <- eduvec_leaveout(TR=TR2,"m",2014,ntrans=2)
tr2014f <- eduvec_leaveout(TR=TR2,"f",2014,ntrans=2)
tr2014m_noprim <- tr2014m
tr2014f_noprim <- tr2014f
n <- length(tr2014m_noprim)
tr2014m_noprim[(n-1):n] <- c(.5,.5)
tr2014f_noprim[(n-1):n] <- c(.5,.5)

dec_fun_redux_leaveout(tr2014m,to=1, age = 50, n=31, ntrans = 2, deduct = TRUE) -
dec_fun_redux_leaveout(tr2014m_noprim,to=1, age = 50, n=31, ntrans = 2, deduct = TRUE)

dec_fun_redux_leaveout(tr2014f,to=1, age = 50, n=31, ntrans = 2, deduct = TRUE) -
		dec_fun_redux_leaveout(tr2014f_noprim,to=1, age = 50, n=31, ntrans = 2, deduct = TRUE)


# redux of Figure 2:
# colors
hrs_male_col <- "#053a8e"
hmd_male_col <- "#88aeea"

hrs_fem_col  <- "#a50847"
hmd_fem_col  <- "#ed9ebe"
hmdyrs <- 1992:2016
#e50hmdm    <- mltper$ex
#e50hmdf    <- fltper$ex
e50hmdm    <- c(26.79, 26.68, 26.89, 27.01, 27.18, 27.39, 27.54, 27.61, 27.81, 
		28, 28.1, 28.29, 28.67, 28.72, 29.03, 29.25, 29.29, 29.59, 29.71, 
		29.8, 29.9, 29.89, 29.98, 29.95, 30.07)
e50hmdf    <- c(31.7, 31.48, 31.59, 31.58, 31.64, 31.73, 31.76, 31.66, 31.73, 
		31.84, 31.93, 32.05, 32.4, 32.42, 32.7, 32.92, 32.9, 33.24, 33.33, 
		33.36, 33.47, 33.48, 33.59, 33.53, 33.67)
hmdn <- length(hmdyrs)
hmdi <- 1:hmdn > 3

(maleinc <- e50hmdm[hmdn] - e50hmdm[hmdyrs==1996])
(femaleinc <- e50hmdf[hmdn] - e50hmdf[hmdyrs==1996])
(maleinc / e50hmdm[hmdyrs==1996] * 100)
(femaleinc / e50hmdf[hmdyrs==1996] * 100)

# Author: tim
###############################################################################
#HMD <- local(get(load("/home/tim/git/DistributionTTD/DistributionTTD/Data/HMDltper.Rdata")))
#
#HMD <- HMD[HMD$CNTRY=="USA",]
#HMD <- HMD[HMD$Year >= 1992 & HMD$Age == 50, ]
# LE graph
#yrs <- 1992:2010

# LE HMD:
lem <- dflem+dlem
lef <- dflef+dlef
lef <- t(lef[-2, ])
lem <- t(lem[-2, ])
lef <- cbind(fhrs2, lef)
lem <- cbind(mhrs2, lem)

#colnames(em) <- colnames(ef) <- c("HRS All edu","HRS low edu","high school","HRS high edu")

# need to wait to get HMD update before putting 
# this in a pdf and in the presentation
pdf("Figures/e50_R.pdf")
par(mai = c(1, 1, 1, 2),xpd=TRUE)
plot(NULL, type = "n", ylim = c(24, 38), xlim = c(1992, 2015), axes = FALSE, xlab = "", ylab = "")
lines(hmdyrs[hmdi], e50hmdm[hmdi], col = hmd_male_col,lwd=2,lty="9292")
lines(hmdyrs[hmdi], e50hmdf[hmdi], col = hmd_fem_col,lwd=2,lty="9292")
text(hmdyrs[hmdn],e50hmdm[hmdn],"HMD males",pos=4)
text(hmdyrs[hmdn],e50hmdf[hmdn],"HMD females",pos=4)


axis(1)
axis(2, las = 1)

matplot(c(1996, 2006, 2014), lem, add = TRUE, pch = 16, col = hrs_male_col, 
		type = "o", lty = "8484", lwd = c(3, 1, 1), cex = c(1.5, 1, 1))
matplot(c(1996, 2006, 2014), lef, add = TRUE, pch = 16, col =hrs_fem_col, 
		type = "o", lty = "8484", lwd = c(3, 1, 1), cex = c(1.5, 1, 1))
text(2014, ef[3, -3], colnames(ef)[-3], pos = 4, col = hrs_fem_col, xpd = TRUE)
text(2014, em[3, -3], colnames(em)[-3], pos = 4, col = hrs_male_col, xpd = TRUE)

dev.off()








## stack trans,frac,trans,frac,trans,frac (where frac is 4 pieces)
#eduvec <- function(TR,.sex="m",.time=1996,ntrans=3){
#	outcols <- getcols(ntrans,self=FALSE)
#	TR  <- as.data.frame(TR, stringsAsFactors = FALSE)
#	pri <- subset(TR,sex == .sex & time == .time & edu == "primary")
#	sec <- subset(TR,sex == .sex & time == .time & edu == "secondary")
#	uni <- subset(TR,sex == .sex & time == .time & edu == "terciary")
#	
#	priout <- as.matrix(pri[,outcols])
#	secout <- as.matrix(sec[,outcols])
#	uniout <- as.matrix(uni[,outcols])
#	
#	priv <- c(priout)
#	secv <- c(secout)
#	univ <- c(uniout)
#	
#	pcols <- paste0("s", 1:ntrans, "_prop")
#	
#	prifrac <- unlist(pri[1,pcols])
#	secfrac <- unlist(sec[1,pcols])
#	unifrac <- unlist(uni[1,pcols])
#	
#	fracs    <- cbind(prifrac, secfrac, unifrac)
#	edufracs <- colSums(fracs)
#	pifracs  <- rbind(t(t(fracs) / edufracs),edufracs)
#	
#	# now separate pi and edu fracs
#	c(priv, pifracs[,1], secv, pifracs[,2], univ, pifracs[,3])
#	
#}
#
#dec_fun_redux <- function(datoutvec, to, age = 50, n=31, ntrans = 3, deduct = TRUE){
#	
#	# first get into 3 edu column matrix:
#	dim(datoutvec) <- c(ntrans^2 * n+ntrans+1,3)
#
#	# now ciphen off fractions
#	fracs     <- tail(datoutvec, ntrans+1)
#	datoutvec <- datoutvec[1:(ntrans^2 * n), ]
#	# requires input as vector, first reshape to matrix or df
#	dc        <- rep(0,3)
#	for (i in 1:3){
#		datouti    <- v2m(datoutvec[,i], ntrans = ntrans)
#		# remove death rates and replace with self-arrows, needed to make
#		# transition matrices
#		datselfi   <- out2self(datouti, ntrans = ntrans)
#		# append fractions: normalize so group expectancies look right, then reweight
#        propi      <- fracs[1:ntrans, i] 
#		# then compute e50 using the correct transition rates, and relevant fractions
#		dc[i]      <- e50(datselfi, prop = propi, to = to, age = age, ntrans = ntrans, deduct = deduct)
#	}
#	# radix-weighted mean
#	wmean(dc,fracs[ntrans+1,])
#}