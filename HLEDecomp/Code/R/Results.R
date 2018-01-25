me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
}
source("Code/R/Functions.R")


m1995    <- get_data(time = 1995, self = FALSE)
m2004    <- get_data(time = 2004, self = FALSE)
m2014    <- get_data(time = 2014, self = FALSE)

# decompose 1995 vs 2004
dec1.1   <- HLEDecomp(m1995, m2004, N = 100, to = 1)[-1, ]
dec1.2   <- HLEDecomp(m1995, m2004, N = 100, to = 2)[-1, ]
dec1.3   <- HLEDecomp(m1995, m2004, N = 100, to = 3)[-1, ]
dec1.tot <- dec1.1 + dec1.2 + dec1.3

# decompose 2004 vs 2014
dec2.1   <- HLEDecomp(m2004, m2014, N = 100, to = 1)[-1, ]
dec2.2   <- HLEDecomp(m2004, m2014, N = 100, to = 2)[-1, ]
dec2.3   <- HLEDecomp(m2004, m2014, N = 100, to = 3)[-1, ]
dec2.tot <- dec2.1 + dec2.2 + dec2.3

# decompose 1995 vs 2014
dec3.1   <- HLEDecomp(m1995, m2014, N = 100, to = 1)[-1, ]
dec3.2   <- HLEDecomp(m1995, m2014, N = 100, to = 2)[-1, ]
dec3.3   <- HLEDecomp(m1995, m2014, N = 100, to = 3)[-1, ]
dec3.tot <- dec3.1 + dec3.2 + dec3.3

1+1

library(popbio)
out2U <- function(datout){
	data_2_U(out2self(datout))
}
U1995 <- out2U(m1995)
U2004 <- out2U(m2004)
U2014 <- out2U(m2014)
colSums(U1995)

Dm <- U2004 - U1995
Ac <- (U2004 + U1995) / 2
SAc <- sensitivity(Ac)

ev <- eigen(Ac)
lmax <- which.max(Re(ev$values))
W <- ev$vectors

w <- abs(Re(W[, lmax]))
V <- try(Conj(solve(W)), silent = TRUE)
if (class(V) == "try-error") {
	stop("matrix A is singular")
}
v <- abs(Re(V[lmax, ]))
s <- v %o% w


Cm <- Dm * SAc

image(eigen(Ac)$vectors)

data(calathea)
calathea_pool<-calathea[['pooled']]

## Create plots like FIGURE 7 in Horvitz et al 1997
##PLOTS
plots<- split(calathea[-17], rep(1:4,each=4))
## use Mean matrix since pooled not available by plot
plots<- lapply(plots, mean)
Cm<-LTRE(plots, calathea_pool)
pe<-sapply(Cm, sum)
barplot(pe, xlab="Plot", ylab="Plot effect" , ylim=c(-.25, .25),
		col="blue", las=1)
abline(h=0)
box()
title(expression(italic("Calathea ovandensis")))
#matplot(out2self(m2004)-out2self(m2014),type='l')

#
#m1995 <- get_data(time = 1995)
#m2004 <- get_data(time = 2004)
#m2014 <- get_data(time = 2014)
#
## hle
#hle95 <- e50(m1995, to = 1, age = 52)
#hle04 <- e50(m2004, to = 1, age = 52)
#hle14 <- e50(m2014, to = 1, age = 52)
#
## adl1
#a195 <- e50(m1995, to = 2, age = 52)
#a104 <- e50(m2004, to = 2, age = 52)
#a114 <- e50(m2014, to = 2, age = 52)
#
## adl2
#a295 <- e50(m1995, to = 3, age = 52)
#a204 <- e50(m2004, to = 3, age = 52)
#a214 <- e50(m2014, to = 3, age = 52)
#
## tot
#tot95 <- hle95 + a195 + a295
#tot04 <- hle04 + a104 + a204
#tot14 <- hle14 + a114 + a214
#
#print(data.frame(hle = c(hle95,hle04,hle14),
#		adl1 = c(a195,a104,a114),
#		adl2p = c(a295,a204,a214),
#		tot = c(tot95,tot04,tot14)),digits=3)
