# Author: tim
###############################################################################


# This is a horiuchified version of LTRE
hg <- function(f, pars1, pars2, N = 20, ...){
	d 			<- pars2-pars1
	n 			<- length(pars1)
	delta 		<- d/N
	x           <- pars1 + d * matrix(rep(.5:(N-.5)/N,n), byrow = TRUE, ncol = N)
	cc          <- matrix(0, nrow = n, ncol = N)
	for (i in 1:N){
		cc[,i] <- grad(f, x[,i],...) * delta
	}
	rowSums(cc)
}

# a simple nonsensical function that is based on two out of three
# elements of a composition.
f <- function(x){
	stopifnot(length(x)==3)
	# composition enforced
	x <- x/sum(x)
	I <- diag(3)
	U <- cbind(rbind(0,(diag(x[1:2]))),0)
	colSums(solve(I-U))[1] 
}

f_noconst <- function(x){
	stopifnot(length(x)==3)
	# composition not enforced
	I <- diag(3)
	U <- cbind(rbind(0,(diag(x[1:2]))),0)
	colSums(solve(I-U))[1] 
}



# we can get the same result using any 
# leave-one-out combination of parameters
f12 <- function(x12){
	x <- c(x12,1-sum(x12))
	f(x) 
}

f13 <- function(x13){
	x <- c(x13[1],1-sum(x13),x13[2])
	f(x) 
}

f23 <- function(x23){
	x <- c(1-sum(x23),x23)
	f(x) 
}

# generate input vectors
set.seed(1)
x1 <- runif(3)
x2 <- runif(3)
# pre-constrained
x1 <- x1/sum(x1)
x2 <- x2/sum(x2)

f(x2)-f(x1)

f12(x2[1:2]) - f12(x1[1:2])
f13(x2[c(1,3)]) - f13(x1[c(1,3)])
f23(x2[2:3]) - f23(x1[2:3])

d12 <- hg(f12,x2[1:2],x1[1:2],20)
d13 <- hg(f13,x2[c(1,3)],x1[c(1,3)],20)
d23 <- hg(f23,x2[2:3],x1[2:3],20)

d123 <- cbind(d12=c(d12,NA ), 
		d13 = c(d13[1],NA,d13[2]),
		d23 = c(NA,d23))

hg(f,x2,x1,20)
hg(f_noconst,x2,x1,20)
#
1 / (colSums( 1/d123, na.rm=TRUE)/3)


# gradient methods don't matter.
(hg(f12,x2[1:2],x1[1:2],20,method="simple"))-
(hg(f12,x2[1:2],x1[1:2],20,method="Richardson"))

(hg(f12,x2[1:2],x1[1:2],20,method="complex"))-
		(hg(f12,x2[1:2],x1[1:2],20,method="Richardson"))

# comapre w horiuchi
library(DemoDecomp)
(d12h <- horiuchi(f12,x2[1:2],x1[1:2],20))
(d13h <- horiuchi(f13,x2[c(1,3)],x1[c(1,3)],20))
(d23h <- horiuchi(f23,x2[2:3],x1[2:3],20))

# asymptotically the same!
d12-d12h
d13-d13h
d23-d23h

