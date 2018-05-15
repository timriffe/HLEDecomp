
# Author: tim
###############################################################################

# two ways: either collapse before getting results or after
# make redundant collapsed rates I guess.
me <- system("whoami",intern=TRUE)
if (me == "mpidr_d\\riffe"){
	setwd("U:/git/HLEDecomp/HLEDecomp")
	read.path <- "N:\\dcs\\proj\\hledecomp\\results"
}
if (me == "tim"){
	setwd("/home/tim/git/HLEDecomp/HLEDecomp")
	read.path <- "/home/tim/Data/hledecomp/results"
}

# 2 state model
# m11 <- m11
# m14 <- m14 # keep 4 as dead
# m12 <- m12 + m13
# m22 <- ? residual?
# m21 <- (m21 * pi2 + m31 * pi3) / (pi2 + pi3)
# m24 <- (m24 * pi2 + m34 * pi3) / (pi2 + pi3) # and check that it equals the residual?

version <- "02"

readTR <- function(version){
	path <- file.path("Data", "Transitions", "DCS", paste0("TR_v", version, ".Rdata"))
	TR.i <- local(get(load(path)))
	TR.i
}
TR.i <- readTR("02")

library(data.table)
TR.i    <- data.table(TR.i)
#TR.prev <- TR.i[,do_prev_chunk(.SD)]
# now conundrum, need a good way to attach the initprop info. When in chunks can save as metadata,
# but how to attach metadata to a .SD chunk?

do_prev_chunk <- function(X,
		age = 52, 
		deduct = TRUE, 
		dcs = FALSE, 
		path = "N:\\dcs\\proj\\hledecomp\\results\\margins"){
			
			prop    <- attr(X, "initprop")
			year    <- attr(X, "time")
			
			U       <- data_2_U(X)
			N       <- U2N(U, interval = 2)
			cind    <- rep(seq(50,112,by=2), 3) == age
			DF      <- as.data.frame(matrix(rowSums(N[,cind] %*% diag(prop)),ncol=3,dimnames=list(NULL,1:3)))
			DF$time <- year
			DF

}




