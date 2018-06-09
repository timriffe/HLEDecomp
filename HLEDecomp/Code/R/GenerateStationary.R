# this script needs a redux, actually turn into function where you give matrix, starting proportions
# etc, and it spits back random trajectories.

# Author: tim
###############################################################################


library(markovchain)
library(spatstat)
# here I just read in the first matrix given in the supplementary material to:
# Christian Dudel & Mikko Myrskylä (2017)
# Working Life Expectancy at Age 50 in the United States and the Impact of the Great Recession
# Demography https://link.springer.com/article/10.1007/s13524-017-0619-6


# get a transition matrix
TM <- as.matrix(
		read.csv("/home/tim/git/TransientSymmetry/TransientSymmetry/Data/Pmat_b_f_1994.csv",
				check.names=FALSE)
)


# transpose for standard MC stuff.
# demographers do stuff backwards I guess
TM <- t(TM)

# make s4 transition matrix from markovchain package
mcEmpl <- new("markovchain", states = rownames(TM),
		byrow = TRUE, transitionMatrix = TM,
		name = "Empl")

# how many sequences should we generate?
N      <- 1e4

# each assuming a start in employment at age 50.
RTraj  <- replicate(N,
		rmarkovchain(n = 50, object = mcEmpl, t0 = "50::Employed", parallel = TRUE)
) 
RTraj                 <- rbind(rep("50::Employed",N), RTraj)
RTraj_clean           <- gsub(".*:","",RTraj)
rownames(RTraj_clean) <- 50:100


