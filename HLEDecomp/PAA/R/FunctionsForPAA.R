# NOPE NOPE NOPE, Stuff just doesn't sum properly, so the decomposition
# resulting from this function is somehow invalid.
# Will stick with 'self-as-residual' decomposition results for now.

# this code does the following decomp tweak:
# tweak and element, the rescale the full composition (inclusive) to sum to 1.
# this does not preserve differences, however, need to retry.

# Author: tim
###############################################################################


# start single subset functions- still valid for subsets
f_dec_rescale_all <- function(vecall,to=1,age=52,ntrans=2,deduct=TRUE){
	
	# first, siphen off the starting proportions.
	n                <- length(vecall)
	ii               <- (n-ntrans+1):n
	propi            <- vecall[ii]
	vecall           <- vecall[-ii]
	
	# form into expected table layout
	datall           <- v2mall(vecall)
	colnames(datall) <- getcolsall(ntrans)
	
	# ensure constrained
	for (i in 1:ntrans){
		iii <- ((i-1)*(ntrans+1) +1):(i*(ntrans+1))
		ddd <- datall[,iii]
		ddd <- ddd / rowSums(ddd)
		datall[,iii] <- ddd
	}
	
	# gotta scale this in case it's being perturbed
	propi <- propi / sum(propi)
	
	# can just send in all columns because e50 selects the self-arrows anyway, no worries.
	e50(datall, 
			prop = propi, 
			to = to, 
			age = age, 
			ntrans = ntrans, 
			deduct = deduct, 
			dead = ntrans + 1)
}

#TR2
#datall           <- v2mall(vecall.1)
#colnames(datall) <- getcolsall(ntrans)


# retrieve vec for subset
getvecall <- function(.SD,ntrans=2){
	.SD       <- as.data.frame(.SD)
	propnames <- paste0("s",1:ntrans,"_prop")
	
	# need to rescale prop too
	prop      <- unlist(.SD[1,propnames])
	prop      <- prop / sum(prop)
	
	colsall   <- getcolsall(ntrans)
	datall    <- as.matrix(.SD[,colsall])
	vecall    <- c(datall, prop)
	vecall
}

# no composition here except starting composition. Education, etc can be worked in
# in a later iteration.


f_dec_rescale_all_edu_dt <- function(.SD,to=1,age=50,ntrans=2,deduct=TRUE){
	
	pri_all <- getvecall(.SD[edu=="primary"])
	sec_all <- getvecall(.SD[edu=="secondary"])
	ter_all <- getvecall(.SD[edu=="terciary"])
	
	# calcualte result
	pri_obj <- f_dec_rescale_all(pri_all,
			to = to,
			age = age, 
			ntrans = ntrans, 
			deduct = TRUE)
	sec_obj <- f_dec_rescale_all(sec_all,
			to = to,
			age = age, 
			ntrans = ntrans, 
			deduct = TRUE)
	ter_obj <- f_dec_rescale_all(ter_all,
			to = to,
			age = age, 
			ntrans = ntrans, 
			deduct = TRUE)
	
	eduprop <- rowSums(.SD[age==50,c("s1_prop","s2_prop")])
	eduprop <- eduprop / sum(eduprop)
	sum(eduprop * c(pri_obj, sec_obj, ter_obj))
}

f_dec_rescale_all_edu_vec <- function(vec_all_edu,to=1,age=50,ntrans=2,deduct=TRUE){
	
	# layout?
	# transpri,sproppri,transsec,spropsex,transter,spropter
	# 33,33,33,3
	n                <- length(vec_all_edu)
	
	edu_prop         <- vec_all_edu[(n-2):n]
	vec_all_edu      <- vec_all_edu[-c((n-2):n)]
	
	# now re-dim! (3 is nr of edu groups
	n                <- n - 3
	dim(vec_all_edu) <- c(n / 3, 3)
	
	pri_all          <- vec_all_edu[, 1]
	sec_all          <- vec_all_edu[, 2]
	ter_all          <- vec_all_edu[, 3]
	
	pri_obj <- f_dec_rescale_all(pri_all,
			to = to,
			age = age, 
			ntrans = ntrans, 
			deduct = deduct)
	sec_obj <- f_dec_rescale_all(sec_all,
			to = to,
			age = age, 
			ntrans = ntrans, 
			deduct = deduct)
	ter_obj <- f_dec_rescale_all(ter_all,
			to = to,
			age = age, 
			ntrans = ntrans, 
			deduct = deduct)
	sum(edu_prop * c(pri_obj, sec_obj, ter_obj))
}

chunk_2_vec_all_edu <- function(.SD){
	pri_all <- getvecall(.SD[edu=="primary"])
	sec_all <- getvecall(.SD[edu=="secondary"])
	ter_all <- getvecall(.SD[edu=="terciary"])
	eduprop <- rowSums(.SD[age==50,c("s1_prop","s2_prop")])
	c(pri_all, sec_all, ter_all, eduprop)
}


# This is the one to compare with the PAA submission results.
# everything sums into the same cells as in the tables. Pretty
# slow, as there are 1/3 more elements to include in the decomposition
f_dec_rescale_all_edu_decomp_dt <- function(
		DT,
		to=5,
		ntrans = 2,
		deduct = TRUE,
		N = 20,
		sex1="f",
		time1 = 1996,
		sex2=sex1,
		time2 = 2006){
	
	
	vec_all_edu_1 <- chunk_2_vec_all_edu(DT[sex==sex1 & time==time1])
	vec_all_edu_2 <- chunk_2_vec_all_edu(DT[sex==sex2 & time==time2])
	
	dec <- DemoDecomp::horiuchi(func = f_dec_rescale_all_edu_vec,
			pars1 = vec_all_edu_1,
			pars2 = vec_all_edu_2,
			N = N,
			to = to,
			ntrans = ntrans)
	
	n                <- length(dec)
	
	edu_comp         <- dec[(n - 2):n]
	dec              <- dec[-c((n-2):n)]
	
	# now re-dim! (3 is nr of edu groups
	n                <- n - 3
	dim(dec)         <- c(n / 3, 3)
	
	# last two rows are disability component
	n                <- nrow(dec)
	# a 2x3 matrix, just sum it
	dis_comp         <- dec[(n - 1):n, ]
	dec              <- dec[1:(n - 2), ]
	
	# ok
	pri_all          <- v2mall(dec[, 1], ntrans = ntrans)
	sec_all          <- v2mall(dec[, 2], ntrans = ntrans)
	ter_all          <- v2mall(dec[, 3], ntrans = ntrans)
	
	# last 2 elements of each are disability composition
	cols             <- getcolsall(ntrans=ntrans)
	trans_comp       <- pri_all + sec_all + ter_all
	colnames(trans_comp) <- cols
	c(colSums(trans_comp),disability=sum(dis_comp),edu=sum(edu_comp))
}
# so give pieces to e50
# at least one subset item must be different, otherwise pointless

