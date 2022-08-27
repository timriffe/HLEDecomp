# for working with basic leave-one-out cases
library(tidyverse)

# this Lx function uses full info on transfers. Typical mid-interval assumption,
# but no lx linear interpolation happens
calc_Lx_loop3 <- function(Ptibble, init){
	n        <- nrow(Ptibble)
	Ptibble  <- as.matrix(Ptibble)
	lx       <- matrix(NA, n + 1, 2, dimnames = list(0:n,c("H","U")))
	dx       <- matrix(0, n + 1,2, dimnames = list(0:n,c("H","U")))
	lx[1, ]  <- init
	Lx       <- lx * 0
	Lx       <- Lx[1:n, ]
	for (i in 1:n){
		lx[i+1,"H"] <- lx[i,"H"] * Ptibble[i,"HH"] + lx[i,"U"] * Ptibble[i,"UH"]
		lx[i+1,"U"] <- lx[i,"U"] * Ptibble[i,"UU"] + lx[i,"H"] * Ptibble[i,"HU"]
		
		Lx[i, "H"] <- lx[i,"H"] * Ptibble[i,"HH"] + 
			# exits / 2
			(lx[i,"H"] * (Ptibble[i,"HU"] + Ptibble[i,"HD"])) / 2 +
			# entries / 2
			(lx[i,"U"] * Ptibble[i,"UH"]) / 2
		
		Lx[i, "U"] <- lx[i,"U"] * Ptibble[i,"UU"] + 
			# exits / 2
			(lx[i,"U"] * (Ptibble[i,"UH"] + Ptibble[i,"UD"])) / 2 +
			# entries / 2
			(lx[i,"H"] * Ptibble[i,"HU"]) / 2
	}
	
	Lx
}

# gives selected column sum, or else the full sum of Lx
calc_ex_3 <- function(Ptibble, init = c(.8,.2), state = "H"){
	Lx <- Ptibble %>% calc_Lx_loop3(init = init)
	if (state %in% c("U","H")){
		return(colSums(Lx)[state])
	} else {
		return(sum(Lx))
	}
}

# now we need function machinery for partial information settings:
complete_partial_Ptibble <- function(partial_Ptibble){
	partial_Ptibble <- as.data.frame(partial_Ptibble)
	
	all_from_to <- c("HH","HU","HD","UH","UU","UD")
	Missing <- setdiff(all_from_to, names(partial_Ptibble))
	
	if (length(Missing) == 0){
		return(partial_Ptibble)
	}
	partial_Ptibble[Missing] <- NA
	
	n <- nrow(partial_Ptibble)
	rownames(partial_Ptibble) <- 0:(n-1)
	
	partial_Ptibble %>% 
		as_tibble(rownames = NA) %>% 
		rownames_to_column("age") %>% 
		pivot_longer(-age,names_to = "from_to", values_to = "p") %>% 
		mutate(from = substr(from_to, 1, 1)) %>% 
		group_by(from, age) %>% 
		mutate(p = if_else(is.na(p), 1 - sum(p, na.rm = TRUE), p)) %>% 
		ungroup() %>% 
		select(-from) %>% 
		pivot_wider(names_from = from_to, values_from = p) %>% 
		column_to_rownames(var="age")
}

# some standards for jumping between named vec and standard Ptibble
# just because it makes life easier
partial_Ptibble_to_vec <- function(partial_Ptibble){
	# trick for reshaping is to name things
	n <- nrow(partial_Ptibble)
	
	outl <-
		partial_Ptibble %>% 
		mutate(age = 0:(n-1)) %>% 
		pivot_longer(-age, names_to = "from_to", values_to = "p") %>% 
		mutate(from_to = paste(age, from_to))  %>% 
		as.list()
	
	out <- outl[["p"]]
	names(out) <- outl[["from_to"]]  
	out
}

# this is premised on having a named vector of the form
# "0 HH" where we have {age, space, fromto}
vec_to_partial_Ptibble <- function(vec_with_names){
	vec_with_names %>% 
		as.data.frame() %>% 
		rownames_to_column("from_to") %>% 
		separate(from_to, into = c("age", "from_to"), sep = " ") %>% 
		pivot_wider(names_from = from_to, values_from = ".") %>% 
		column_to_rownames(var = "age") 
	
}

# and for jumping from a named vec to a given expectancy
partial_vec_to_ex <- function(vec_with_names, init = c(.8,.2), state = "H"){
	vec_with_names %>% 
		vec_to_partial_Ptibble() %>% 
		complete_partial_Ptibble() %>% 
		calc_ex_3(init = init, state = state)
}

# slightly modified horiuchi from DemoDecomp: this one preserves
# names in the internal matrices, which we need in order to reconstitute
# Ptibble on the fly..
horiuchi2 <- function(func, pars1, pars2, N, ...){ 
	d     <- pars2 - pars1
	n     <- length(pars1)
	delta <- d / N
	grad  <- matrix(rep(0.5:(N - 0.5)/N, n), byrow = TRUE, ncol = N,
					dimnames = list(names(pars1), 1:N))
	x     <- pars1 + d * grad
	cc    <- matrix(0, nrow = n, ncol = N,
					dimnames = list(names(pars1), 1:N))
	for (j in 1:N) {
		DD <- diag(delta/2)
		for (i in 1:n) {
			cc[i, j] <- 
				func((x[, j] + DD[, i]), ...) - 
				func((x[, j] - DD[, i]), ...)
		}
	}
	rowSums(cc)
}

# ---------------------------------------------------------- #
# functions for conditional vectors                          #
# ---------------------------------------------------------- #
# special functions
Ptibble_to_conditional_vec <- function(Ptibble, 
									   base_to = c("HD","UD"), 
									   secondary_to = c("HH","UU")){
	
	# solve matching using joins
	.base_to <-
		base_to %>% 
		as_tibble() %>% 
		rename(base_to = value) %>% 
		mutate(from = substr(`base_to`,1,1))
	
	.secondary_to <-
		secondary_to %>% 
		as_tibble() %>% 
		rename(secondary_to = value) %>% 
		mutate(from = substr(`secondary_to`,1,1))
	
	outl <-
		Ptibble %>% 
		rownames_to_column(var = "age") %>% 
		pivot_longer(2:7, names_to = "from_to", values_to = "p") %>% 
		mutate(from = substr(from_to, 1, 1)) %>% 
		left_join(.base_to, by = "from") %>% 
		left_join(.secondary_to, by = "from") %>% 
		dplyr::filter(from_to %in% c(base_to,secondary_to)) %>% 
		group_by(age,from) %>% 
		mutate(denom = 1- p[from_to == base_to],
			   p_conditional = if_else(from_to == base_to, p, p / denom)) %>% 
		ungroup() %>% 
		mutate(label = case_when(from_to == base_to ~ base_to,
								 from_to == secondary_to ~ paste0(secondary_to,"/","(1-",base_to,")")),
			   label = paste(age,label)) %>% 
		select(p_conditional, label) %>% 
		as.list()
	
	out <- outl[["p_conditional"]]
	names(out) <- outl[["label"]]  
	out
}


# we need a function to get back the original parameters, will require trickery
conditional_vec_to_partial_Ptibble <- function(labelled_conditional_vec){
	labelled_conditional_vec %>% 
		as.data.frame() %>% 
		rownames_to_column(var = "label") %>% 
		rename(p_conditional = ".") %>% 
		separate(label, into = c("age", "label"), sep = " ") %>%
		mutate(from = substr(label, 1, 1)) %>% 
		group_by(age, from) %>% 
		mutate(denom = 1 - p_conditional[nchar(label) == 2],
			   p = case_when(nchar(label) == 2 ~ p_conditional,
			   			  TRUE ~ p_conditional * denom)) %>% 
		ungroup() %>% 
		mutate(from_to = substr(label, 1, 2)) %>% 
		select(age, from_to, p) %>% 
		pivot_wider(names_from = from_to,
					values_from = p) %>% 
		column_to_rownames("age") 
}

# take it the whole path from conditional parameters
# to the desired expectancy
conditional_vec_to_ex <- function(labelled_conditional_vec, init, state){
	labelled_conditional_vec %>% 
		conditional_vec_to_partial_Ptibble() %>% 
		complete_partial_Ptibble() %>% 
		calc_ex_3(init = init, state = state)
}

# reshaper function for making sense of decomp results
reshape_conditional_decomp <- function(conditional_cc){
	conditional_cc %>% 
		as.data.frame() %>% 
		rownames_to_column(var = "label") %>% 
		rename(p_conditional = ".") %>% 
		separate(label, into = c("age", "label"), sep = " ")
}

# ----------------------------------------------------------- #
# Functions for rate-based decompositions (instead of prob)
# ----------------------------------------------------------- #

# makes a single transition block
pi2u <- function(pivec, 
				 from ="H",
				 to = "H",
				 start_age = 50,
				 interval = 2){
	out           <- cbind(rbind(0, diag(pivec)), 0)
	n             <- length(pivec) 
	# the final subtraction of the interval is particular to
	# the way these probabilities were estimated and labelled.
	# to technically our first one goes from 48 to 50, not from 50 to 52.
	ages          <- ((0:n) * interval) + start_age - interval
	from_names    <- paste(from, ages, sep = "::")
	to_names      <- paste(to, ages, sep = "::")
	dimnames(out) <- list(to_names, from_names)
	out
}

u2U_closed <- function(HH, HU, UH, UU){
	out <- rbind(
		cbind(HH, UH),
		cbind(HU, UU))
	
	out <- cbind(rbind(out, 1 - colSums(out)),0)
	colnames(out)[ncol(out)] <- "D::Inf"
	rownames(out)[nrow(out)] <- "D::Inf"
	out[nrow(out),ncol(out)] <- 1
	out
}

# this version avoids singularity issues when inverting the matrix
# namely, the first row  of 0s is a problem, but it's fine to just omit
# that row. 
Ptibble2U_closed <- function(Ptibble, interval = 1){
	n <- nrow(Ptibble) + 1
	HH <- Ptibble %>% 
		pull(HH) %>% 
		pi2u("H","H", start_age = 1, interval = interval) %>% 
		'['(-1,-n) # hard coded for this example. 
	# Could be dealt with in pi2u() more generally
	HU <- Ptibble %>% 
		pull(HU) %>% 
		pi2u("H","U", start_age = 1, interval = interval) %>% 
		'['(-1,-n)
	UU <- Ptibble %>% 
		pull(UU) %>% 
		pi2u("U","U", start_age = 1, interval = interval) %>% 
		'['(-1,-n)
	UH <- Ptibble %>% 
		pull(UH) %>% 
		pi2u("U","H", start_age = 1, interval = interval) %>% 
		'['(-1,-n)
	
	U <- u2U_closed(HH, HU, UH, UU)
	U
}

# Note we don't need to transpose, the dims and locations
# of everything match conveniently. And we assign the original
# names of U to it. Even though this is technically not correct,
# since U is framed in terms of from-to, whereas Q is a rate over
# an interval
U2Q <- function(U, interval = 1){
	Q <-
		U %>% 
		expm::logm() %>% 
		zapsmall() %>% 
		'/'(interval)
	dimnames(Q) <- dimnames(U)
	Q
}

# create a rate tibble (better use of space!) from Q
Q2Rtibble <- function(Q){
	Q %>% 
		as.data.frame() %>% 
		rownames_to_column(var = "to") %>% 
		pivot_longer(-1, names_to = "from", values_to = "R") %>% 
		separate(to, 
				 into = c("state_to", "age_to"), 
				 sep = "::", 
				 convert = TRUE) %>% 
		separate(from, 
				 into = c("state_from", "age_from"), 
				 sep = "::",
				 convert = TRUE) %>% 
		filter(age_to == (age_from + 1) |
			   	is.infinite(age_to),
			   !is.infinite(age_from)) %>% 
		select(-age_to) %>% 
		mutate(transition = paste0(state_from, state_to), 
			   .keep = "unused") %>% 
		pivot_wider(names_from = transition,
					values_from = R) %>% 
		rename(age = age_from)
}

# take the chain all the way from a Ptibble to a Qtibble
Ptibble2Rtibble <- function(Ptibble, interval){
	Ptibble %>% 
		Ptibble2U_closed() %>% 
		U2Q() %>% 
		Q2Rtibble() %>% 
		column_to_rownames("age")
}

# given partial information on rates, we can complete
# the missing rates. Note incoming self-arrows do not
# here need to have a negative sign. This will be forced
# internally so make the machinery work.
complete_partial_Rtibble <- function(partial_Rtibble){
	partial_Rtibble <- as.data.frame(partial_Rtibble)
	
	all_from_to <- c("HH","HU","HD","UH","UU","UD")
	Missing <- setdiff(all_from_to, names(partial_Rtibble))
	if (length(Missing) == 0){
		return(partial_Rtibble)
	}
	partial_Rtibble[Missing] <- NA
	
	# ----------------------------- #
	# ensure self-arrows are negative
	partial_Rtibble$HH <- 
		partial_Rtibble %>% 
		pull(HH) %>% 
		'*'(sign(.)) %>% 
		'*'(-1)
	partial_Rtibble$UU <- 
		partial_Rtibble %>% 
		pull(UU) %>% 
		'*'(sign(.)) %>% 
		'*'(-1)
	# ----------------------------- #
	
	# similar logic to that used in
	# complete_partial_Ptibble()
	partial_Rtibble %>% 
		as_tibble(rownames = NA) %>% 
		rownames_to_column("age") %>% 
		pivot_longer(-age,names_to = "from_to", values_to = "p") %>% 
		mutate(from = substr(from_to, 1, 1)) %>% 
		group_by(from, age) %>% 
		mutate(p = if_else(is.na(p), 0 - sum(p, na.rm = TRUE), p)) %>% 
		ungroup() %>% 
		select(-from) %>% 
		pivot_wider(names_from = from_to, values_from = p) %>% 
		column_to_rownames(var="age")
}

# Create Q from an Rtibble. This creates a necessary object/link to derive
# back probabilities, but allows us to decompose wrt rates.
Rtibble2Q <- function(Rtibble){
	HH <- Rtibble %>% 
		pull("HH") %>% 
		pi2u(from = "H", to = "H", start_age = 1, interval = 1) %>% 
		'['(-1,-7)
	
	HU <- Rtibble %>% 
		pull(HU) %>% 
		pi2u("H","U", start_age = 1, interval = 1) %>% 
		'['(-1,-7)
	
	UU <- Rtibble %>% 
		pull(UU) %>% 
		pi2u("U","U", start_age = 1, interval = 1) %>% 
		'['(-1,-7)
	
	UH <- Rtibble %>% 
		pull(UH) %>% 
		pi2u("U","H", start_age = 1, interval = 1) %>% 
		'['(-1,-7)
	
	Q <- rbind(
		cbind(HH, UH),
		cbind(HU, UU))
	
	Q <- cbind(rbind(Q, -colSums(Q)),0)
	colnames(Q)[ncol(Q)] <- "D::Inf"
	rownames(Q)[nrow(Q)] <- "D::Inf"
	Q
}

# Full path from Rtibble to Ptibble, involving first creation of Q,
# then expm and interval adjustment, then conversion of the resulting 
# U to a Ptibble.
Rtibble2Ptibble <- function(Rtibble, interval = 1){
	Rtibble %>% 
		Rtibble2Q() %>% 
		expm::expm() %>% 
		'*'(interval) %>% 
		as.data.frame() %>% 
		rownames_to_column("to") %>% 
		pivot_longer(-1, names_to = "from", values_to = "P") %>% 
		separate(to, 
				 into = c("state_to", "age_to"), 
				 sep = "::", 
				 convert = TRUE) %>% 
		separate(from, 
				 into = c("state_from", "age_from"), 
				 sep = "::",
				 convert = TRUE) %>% 
		filter(age_to == age_from |
			   	is.infinite(age_to),
			   !is.infinite(age_from)) %>% 
		select(-age_to) %>% 
		mutate(transition = paste0(state_from, state_to), 
			   .keep = "unused") %>% 
		pivot_wider(names_from = transition,
					values_from = P) %>% 
		rename(age = age_from)
}

# Full chain (including optional undoing of a pre-transformation)
# taking the partial rate vector, converting it to a tibble, completing it,
# turning it into a Ptibble, then calculating ex, whew
partialR_vec_to_ex <- function(vec_with_names, 
							   init = c(.8,.2), 
							   state = "H",
							   anti_function = function(x){x}){
	vec_with_names %>% 
		anti_function() %>% 
		# also works for Rates
		vec_to_partial_Ptibble() %>% 
		complete_partial_Rtibble() %>% 
		Rtibble2Ptibble() %>% 
		calc_ex_3(init = init, state = state)
}






