
# determine columns minus the resid
cols_resid <- function(ntrans, 
                       resid = c("self","other","dead")){
  resid      <- match.arg(resid)
  allcols    <- getcolsall(ntrans)
  from       <- substr(allcols, 2, 2)
  to         <- substr(allcols, 3, 3)
  
  # figure out which to take:
  if (resid == "self"){
    havecols <- allcols[to != from]
  }
  if (resid == "dead"){
    to       <- as.integer(to)
    havecols <- allcols[to <= ntrans]
  }
  if (resid == "other"){
    to       <- as.integer(to)
    from     <- as.integer(from)
    havecols <- allcols[!((to <= ntrans) & (from <= ntrans) & (to != from))]
  }
  havecols
}

datvec_resid <- function(chunk, ntrans, resid = c("self","other","dead")){
  resid     <- match.arg(resid)
  
  havecols  <- cols_resid(ntrans, resid)
  datmat    <- as.matrix(chunk[, havecols])
  # vec it!
  c(datmat)
}

v2m_resid <- function(vec, ntrans, resid = c("self","other","dead")){
  resid     <- match.arg(resid)
  havecols  <- cols_resid(ntrans, resid)
  mat       <- v2m(vec, ntrans)
  colnames(mat) <- havecols
  mat
}

# this function is for decomposing: 
# 1) not taking initial conditions into account.
# 2) where a specified state is a defacto residual for decompositon purposes.
# 3) without strata
dec_fun_resid <-
  function(datvec,
           to,
           age = 50,
           prop,
           ntrans = 2,
           deduct = TRUE,
           resid = "dead") {
    # requires input as vector, first reshape to matrix or df
    # vm assumes one destination state is excluded,
    # but it doesn't matter which
    datmat           <- v2m_resid(datvec, ntrans = ntrans, resid = resid)
    # ** now we figure out columns **
    # set of all transitions
    allcols          <- getcolsall(ntrans)
    # transitions given
    havecols         <- colnames(datmat)
    # make datself, which really just needs to 
    # 'include' self, it can have more than 
    # necessary too.
    needcols <- allcols[!allcols %in% havecols]
    # we know we have ntrans columns per ntrans
    # origin states. 
    # make a box to put it in
    datself  <- matrix(NA, 
                       ncol = ntrans^2 + ntrans, 
                       nrow = nrow(datmat),
                       dimnames = list(NULL, allcols))
    # iterate over n origin states
    for (i in 1:ntrans){
        ind    <- (i * ntrans - (ntrans - 1)):(i * ntrans)
        mati   <- datmat[, ind]
        mati   <- cbind(mati, 1 - rowSums(mati))
        ind2    <- (i * (ntrans + 1) - ntrans):(i * (ntrans+1))
        colnames(mati) <- c(havecols[ind], needcols[i])
        datself[,colnames(mati)]  <- mati
    }
    # get columns in the right order
    datself <- datself[, allcols]

    # then compute e50 using the correct transition rates
    dc      <-
      e50(
        datself,
        to = to,
        age = age,
        prop = prop,
        ntrans = ntrans,
        deduct = deduct
      )
    
    dc
  }


dec_fun_resid_wrapper <- function(
    TR,
    time1=2006,
    time2=2014,
    .sex="f",
    prop,
    ntrans=2,
    to=5,
    deduct = TRUE,
    N=20,
    age=50,
    resid = c("self","other","dead")){
  
    resid <- match.arg(resid)
  
    # assuming all edu
    D1    <- subset(TR, time == time1 & sex == .sex)
    D2    <- subset(TR, time == time2 & sex == .sex)
  
    vec1  <- datvec_resid(D1,ntrans=ntrans, resid = resid)
    vec2  <- datvec_resid(D2,ntrans=ntrans, resid = resid)
    
    deci  <- horiuchi(dec_fun_resid, 
                      pars1 = vec1,
                      pars2 = vec2,
                      N = N,
                      ntrans = ntrans,
                      deduct = deduct,
                      prop = prop,
                      to = to,
                      age = age,
                      resid = resid)
    
    deci <- v2m_resid(deci, ntrans = ntrans, resid = resid)
    
    # throw out age for this.
    out  <- colSums(deci)
    out
}


# ---------------------------------------------- #
# Functios for conditional decomp                #
# ---------------------------------------------- #
logit <- function(x){
  log(x / (1 - x))
  }
expit <- function(x){
  exp(x) / (1 + exp(x))
}

# vec operation, also performs compliments as necessary
datvec_cond <- function(chunk, ntrans = 2, compliment = c(surv = FALSE, leave = FALSE)){
  
  # HARD CODED FOR NOW
  stopifnot(ntrans == 2)
  
  allcols <- getcolsall(ntrans=ntrans)
  mort    <- allcols[grepl(as.character(ntrans+1),allcols)]
  
  #trans   <- allcols[!allcols %in% mort]
  
  # THIS CODE IS NOT GENERALIZED WARNING, NEEDED QUICK RESULTS.
  trans1 <- c("m11","m12")
  trans2 <- c("m22","m21")
  # rescale transient transfers
  Trans1 <- as.matrix(chunk[,trans1])
  Trans2 <- as.matrix(chunk[,trans2])
  Trans1 <- Trans1 / rowSums(Trans1)
  Trans2 <- Trans2 / rowSums(Trans2) 
  
  # by default stay
  Trans  <- cbind(Trans1[,"m11"],Trans2[,"m22"])
  # by default death
  M <- as.matrix(chunk[, mort])
  if (compliment[1]){
    M  <- 1 - M
  }
  if (compliment[2]){
    Trans <- 1 - Trans
  }
  # just remember the column orderings
  c(c(M),c(Trans))
  
}

v2m_cond <- function(datvec, ntrans = 2, compliment = c(surv = FALSE, leave = FALSE)){
  # step 1, get mort
  # indexing might be off if ntrans > 2
  dim(datvec) <- c(length(datvec)/(ntrans^2),ntrans^2)
  
  # ensure first ntrans columns refer to mortality and not survival
  if (compliment[1]){
    datvec[,1:ntrans] <- 1 - datvec[,1:ntrans]
  }
  
  # ensure next two columns refer to staying
  if (compliment[2]){
    # indexing might be off if ntrans > 2
    ind <- (ntrans+1):(ntrans^2)
    datvec[, ind] <- 1 - datvec[, ind]  
  }
  
  # now we know we are in order m13, m23, m11, m22
  colnames(datvec) <- c("m13","m23","m11","m22")
  # separate blocks for easier manipulation:
  mort <- datvec[,c("m13","m23")]
  stay <- datvec[,c("m11","m22")]
  # rescale stay:
  stay <- stay * (1 - mort)
  # calculate leave:
  leave <- 1 - (stay + mort)
  # HARD CODE NAMES AAAAAAH
  colnames(leave) <- c("m12","m21")
  
  # bind together
  mat <- cbind(mort, stay, leave)
  # reorder
  mat <- mat[, getcolsall(ntrans)]
  mat
}

# conditional decomp objective function
dec_fun_cond <-
  function(datvec,
           to,
           age = 50,
           prop,
           ntrans = 2,
           deduct = TRUE,
           compliment = c(surv = FALSE, leave = FALSE),
           .logit = TRUE) {
    
    # logit would indicate that data passed in were subject to a logit
    # transform that we need to undo
    if (.logit){
      datvec <- expit(datvec)
    }
    # get back full set of usuable probabilities
    datself <- v2m_cond(datvec, ntrans = ntrans, compliment = compliment)
    
    # then compute e50 using the correct transition rates
    dc      <-
      e50(
        datself,
        to = to,
        age = age,
        prop = prop,
        ntrans = ntrans,
        deduct = deduct
      )
    
    dc
  }

dec_fun_cond_wrapper <- function(
  TR,
  time1=2006,
  time2=2014,
  .sex="f",
  prop,
  ntrans=2,
  to=5,
  deduct = TRUE,
  N=20,
  age=50,
  compliment = c(surv=FALSE,leave=FALSE),
  .logit = TRUE){
  
  # assuming all edu
  D1    <- subset(TR, time == time1 & sex == .sex)
  D2    <- subset(TR, time == time2 & sex == .sex)
  
  vec1  <- datvec_cond(D1,ntrans=ntrans, compliment = compliment)
  vec2  <- datvec_cond(D2,ntrans=ntrans, compliment = compliment)
  
  if (.logit){
    vec1 <- logit(vec1)
    vec2 <- logit(vec2)
  }
  
  deci  <- horiuchi(dec_fun_cond, 
                    pars1 = vec1,
                    pars2 = vec2,
                    N = N,
                    ntrans = ntrans,
                    deduct = deduct,
                    prop = prop,
                    to = to,
                    age = age,
                    compliment = compliment,
                    .logit = .logit)
  
  # indexing might be off if ntrans > 2
  dim(deci) <- c(length(deci)/(ntrans^2),ntrans^2)
  
  # colnames are "surv/mort", "stay/leave"
  # default: mort, leave (FALSE, FALSE compliment)
  # in order healthy, disabled, within sets.
  c1             <- c(ifelse(compliment[1],"surv","mort"),ifelse(compliment[2],"leave","stay"))
  cnames         <- c(outer(c("healthy","disabled"),c1,paste))
  colnames(deci) <- cnames
  # throw out age for this.
  out  <- colSums(deci)
  out
}














