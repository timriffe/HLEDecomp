library(here)
source("Code/R/Functions.R")
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

