
# -------------------------
# in this code, we generate results for 'residual' approaches, to 
# point out how unreasonable it is.
# -------------------------


library(data.table)
library(DemoDecomp)
library(RColorBrewer)
library(here)
setwd(here("HLEDecomp"))

source("Code/R/Functions.R")
source("DecompNote/R/FunctionsLabTalk.R")
# Author: tim
###############################################################################
TR    <- local(get(load("Data/Transitions/DCS/initdetail/TR_v06.Rdata")))
TR    <- data.table(TR)
# TR: had to fix colnames problem
PREV  <- TR[ , get_prev_dt(.SD), by = list(sex, edu, time)]

# TR2 just means that there are now TWO living states,
# healthy and disabled.
TR2   <- collapseTR(TR = TR, PREV = PREV)

setnames(TR2,c("m14","m24"),c("m13","m23"))

# just do this for a single strata
TR2 <- subset(TR2,edu == "secondary")
TR2 <- as.data.frame(TR2)
args(dec_fun_resid_wrapper)
prop <- unlist(TR2[1,c("s1_prop","s2_prop")])
prop <- prop / sum(prop)

D1 <- subset(TR2, sex == "f" & time == 2006)
vec1 <- datvec_resid(D1,2,"self")
D2 <- subset(TR2, sex == "f" & time == 2014)
vec2 <- datvec_resid(D2,2,"self")

e1 <- dec_fun_resid(
              vec1,
              to=1,
              age=50,
              prop=prop,
              ntrans=2,
              deduct=TRUE,
              resid="self")
e2 <- dec_fun_resid(
              vec2,
              to=1,
              age=50,
              prop=prop,
              ntrans=2,
              deduct=TRUE,
              resid="self")
round(e1,2)
round(e2,2)
round(e2-e1,2)
resid.self <- dec_fun_resid_wrapper(TR2, 
                      time1 = 2006, 
                      time2 = 2014,
                      .sex = "f",
                      ntrans = 2,
                      prop = prop,
                      to = 1,
                      deduct = TRUE, 
                      N = 10,
                      age = 50,
                      resid = "self")
resid.dead <- dec_fun_resid_wrapper(TR2, 
                      time1 = 2006, 
                      time2 = 2014,
                      .sex = "f",
                      ntrans = 2,
                      prop = prop,
                      to = 1,
                      deduct = TRUE, 
                      N = 10,
                      age = 50,
                      resid = "dead")
resid.other <- dec_fun_resid_wrapper(TR2, 
                      time1 = 2006, 
                      time2 = 2014,
                      .sex = "f",
                      ntrans = 2,
                      prop = prop,
                      to = 1,
                      deduct = TRUE, 
                      N = 10,
                      age = 50,
                      resid = "other")
sum(resid.self)
sum(resid.dead)
sum(resid.other)

Resid <- matrix(nrow=3,ncol=6,dimnames=list(c("self","dead","other"),getcolsall(ntrans=2)))

Resid["self", names(resid.self)]   <- resid.self
Resid["dead", names(resid.dead)]   <- resid.dead
Resid["other", names(resid.other)] <- resid.other
colnames(Resid) <-
c("Stay healthy","Get disabled","Die healthy","Recover","Stay disab.","Die disabled")
dput(round(Resid,2))
