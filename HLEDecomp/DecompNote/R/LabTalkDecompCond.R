

library(data.table)
library(DemoDecomp)
library(RColorBrewer)
library(here)

here()
source(here("HLEDecomp","Code/R/Functions.R"))
source(here("HLEDecomp","DecompNote/R/FunctionsLabTalk.R"))
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

prop <- unlist(TR2[1,c("s1_prop","s2_prop")])
prop <- prop / sum(prop)



D1 <- subset(TR2, sex == "f" & time == 2006)
vec1 <- datvec_cond(D1,2,compliment=c(F,F))
D2 <- subset(TR2, sex == "f" & time == 2014)
vec2 <- datvec_cond(D2,2,compliment=c(F,F))

(e1 <- dec_fun_cond(
  vec1,
  to=1,
  age=50,
  prop=prop,
  ntrans=2,
  deduct=TRUE,
  compliment=c(FALSE,FALSE),
  .logit = FALSE))
(e2 <- dec_fun_cond(
  vec2,
  to=1,
  age=50,
  prop=prop,
  ntrans=2,
  deduct=TRUE,
  compliment=c(FALSE,FALSE),
  .logit = FALSE))
round(e1,2)
round(e2,2)
round(e2-e1,2)


# mort stay
resid.mortstay <- dec_fun_cond_wrapper(TR2, 
                                    time1 = 2006, 
                                    time2 = 2014,
                                    .sex = "f",
                                    ntrans = 2,
                                    prop = prop,
                                    to = 1,
                                    deduct = TRUE, 
                                    N = 10,
                                    age = 50,
                                    compliment=c(surv=F,leave=F),
                                    .logit=TRUE)
# surv leave
resid.survleave <- dec_fun_cond_wrapper(TR2, 
                                        time1 = 2006, 
                                        time2 = 2014,
                                        .sex = "f",
                                        ntrans = 2,
                                        prop = prop,
                                        to = 1,
                                        deduct = TRUE, 
                                        N = 10,
                                        age = 50,
                                        compliment=c(surv=T,leave=T),
                                        .logit = TRUE)

# mort leave
resid.mortleave <- dec_fun_cond_wrapper(TR2, 
                                        time1 = 2006, 
                                        time2 = 2014,
                                        .sex = "f",
                                        ntrans = 2,
                                        prop = prop,
                                        to = 1,
                                        deduct = TRUE, 
                                        N = 10,
                                        age = 50,
                                        compliment=c(surv=F,leave=T),
                                        .logit = TRUE)

# surv stay
resid.survstay <- dec_fun_cond_wrapper(TR2, 
                                        time1 = 2006, 
                                        time2 = 2014,
                                        .sex = "f",
                                        ntrans = 2,
                                        prop = prop,
                                        to = 1,
                                        deduct = TRUE, 
                                        N = 10,
                                        age = 50,
                                        compliment=c(surv=T,leave=F),
                                        .logit = TRUE)

Contrib <- rbind(resid.mortleave)

round(resid.mortleave,2)



library(tidyverse)

D1 %>% 
  select(age, m13,m12,m11) %>% 
  pivot_longer(2:4,names_to = "transition",values_to="prob.") %>% 
  mutate(transition = factor(transition),
         transition = factor(transition,levels(transition)[c(3:1)])) %>% 
  ggplot(mapping = aes(x=age,y=prob.,fill=transition))+
  geom_col(space=0, width=2)+
  theme(legend.pos = "none") + 
  annotate("text",80,.25,label = "stay healthy", size = 8) + 
  annotate("text",95,.6,label = "become disabled", size = 8, angle = -50) +
  annotate("text",100,.9, label = "die", size = 8)









