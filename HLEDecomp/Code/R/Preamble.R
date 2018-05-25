
# Author: tim
###############################################################################
# stuff that was running at the top of every script anyway...

# sex recode vectors
sexes        <- c("m", "f", "b")
Sexes        <- c("1.men", "2.wmn", "0.all")
names(sexes) <- Sexes
names(Sexes) <- sexes

# education recode vectors
edus         <- c("all_edu", "primary" , "secondary", "terciary"  )
Edus         <- c("0.All edu", "1.Less HS" , "4.HS/GED/Sm coll ex AA", "5.AA/BS/+"  )
names(Edus)  <- edus
names(edus)  <- Edus

# number of steps for decompositions
N            <- 20


