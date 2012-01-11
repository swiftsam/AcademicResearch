#libraries
library(psych)

#import data
sun.i <- read.csv("http://swift.cbdr.cmu.edu/data/SUN-data-indiv-2011-01-30.csv")
attach(sun.i)

#clean data
sun.i$cond <- factor(cond)
sun.i$role <- factor(buyer, levels = c(0,1),labels=c("Seller","Buyer"))
sun.i$buyer <-NULL
sun.i$deal <- factor(deal, levels =c(0,1), labels=c("Impasse","Deal"))

#compute variables
sun.i$util.price.z      <- scale(utility_price)
sun.i$util.entertain.z  <- scale(utility_entertain)
sun.i$util.useful.z     <- scale(utility_useful)
sun.i$util.valuable.z   <- scale(utility_valuable)

sun.i$WC.log <- log(WC)
sun.i$negate.log <-log(negate+1)

utilmeasures <- data.frame(sun.i$util.price.z,sun.i$util.entertain.z,sun.i$util.useful.z,sun.i$util.valuable.z)
alpha(utilmeasures)
utilmeasures <- data.frame(sun.i$util.entertain.z,sun.i$util.useful.z,sun.i$util.valuable.z)
alpha(utilmeasures)
rm(utilmeasures)

sun.i$utility <- (sun.i$util.entertain.z + sun.i$util.useful.z + sun.i$util.valuable.z)/3

#test for impasse ~ condition
#FIXME needs to be done on dyad data
#chisq.test(table(cond,deal))

sun.i.arg <- subset(sun.i, cond != "control")
sun.i.arg.b <- subset(sun.i.arg, role = "buyer")



summary(sun.i)
