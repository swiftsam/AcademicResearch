#libraries
library(psych)
library(irr)

#import data
sun.i <- read.csv("http://samswift.org/data/SUN-data-indiv-2011-01-30.csv")
sun.coded.free <-read.csv("http://samswift.org/data/SUN-coded-freecond-arg-type-2012-04-28.csv")
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


##coded argument types in the free argument condition
sun.coded.free[is.na(sun.coded.free)] <- 0
kappa2(sun.coded.free[c("Coder1","Coder2")])

sun.coded.free$CoderAgreement <- rep(0,nrow(sun.coded.free))
sun.coded.free[sun.coded.free$Coder1 == 1 & sun.coded.free$Coder2 == 1, "CoderAgreement"] <- 1





