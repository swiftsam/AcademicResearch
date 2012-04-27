library(sciplot)
library(ggplot2)

##############################
#Study 4, ABS2
##############################
source("ABS2 Analysis.R")

abs2sv5 <- ggplot(ABS2, aes(sv5,fill=RoleCond)) + geom_density(alpha=.5)
abs2rp <- ggplot(ABS2, aes(RP,fill=c(ArgCond, RoleCond))) + geom_density(alpha=.5, position="dodge")

bargraph.CI(ArgCond,sv5,group=RoleCond, data=ABS2, legend=T, ylab="Subjective Valuation of Car (5 item, Z-score)",main="Study 4: Role x Condition on Subjective Value of Car")
bargraph.CI(ArgCond,RP,group=RoleCond,data=ABS2, ylim=c(2000,2500), legend=T, ylab="Reservation Price ($)", main="Study 4: Role x Condition on Reservation Price")


