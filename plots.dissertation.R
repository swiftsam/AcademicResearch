library(sciplot)
library(ggplot2)

##############################
#Study 1, JNM
##############################
source("JNM Analysis.R")
bargraph.CI(Negotiate,
            Satisfaction, 
            ylim=c(1,7),            
            col=c("gray75","gray35"),
            xlab="Choice to Negotiate", 
            ylab="Mean Satisfaction",
            main="Study 1: Satisfaction with Outcome")
rm(list=ls())

##############################
#Study 2, CSC
##############################
source("CSC Analysis.R")
bargraph.CI(ArgCond,GlobSatZ,ylab="Satisfaction", xlab="Condition",main="Study 2: Satisfaction by Argument Condition")
rm(list=ls())

##############################
#Study 3
##############################

##############################
#Study 4, ABS2
##############################
source("ABS2 Analysis.R")
bargraph.CI(ArgCond,sv5,group=RoleCond, data=ABS2, legend=T, ylab="Subjective Valuation of Car (5 item, Z-score)",main="Study 4: Role x Condition on Subjective Value of Car")
bargraph.CI(ArgCond,RP,group=RoleCond,data=ABS2, ylim=c(2000,2500), legend=T, ylab="Reservation Price ($)", main="Study 4: Role x Condition on Reservation Price")

#Relative Topic Freq in Argument
ggplot(tfDiff.BS.Arg, aes(reorder(factor(topic),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=as.numeric(mean))) +
  scale_fill_continuous(limits=c(-1.75, 1.75),low="red",high="green",name="Rated Value") +
  coord_flip() +
  scale_y_continuous() +
  xlab("Topics") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  opts(title="Study 4: Relative Topic use in Argument by Role")

rm(list=ls())

##############################
#Study 5, LVE
##############################
source("LVE Analysis.R")

