library(sciplot)
library(ggplot2)

barVals <- function(df,factors,var){
  ddply(df, factors, function(df)
    return(c(mean=mean(df[,var],na.rm=T), se=sd(df[,var],na.rm=T)/sqrt(nrow(df)))))
}

limits  <- aes(ymax = mean+se, ymin=mean-se) 
dodge   <- position_dodge(width=0.9)

##############################
#Study 4, ABS2
##############################
source("ABS2 Analysis.R")

sv5_sum <- barVals(ABS2, c("RoleCond", "ArgCond"),"sv5")
ggplot(data=sv5_sum, aes(fill=RoleCond, y=mean, x=ArgCond)) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_errorbar(limits, position=dodge, width=0.1) 


rp_sum <- barVals(ABS2, c("RoleCond", "ArgCond"),"RP")
ggplot(data=rp_sum, aes(fill=RoleCond, y=mean, x=ArgCond)) + 
  scale_y_continuous(limits=c(2000,2500)) +
  geom_bar(position="dodge", stat="identity") + 
  geom_errorbar(limits, position=dodge, width=0.1) +
  theme_bw()