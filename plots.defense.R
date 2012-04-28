library(sciplot)
library(ggplot2)
library(grid)
source("ggplot_themes.R")
rhg_cols1<- c("#006699","#AA1111","#7CAE00","#00BFC4")

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

ABS2$ArgCond <- factor(ABS2$ArgCond, levels=c("NoArg","Arg"), labels=c("No Argument","Argument"))

sv5_sum <- barVals(ABS2, c("RoleCond", "ArgCond"),"sv5")
ggplot(data=sv5_sum, aes(fill=RoleCond, y=mean, x=ArgCond)) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_errorbar(limits, position=dodge, width=0.1, size=.75, col="white") +
  xlab("Argument Condition") +
  ylab("Subjective Value (5-item, Z-score)") +
  scale_fill_manual(values = rhg_cols1, name="Role") +
  theme_black_presentation()

rp_sum <- barVals(ABS2, c("RoleCond", "ArgCond"),"RP")
ggplot(data=rp_sum, aes(fill=RoleCond, y=mean, x=ArgCond)) + 
  geom_bar(position="dodge") +
  geom_errorbar(limits, position=dodge, width=0.1,size=.75, col="white") +
  coord_cartesian(ylim=c(2000,2400)) +
  scale_y_continuous(breaks=seq(2000,2500,100)) +
  xlab("Argument Condition") +
  ylab("Reservation Price ($)") +
  scale_fill_manual(values = rhg_cols1, name="Role") +
  theme_black_presentation()

resreact_sum <- barVals(ABS2, c("RoleCond", "ArgCond"),"ResReact")
ggplot(data=resreact_sum, aes(fill=RoleCond, y=mean, x=ArgCond)) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_errorbar(limits, position=dodge, width=0.1, size=.75, col="white") +
  xlab("Argument Condition") +
  ylab("Reaction to Response (3-item, Z-score)") +
  scale_fill_manual(values = rhg_cols1, name="Role") +
  theme_black_presentation()

