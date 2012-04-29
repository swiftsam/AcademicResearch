library(sciplot)
library(ggplot2)
library(grid)
library(scales)
source("ggplot_themes.R")
rhg_cols1<- c("#006699","#AA1111","#7CAE00","#00BFC4")

barVals <- function(df,factors,var){
  ddply(df, factors, function(df)
    return(c(mean=mean(df[,var],na.rm=T), se=sd(df[,var],na.rm=T)/sqrt(nrow(df)))))
}

limits  <- aes(ymax = mean+se, ymin=mean-se) 
dodge   <- position_dodge(width=0.9)

##############################
#Study 1, JNM
##############################
source("JNM Analysis.R")

jnm.sat_sum <- barVals(jnm, c("Negotiate"),"Satisfaction")
ggplot(data=jnm.sat_sum, aes(fill=Negotiate, y=mean, x=Negotiate)) + 
  geom_bar(position="dodge", stat="identity",color="grey70") + 
  geom_errorbar(limits, position=dodge, width=0.1, size=.75, col="white") +
  scale_y_continuous(limits=c(1,7),oob=rescale_none) +
  xlab("Choice to Negotiate") +
  ylab("Satisfaction with Offer") +
  scale_fill_manual(values = rhg_cols1, name="Negotiate") +
  opts(legend.position = "none") +
  theme_black_presentation()

rm(list=ls())
##############################
#Study 2, CSC
##############################
source("CSC Analysis.R")
bargraph.CI(ArgCond,GlobSatZ,ylab="Satisfaction", xlab="Condition",main="Study 2: Satisfaction by Argument Condition")
rm(list=ls())

##############################
#Study 4, ABS2
##############################
source("ABS2 Analysis.R")

ABS2$ArgCond <- factor(ABS2$ArgCond, levels=c("NoArg","Arg"), labels=c("No Argument","Argument"))

sv5_sum <- barVals(ABS2, c("RoleCond", "ArgCond"),"sv5")
ggplot(data=sv5_sum, aes(fill=RoleCond, y=mean, x=ArgCond)) + 
  geom_bar(position="dodge", stat="identity",color="grey70") + 
  geom_errorbar(limits, position=dodge, width=0.1, size=.75, col="white") +
  xlab("Argument Condition") +
  ylab("Subjective Value (5-item, Z-score)") +
  scale_fill_manual(values = rhg_cols1, name="Role") +
  theme_black_presentation()

rp_sum <- barVals(ABS2, c("RoleCond", "ArgCond"),"RP")
ggplot(data=rp_sum, aes(fill=RoleCond, y=mean, x=ArgCond)) + 
  geom_bar(position="dodge", stat="identity",color="grey70") +
  geom_errorbar(limits, position=dodge, width=0.1,size=.75, col="white") +
  scale_y_continuous(limits=c(2000,2500),oob=rescale_none) +
  xlab("Argument Condition") +
  ylab("Reservation Price ($)") +
  scale_fill_manual(values = rhg_cols1, name="Role") +
  theme_black_presentation()

resreact_sum <- barVals(ABS2, c("RoleCond", "ArgCond"),"ResReact")
ggplot(data=resreact_sum, aes(fill=RoleCond, y=mean, x=ArgCond)) + 
  geom_bar(position="dodge", stat="identity",color="grey70") + 
  geom_errorbar(limits, position=dodge, width=0.1, size=.75, col="white") +
  xlab("Argument Condition") +
  ylab("Reaction to Response (3-item, Z-score)") +
  scale_fill_manual(values = rhg_cols1, name="Role") +
  theme_black_presentation()

ggplot(wfDiff.BS.Arg, aes(reorder(factor(word),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=as.numeric(rating)),color="white") + 
  scale_fill_continuous(limits=c(-1.75, 1.75), low="red",high="green",name="Rated Value") +
  coord_flip() +
  scale_y_continuous() +
  xlab("Words") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  opts(title="Relative word use in Argument by Role")+
  theme_black_presentation()

ggplot(wfDiff.BS.Sal, aes(reorder(factor(word),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=as.numeric(rating)),color="white") +
  scale_fill_continuous(limits=c(-1.75, 1.75), low="red",high="green",name="Rated Value") +
  coord_flip() +
  scale_y_continuous() +
  xlab("Words") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  opts(title="Relative word use in 'Most Memorable Details' by Role")+
  theme_black_presentation()

