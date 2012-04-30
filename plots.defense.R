library(sciplot)
library(ggplot2)
library(plyr)
library(grid)
library(scales)
library(RColorBrewer)
source("ggplot_themes.R")
palette<- brewer.pal(6,name="Set1")

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
  scale_fill_manual(values = palette, name="Negotiate") +
  opts(legend.position = "none") +
  theme_black_presentation()

##############################
#Study 2, CSC
##############################
source("CSC Analysis.R")

csc$ArgCond <- factor(csc$ArgCond, 
                      levels=c("None","Free","OwnLow","OtherHigh","OwnBatna","OtherBatna"), 
                      labels=c("No Argument","Uncoached\nArgument","Own\nLow Value","Other\nHigh Value","Own\nBATNA Strong","Other\nBATNA Weak"))
csc.sat_sum <- barVals(csc,c("ArgCond"),"GlobSatZ")

ggplot(data=csc.sat_sum, aes(y=mean, x=ArgCond,fill=ArgCond)) +
  geom_bar(position="dodge", stat="identity",color="grey70") + 
  geom_errorbar(limits, position=dodge, width=0.1, size=.75, col="white") +
  xlab("Argument Condition") +
  ylab("Satisfaction (5 item, Z-score)") +
  scale_fill_manual(values = brewer.pal(6,name="Paired"), name="Condition") +
  opts(legend.position = "none") +
  theme_black_presentation()


##############################
#Study 3, SUN
##############################

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
  scale_fill_manual(values = palette, name="Role") +
  theme_black_presentation()

rp_sum <- barVals(ABS2, c("RoleCond", "ArgCond"),"RP")
ggplot(data=rp_sum, aes(fill=RoleCond, y=mean, x=ArgCond)) + 
  geom_bar(position="dodge", stat="identity",color="grey70") +
  geom_errorbar(limits, position=dodge, width=0.1,size=.75, col="white") +
  scale_y_continuous(limits=c(2000,2500),oob=rescale_none) +
  xlab("Argument Condition") +
  ylab("Reservation Price ($)") +
  scale_fill_manual(values = palette, name="Role") +
  theme_black_presentation()

resreact_sum <- barVals(ABS2, c("RoleCond", "ArgCond"),"ResReact")
ggplot(data=resreact_sum, aes(fill=RoleCond, y=mean, x=ArgCond)) + 
  geom_bar(position="dodge", stat="identity",color="grey70") + 
  geom_errorbar(limits, position=dodge, width=0.1, size=.75, col="white") +
  xlab("Argument Condition") +
  ylab("Reaction to Response (3-item, Z-score)") +
  scale_fill_manual(values = palette, name="Role") +
  theme_black_presentation()

ggplot(wfDiff.BS.Arg, aes(reorder(factor(word),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=as.numeric(rating),10)) + 
  scale_fill_gradient2(low="#A50026",mid="#FFFFBF",high="#006837", name="Rated Value", limits=c(-1.5,1.5)) +
  coord_flip() +
  scale_y_continuous() +
  xlab("Words") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  opts(title="Relative word use in Argument by Role")+
  theme_black_presentation()

ggplot(wfDiff.BS.Sal, aes(reorder(factor(word),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=as.numeric(rating))) +
  scale_fill_gradient2(low="#A50026",mid="#FFFFBF",high="#006837", name="Rated Value", limits=c(-1.5,1.5)) +
  coord_flip() +
  scale_y_continuous() +
  xlab("Words") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  opts(title="Relative word use in 'Most Memorable Details' by Role")+
  theme_black_presentation()

