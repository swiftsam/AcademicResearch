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

jnm.percentile_sum <- barVals(jnm, c("Negotiate"),"Percentile")
ggplot(data=jnm.percentile_sum, aes(fill=Negotiate, y=mean, x=Negotiate)) + 
  geom_bar(position="dodge", stat="identity",color="grey70") + 
  geom_errorbar(limits, position=dodge, width=0.1, size=.75, col="white") +
  scale_y_continuous(limits=c(1,100),oob=rescale_none) +
  xlab("\nChoice to Negotiate") +
  ylab("Estimated Percentile of Offer Value") +
  scale_fill_manual(values = palette, name="Negotiate") +
  opts(legend.position = "none") +
  theme_black_presentation()
ggsave(filename="~/Desktop/jnm.percentile_sum.png", width=6, height=7)

jnm.sat_sum <- barVals(jnm, c("Negotiate"),"Satisfaction")
ggplot(data=jnm.sat_sum, aes(fill=Negotiate, y=mean, x=Negotiate)) + 
  geom_bar(position="dodge", stat="identity",color="grey70") + 
  geom_errorbar(limits, position=dodge, width=0.1, size=.75, col="white") +
  scale_y_continuous(limits=c(1,7),oob=rescale_none) +
  xlab("\nChoice to Negotiate") +
  ylab("Satisfaction with Offer") +
  scale_fill_manual(values = palette, name="Negotiate") +
  opts(legend.position = "none") +
  theme_black_presentation()
ggsave(filename="~/Desktop/jnm.sat_sum.png", width=6, height=7)

##############################
#Study 2, CSC
##############################
source("CSC Analysis.R")

csc$ArgCond <- factor(csc$ArgCond, 
                      levels=c("None","Free","OwnLow","OtherHigh","OwnBatna","OtherBatna"), 
                      labels=c("No Argument","Uncoached\nArgument","Own Value\nLow","Other Value\nHigh","Own BATNA\nStrong","Other BATNA\nWeak"))
csc.sat_sum <- barVals(csc,c("ArgCond"),"GlobSatZ")

ggplot(data=csc.sat_sum, aes(y=mean, x=ArgCond,fill=ArgCond)) +
  geom_bar(position="dodge", stat="identity",color="grey70") + 
  geom_errorbar(limits, position=dodge, width=0.1, size=.75, col="white") +
  xlab("\nArgument Condition") +
  ylab("Satisfaction (5 item, Z-score)") +
  scale_fill_manual(values = brewer.pal(6,name="Paired"), name="Condition") +
  opts(legend.position = "none") +
  theme_black_presentation()
ggsave(filename="~/Desktop/csc.sat_sum.png", width=11, height=7)


##############################
#Study 3, SUN
##############################
source("SUN Analysis.R")
sun.coded.free$ArgType <- factor(sun.coded.free$ArgType, 
                      levels=c("OwnValueLow","OtherValueHigh","OwnBatnaStrong","OtherBatnaWeak"), 
                      labels=c("Own Value\nLow","Other Value\nHigh","Own BATNA\nStrong","Other BATNA\nWeak"))

sun.freq_sum <- ddply(sun.coded.free, c("ArgType","Role"), function(df)
                  c(count=sum(df$CoderAgreement), percentage=sum(df$CoderAgreement)/nrow(df)))

ggplot(data=sun.freq_sum, aes(y=percentage*100, x=ArgType,fill=Role)) +
  geom_bar(position="dodge", stat="identity",color="grey70") + 
  xlab("\nArgument Type") +
  ylab("% Using Argument Type") +
  scale_fill_manual(values = palette, name="Role") +
  scale_y_continuous(limits=c(0,100),oob=rescale_none) +
  theme_black_presentation()
ggsave(filename="~/Desktop/sun.freq_sum.png", width=10, height=7)

##############################
#Study 4, ABS2
##############################
source("ABS2 Analysis.R")

ABS2$ArgCond <- factor(ABS2$ArgCond, levels=c("NoArg","Arg"), labels=c("No Argument","Argument"))

abs2.sv5_sum <- barVals(ABS2, c("RoleCond", "ArgCond"),"sv5noZ")
ggplot(data=abs2.sv5_sum, aes(fill=RoleCond, y=mean, x=ArgCond)) + 
  geom_bar(position="dodge", stat="identity",color="grey70") + 
  geom_errorbar(limits, position=dodge, width=0.1, size=.75, col="white") +
  xlab("\nArgument Condition") +
  ylab("Subjective Value (5-item, Z-score)") +
  scale_fill_manual(values = palette, name="Role") +
  theme_black_presentation()
ggsave(filename="~/Desktop/abs2.sv5_sum.png", width=10, height=7)

abs2.rp_sum <- barVals(ABS2, c("RoleCond", "ArgCond"),"RP")
ggplot(data=abs2.rp_sum, aes(fill=RoleCond, y=mean, x=ArgCond)) + 
  geom_bar(position="dodge", stat="identity",color="grey70") +
  geom_errorbar(limits, position=dodge, width=0.1,size=.75, col="white") +
  scale_y_continuous(limits=c(2000,2500),oob=rescale_none) +
  xlab("\nArgument Condition") +
  ylab("Reservation Price ($)") +
  scale_fill_manual(values = palette, name="Role") +
  theme_black_presentation()
ggsave(filename="~/Desktop/abs2.rp_sum.png", width=10, height=7)

abs2.resreact_sum <- barVals(ABS2, c("RoleCond", "ArgCond"),"ResReact")
ggplot(data=abs2.resreact_sum, aes(fill=RoleCond, y=mean, x=ArgCond)) + 
  geom_bar(position="dodge", stat="identity",color="grey70") + 
  geom_errorbar(limits, position=dodge, width=0.1, size=.75, col="white") +
  xlab("\nArgument Condition") +
  ylab("Reaction to Response (3-item, Z-score)") +
  scale_fill_manual(values = palette, name="Role") +
  theme_black_presentation()
ggsave(filename="~/Desktop/abs2.resreact_sum.png", width=10, height=7)

abs2.reject_sum <- ddply(ABS2, c("RoleCond","ArgCond"), function(df)
  c(count=sum(df$ResRejectInt,na.rm=T), percentage=sum(df$ResRejectInt,na.rm=T)/nrow(df)))

ggplot(data=abs2.reject_sum, aes(y=(1-percentage)*100, x=ArgCond,fill=RoleCond)) +
  geom_bar(position="dodge", stat="identity",color="grey70") + 
  xlab("\nArgument Condition") +
  ylab("% Accepting Counter-Offer") +
  scale_fill_manual(values = palette, name="Role") +
  scale_y_continuous(limits=c(0,100),oob=rescale_none) +
  theme_black_presentation()
ggsave(filename="~/Desktop/abs2.accept_sum.png", width=10, height=7)

#Relative Word Freq in Argument - no color
ggplot(wfDiff.BS.Arg, aes(reorder(factor(word),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=as.numeric(rating))) + 
  scale_fill_gradient2(low="grey50",mid="grey50",high="grey50", na.value="grey50", name="Rated Value", limits=c(-1.5,1.5)) +
  coord_flip() +
  scale_y_continuous() +
  xlab("Words Used") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  theme_black_presentation()

#Relative Word Freq in Argument
ggplot(wfDiff.BS.Arg, aes(reorder(factor(word),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=as.numeric(rating))) + 
  scale_fill_gradient2(low="#A50026",mid="#FFFFBF",high="#006837", name="Rated Value", limits=c(-1.5,1.5)) +
  coord_flip() +
  scale_y_continuous() +
  xlab("Words Used") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  theme_black_presentation()

#Relative Word Freq in Salience
ggplot(wfDiff.BS.Sal, aes(reorder(factor(word),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=as.numeric(rating))) +
  scale_fill_gradient2(low="#A50026",mid="#FFFFBF",high="#006837", name="Rated Value", limits=c(-1.5,1.5)) +
  coord_flip() +
  scale_y_continuous() +
  xlab("Words Used") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  theme_black_presentation()

#Relative Word Freq in Weight
ggplot(wfDiff.BS.IssW, aes(reorder(factor(word),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=as.numeric(rating))) +
  scale_fill_gradient2(low="#A50026",mid="#FFFFBF",high="#006837", name="Rated Value", limits=c(-1.5,1.5)) +
  coord_flip() +
  scale_y_continuous() +
  xlab("Words Used") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  theme_black_presentation()

#Relative Topic Freq in Argument
ggplot(tfDiff.BS.Arg, aes(reorder(factor(topic),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=as.numeric(mean))) +
  scale_fill_gradient2(low="#A50026",mid="#FFFFBF",high="#006837", name="Rated Value", limits=c(-1.5,1.5)) +
  coord_flip() +
  scale_y_continuous() +
  xlab("Topics Used") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  theme_black_presentation()

##############################
#Study 5, LVE
##############################
source("LVE Analysis.R")

lve$ArgCond <- factor(lve$ArgCond, levels=c("NoArg","Arg"), labels=c("No Argument","Argument"))

lve.sv6_sum <- barVals(lve, c("ArgCond"),"sv6noZ")
ggplot(data=lve.sv6_sum, aes(fill=ArgCond, y=mean, x=ArgCond)) + 
  geom_bar(position="dodge", stat="identity",color="grey70") + 
  geom_errorbar(limits, position=dodge, width=0.1, size=.75, col="white") +
  scale_y_continuous(limits=c(1,7),oob=rescale_none) +
  xlab("\nArgument Condition") +
  ylab("Subjective Value (6 items)") +
  scale_fill_manual(values = palette, name="Argument") +
  opts(legend.position = "none", title="Subjective Value of Book") +
  theme_black_presentation()
ggsave(filename="~/Desktop/lve.sv6_sum.png", width=5, height=7)

lve.req_sum <- ddply(lve, c("ArgCond"), function(df)
  c(count=sum(df$bookReq), percentage=sum(df$bookReq)/nrow(df)))

ggplot(data=lve.req_sum, aes(y=percentage*100, x=ArgCond,fill=ArgCond)) +
  geom_bar(position="dodge", stat="identity",color="grey70") + 
  xlab("\nArgument Condition") +
  ylab("% Requesting Book") +
  scale_fill_manual(values = palette, name="ArgCond") +
  scale_y_continuous(limits=c(0,100),oob=rescale_none) +
  opts(legend.position = "none", title="Likelihood of Requesting Book") +
  theme_black_presentation()
ggsave(filename="~/Desktop/lve.freq_sum.png", width=5, height=7)

##############################
#Study 6, ASM
##############################
source("ASM Analysis.R")


ASM$ArgCond <- factor(ASM$ArgCond, 
                      levels=c("NoArg","ArgExp","Arg"), 
                      labels=c("No Argument","Argument\nExposure","Argument\nSelection"))
asm.sv5_sum <- barVals(ASM,c("ArgCond"),"sv5noZ")

ggplot(data=asm.sv5_sum, aes(y=mean, x=ArgCond,fill=ArgCond)) +
  geom_bar(position="dodge", stat="identity",color="grey70") + 
  geom_errorbar(limits, position=dodge, width=0.1, size=.75, col="white") +
  xlab("\nArgument Condition") +
  ylab("Subjective Value (5 item, mean)") +
  scale_fill_manual(values = palette, name="ArgCond") +
  scale_y_continuous(limits=c(1,7),oob=rescale_none) +
  theme(legend.position = "none")
ggsave(filename="~/Desktop/asm.sat_sum.png", width=9, height=7)


