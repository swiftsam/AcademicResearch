library(plyr)
library(ggplot2)
library(psych)

#load data
ASM <- read.csv("ASM.csv", stringsAsFactors=F, na.strings=c(""))

#remove uncessary columns
ASM <- ASM[,!names(ASM) %in% c("X.V1","V2","V3","V4","V5","V7","V10",
                               "Consent2","Consent3","Consent4",
                               "BScenTime_1","BScenTime_2","BScenTime_4","BScen_1",
                               "BArgTime_1","BArgTime_2","BArgTime_4",
                               "BNoArgTime_1","BNoArgTime_2","BNoArgTime_4",
                               "BNoArgExpT_1","BNoArgExpT_2","BNoArgExpT_4",
                               "BArgExpTim_1","BArgExpTim_2","BArgExpTim_4","BArgExp",
                               "X")]
ASM <- rename(ASM, c("V6"="IP","V8"="Starttime","V9"="Endtime",
                     "Q65"="IssWSafe","Q66"="IssWFuel",
                     "BScenTime_3"="ScenarioTime","BArgExpTim_3"="ArgExposureTime"))

ASM$ArgCond <- rep(NA, nrow(ASM))
ASM$ArgCond[which(ASM$BArgTime_3>0)] <- "Arg"
ASM$ArgCond[which(ASM$BNoArgTime_3>0)] <- "NoArg"
ASM$ArgCond[which(ASM$BNoArgExpT_3>0)] <- "ArgExp"
ASM$ArgCond <- factor(ASM$ArgCond)
ASM$src <-factor(ASM$src)

#Check allocation to conditions
table(ASM$ArgCond)

# remove participants who dropped out before being assigned to conditions
ASM <- ASM[!is.na(ASM$ArgCond),]

#Consolidate data from condition-specific columns into single columns
ASM$ArgTime <- rep(NA,nrow(ASM))
ASM$ArgTime[is.na(ASM$ArgTime)] <- ASM$BArgTime_3[is.na(ASM$ArgTime)]
ASM$ArgTime[is.na(ASM$ArgTime)] <- ASM$BNoArgTime_3[is.na(ASM$ArgTime)]
ASM$ArgTime[is.na(ASM$ArgTime)] <- ASM$BNoArgExpT_3[is.na(ASM$ArgTime)]
ASM <- ASM[,!names(ASM) %in% c("BArgTime_3","BNoArgTime_3","BNoArgExpT_3")]

#create composite measures
alpha(ASM[,c("SVAttr","SVFav","SVShowoff","BSVWant","BSVExcite")])
ASM$sv <- rowMeans(ASM[,c("SVAttr","SVFav","SVShowoff","BSVWant","BSVExcite")], na.rm=TRUE)

alpha(ASM[,c("BResSat","BResFair")])
ASM$ResReact <- rowMeans(ASM[,c("BResSat","BResFair")])

#Does subjective value of the car vary by condition? No
summary(aov(sv ~ ArgCond, data=ASM))
qplot(ArgCond,sv,data=ASM,geom="boxplot")

#Does reservation price vary by condition? No      
summary(aov(BRP ~ ArgCond, data=ASM))
qplot(ArgCond,BRP,data=ASM,geom="boxplot")

#Does reaction to the offer vary by condition? No
summary(aov(ResReact ~ ArgCond, data=ASM))
qplot(ArgCond,ResReact,data=ASM,geom="boxplot")

#Does the decision to accept the offer vay by condition?
ASM$BResAccept <- factor(ASM$BResAccept,labels=c("Accept","Reject"))
table(ASM$ArgCond, ASM$BResAccept)
summary(glm(as.integer(BResAccept)-1 ~ ArgCond, data=ASM))
chisq.test(ASM$ArgCond, ASM$BResAccept)
        
        