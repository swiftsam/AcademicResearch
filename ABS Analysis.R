library(plyr)
library(ggplot2)

#load data
ABS <- read.csv("~/research/ABS.csv", stringsAsFactors=F, na.strings=c(""))
#remove uncessary columns
ABS <- ABS[,!names(ABS) %in% c("X.V1","V2","V3","V4","V5","V7","V10",
                               "Consent1","Consent2","Consent3","Consent4",
                               "BScen_1","BScenTime_1","BScenTime_2","BScenTime_4",
                               "SScen_1","SScenTime_1","SScenTime_2","SScenTime_4",
                               "BArgTime_1","BArgTime_2","BArgTime_4",
                               "SArgTime_1","SArgTime_2","SArgTime_4",
                               "BNoArgTime_1","BNoArgTime_2","BNoArgTime_4",
                               "SNoArgTime_1","SNoArgTime_2","SNoArgTime_4",
                               "X")]
#rename unnamed columns
ABS <- rename(ABS, c("V6"="IP","V8"="Starttime","V9"="Endtime","Q58"="SSVWant","Q59"="SSVExcite"))

#Set conditions and factors
ABS$RoleCond[is.na(ABS$RoleCond)] <- ABS$RoieCond[is.na(ABS$RoleCond)] #correcting typo in qualtrics
ABS$RoieCond <- NULL
ABS$RoleCond <- factor(ABS$RoleCond)
ABS$ArgCond <- rep(NA, nrow(ABS))
ABS$ArgCond[which(ABS$SArgTime_3 | ABS$BArgTime_3)] <- "Arg"
ABS$ArgCond[which(ABS$SNoArgTime_3 | ABS$BNoArgTime_3)] <- "NoArg"
ABS$ArgCond <- factor(ABS$ArgCond)
ABS$src <-factor(ABS$src)

#Check allocation to conditions
table(ABS$ArgCond, ABS$RoleCond)

# remove participants who dropped out before being assigned to conditions
ABS <- ABS[!is.na(ABS$RoleCond) & !is.na(ABS$ArgCond),]

#Consolidate data from condition-specific columns into single columns
ABS$ScenTime <- rep(NA,nrow(ABS))
ABS$ScenTime[is.na(ABS$ScenTime)] <- ABS$BScenTime_3[is.na(ABS$ScenTime)]
ABS$ScenTime[is.na(ABS$ScenTime)] <- ABS$SScenTime_3[is.na(ABS$ScenTime)]

ABS$Arg <- rep(NA,nrow(ABS))
ABS$Arg[is.na(ABS$Arg)] <- ABS$BArg[is.na(ABS$Arg)]
ABS$Arg[is.na(ABS$Arg)] <- ABS$SArg[is.na(ABS$Arg)]

ABS$ArgTime <- rep(NA,nrow(ABS))
ABS$ArgTime[is.na(ABS$ArgTime)] <- ABS$BArgTime_3[is.na(ABS$ArgTime)]
ABS$ArgTime[is.na(ABS$ArgTime)] <- ABS$SArgTime_3[is.na(ABS$ArgTime)]
ABS$ArgTime[is.na(ABS$ArgTime)] <- ABS$BNoArgTime_3[is.na(ABS$ArgTime)]
ABS$ArgTime[is.na(ABS$ArgTime)] <- ABS$SNoArgTime_3[is.na(ABS$ArgTime)]

ABS$RP <- rep(NA,nrow(ABS))
ABS$RP[is.na(ABS$RP)] <- ABS$BRP[is.na(ABS$RP)]
ABS$RP[is.na(ABS$RP)] <- ABS$SRP[is.na(ABS$RP)]

ABS$SVWant <- rep(NA,nrow(ABS))
ABS$SVWant[is.na(ABS$SVWant)] <- ABS$SSVWant[is.na(ABS$SVWant)]
ABS$SVWant[is.na(ABS$SVWant)] <- ABS$BSVWant[is.na(ABS$SVWant)]

ABS$SVExcite <- rep(NA,nrow(ABS))
ABS$SVExcite[is.na(ABS$SVExcite)] <- ABS$SSVExcite[is.na(ABS$SVExcite)]
ABS$SVExcite[is.na(ABS$SVExcite)] <- ABS$BSVExcite[is.na(ABS$SVExcite)]
                                                   
ABS$ResSat <- rep(NA,nrow(ABS))
ABS$ResSat[is.na(ABS$ResSat)] <- ABS$BResSat[is.na(ABS$ResSat)]
ABS$ResSat[is.na(ABS$ResSat)] <- ABS$SResSat[is.na(ABS$ResSat)]

ABS$ResFair <- rep(NA,nrow(ABS))
ABS$ResFair[is.na(ABS$ResFair)] <- ABS$BResFair[is.na(ABS$ResFair)]
ABS$ResFair[is.na(ABS$ResFair)] <- ABS$SResFair[is.na(ABS$ResFair)]
                                                   
ABS$ResAccept <- rep(NA,nrow(ABS))
ABS$ResAccept[is.na(ABS$ResAccept)] <- ABS$BResAccept[is.na(ABS$ResAccept)]
ABS$ResAccept[is.na(ABS$ResAccept)] <- ABS$SResAccept[is.na(ABS$ResAccept)]
ABS$ResAccept <- factor(ABS$ResAccept,labels=c("Accept","Reject"))
                                         
ABS <- ABS[,!names(ABS) %in% c("BScenTime_3","SScenTime_3",
                               "BArg","SArg","BNoArg","SNoArg",
                               "BArgTime_3","SArgTime_3","BNoArgTime_3","SNoArgTime_3",
                               "BRP","SRP",
                               "BSVWant","SSVWant","BSVExcite","SSVExcite",
                               "BResSat","SResSat","BResFair","SResFair","BResAccept","SResAccept")]

sellers <- ABS[ABS$RoleCond=="Seller",]
table(sellers$ArgCond, sellers$ResAccept)
summary(glm(as.integer(ResAccept)-1 ~ ArgCond, data=sellers))
chisq.test(sellers$ArgCond, sellers$ResAccept)

