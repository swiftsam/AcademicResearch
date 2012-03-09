#ABS2 Data Prep


#set empty strings to NA
ABS2[ABS2==""] <- NA

#remove uncessary columns
ABS2 <- ABS2[,!names(ABS2) %in% c("X.V1","V2","V3","V4","V5","V7","V10",
                                  "Consent1","Consent2","Consent3","Consent4",
                                  "Sbletter","Sbletter.1",
                                  "BScen_1","BScenTime_1","BScenTime_2","BScenTime_4",
                                  "SScen_1","SScenTime_1","SScenTime_2","SScenTime_4",
                                  "BArgTime_1","BArgTime_2","BArgTime_4",
                                  "SArgTime_1","SArgTime_2","SArgTime_4",
                                  "BNoArgTime_1","BNoArgTime_2","BNoArgTime_4",
                                  "SNoArgTime_1","SNoArgTime_2","SNoArgTime_4",
                                  "X")]
#rename unnamed columns
ABS2 <- rename(ABS2, c("V6"="IP","V8"="Starttime","V9"="Endtime","Q58"="SSVWant","Q59"="SSVExcite"))

#add simple IDs for each record
ABS2$ID <- factor(c(1:nrow(ABS2)))

#Set conditions and factors
ABS2$RoleCond <- factor(ABS2$RoleCond)
ABS2$ArgCond <- rep(NA, nrow(ABS2))
ABS2$ArgCond[which(ABS2$SArgTime_3 | ABS2$BArgTime_3)] <- "Arg"
ABS2$ArgCond[which(ABS2$SNoArgTime_3 | ABS2$BNoArgTime_3)] <- "NoArg"
ABS2$ArgCond <- factor(ABS2$ArgCond)
ABS2$src <-factor(ABS2$src)

#Check allocation to conditions
table(ABS2$ArgCond, ABS2$RoleCond)

# remove participants who dropped out before being assigned to conditions
ABS2 <- ABS2[!is.na(ABS2$RoleCond) & !is.na(ABS2$ArgCond),]

#Consolidate data from condition-specific columns into single columns
ABS2$ScenTime <- rep(NA,nrow(ABS2))
ABS2$ScenTime[is.na(ABS2$ScenTime)] <- ABS2$BScenTime_3[is.na(ABS2$ScenTime)]
ABS2$ScenTime[is.na(ABS2$ScenTime)] <- ABS2$SScenTime_3[is.na(ABS2$ScenTime)]

ABS2$Arg <- rep(NA,nrow(ABS2))
ABS2$Arg[is.na(ABS2$Arg)] <- ABS2$BArg[is.na(ABS2$Arg)]
ABS2$Arg[is.na(ABS2$Arg)] <- ABS2$SArg[is.na(ABS2$Arg)]

ABS2$ArgTime <- rep(NA,nrow(ABS2))
ABS2$ArgTime[is.na(ABS2$ArgTime)] <- ABS2$BArgTime_3[is.na(ABS2$ArgTime)]
ABS2$ArgTime[is.na(ABS2$ArgTime)] <- ABS2$SArgTime_3[is.na(ABS2$ArgTime)]
ABS2$ArgTime[is.na(ABS2$ArgTime)] <- ABS2$BNoArgTime_3[is.na(ABS2$ArgTime)]
ABS2$ArgTime[is.na(ABS2$ArgTime)] <- ABS2$SNoArgTime_3[is.na(ABS2$ArgTime)]

ABS2$RP <- rep(NA,nrow(ABS2))
ABS2$RP[is.na(ABS2$RP)] <- ABS2$BRP[is.na(ABS2$RP)]
ABS2$RP[is.na(ABS2$RP)] <- ABS2$SRP[is.na(ABS2$RP)]

ABS2$SVWant <- rep(NA,nrow(ABS2))
ABS2$SVWant[is.na(ABS2$SVWant)] <- ABS2$SSVWant[is.na(ABS2$SVWant)]
ABS2$SVWant[is.na(ABS2$SVWant)] <- ABS2$BSVWant[is.na(ABS2$SVWant)]

ABS2$SVExcite <- rep(NA,nrow(ABS2))
ABS2$SVExcite[is.na(ABS2$SVExcite)] <- ABS2$SSVExcite[is.na(ABS2$SVExcite)]
ABS2$SVExcite[is.na(ABS2$SVExcite)] <- ABS2$BSVExcite[is.na(ABS2$SVExcite)]

ABS2$ResSat <- rep(NA,nrow(ABS2))
ABS2$ResSat[is.na(ABS2$ResSat)] <- ABS2$BResSat[is.na(ABS2$ResSat)]
ABS2$ResSat[is.na(ABS2$ResSat)] <- ABS2$SResSat[is.na(ABS2$ResSat)]

ABS2$ResFair <- rep(NA,nrow(ABS2))
ABS2$ResFair[is.na(ABS2$ResFair)] <- ABS2$BResFair[is.na(ABS2$ResFair)]
ABS2$ResFair[is.na(ABS2$ResFair)] <- ABS2$SResFair[is.na(ABS2$ResFair)]

ABS2$ResAccept <- rep(NA,nrow(ABS2))
ABS2$ResAccept[is.na(ABS2$ResAccept)] <- ABS2$BResAccept[is.na(ABS2$ResAccept)]
ABS2$ResAccept[is.na(ABS2$ResAccept)] <- ABS2$SResAccept[is.na(ABS2$ResAccept)]
ABS2$ResAccept <- factor(ABS2$ResAccept,labels=c("Accept","Reject"))

ABS2 <- ABS2[,!names(ABS2) %in% c("BScenTime_3","SScenTime_3",
                                  "BArg","SArg","BNoArg","SNoArg",
                                  "BArgTime_3","SArgTime_3","BNoArgTime_3","SNoArgTime_3",
                                  "BRP","SRP",
                                  "BSVWant","SSVWant","BSVExcite","SSVExcite",
                                  "BResSat","SResSat","BResFair","SResFair","BResAccept","SResAccept")]

