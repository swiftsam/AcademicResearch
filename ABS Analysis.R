library(plyr)
library(ggplot2)
library(psych)

####### ---------------------------------
#######  Data Prep
####### ---------------------------------

#load data
ABS <- read.csv("ABS.csv", stringsAsFactors=F, na.strings=c(""))
ABS$ID <- row
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

#add simple IDs for each record
ABS$ID <- factor(c(1:nrow(ABS)))

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

####### ---------------------------------
#######  Sample Characteristics
####### ---------------------------------
ABS$DemGeo <- factor(ABS$DemGeo)
str(ABS$DemGeo)
ABS$DemGen <- factor(ABS$DemGen, labels=c("Male","Female"))
table(ABS$DemGen)
ABS$DemYOB <- ABS$DemYOB+1919  #values start at 1920=1
ABS$DemAge <- 2012 - ABS$DemYOB
describe(ABS$DemAge)
table(ABS$src)
ABS$DemLang <- factor(tolower(ABS$DemLang))
length(ABS$DemLang)
table(ABS$DemLang)

####### ---------------------------------
#######  Calculated Variables
####### ---------------------------------
ABS$ArgChars <- nchar(ABS$Arg)  #character count of arguments written

ABS <- ABS[order(ABS$Starttime),]
ABS$IPdup <- duplicated(ABS$IP)  
ABS$Emaildup <- duplicated(ABS$Email,incomparables= c(NA))

ABS$Starttime <- as.POSIXct(ABS$Starttime)
ABS$Endtime   <- as.POSIXct(ABS$Endtime)
ABS$TotalTime <- as.double((ABS$Starttime - ABS$Endtime)/-60) #number of positive minutes between start- and end- time

####### ---------------------------------
#######  Exclusion Criteria
####### ---------------------------------
#total study completion time
describe.by(ABS$TotalTime,group=ABS$ArgCond)
#qplot(ABS$TotalTime, geom="histogram", group=ABS$ArgCond, fill=ABS$ArgCond, xlim=c(0,30), binwidth=1, position="dodge")
length(ABS$ID[ABS$TotalTime<1]) 
ABS <- subset(ABS,ABS$TotalTime>1)

#Time spent reading scenario
describe(ABS$ScenTime)
#qplot(ABS$ScenTime, geom="histogram", xlim=c(0,150))
length(ABS$ID[ABS$ScenTime<10])
ABS <- subset(ABS,ABS$ScenTime > 10)

#Time spent writing argument
describe.by(ABS$ArgTime, group=ABS$ArgCond)
#qplot(ABS$ArgTime[ABS$ArgCond=="Arg"], geom="histogram", xlim=c(0,1000))
length(ABS$ID[ABS$ArgCond=="Arg" & ABS$ArgTime<15])

#Length of argument written
describe(ABS$ArgChars[ABS$ArgCond=="Arg"])
#qplot(ABS$ArgChars[ABS$ArgCond=="Arg"], geom="histogram", binwidth=10)
length(ABS$ID[ABS$ArgCond=="Arg" & ABS$ArgChars<10])
ABS <- subset(ABS,xor(ABS$ArgCond=="NoArg", ABS$ArgChars>10))

#Check allocation to conditions
table(ABS$ArgCond, ABS$RoleCond)

####### ---------------------------------
#######  Create composite measures
####### ---------------------------------

#Subjective Utility of Car, 5 items
alpha(ABS[,c("SVAttr","SVFav","SVShowoff","SVWant","SVExcite")])
ABS$sv <- rowMeans(ABS[,c("SVAttr","SVFav","SVShowoff","SVWant","SVExcite")], na.rm=TRUE)

#Reaction to counterpart's response, 2 items
alpha(ABS[,c("ResSat","ResFair")])
ABS$ResReact <- rowMeans(ABS[,c("ResSat","ResFair")])

####### ---------------------------------
#######  Hypothesis Tests
####### ---------------------------------

#Qualtrics survey flow error resulted in most DV measures being shown only to Sellers
#See if you can spot the error: http://i.imgur.com/nqtkc.png
#Salvage data by analyzing only sellers
sellers <- ABS[ABS$RoleCond=="Seller",]

#Does the decision to accept the offer vay by condition?
table(sellers$ArgCond, sellers$ResAccept)
summary(glm(as.integer(ResAccept)-1 ~ ArgCond, data=sellers))
chisq.test(sellers$ArgCond, sellers$ResAccept)

#Does subjective value of the car vary by condition?
summary(aov(sv ~ ArgCond, data=sellers))
qplot(ArgCond,sv,data=sellers,geom="boxplot")

#Does reaction to the offer vary by condition?
summary(aov(ResReact ~ ArgCond, data=sellers))
qplot(ArgCond,ResReact,data=sellers,geom="boxplot")

#Does RP vary by condition?
summary(aov(RP ~ ArgCond, data=sellers))
qplot(ArgCond,RP,data=sellers,geom="boxplot")


