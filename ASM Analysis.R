library(reshape)
library(plyr)
library(ggplot2)
library(sciplot)
library(psych)
library(nlme)

####### ---------------------------------
#######  Data Prep
####### ---------------------------------

#load data
ASM <- read.csv("http://swift.cbdr.cmu.edu/data/ASM-data-2012-02-07.csv", stringsAsFactors=F)
argUseCoded <- read.csv("http://swift.cbdr.cmu.edu/data/ASM-coded-ArgUse-2012-03-28.csv", stringsAsFactors=F)
attrSalCoded <- read.csv("http://swift.cbdr.cmu.edu/data/ASM-coded-AttrSal-2012-04-02.csv")

#set empty strings to NA
ASM[ASM==""] <- NA

#remove uncessary columns
ASM <- ASM[,!names(ASM) %in% c("X.V1","V2","V3","V4","V5","V7","V10",
                               "Consent2","Consent3","Consent4",
                               "BScenTime_1","BScenTime_2","BScenTime_4","BScen_1",
                               "BArgTime_1","BArgTime_2","BArgTime_4",
                               "BNoArgTime_1","BNoArgTime_2","BNoArgTime_4",
                               "BNoArgExpT_1","BNoArgExpT_2","BNoArgExpT_4",
                               "BArgExpTim_1","BArgExpTim_2","BArgExpTim_4","BArgExp",
                               "X")]

#rename unnamed and poorly columns
ASM <- rename(ASM, c("V6"="IP","V8"="Starttime","V9"="Endtime",
                     "Q65"="IssWSafe","Q66"="IssWFuel",
                     "BScenTime_3"="ScenTime","BArgExpTim_3"="ArgExposureTime",
                     "BArg"="Arg","BSVWant"="SVWant","BSVExcite"="SVExcite","BRP"="RP",
                     "BResSat"="ResSat","BResFair"="ResFair","BResAccept"="ResAccept"))

#add simple IDs for each record
ASM$ID <- factor(c(1:nrow(ASM)))

#merge raw and coded data on IDs
ASM <- merge(ASM, argUseCoded, by=c("ID"),all.x=T)
ASM <- merge(ASM, attrSalCoded, by=c("ID"),all.x=T)


#Set conditions and factors
ASM$ArgCond <- rep(NA, nrow(ASM))
ASM$ArgCond[which(ASM$BArgTime_3>0)] <- "Arg"
ASM$ArgCond[which(ASM$BNoArgTime_3>0)] <- "NoArg"
ASM$ArgCond[which(ASM$BNoArgExpT_3>0)] <- "ArgExp"
ASM$ArgCond <- factor(ASM$ArgCond)
ASM$src <-factor(ASM$src)
ASM$ResAccept <- factor(ASM$ResAccept,labels=c("Accept","Reject"))

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

####### ---------------------------------
#######  Calculated Variables
####### ---------------------------------
ASM$ArgChars <- nchar(ASM$Arg)  #character count of arguments written

ASM <- ASM[order(ASM$Starttime),]
ASM$IPdup <- duplicated(ASM$IP)  
ASM$Emaildup <- duplicated(ASM$Email,incomparables= c(NA))

ASM$Starttime <- as.POSIXct(ASM$Starttime)
ASM$Endtime   <- as.POSIXct(ASM$Endtime)
ASM$TotalTime <- -as.double(ASM$Starttime - ASM$Endtime) #number of seconds between start- and end- time

ASM$OtherTime <- ASM$TotalTime - ASM$ArgTime #number of seconds doing everything other than the arg

#The total number of arguments used 
ASM$ArgUseSum <- rowSums(ASM[c("ArgUseMi","ArgUseYr","ArgUseCon","ArgUseSafe","ArgUseParts","ArgUseFuel","ArgUseOther")])

####### ---------------------------------
#######  Exclusion Criteria
####### ---------------------------------
#duplicate emails
length(ASM$ID[ASM$Emaildup])
ASM <- subset(ASM,ASM$Emaildup == FALSE)

#duplicate IP address
length(ASM$ID[ASM$IPdup])
#ASM <- subset(ASM,ASM$Emaildup == FALSE)

#total study completion time
describe.by(ASM$TotalTime,group=ASM$ArgCond)
#qplot(ASM$TotalTime, geom="histogram", group=ASM$ArgCond, fill=ASM$ArgCond, xlim=c(0,1500), binwidth=30, position="dodge")
length(ASM$ID[ASM$TotalTime<60]) 
ASM <- subset(ASM,ASM$TotalTime>60)

#Time spent reading scenario
describe(ASM$ScenTime)
#qplot(ASM$ScenTime, geom="histogram", xlim=c(0,150))
length(ASM$ID[ASM$ScenTime<10])
ASM <- subset(ASM,ASM$ScenTime > 10)

#Time spent writing argument
describe.by(ASM$ArgTime, group=ASM$ArgCond)
#qplot(ASM$ArgTime[ASM$ArgCond=="Arg"], geom="histogram", xlim=c(0,350))
length(ASM$ID[ASM$ArgCond=="Arg" & ASM$ArgTime<15])

#Length of argument written
describe(ASM$ArgChars[ASM$ArgCond=="Arg"])
#qplot(ASM$ArgChars[ASM$ArgCond=="Arg"], geom="histogram", binwidth=10)
length(ASM$ID[ASM$ArgCond=="Arg" & ASM$ArgChars<10])
ASM <- subset(ASM,xor(xor(ASM$ArgCond=="NoArg", ASM$ArgCond=="ArgExp"), ASM$ArgChars>10))

#Check allocation to conditions
table(ASM$ArgCond)

#write potential lottery winners to file
#write.csv(ASM[!is.na(ASM$Email),c("Email","src")],"ASM-lottery.csv")

####### ---------------------------------
#######  Sample Characteristics
####### ---------------------------------
ASM$DemGeo <- factor(ASM$DemGeo)
str(ASM$DemGeo)
ASM$DemGen <- factor(ASM$DemGen, labels=c("Male","Female"))
table(ASM$DemGen)
ASM$DemYOB <- ASM$DemYOB+1919  #values start at 1920=1
ASM$DemAge <- 2012 - ASM$DemYOB
describe(ASM$DemAge)
table(ASM$src)
ASM$DemLang <- factor(tolower(ASM$DemLang))
length(ASM$DemLang)
table(ASM$DemLang)

ASM$DemOwnCar <- factor(ASM$DemOwnCar, labels=c("OwnsCar","DoesNotOwnCar"))
ASM$DemCarPurc <- factor(ASM$DemCarPurc, labels=c("HavePurchased","HaveNotPurchased"))
carbuyers <- subset(ASM, ASM$DemCarPurc=="HavePurchased")
colSums(carbuyers[c("DemCarPTyp_1","DemCarPTyp_2","DemCarPTyp_3","DemCarPTyp_4")],na.rm=T)

ASM$DemAffect <- factor(ASM$DemAffect, label=c("Like More","No Change","Like Less"))

####### ---------------------------------
#######  Create composite measures
####### ---------------------------------
alpha(ASM[,c("SVAttr","SVFav","SVShowoff","SVWant","SVExcite")])
ASM$sv5 <- rowMeans(
  data.frame(
    scale(ASM$SVAttr),
    scale(ASM$SVFav),
    scale(ASM$SVShowoff),
    scale(ASM$SVWant),
    scale(ASM$SVExcite)
    ),
  na.rm=TRUE)

alpha(ASM[,c("ResSat","ResFair")])
ASM$ResReact <- rowMeans(
  data.frame(
    scale(ASM$ResSat),
    scale(ASM$ResFair)
    ),
  na.rm=TRUE)

alpha(ASM[c("IssWYr","IssWMi","IssWCon","IssWMod","IssWMat","IssWExt","IssWSafe","IssWFuel")])


####### ---------------------------------
#######  Effects of Condition on Outcomes
####### ---------------------------------

#Does subjective value of the car vary by condition?
summary(aov(sv5 ~ ArgCond, data=ASM))
qplot(ArgCond,sv5,data=ASM,geom="boxplot")
bargraph.CI(ArgCond,sv5, data=ASM, legend=T)

sv5.lm <- lm(sv5 ~ ArgCond + log(OtherTime) + log(ArgTime), data=ASM)
summary(sv5.lm)

#Does reservation price vary by condition?
summary(aov(RP ~ ArgCond, data=ASM))
qplot(ArgCond,RP,data=ASM,geom=c("boxplot","jitter"))
rp.lm <-lm(RP ~ ArgCond + log(OtherTime) + log(ArgTime), data=ASM)
summary(rp.lm)

#Does reaction to the offer vary by condition?
summary(aov(ResReact ~ ArgCond, data=ASM))
qplot(ArgCond,ResReact,data=ASM,geom="boxplot")
react.lm <- lm(ResReact ~ ArgCond + log(OtherTime) + log(ArgTime), data=ASM)
summary(react.lm)

#Does the decision to accept the offer vay by condition?
table(ASM$ArgCond, ASM$ResAccept)
summary(glm(as.integer(ResAccept)-1 ~ ArgCond, data=ASM))
chisq.test(ASM$ArgCond, ASM$ResAccept)

####### ---------------------------------
#######  Process Hypotheses
####### ---------------------------------

#Subset the argument condition
ASM.arg <- subset(ASM, ArgCond=="Arg")

####### Arg --> Salience
####### -------------------------------------
#Does recall/salience vary by argument condition?
summary(glm(AttrSalYrCoded ~ ArgCond + log(OtherTime) + log(ArgTime), data=ASM)) #Model Year
summary(glm(AttrSalMiCoded ~ ArgCond + log(OtherTime) + log(ArgTime), data=ASM)) #Mileage
summary(lm(AttrSalModCoded ~ ArgCond + log(OtherTime) + log(ArgTime), data=ASM)) #Make & Model
summary(lm(AttrSalExtCoded ~ ArgCond + log(OtherTime) + log(ArgTime), data=ASM)) #Extras

#Does the use of a specific argument increase the recall of the related fact?
table     (ASM.arg$ArgUseMi,ASM.arg$AttrSalMiCoded) # Mileage
chisq.test(ASM.arg$ArgUseMi,ASM.arg$AttrSalMiCoded)
table     (ASM.arg$ArgUseYr,ASM.arg$AttrSalYrCoded) # Model Year
chisq.test(ASM.arg$ArgUseYr,ASM.arg$AttrSalYrCoded)

####### Salience --> Subjective Value
####### -------------------------------------
#Does recall/salience of different attributes affect outcomes?
summary(lm(sv5 ~ ArgCond + log(OtherTime) + log(ArgTime) + AttrSalMiCoded + AttrSalYrCoded + AttrSalModCoded + AttrSalExtCoded, data=ASM))
summary(lm(RP ~ ArgCond + log(OtherTime) + log(ArgTime) + AttrSalMiCoded + AttrSalYrCoded + AttrSalModCoded + AttrSalExtCoded, data=ASM))
summary(glm(as.integer(ResAccept)-1 ~ ArgCond + log(OtherTime) + log(ArgTime) + AttrSalMiCoded + AttrSalYrCoded + AttrSalModCoded + AttrSalExtCoded, data=ASM))

####### Arg --> Issue Weight
####### -------------------------------------
#Is the use of an argument associated with placing more weight on that issue?
ArgUse.long    <- melt(ASM.arg[c("ID","ArgUseYr","ArgUseMi","ArgUseCon","ArgUseSafe","ArgUseFuel")],id="ID")
IssW.long      <- melt(ASM.arg[c("ID","IssWYr","IssWMi","IssWCon","IssWSafe","IssWFuel")],id="ID")
UseWeight.long <- data.frame(ArgUse.long,IssW.long[3])
names(UseWeight.long) <- c("ID","Topic","ArgUse","IssW")
summary(lme(IssW ~ factor(ArgUse),random=~1|ID, data=UseWeight.long,na.action="na.exclude"))

####### Issue Weight --> Subjective Value
####### -------------------------------------
#Do the weights placed on certain issues predict outcomes?
summary(lm(sv5 ~ IssWYr + IssWMi + IssWCon + IssWMod + IssWMat + IssWExt + IssWSafe + IssWFuel, data=ASM))

####### Arg --> Subjective Value
####### -------------------------------------

#Does the number of arguments made use predict outcomes?
summary(lm(sv5 ~ ArgUseSum + log(OtherTime) + log(ArgTime), data=ASM))
summary(lm(RP ~ ArgUseSum + log(OtherTime) + log(ArgTime), data=ASM))
summary(glm(as.integer(ResAccept)-1 ~ ArgUseSum + log(OtherTime) + log(ArgTime), data=ASM))

#Do the use of specific arguments predict outcomes?
summary(lm(sv5 ~ ArgUseMi + ArgUseYr + ArgUseCon + ArgUseSafe + ArgUseParts + ArgUseFuel + ArgUseOther, data=ASM.arg))
summary(lm(RP ~ ArgUseMi + ArgUseYr + ArgUseCon + ArgUseSafe + ArgUseParts + ArgUseFuel + ArgUseOther, data=ASM.arg))



        