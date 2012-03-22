library(plyr)
library(ggplot2)
library(sciplot)
library(psych)

####### ---------------------------------
#######  Data Prep
####### ---------------------------------

#load data
ABS2 <- read.csv("http://swift.cbdr.cmu.edu/data/ABS2-data-2012-02-24.csv", stringsAsFactors=F)

#data prep
source("ABS2 Data Prep.R")

####### ---------------------------------
#######  Sample Characteristics
####### ---------------------------------
ABS2$DemGeo <- factor(ABS2$DemGeo)
str(ABS2$DemGeo)
ABS2$DemGen <- factor(ABS2$DemGen, labels=c("Male","Female"))
table(ABS2$DemGen)
ABS2$DemYOB <- ABS2$DemYOB+1919  #values start at 1920=1
ABS2$DemAge <- 2012 - ABS2$DemYOB
describe(ABS2$DemAge)
table(ABS2$src)
ABS2$DemLang <- factor(tolower(ABS2$DemLang))
length(ABS2$DemLang)
table(ABS2$DemLang)

ABS2$DemOwnCar <- factor(ABS2$DemOwnCar, labels=c("OwnsCar","DoesNotOwnCar"))
ABS2$DemCarPurc <- factor(ABS2$DemCarPurc, labels=c("HavePurchased","HaveNotPurchased"))
carbuyers <- subset(ABS2, ABS2$DemCarPurc=="HavePurchased")
colSums(carbuyers[c("DemCarPTyp_1","DemCarPTyp_2","DemCarPTyp_3","DemCarPTyp_4")],na.rm=T)

ABS2$DemAffect <- factor(ABS2$DemAffect, label=c("Like More","No Change","Like Less"))

####### ---------------------------------
#######  Calculated Variables
####### ---------------------------------
ABS2$ArgChars <- nchar(ABS2$Arg)  #character count of arguments written

ABS2 <- ABS2[order(ABS2$Starttime),]
ABS2$IPdup <- duplicated(ABS2$IP)  
ABS2$Emaildup <- duplicated(ABS2$Email,incomparables= c(NA))

ABS2$Starttime <- as.POSIXct(ABS2$Starttime)
ABS2$Endtime   <- as.POSIXct(ABS2$Endtime)
ABS2$TotalTime <- -as.double(ABS2$Starttime - ABS2$Endtime) #number of seconds between start- and end- time

ABS2$OtherTime <- ABS2$TotalTime - ABS2$ArgTime #number of seconds doing everything other than the arg

####### ---------------------------------
#######  Exclusion Criteria
####### ---------------------------------
#duplicate emails
length(ABS2$ID[ABS2$Emaildup])
ABS2 <- subset(ABS2,ABS2$Emaildup == FALSE)

#duplicate IP address
length(ABS2$ID[ABS2$IPdup])
#ABS2 <- subset(ABS2,ABS2$Emaildup == FALSE)

#total study completion time
describe.by(ABS2$TotalTime,group=ABS2$ArgCond)
#qplot(ABS2$TotalTime, geom="histogram", group=ABS2$ArgCond, fill=ABS2$ArgCond, xlim=c(0,300), binwidth=1, position="dodge")
length(ABS2$ID[ABS2$TotalTime<60]) 
ABS2 <- subset(ABS2,ABS2$TotalTime>60)

#Time spent reading scenario
describe(ABS2$ScenTime)
#qplot(ABS2$ScenTime, geom="histogram", xlim=c(0,150))
length(ABS2$ID[ABS2$ScenTime<10])
ABS2 <- subset(ABS2,ABS2$ScenTime > 10)

#Time spent writing argument
describe.by(ABS2$ArgTime, group=ABS2$ArgCond)
#qplot(ABS2$ArgTime[ABS2$ArgCond=="Arg"], geom="histogram", xlim=c(0,1000))
length(ABS2$ID[ABS2$ArgCond=="Arg" & ABS2$ArgTime<15])

#Length of argument written
describe(ABS2$ArgChars[ABS2$ArgCond=="Arg"])
#qplot(ABS2$ArgChars[ABS2$ArgCond=="Arg"], geom="histogram", binwidth=10)
length(ABS2$ID[ABS2$ArgCond=="Arg" & ABS2$ArgChars<10])
ABS2 <- subset(ABS2,xor(ABS2$ArgCond=="NoArg", ABS2$ArgChars>10))

#Check allocation to conditions
table(ABS2$ArgCond, ABS2$RoleCond)

#write potential lottery winners to file
write.csv(ABS2[!is.na(ABS2$Email),c("Email","src")],"ABS2-lottery.csv")

####### ---------------------------------
#######  Create composite measures
####### ---------------------------------

#Subjective Utility of Car, 5 items
# reverse score seller ratings of wanting to sell and being excited to sell
ABS2$SVWant[ABS2$RoleCond=="Seller"] <- 8-ABS2$SVWant[ABS2$RoleCond=="Seller"]
ABS2$SVExcite[ABS2$RoleCond=="Seller"] <- 8-ABS2$SVExcite[ABS2$RoleCond=="Seller"]

alpha(ABS2[,c("SVAttr","SVFav","SVShowoff")])
ABS2$sv3 <- rowMeans(
  data.frame(
    scale(ABS2$SVAttr),
    scale(ABS2$SVFav),
    scale(ABS2$SVShowoff)
    ),
  na.rm=TRUE)

alpha(ABS2[,c("SVAttr","SVFav","SVShowoff","SVWant","SVExcite")])
ABS2$sv5 <- rowMeans(
  data.frame(
    scale(ABS2$SVAttr),
    scale(ABS2$SVFav),
    scale(ABS2$SVShowoff),
    scale(ABS2$SVWant),
    scale(ABS2$SVExcite)
    ),
  na.rm=TRUE)

#Reaction to counterpart's response, 2 items
alpha(ABS2[,c("ResSat","ResFair")])
ABS2$ResReact <- rowMeans(
  data.frame(
    scale(ABS2$ResSat),
    scale(ABS2$ResFair)
    ),
  na.rm=TRUE)  

####### ---------------------------------
#######  Hypothesis Tests
####### ---------------------------------

#Does subjective value of the car vary by condition (3-item scale for comparison to ABS)?
summary(aov(sv3 ~ ArgCond*RoleCond, data=ABS2))
bargraph.CI(ArgCond,sv3,group=RoleCond,data=ABS2, legend=T, ylab="Subjective Valuation of Car (3 item, Z-score)",main="Role x Condition on SV of Car")
sv3.lm <- (lm(sv3 ~ ArgCond*RoleCond + log(OtherTime) + log(ArgTime), data=ABS2))
sv3.lm <- (lm(sv3 ~ ArgCond*RoleCond, data=ABS2))
summary(sv3.lm)

#Does subjective value of the car vary by condition (5-item scale)?
summary(aov(sv5 ~ ArgCond*RoleCond, data=ABS2))
bargraph.CI(ArgCond,sv5,group=RoleCond, data=ABS2, legend=T, ylab="Subjective Valuation of Car (5 item, Z-score)",main="ABS2: Role x Condition on SV(5) of Car")
sv5.lm <- (lm(sv5 ~ ArgCond*RoleCond + log(OtherTime) + log(ArgTime), data=ABS2))
sv5.lm <- (lm(sv5 ~ ArgCond*RoleCond, data=ABS2))
summary(sv5.lm)

#Does the decision to accept the offer vary by condition?
table(ABS2$ArgCond, ABS2$ResAccept, ABS2$RoleCond)
summary(glm(as.integer(ResAccept)-1 ~ ArgCond*RoleCond, data=ABS2))
chisq.test(ABS2$ArgCond, ABS2$ResAccept)

#Does reaction to the offer vary by condition?
summary(aov(ResReact ~ ArgCond, data=ABS2))
bargraph.CI(ArgCond,ResReact,data=ABS2)

#Does RP vary by condition?
summary(aov(RP ~ ArgCond*RoleCond, data=ABS2))
rp.lm <- lm(RP ~ ArgCond*RoleCond, data=ABS2)
summary(rp.lm)
bargraph.CI(ArgCond,RP,group=RoleCond,data=ABS2, ylim=c(2000,2500), legend=T, ylab="Reservation Price", main="ABS2: Role x Condition on Reservation Price")

#Does Satisfaction with participants' own actual car vary by condition?
table(ABS2$DemOwnCar)
carowners <- subset(ABS2,ABS2$DemOwnCar=="OwnsCar")
summary(aov(DemCarSat ~ ArgCond*RoleCond, data=carowners))
bargraph.CI(RoleCond, DemCarSat, group=ArgCond, data=carowners, legend=T, y.leg=7, x.leg=1, ylim=c(1,7))
describe.by(ABS2$DemCarSat, ABS2$RoleCond)
