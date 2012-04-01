library(plyr)
library(ggplot2)
library(sciplot)
library(psych)

####### ---------------------------------
#######  Data Prep
####### ---------------------------------

#load data
lve <- read.csv("http://swift.cbdr.cmu.edu/data/LVE-data-2012-03-01.csv", stringsAsFactors=F)
reqs <- read.csv("~/Desktop/reqs.txt", stringsAsFactors=F, header=F)
log <- read.csv("~/Desktop/log.txt",stringsAsFactors=F, header=F)

#set empty strings to NA
lve[lve==""] <- NA

#remove uncessary columns
lve <- lve[,!names(lve) %in% c("X.V1","V2","V3","V4","V5","V7","V10","X")]
#rename unnamed columns
lve <- rename(lve, c("V6"="IP","V8"="Starttime","V9"="Endtime","Q30"="ReqSubmitted",
                     "Q31_1"="Use_K1","Q31_2"="Use_K2","Q31_3"="Use_K3","Q31_4"="Use_DX1","Q31_5"="Use_DX2",
                     "Q31_6"="Use_K4","Q31_7"="Use_KF","Q31_8"="Use_KT","Q31_9"="Use_KM","Q31_10"="Use_KC","Q31_11"="Use_0"))

#add simple IDs for each record
lve$ID <- factor(c(1:nrow(lve)))

#Set conditions and factors
lve$ArgCond <- factor(lve$ArgCond)
lve$src <-factor(lve$src)
lve$BCread <- factor(lve$BCread, labels=c("Read","PartRead","NotRead"))
lve$BCown <- factor(lve$BCown, labels=c("No","Yes-Digital","Yes-Paper"))
lve$OwnOrReadBook <- rep(0,nrow(lve)) #combine own and read into a single binary measure of previous exposure
lve$OwnOrReadBook[lve$BCread=="Read" | lve$BCread=="PartRead" | lve$BCown=="Yes-Digital" | lve$BCown=="Yes-Paper"] <- 1
lve$OwnOrReadBook <- factor(lve$OwnOrReadBook, labels=c("No","Yes"))

lve$DemGen <- factor(lve$DemGen, labels=c("Male","Female"))

#Check allocation to conditions
table(lve$ArgCond)

# remove participants who dropped out before being assigned to conditions
lve <- subset(lve, !is.na(ArgCond))

####### ---------------------------------
#######  Calculated Variables
####### ---------------------------------
lve$ArgChars <- nchar(lve$Arg)  #character count of arguments written

lve <- lve[order(lve$Starttime),]
lve$IPdup <- duplicated(lve$IP)  
lve$Emaildup <- duplicated(lve$Email,incomparables= c(NA))

lve$Starttime <- as.POSIXct(lve$Starttime)
lve$Endtime   <- as.POSIXct(lve$Endtime)
lve$TotalTime <- -as.double(lve$Starttime - lve$Endtime) #number of seconds between start- and end- time

lve$TimeScen <- lve$TimeIntro_3 + lve$TimeDesc_3 + lve$TimeTask_3

####### ---------------------------------
#######  Exclusion Criteria
####### ---------------------------------
#Got to the last page of the study
length(lve$ID[is.na(lve$Link)])
lve <- subset(lve,!is.na(lve$Link))

#duplicate emails
length(lve$ID[lve$Emaildup])
#lve <- subset(lve,lve$Emaildup == FALSE)

#duplicate IP address
length(lve$ID[lve$IPdup])
#lve <- subset(lve,lve$Emaildup == FALSE)

#total study completion time
describe.by(lve$TotalTime,group=lve$ArgCond)
qplot(lve$TotalTime, geom="histogram", group=lve$ArgCond, fill=lve$ArgCond, xlim=c(0,800), binwidth=15, position="dodge")
length(lve$ID[lve$TotalTime<120]) 
#lve <- subset(lve,lve$TotalTime>120)

#Time spent reading scenario
describe(lve$TimeScen)
qplot(lve$TimeScen, geom="histogram", xlim=c(0,250))
length(lve$ID[lve$TimeScen<50])
lve <- subset(lve,lve$TimeScen > 50)

#Length of argument written
describe(lve$ArgChars[lve$ArgCond=="Arg"])
qplot(lve$ArgChars[lve$ArgCond=="Arg"], geom="histogram", binwidth=20)
length(lve$ID[lve$ArgCond=="Arg" & lve$ArgChars<50])
lve <- subset(lve,xor(lve$ArgCond=="NoArg", lve$ArgChars>50))

#Already Own or have read the book
length(lve$ID[lve$OwnOrReadBook=="Yes"])
lve <- subset(lve, lve$OwnOrReadBook=="No")

#Check allocation to conditions
table(lve$ArgCond)

####### ---------------------------------
#######  Sample Characteristics
####### ---------------------------------
lve$DemGeo <- factor(lve$DemGeo)
str(lve$DemGeo)
lve$DemGen <- factor(lve$DemGen, labels=c("Male","Female"))
table(lve$DemGen)
lve$DemYOB <- lve$DemYOB+1919  #values start at 1920=1
lve$DemAge <- 2012 - lve$DemYOB
describe(lve$DemAge)
table(lve$src)
lve$DemLang <- factor(tolower(lve$DemLang))
length(lve$DemLang)
table(lve$DemLang)

####### ---------------------------------
#######  Create composite measures
####### ---------------------------------

#Perception of Information Positivity
alpha(lve[,c("PercStars_1","PercEdRev")])

#Subjective Value of Book
alpha(lve[,c("SUenter","SUsusp","SUkingrel","SUshare","SUexcite","SUsoonread")])
lve$sv6 <- rowMeans(
  data.frame(
    scale(lve$SUenter),
    scale(lve$SUsusp),
    scale(lve$SUkingrel),
    scale(lve$SUshare),
    scale(lve$SUexcite),
    scale(lve$SUsoonread)
    ),
  na.rm=TRUE)

####### ---------------------------------
#######  Match survey data with server logs
####### ---------------------------------
names(reqs) <- c("timestamp","ip","userKey","email","optin")
reqs$bookReq <- rep(1,nrow(reqs))
lve <- merge(lve,reqs[,c("userKey","bookReq")],all.x=T)
lve$bookReq[is.na(lve$bookReq)] <- 0

names(log) <- c("timestamp","ip","userKey")
nVisit <- ddply(log, c("userKey"),function(df){ nrow(df)})
names(nVisit) <- c("userKey","nVisits")
lve <- merge(lve,nVisit,all.x=T)
lve$nVisits[is.na(lve$nVisits)] <- 0

####### ---------------------------------
#######  Hypothesis Tests
####### ---------------------------------

#Does subjective value of the book vary by condition?
t.test(sv6 ~ ArgCond, data=lve)
bargraph.CI(ArgCond,sv6,data=lve)
qplot(ArgCond,sv6,data=lve,geom="boxplot",ylab="Subjective Value of Book (6 item, Z score)")

#Did perception of the user review vary by condition?
t.test(PercStars_1 ~ ArgCond, data=lve)
bargraph.CI(ArgCond,PercStars_1, data=lve, ylim=c(1,5))
qplot(ArgCond,PercStars_1,data=lve,geom=c("boxplot","jitter"),ylim=c(1,5),ylab="Perception of mean user review")

#Did perception of the editorial review vary by condition?
t.test(PercEdRev ~ ArgCond, data=lve)

#Does the liklihood of requesting the book vary by condition?
table(lve$ArgCond, lve$bookReq)
chisq.test(lve$ArgCond, lve$bookReq)
summary(glm(bookReq ~ ArgCond + sv6, data=lve))
summary(glm(bookReq ~ ArgCond + sv6 + log(TimeScen) + log(TotalTime), data=lve))

####### ---------------------------------
#######  Failures of Randomization?
####### ---------------------------------

#Did the samples differ in how long they looked at the book info?
t.test(TimeDesc_3 ~ ArgCond, data=lve)
qplot(ArgCond,TimeDesc_3,data=lve, geom="boxplot")

#Did the samples differ in how many clicks on the book info?
describe.by(lve$TimeDesc_4, lve$ArgCond)
t.test(log(TimeDesc_4) ~ ArgCond, data=lve)
qplot(ArgCond,TimeDesc_4,data=lve, geom="boxplot")

#Did the samples differ int terms of time spent reading materials?
describe.by(lve$TimeScen, lve$ArgCond)
t.test(logTimeScen)~ ArgCond, data=lve)
qplot(ArgCond,TimeScen,data=lve, geom="boxplot")

####### ---------------------------------
#######  Differences among those who returned for the book?
####### ---------------------------------

requesters <- subset(lve, bookReq==1)

#Does subjective value of the book vary by condition?
t.test(sv6 ~ ArgCond, data=requesters)
bargraph.CI(ArgCond,sv6,data=requesters)
qplot(ArgCond,sv6,data=requesters,geom="boxplot",ylab="Subjective Value of Book (6 item, Z score)")

#Did perception of the user review vary by condition?
t.test(PercStars_1 ~ ArgCond, data=requesters)
bargraph.CI(ArgCond,PercStars_1, data=requesters, ylim=c(1,5))
qplot(ArgCond,PercStars_1,data=requesters,geom=c("boxplot","jitter"),ylim=c(1,5),ylab="Perception of mean user review")

#Did perception of the editorial review vary by condition?
t.test(PercEdRev ~ ArgCond, data=requesters)

#Did the samples differ in how long they looked at the book info?
t.test(TimeDesc_3 ~ ArgCond, data=requesters)
qplot(ArgCond,TimeDesc_3,data=requesters, geom="boxplot")

#Did the samples differ in how many clicks on the book info?
describe.by(requesters$TimeDesc_4, requesters$ArgCond)
t.test(log(TimeDesc_4) ~ ArgCond, data=requesters)
qplot(ArgCond,TimeDesc_4,data=requesters, geom="boxplot")
