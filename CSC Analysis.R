#load data
csc <- read.csv("~/Documents/Research/Negotiation - Self Perception/CSC/data/CSC-CSBA Data-2011-12-01.csv", stringsAsFactors=FALSE)

#rename variables and delete uneeded columns
source("~/Documents/Research/Negotiation - Self Perception/CSC/analysis/CSC-CSBA data prep.R")

#set correct data types
csc$ArgCond <- factor(csc$ArgCond,levels=c("None","Free","OwnLow","OtherHigh","OwnBatna","OtherBatna"))
table(csc$ArgCond)
csc$ArgTarget <- factor(csc$ArgTarget)
csc$ArgType <- factor(csc$ArgType)

#calculate total time spent completing experiment
csc$StartDate <- as.POSIXct(csc$StartDate)
csc$EndDate   <- as.POSIXct(csc$EndDate)
csc$TotalTime <- csc$EndDate - csc$StartDate

#calculate character counts of arguments
csc$ArgCharCount <- nchar(csc$Arg)

#output coding documents
#args <- subset(csc[,c("ID","QualtricsID","Arg")],nchar(Arg)>2)
#write.csv(args,file="argcoding.csv",sep=",")

#Check for duplicate IP addresses & Emails
csc <- csc[order(csc$StartDate),]
csc$IPdup <- duplicated(csc$IP)
csc$Emaildup <- duplicated(csc$DemEmail,incomparables= c(NA))

##Count cases by certain exclusion criteria
length(csc$ID[is.na(csc$ArgCond)])
length(csc$ID[csc$ArgCharCount<2])
length(csc$ID[csc$ScenTS<10])
length(csc$ID[csc$TotalTime<180])
length(csc$ID[csc$TotalTime>5400])
length(csc$ID[csc$Finished==0])
length(csc$ID[csc$IPdup==TRUE])
length(csc$ID[csc$Emaildup==TRUE])

##Exclude cases based on ...
csc<-subset(csc,!is.na(ArgCond))    #Didn't get far enough to be assigned to a condition
csc<-subset(csc,ArgCharCount>0)     #Didn't write anything for their argument (Arg = "NA" for those in the Arg=None condition)
csc<-subset(csc,ScenTS > 10)        #Reading the scenario in less than 10 seconds
#csc<-subset(csc,TotalTime > 180)   #Finishing the study in less than 3 minutes
#csc<-subset(csc,TotalTime < 5400)  #Took more than 90 minutes to finish study
#csc<-subset(csc,IPdup == FALSE)     #duplicated IP
csc<-subset(csc,Emaildup == FALSE)     #duplicated email

#Pick Lottery Winners
#CBDR <- csc[csc$DemSource=="CBDR","DemEmail"]
#CBDR.winners <- sample(CBDR,2)
#Mturk <- csc[csc$DemSource=="mturk","DemEmail"]
#Mturk.winners <- sample(Mturk,2)

#Resulting distribution of cases across conditions
table(csc$ArgCond)

#Demographics
csc$DemState <- factor(csc$DemState)
str(csc$DemState)
csc$DemGender <- factor(csc$DemGender)
table(csc$DemGender)
csc$DemYOB <- csc$DemYOB+1919  #values start at 1920=1
csc$DemAge <- 2011 - csc$DemYOB
describe(csc$DemAge)

library(psych)

#create composite measures
alpha(data.frame(csc$SatQual,csc$SatNet, csc$SatCust))
csc$SpecSatZ <- rowMeans(
                  data.frame(
                    scale(csc$SatQual),
                    scale(csc$SatNet),
                    scale(csc$SatCust)
                    ),
                  na.rm=TRUE)

alpha(data.frame(csc$GlobSat,csc$GlobSatFavorable,csc$GlobSatRecommend,csc$GlobSatRenew,csc$GlobSatTrust))
csc$GlobSatZ <- rowMeans(
                  data.frame(
                    scale(csc$GlobSat),
                    scale(csc$GlobSatFavorable),
                    scale(csc$GlobSatRecommend),
                    scale(csc$GlobSatRenew),
                    scale(csc$GlobSatTrust)
                    ),
                  na.rm=TRUE)

alpha(data.frame(csc$WeightQual,csc$WeightNet, csc$WeightCust))
csc$WeightZ <- rowMeans(
                  data.frame(
                    scale(csc$WeightQual),
                    scale(csc$WeightNet),
                    scale(csc$WeightCust)
                    ),
                  na.rm=TRUE)            
                    
alpha(data.frame(csc$RespSat,csc$RespFair))                   
csc$RespZ <- rowMeans(
                  data.frame(
                    scale(csc$RespSat),
                    scale(csc$RespFair)
                    ),
                  na.rm=TRUE)

alpha(data.frame(csc$GlobSatPost,csc$GlobSatFavorablePost,csc$GlobSatRecommendPost,csc$GlobSatRenewPost,csc$GlobSatTrustPost))
csc$GlobSatPostZ <- rowMeans(
                  data.frame(
                    scale(csc$GlobSatPost),
                    scale(csc$GlobSatFavorablePost),
                    scale(csc$GlobSatRecommendPost),
                    scale(csc$GlobSatRenewPost),
                    scale(csc$GlobSatTrustPost)
                    ),
                  na.rm=TRUE)

#Calculate post-pre change in GlobSat
csc$GlobSatZDelta <- csc$GlobSatPostZ - csc$GlobSatZ

attach(csc)

#####Analysis#####

###contrast coding
##Orthogonal Set of 5
#coached v uncoached
csc[ArgCond=="OwnLow"|ArgCond=="OtherHigh"|ArgCond=="OwnBatna"|ArgCond=="OtherBatna","ctCoached"] <- .25
csc[ArgCond=="None"|ArgCond=="Free","ctCoached"] <- -.5
#Self v Other
csc[ArgCond=="OwnLow"|ArgCond=="OwnBatna","ctSelfOther"] <- .5
csc[ArgCond=="OtherHigh"|ArgCond=="OtherBatna","ctSelfOther"] <- -.5
csc[ArgCond=="None"|ArgCond=="Free","ctSelfOther"] <- 0
#own value v own batna
csc[ArgCond=="OwnLow","ctOwnType"] <- .5
csc[ArgCond=="OwnBatna","ctOwnType"] <- -.5
csc[ArgCond=="None"|ArgCond=="Free"|ArgCond=="OtherHigh"|ArgCond=="OtherBatna","ctOwnType"] <- 0
#other value v other batna
csc[ArgCond=="OtherHigh","ctOtherType"] <- .5
csc[ArgCond=="OtherBatna","ctOtherType"] <- -.5
csc[ArgCond=="None"|ArgCond=="Free"|ArgCond=="OwnLow"|ArgCond=="OwnBatna","ctOtherType"] <- 0
#none v free
csc[ArgCond=="None","ctNoneFree"] <- .5
csc[ArgCond=="Free","ctNoneFree"] <- -.5
csc[ArgCond=="OwnLow"|ArgCond=="OtherHigh"|ArgCond=="OwnBatna"|ArgCond=="OtherBatna","ctNoneFree"] <- 0

##Other Contrasts
#Other v none
csc[ArgCond=="OtherHigh"|ArgCond=="OtherBatna","ctOtherNone"] <- .5
csc[ArgCond=="None","ctOtherNone"] <- -1
csc[ArgCond=="OwnLow"|ArgCond=="OwnBatna"|ArgCond=="Free","ctOtherNone"] <- 0
#Own v none
csc[ArgCond=="None","ctOwnNone"] <- -1
csc[ArgCond=="OwnLow"|ArgCond=="OwnBatna","ctOwnNone"] <- .5
csc[ArgCond=="OtherHigh"|ArgCond=="OtherBatna"|ArgCond=="Free","ctOwnNone"] <- 0
#Value v BATNA
csc[ArgCond=="OwnLow"|ArgCond=="OtherHigh","ctValueBatna"] <- .5
csc[ArgCond=="OwnBatna"|ArgCond=="OtherBatna","ctValueBatna"] <- -.5
csc[ArgCond=="None"|ArgCond=="Free","ctValueBatna"] <- 0

detach(csc)
attach(csc)
                  
describe.by(SpecSatZ,    ArgCond)
describe.by(GlobSatZ,    ArgCond)
describe.by(WeightZ,     ArgCond)
describe.by(RespZ,       ArgCond)
describe.by(GlobSatPostZ,ArgCond)

library(sciplot)

######Global Satisfaction######
#overall anova
GlobSat.aov <- aov(GlobSatZ~ArgCond,data=csc)
summary(GlobSat.aov)
lineplot.CI(ArgCond,GlobSatZ,ylab="Satisfaction", xlab="Condition",main="Global Satisfaction After Sending Argument")
bargraph.CI(ArgCond,GlobSatZ,ylab="Satisfaction", xlab="Condition",main="Global Satisfaction After Sending Argument")

#Arg Characteristics Interaction
GlobSat.factoraov <- aov(GlobSatZ~ArgType*ArgTarget)
summary(GlobSat.factoraov)

#pairwise comparisons
describe.by(GlobSatZ,ArgCond)
t.test(csc[ArgCond=="OwnLow","GlobSatZ"],csc[ArgCond=="OtherHigh","GlobSatZ"])
t.test(csc[ArgCond=="None","GlobSatZ"],  csc[ArgCond=="OtherHigh","GlobSatZ"])
t.test(csc[ArgCond=="Free","GlobSatZ"],  csc[ArgCond=="OtherHigh","GlobSatZ"])
t.test(csc[ArgCond=="OwnLow","GlobSatZ"],csc[ArgCond=="None","GlobSatZ"])

#planned contrasts
GlobSat.ctSelfOther <- aov(GlobSatZ~ctSelfOther, data=csc)
summary(GlobSat.ctSelfOther)
setwd("~/Desktop")
png('csc-globsatselfother.png')
bargraph.CI(ArgCond,
            GlobSatZ, 
            col=c("grey","grey","darkolivegreen3","lightblue","darkolivegreen3","lightblue"), 
            ylab="Satisfaction", 
            xlab="Argument Condition",
            main="Satisfaction After Sending Argument\nSelf/Other Argument Type Contrast")
dev.off()
describe(csc[ArgCond=="OwnLow"|ArgCond=="OwnBatna","GlobSatZ"])
describe(csc[ArgCond=="OtherHigh"|ArgCond=="OtherBatna","GlobSatZ"])

GlobSat.ctOtherNone <- aov(GlobSatZ~ctOtherNone, data=csc)
summary(GlobSat.ctOtherNone)
bargraph.CI(ArgCond,GlobSatZ, 
            col=c("darkolivegreen3","grey","grey","lightblue","grey","lightblue"), 
            ylab="Satisfaction", 
            xlab="Condition",
            main="Global Satisfaction After Sending Argument\nNone:Other Constrast")

GlobSat.ctOwnNone <- aov(GlobSatZ~ctOwnNone, data=csc)
summary(GlobSat.ctOwnNone)
bargraph.CI(ArgCond,
            GlobSatZ,
            col=c("darkolivegreen3","grey","darkgoldenrod2","grey","darkgoldenrod2","grey"),
            ylab="Satisfaction", xlab="Condition",main="Global Satisfaction After Sending Argument\nNone:Own Contrast")

pairwise.t.test(GlobSatZ,ArgCond,pool.SD="F")
t.test(csc[ArgCond=="OwnLow","GlobSatZ"],csc[ArgCond=="OtherHigh","GlobSatZ"])

#orthagonal regression model
summary(lm(GlobSatZ~ctCoached+ctSelfOther+ctOwnType+ctOtherType+ctNoneFree))



####Global Satisfaction Post Response####
GlobSatPost.aov <- aov(GlobSatPostZ~ArgCond,data=csc)
summary(GlobSatPost.aov)
bargraph.CI(ArgCond,GlobSatPostZ,ylab="Satisfaction", xlab="Condition",main="Global Satisfaction After Response")
TukeyHSD(GlobSatPost.aov)
GlobSatPost.factoraov <-aov(GlobSatPostZ~ArgType*ArgTarget, data=csc)
summary(GlobSatPost.factoraov)

####Reaction to concession####
Resp.aov <- aov(RespZ~ArgCond,data=csc)
summary(Resp.aov)
lineplot.CI(ArgCond,RespZ,ylab="Fairness & Satisfaction", xlab="Condition", main="Reaction to Response from XYZ Wireless")
TukeyHSD(Resp.aov)

Resp.factoraov <-aov(RespZ~ArgType*ArgTarget, data=csc)
summary(Resp.factoraov)


describe.by(GlobSatZ,ArgCond)
t.test(csc[ArgCond=="None","RespZ"],csc[ArgCond=="Free","RespZ"])
t.test(csc[ArgCond=="None","RespZ"],csc[ArgCond=="OwnLow","RespZ"])
t.test(csc[ArgCond=="None","RespZ"],csc[ArgCond=="OtherHigh","RespZ"])
t.test(csc[ArgCond=="None","RespZ"],csc[ArgCond=="OwnBatna","RespZ"])
t.test(csc[ArgCond=="None","RespZ"],csc[ArgCond=="OtherBatna","RespZ"])

Resp.ctValueBatna <- aov(RespZ~ctValueBatna, data=csc)
summary(Resp.ctValueBatna)
bargraph.CI(ArgCond,RespZ, col=c("darkolivegreen3","grey","darkgoldenrod2","grey","darkgoldenrod2","grey"), ylab="Satisfaction", xlab="Condition",main="Global Satisfaction After Sending Argument\nNone:Own Contrast")

        
#Change in GlobSat
GlobSatZDelta.aov <- aov(GlobSatZDelta~ArgCond,data=csc)
summary(GlobSatZDelta.aov)
lineplot.CI(ArgCond,GlobSatZDelta,ylab="Change in Global Satisfaction", xlab="Condition", main="Change in Global Satisfaction after Response")
TukeyHSD(GlobSatZDelta.aov)
pairwise.t.test(GlobSatZDelta,ArgCond,pool.SD="F")
GlobSatZDelta.factoraov <-aov(GlobSatZDelta~ArgType*ArgTarget, data=csc)
summary(GlobSatZDelta.factoraov)
                  
#ANCOVA using character count as a predictor
GlobSat.anc <- aov(GlobSatZ~ArgCond + ArgCharCount, data=csc)
summary(lm(GlobSatZ~ArgCond + ArgCharCount, data=csc))
summary(GlobSat.anc)


#Specific Satisfaction anova
SpecSat.aov <- aov(SpecSatZ~ArgCond,data=csc)
summary(SpecSat.aov)
lineplot.CI(ArgCond,SpecSatZ,ylab="Specific Satisfaction", xlab="Condition",main="Specific Satisfaction After Sending Argument")
TukeyHSD(SpecSat.aov)

#Importance of phone attributes
Weight.aov <- aov(WeightZ~ArgCond,data=csc)
summary(Weight.aov)
lineplot.CI(ArgCond,WeightZ,ylab="Issue Weight", xlab="Condition",main="Average Weight of all issues")
TukeyHSD(Weight.aov)

#ANOVA Post-Hoc tests of individual items
TukeyHSD(aov(SatQual~ArgCond,data=csc))
TukeyHSD(aov(SatNet~ArgCond,data=csc))
TukeyHSD(aov(SatCust~ArgCond,data=csc))
TukeyHSD(aov(GlobSat~ArgCond,data=csc))
TukeyHSD(aov(GlobSatFavorable~ArgCond,data=csc))
TukeyHSD(aov(GlobSatRecommend~ArgCond,data=csc))
  bargraph.CI(ArgCond,GlobSatRecommend,ylab="Liklihood to Recommend", xlab="Condition", ylim=c(1,7), main="How likely are you to recommend XYZ Wireless' service to family members or close friends?")
TukeyHSD(aov(GlobSatRenew~ArgCond,data=csc))
  bargraph.CI(ArgCond,GlobSatRenew,ylab="Liklihood to Renew", xlab="Condition", ylim=c(1,7), main="How likely are you to renew your contract with XYZ Wireless when your current contract expires?")
TukeyHSD(aov(GlobSatTrust~ArgCond,data=csc))
  bargraph.CI(ArgCond,GlobSatTrust,ylab="Trust", xlab="Condition", ylim=c(1,5), main="How much do you agree with the statement, \"I trust XYZ Wireless\"?")
TukeyHSD(aov(WeightQual~ArgCond,data=csc))
TukeyHSD(aov(WeightNet~ArgCond,data=csc))
TukeyHSD(aov(WeightCust~ArgCond,data=csc))
TukeyHSD(aov(ProcComfort~ArgCond,data=csc))
TukeyHSD(aov(ProcDifficult~ArgCond,data=csc))
TukeyHSD(aov(ProbConcession~ArgCond,data=csc))
  bargraph.CI(ArgCond,ProbConcession,ylab="Prob of Concession",xlab="Condition",ylim=c(1,7), main="In your opinion, how likely is it that XYZ Wireless will remove the $40 in additional charges?")
TukeyHSD(aov(ExageratePos~ArgCond,data=csc))
  bargraph.CI(ArgCond,ExageratePos,ylab="Exageration of Positive Items",xlab="Condition",ylim=c(1,7), main="To what extent did you exaggerate the things you do like about being an XYZ Wireless customer?")
TukeyHSD(aov(ExagerateNeg~ArgCond,data=csc))
  bargraph.CI(ArgCond,ExagerateNeg,ylab="Exageration of Negative Items",xlab="Condition",ylim=c(1,7), main="To what extent did you exaggerate the things you do not like about being an XYZ Wireless customer?")

