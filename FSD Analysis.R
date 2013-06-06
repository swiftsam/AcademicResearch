#clear memory
rm(list=ls())

#load libraries
if (!require("reshape")) install.packages("reshape") #for rename()ing variables
if (!require("car")) install.packages("car")  #for recode()ing variables
if (!require("xtable")) install.packages("xtable")  #for outputting tables

#load data
fsd <- read.csv("http://samswift.org/data/FSD-data-2011-02-22.csv", header=TRUE, sep=",")
#fsd <- read.csv("FSD-data-2011-02-22.csv", header=TRUE, sep=",")
attach(fsd)

#shape data
fsd <- rename(fsd, c(Outcome0deny1waitlist2admit="Outcome",UG_GPA="GPA",Male="Gender",WorkHistory_years="WorkHistory"))
fsd$Married <- NULL
fsd$UGI <- NULL
fsd$UGI_ID <- NULL
fsd$UGI_Private <- NULL

#clean variables
summary(fsd)

temp <- recode(fsd$Age, "lo:18=NA;64:hi=NA")
fsd$Age <- temp
rm(temp)

temp <-recode(fsd$GMAT, "lo:199=NA;801:hi=NA")
fsd$GMAT <- temp
rm(temp)

temp <-recode(fsd$WorkHistory, "lo:0=NA; 51:hi=NA")
fsd$WorkHistory <- temp
rm(temp)

temp <-recode(fsd$GPA, "0=NA; 4.5:hi=NA")
fsd$GPA <- temp
rm(temp)

temp <-recode(fsd$Interview_Z, "-10:-4=NA")
fsd$Interview_Z <- temp
rm(temp)

#Applicant-level correlations
if (!require("Hmisc")) install.packages("Hmisc")
library(Hmisc)
appvars<-c("Gender","Age","USCitizen","GMAT","Interview_Z","WorkHistory","GPA")
applicants <-fsd[appvars]
rcorr(as.matrix(applicants), type="pearson")
rm(applicants)

#Factors
fsd$Outcome <- factor(fsd$Outcome,ordered=TRUE,levels = c(0,1,2),labels=c("Deny","Waitlist","Accept"))
fsd$admitschool <- factor(fsd$AdmittingSchoolID)
fsd$appyear <- factor(fsd$ApplicationYear)
fsd$Gender <- factor(fsd$Gender,levels=c(0,1),labels=c("Female","Male"))
fsd$USCitizen <-factor(fsd$USCitizen)
fsd$UGI_University <-factor(fsd$UGI_University)

#combine dummy coded race vars into single factor.  White was previously the base level (0,0,0,0,0) so it is added
race<-data.frame(cbind(fsd$Race_AfAmerican,fsd$Race_Asian,fsd$Race_Hispanic,fsd$Race_AmerIndian,fsd$Race_Other))
racemat<-data.matrix(race)
colnames(racemat) <-c("AfAmerican","Asian","Hispanic","AmerIndian","Other")
racefac <-factor((racemat %*% (1:ncol(racemat))) + 1, labels=c("White",colnames(racemat)))
fsd$Race <- racefac
rm(race, racemat, racefac)
fsd$Race_AfAmerican <-NULL
fsd$Race_Asian <-NULL
fsd$Race_Hispanic <-NULL
fsd$Race_AmerIndian <-NULL
fsd$Race_Other <-NULL

summary(fsd)

#count records by year by school
xtabs(~fsd$AdmittingSchoolID + fsd$ApplicationYear)

#Check frequency of outcomes by school
xtabs(~fsd$AdmittingSchoolID + fsd$Outcome)

##Compute Variables##
#relGPA
fsd$relGPA <- fsd$GPA-fsd$UGI_AvgGPA
#Mean-center AvgGPA
fsd$mcAvgGPA <- UGI_AvgGPA - mean(UGI_AvgGPA,0,na.rm=TRUE)

#create standardized school quality predictors
fsd$UGI_USNewsZ <- scale(fsd$UGI_USNews)
fsd$UGI_GourmanZ <- scale(fsd$UGI_Gourman)
fsd$UGI_AvgEntranceSATZ <- scale(fsd$UGI_AvgEntranceSAT)
fsd$UGI_TuitionZ<-scale(fsd$UGI_Tuition)

fsd.z <- fsd
fsd.z$relGPA<-scale(fsd$relGPA)
fsd.z$mcAvgGPA<-scale(fsd$mcAvgGPA)
fsd.z$WorkHistory<-scale(fsd$WorkHistory)
fsd.z$Age<-scale(fsd$Age)
fsd.z$GMAT<-scale(fsd$GMAT)
fsd.z$UGI_USNews<-scale(fsd$UGI_USNews)
fsd.z$UGI_AvgEntranceSAT<-scale(fsd$UGI_AvgEntranceSAT)
fsd.z$UGI_Gourman<-scale(fsd$UGI_Gourman)
fsd.z$UGI_Tuition<-scale(fsd$UGI_Tuition)
fsd$UGI

#create top-schools dataset
fsd.top <-subset(fsd, UGI_USNews>69)
summary(fsd.top)

### Ordinal Logit Models ####

#M1: Barebones
m1<- polr(Outcome ~ relGPA + mcAvgGPA, data=fsd, Hess=TRUE)

#M2: m1 + basic controls
m2 <- polr(Outcome ~ relGPA + mcAvgGPA + Interview_Z + WorkHistory  + GMAT + UGI_USNews, data=fsd, Hess=TRUE)

#M3: m2 + demographics
m3 <- polr(Outcome ~ relGPA + mcAvgGPA + Interview_Z + Age + WorkHistory + GMAT + UGI_USNews + Gender + USCitizen + Race, data=fsd, Hess=TRUE)

#M4: m3 + appyear dummy
m4 <- polr(Outcome ~ relGPA + mcAvgGPA + Interview_Z + Age + WorkHistory + GMAT + UGI_USNews + Gender + USCitizen + Race + appyear, data=fsd, Hess=TRUE)

#M5: m3 + admitschool dummy
m5 <- polr(Outcome ~ relGPA + mcAvgGPA + Interview_Z + Age + WorkHistory + GMAT + UGI_USNews + Gender + USCitizen + Race + admitschool, data=fsd, Hess=TRUE)

#M6: aka "FULL" -> m3 + admitschool and appyear
m6 <- polr(Outcome ~ relGPA + mcAvgGPA + Interview_Z + Age + WorkHistory + GMAT + UGI_USNews + Gender + USCitizen + Race + admitschool + appyear, data=fsd, Hess=TRUE)

#M7: FULL - interview (to get sample size up)
m7 <- polr(Outcome ~ relGPA + mcAvgGPA + Age + WorkHistory + GMAT + UGI_USNews + Gender + USCitizen + Race + admitschool + appyear, data=fsd, Hess=TRUE)

#M8: FULL + interaction
m8 <- polr(Outcome ~ relGPA + mcAvgGPA + relGPA*mcAvgGPA + Interview_Z + Age + WorkHistory + GMAT + UGI_USNews + Gender + USCitizen + Race + admitschool + appyear, data=fsd, Hess=TRUE)

#M9: FULL + interaction - interview (to get sample size up)
m9 <- polr(Outcome ~ relGPA + mcAvgGPA + relGPA*mcAvgGPA + Age + WorkHistory + GMAT + UGI_USNews + Gender + USCitizen + Race + admitschool + appyear, data=fsd, Hess=TRUE)

#M10: M7 with standardized school quality predictors
m10 <- polr(Outcome ~ relGPA + mcAvgGPA + Age + WorkHistory + GMAT + UGI_USNewsZ + Gender + USCitizen + Race + admitschool + appyear, data=fsd, Hess=TRUE)

#M11: M7 with the top schools subset
m11 <- polr(Outcome ~ relGPA + mcAvgGPA + Age + WorkHistory + GMAT + UGI_USNews + Gender + USCitizen + Race + admitschool + appyear, data=fsd.top, Hess=TRUE)

#M12: M10 with UGI_Gourman instead of UGI_USNews
m12 <- polr(Outcome ~ relGPA + mcAvgGPA + Age + WorkHistory + GMAT + UGI_GourmanZ + Gender + USCitizen + Race + admitschool + appyear, data=fsd, Hess=TRUE)

#M13: M10 with UGI_AvgEntranceSAT instead of UGI_USNews
m13 <- polr(Outcome ~ relGPA + mcAvgGPA + Age + WorkHistory + GMAT + UGI_AvgEntranceSATZ + Gender + USCitizen + Race + admitschool + appyear, data=fsd, Hess=TRUE)

#M14: M10 + UGI_AvgEntranceSAT + UGI_Gourman
m14 <- polr(Outcome ~ relGPA + mcAvgGPA + Age + WorkHistory + GMAT + UGI_USNewsZ + UGI_AvgEntranceSATZ + UGI_GourmanZ + Gender + USCitizen + Race + admitschool + appyear, data=fsd, Hess=TRUE)

#M15: M7 with nominal GPA instead of relative GPA
m15 <- polr(Outcome ~ GPA + mcAvgGPA + Age + WorkHistory + GMAT + UGI_USNews + Gender + USCitizen + Race + admitschool + appyear, data=fsd, Hess=TRUE)

#M16: PLOS reviewer request
m16 <- polr(Outcome ~ relGPA + mcAvgGPA + Age + WorkHistory + GMAT + UGI_USNewsZ + UGI_AvgEntranceSATZ + UGI_GourmanZ + Gender + USCitizen + Race + admitschool + appyear, data=fsd, Hess=TRUE)
m17 <- polr(Outcome ~ relGPA +            Age + WorkHistory + GMAT + UGI_USNewsZ + UGI_AvgEntranceSATZ + UGI_GourmanZ + Gender + USCitizen + Race + admitschool + appyear, data=fsd, Hess=TRUE)


##TABLES##
library(memisc)
mtable.main <- mtable("Model 1"=m1,"Model 2"=m2,"Model 3"=m3, "Model 4"=m4, "Model 5"=m5, "Model 6"=m6, "Model 7"=m7, summary.stats=c("N","AIC"))
mtable.quality <-mtable("US News"=m10, "Gourman"=m12, "Entrance SAT"=m13, "All"=m14, summary.stats=c("N","AIC"))
mtable.alt <-mtable("Standard"=m7,"Interaction"=m9,"Top Schools"=m11,"Nominal GPA"=m15, summary.stats=c("N","AIC"))
mtable.plos <-mtable("Kitchen Sink"=m16, "No AvgGPA"=m17, summary.stats=c("N","AIC"))


#Readable output of tables
mtable.main
mtable.quality
mtable.alt

#Tab-delimited output of tables to text files
write.mtable(mtable.main, file="mtable.main.txt")
write.mtable(mtable.quality, file="mtable.quality.txt")
write.mtable(mtable.alt, file="mtable.alt.txt")
write.mtable(mtable.plos, file="mtable.plos.txt")

#interpretation of coefficients
exp(coef(m10))
sd(fsd$mcAvgGPA,na.rm=TRUE)

#social mobility analyses
mmob <- polr(Outcome ~ UGI_TuitionZ + relGPA + Age + WorkHistory + GMAT + UGI_USNews + Gender + USCitizen + Race + admitschool + appyear, data=fsd, Hess=TRUE)
summary(mmob)



##GRAPHS##
##Calculate predicted values##

#general settings
pred.model <- m7
relGPA.sd<-sd(pred.model$model["relGPA"])
mcAvgGPA.sd<-sd(pred.model$model["mcAvgGPA"])
relGPA<-seq(-relGPA.sd*2,relGPA.sd*2,.05)
avgGPA = c(-2*mcAvgGPA.sd,0,2*mcAvgGPA.sd)
#avgGPA = c(-.3,0,.3)

#setting control variable levels
Interview_Z<-0
Age<-mean(pred.model$model["Age"])
WorkHistory<-mean(pred.model$model["WorkHistory"])
GMAT<-mean(pred.model$model["GMAT"])
UGI_USNews<-mean(pred.model$model["UGI_USNews"])
prop.gender = table(pred.model$model$Gender)/nrow(pred.model$model)
prop.uscitizen = table(pred.model$model$USCitizen)/nrow(pred.model$model)
prop.race = table(pred.model$model$Race)/nrow(pred.model$model)
prop.school = table(pred.model$model$admitschool)/nrow(pred.model$model)
prop.year = table(pred.model$model$appyear)/nrow(pred.model$model)

values.fixed<-c(
            Age,
            WorkHistory,
            GMAT,
            UGI_USNews,
            prop.gender["Male"],
            prop.uscitizen["1"],
            prop.race["AfAmerican"],
            prop.race["Asian"],
            prop.race["Hispanic"],
            prop.race["AmerIndian"],
            prop.race["Other"],
            prop.school["2"],
            prop.school["3"],
            prop.school["4"],
            prop.year["2001"],
            prop.year["2002"],
            prop.year["2003"],
            prop.year["2004"],
            prop.year["2005"],
            prop.year["2006"],
            prop.year["2007"],
            prop.year["2008"])

#calculate the constant portion of the term using predictors other than relGPA and avgGPA
partialexp <- pred.model$zeta[2] - sum(values.fixed * pred.model$coef[3:24])

#create a dataframe of predicted values
f <- function(relGPA, avgGPA) 1/(1+exp(partialexp-sum(relGPA*pred.model$coef[1],avgGPA*pred.model$coef[2])))
vf <- Vectorize(f, SIMPLIFY = FALSE)

predVals <- outer(relGPA, avgGPA, FUN=vf)

graphdata<-data.frame(relGPA, predVals)
colnames(graphdata)<-c("relGPA","lowAvgGPA","medAvgGPA","highAvgGPA")

par(bg = "white")
matplot(graphdata$relGPA, 
        graphdata[, c("lowAvgGPA","medAvgGPA","highAvgGPA")],
        type="l",
        lty=c(2,1,3),
        lwd=3,
        col=1,
        ylab="Predicted Prob of Acceptance",
        xlab="Performance: Relative GPA")
legend("topleft", 
        inset=.05, 
        title="Situation:\nAvg GPA at alma mater", 
        legend=c("Low   (GPA = 2.93)","Mean (GPA = 3.25)","High  (GPA = 3.57)"),
        col=1,
        lwd=2,
        lty=c(2,1,3),
        bty="n")



