# Don A Moore, Samuel A Swift, Zachariah S Sharek, Francesca Gino (2010)
# Correspondence Bias in Performance Evaluation: Why Grade Inflation Works
# Personality and Social Psychology Bulletin 36 (6) p.843-852

#clean slate
rm(list=ls())

#load libraries
library(reshape2)
library(psych)

####Experiment 1 (Project Code: scsc3)####
s3 <- read.csv("http://swift.cbdr.cmu.edu/data/SCSC3-2006-10-10.csv")
#column label code ... [item][Candidate number][disposition condition][situation condition]
#example "Success.CA.GH.IH" = Success rating, Candidate A, Grades:High, Inflation:High

#reformat data for analysis
#----------------------------
s3 <- melt(s3,id="Participant")
s3 <- cbind(s3,colsplit(s3$variable,"[.]",c("Item","Candidate","Performance","Situation")))
s3$variable <- NULL
s3$Candidate <- NULL
s3 <- dcast(s3, Participant + Performance + Situation ~ Item, value_var = 'value' )

#label factors
s3$Performance <- factor(s3$Performance, ordered=TRUE,
                         levels = c("GL","GM","GH"),
                         labels=c("Below Avg (-.3)","Average","Above Avg (+.3)"))
s3$Situation   <- factor(s3$Situation, ordered=TRUE,
                         levels = c("IL","IM","IH"),
                         labels=c("Low(~2.4)","Med(~3.0)","High(~3.6)"))
s3$Participant <- factor(s3$Participant)

#translating categories to continuous vars for regression
s3$Sit.q[s3$Situation == "Low(~2.4)"] <- 2.4
s3$Sit.q[s3$Situation == "Med(~3.0)"] <- 3.0
s3$Sit.q[s3$Situation == "High(~3.6)"] <- 3.6
s3$Per.q[s3$Performance == "Below Avg (-.3)"] <- (s3$Sit.q - .3)
s3$Per.q[s3$Performance == "Average"] <- s3$Sit.q
s3$Per.q[s3$Performance == "Above Avg (+.3)"] <- s3$Sit.q + .3

#dv correlations
cor(s3$Admit, s3$ProbAdmit, use="complete.obs")
cor(s3$Admit, s3$Success, use="complete.obs")
cor(s3$ProbAdmit, s3$Success, use="complete.obs")
alpha(data.frame(s3$Success, s3$ProbAdmit, s3$Admit))

#given high internal reliability, standardize and combine judgments
s3$Success.z <- scale(s3$Success)
s3$ProbAdmit.z <- scale(s3$ProbAdmit)
s3$Admit.z <- scale(s3$Admit)

s3$Success.z[is.na(s3$Success.z)] <- 0
s3$ProbAdmit.z[is.na(s3$ProbAdmit.z)] <- 0
s3$Admit.z[is.na(s3$Admit.z)] <- 0
s3$eval.z <- (s3$Admit.z+s3$ProbAdmit.z+s3$Success.z)/3

#descriptive statistics
#----------------------------
describe.by(s3$eval.z,s3$Performance)
describe.by(s3$eval.z,s3$Situation)
boxplot(eval.z~Performance*Situation,data=s3,las=2) #graphical summary of means of the 6 cells

#ANOVAs
#----------------------------
library(ez)

#overall assessment 
eval.aov = ezANOVA( 
    data = s3 
    , dv = .(eval.z) 
    , wid = .(Participant) 
    , within = .(Performance,Situation)
) 
print(eval.aov)

#probability of admission
ProbAdmit.aov = ezANOVA(
    data = s3 
    , dv = .(ProbAdmit) 
    , wid = .(Participant) 
    , within = .(Performance,Situation)
)
print(ProbAdmit.aov)

#rated success
Success.aov = ezANOVA(
    data = s3 
    , dv = .(Success) 
    , wid = .(Participant) 
    , within = .(Performance,Situation)
)
print(Success.aov)

#Regressions
#----------------------------
s3.glm <- glm(s3$Admit ~ s3$Sit.q+s3$Per.q, family=binomial(link="logit"))
summary(s3.glm)

#plots
#----------------------------
s3.plot <- ezPlot(data=s3, dv=.(eval.z), wid=.(Participant), within=.(Performance,Situation), x=.(Situation), do_lines=FALSE)
print(s3.plot)

