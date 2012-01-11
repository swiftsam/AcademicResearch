# Correspondence Bias in Performance Evaluation: Why Grade Inflation Works
# Don A Moore, Samuel A Swift, Zachariah S Sharek, Francesca Gino (2010)
# Personality and Social Psychology Bulletin 36 (6) p.843-852

####Experiment 2 (Project Code: SCSC2)####

#clean slate
rm(list=ls())

#load libraries
library(reshape2)
library(psych)

s2 <- read.csv("http://swift.cbdr.cmu.edu/open/SCSC2-2003-12-07.csv")
#column label code ... [item][Candidate number][disposition condition][situation condition]
#example "Success.CA.GH.IH" = Success rating, Candidate A, Grades:High, Inflation:High

#reformat data for analysis
#----------------------------
s2 <- melt(s2,id="Participant")
s2 <- cbind(s2,colsplit(s2$variable,"[.]",c("Item","Candidate","Performance","Situation")))
s2$variable <- NULL
s2$Candidate <- NULL
s2 <- dcast(s2, Participant + Performance + Situation ~ Item, value_var = 'value' )

#label factors
s2$Performance <- factor(s2$Performance, ordered=TRUE,
                         levels = c("GL","GM","GH"),
                         labels=c("Low(~2.4)","Med(~3.0)","High(~3.6)"))
s2$Situation   <- factor(s2$Situation, ordered=TRUE,
                         levels = c("IL","IM","IH"),
                         labels=c("Low(~2.4)","Med(~3.0)","High(~3.6)"))
s2$Participant <- factor(s2$Participant)

#translating categories to continuous vars for regression
s2$Sit.q[s2$Situation == "Low(~2.4)"] <- 2.4
s2$Sit.q[s2$Situation == "Med(~3.0)"] <- 3.0
s2$Sit.q[s2$Situation == "High(~3.6)"] <- 3.6
s2$Per.q[s2$Performance == "Low(~2.4)"] <- 2.4
s2$Per.q[s2$Performance == "Med(~3.0)"] <- 3.0
s2$Per.q[s2$Performance == "High(~3.6)"] <- 3.6

#dv correlations
cor(s2$Admit, s2$ProbAdmit, use="complete.obs")
cor(s2$Admit, s2$Success, use="complete.obs")
cor(s2$ProbAdmit, s2$Success, use="complete.obs")
alpha(data.frame(s2$Success, s2$ProbAdmit, s2$Admit))

#given high internal reliability, standardize and combine judgments
s2$Success.z <- scale(s2$Success)
s2$ProbAdmit.z <- scale(s2$ProbAdmit)
s2$Admit.z <- scale(s2$Admit)

s2$Success.z[is.na(s2$Success.z)] <- 0
s2$ProbAdmit.z[is.na(s2$ProbAdmit.z)] <- 0
s2$Admit.z[is.na(s2$Admit.z)] <- 0
s2$eval.z <- (s2$Admit.z+s2$ProbAdmit.z+s2$Success.z)/3

#descriptive statistics
#----------------------------
describe.by(s2$eval.z,s2$Performance)
describe.by(s2$eval.z,s2$Situation)
boxplot(eval.z~Performance*Situation,data=s2,las=2) #graphical summary of means of the 6 cells

#ANOVAs
#----------------------------
library(ez)

#overall assessment 
eval.aov = ezANOVA( 
    data = s2 
    , dv = .(eval.z) 
    , wid = .(Participant) 
    , within = .(Performance,Situation)
) 
print(eval.aov)

#probability of admission
ProbAdmit.aov = ezANOVA(
    data = s2 
    , dv = .(ProbAdmit) 
    , wid = .(Participant) 
    , within = .(Performance,Situation)
)
print(ProbAdmit.aov)

#rated success
Success.aov = ezANOVA(
    data = s2 
    , dv = .(Success) 
    , wid = .(Participant) 
    , within = .(Performance,Situation)
)
print(Success.aov)

#Regressions
#----------------------------
s2.glm <- glm(s2$Admit ~ s2$Sit.q+s2$Per.q, family=binomial(link="logit"))
summary(s2.glm)

#plots
#----------------------------
s2.plot <- ezPlot(data=s2, dv=.(eval.z), wid=.(Participant), within=.(Performance,Situation), x=.(Situation), do_lines=FALSE)
print(s2.plot)