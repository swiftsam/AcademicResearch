library(plyr)
library(psych)
library(ggplot2)
library(sciplot)

####### ---------------------------------
#######  Data Prep
####### ---------------------------------

# load data
sqef.a <- read.csv("http://samswift.org/data/SQEF-data-a-2012-09-07.csv", stringsAsFactors=F)
sqef.b <- read.csv("http://samswift.org/data/SQEF-data-b-2012-09-07.csv", stringsAsFactors=F)

# combine datasets
sqef.a <- rename(sqef.a, c("Q34"="Q38"))
sqef   <- rbind(sqef.a,sqef.b)
rm(sqef.a,sqef.b)

# set empty strings to NA
sqef[sqef==""] <- NA

# remove uncessary columns
sqef <- sqef[,!names(sqef) %in% c("V1","V2","V3","V4","V5","V7","V10")]

# rename unnamed columns
sqef <- rename(sqef, c("V6"="ip",
                     "V8"="start.time",
                     "V9"="end.time",
                     "frame"="frameCond",
                     "exp"="sourceCond",
                     "pAFCeast_1"="pAE",
                     "pAFCnorth_1"="pAN",
                     "Q17"="comments",
                     "Q38"="contact"))

# rename probability judgment columns somethign shorter
sqef <- rename(sqef, c("pAFCeast_1"  = "pAE",
                       "pAFCnorth_1" = "pAN",
                       "pAFCsouth_1" = "pAS",
                       "pAFCwest_1"  = "pAW",
                       "pNFCeast_1"  = "pNE",
                       "pNFCnorth_1" = "pNN",
                       "pNFCsouth_1" = "pNS",
                       "pNFCwest_1"  = "pNW"))

# add simple IDs for each record
sqef$id <- factor(c(1:nrow(sqef)))

# make factors
sqef$src <- factor(sqef$src, levels=c("rnfl","mturk"),
                   labels=c("reddit","mturk"))
sqef$frameCond  <- factor(sqef$frameCond, 
                         labels=c("repeat as division champion" = "Status Quo",
                                  "lose their division champion title to another team" = "Change"))
sqef$sourceCond <- factor(sqef$sourceCond,
                          labels=c("by an NFL expert" = "Expert",
                                   "randomly by a computer" = "Random"))

sqef$gender     <- factor(sqef$gender, 
                          labels=c("1"="Male","2"="Female"))

sqef$edu        <- factor(sqef$edu, ordered=TRUE,
                          labels=c("1" = "< HS",
                                   "2" = "HS",
                                   "3" = "some college",
                                   "4" = "2yr college",
                                   "5" = "Bachelors",
                                   "6" = "Masters",
                                   "7" = "PhD",
                                   "8" = "JD/MD"))

####### ---------------------------------
#######  Calculated Variables
####### ---------------------------------

# time to complete study
sqef$start.time <- as.POSIXct(sqef$start.time)
sqef$end.time   <- as.POSIXct(sqef$end.time)
sqef$total.time <- -as.double(sqef$start.time - sqef$end.time) #number of seconds between start- and end- time

#reversing probabilities of status quo for the change frame condition
prob.cols <- c("pAE", "pAN", "pAS", "pAW", "pNE", "pNN", "pNS", "pNW")
sqef[sqef$frameCond == "Change", prob.cols] <- 100 - sqef[sqef$frameCond == "Change", prob.cols]

#mean probability assigned to 4 teams
sqef$pMean <- rowMeans(sqef[prob.cols], na.rm=T)

####### ---------------------------------
#######  Exclusion Criteria
####### ---------------------------------

# quit before the page with instructions
length(sqef$id[is.na(sqef$intro)])
sqef <- subset(sqef,!is.na(sqef$intro))

# time to complete study < 30 seconds
qplot(sqef$total.time, geom="histogram") + xlim(30,500)
length(sqef$id[sqef$total.time < 30])
sqef <- subset(sqef,sqef$total.time > 30)

#duplicate ip address
sqef <- sqef[order(sqef$start.time),]
sqef$ip.dup <- duplicated(sqef$ip)
length(sqef$id[sqef$ip.dup])
sqef <- subset(sqef,!ip.dup)

####### ---------------------------------
#######  Lottery Winner
####### ---------------------------------

#
entrants <- subset(sqef, src=="reddit" & !is.na(contact))
winner <- sample(rownames(entrants),1)
entrants[winner,]

####### ---------------------------------
#######  Sample Characteristics
####### ---------------------------------

sqef$yob    <- sqef$yob+1919    #values start at 1920=1
sqef$age    <- 2012 - sqef$yob
describe(sqef$age)              # mean and sd of age

table(sqef$gender)              # ratio of gender
table(sqef$edu)                 # table of educational attainment
length(unique(sqef$geo))        # number of US states represented
table(sqef$src)                 # recruitment source

# football fandom
ggplot(data=sqef, aes(x=fanLikert, fill=src)) + geom_density(alpha=.5, adjust=1.4)
ggplot(data=sqef, aes(x=fanNgames_1, fill=src)) + geom_density(alpha=.5)

####### ---------------------------------
#######  Hypothesis Tests
####### ---------------------------------

#status quo prob X 
aov <- aov(pMean ~ sourceCond*frameCond, data=sqef)
bargraph.CI(data=sqef, x.factor=frameCond, response=pMean, group=sourceCond, 
            legend=T, ylim=c(0,100), 
            ylab="Probability of Status Quo", xlab="Framing Condition")





