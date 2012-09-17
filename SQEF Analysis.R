library(plyr)
library(reshape2)
library(psych)
library(ggplot2)
library(sciplot)

####### ---------------------------------
#######  Data Prep
####### ---------------------------------

# load data
sqef.a <- read.csv("http://samswift.org/data/SQEF-data-a-2012-09-07.csv", stringsAsFactors=F)
sqef.b <- read.csv("http://samswift.org/data/SQEF-data-b-2012-09-07.csv", stringsAsFactors=F)
nfl.hist <- read.csv("http://samswift.org/data/SQEF-nfl-division-winners-2002-2011.csv", stringsAsFactors=F)
book.odds <- read.csv("http://samswift.org/data/SQEF-book-odds-2012-09-05.csv", stringsAsFactors=F)

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
                     "fanNgames_1" = "fanNgames",
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
                         levels=c("repeat as division champion", 
                                  "lose their division champion title to another team"),
                         labels=c("Status Quo","Change"))
sqef$sourceCond <- factor(sqef$sourceCond,
                          levels=c("by an NFL expert",
                                   "randomly by a computer"),
                          labels=c("Expert","Random"))

sqef$gender     <- factor(sqef$gender,
                          levels=c(1,2),
                          labels=c("Male","Female"))

sqef$edu        <- factor(sqef$edu, ordered=TRUE, levels=seq(1:8),
                          labels=c("< HS",
                                   "HS",
                                   "some college",
                                   "2yr college",
                                   "Bachelors",
                                   "Masters",
                                   "PhD",
                                   "JD/MD"))

####### ---------------------------------
#######  Calculated Variables
####### ---------------------------------

# time to complete study
sqef$start.time <- as.POSIXct(sqef$start.time)
sqef$end.time   <- as.POSIXct(sqef$end.time)
sqef$total.time <- -as.double(sqef$start.time - sqef$end.time) #number of seconds between start- and end- time

# reversing probabilities of status quo for the change frame condition
prob.cols <- c("pAE", "pAN", "pAS", "pAW", "pNE", "pNN", "pNS", "pNW")
sqef[sqef$frameCond == "Change", prob.cols] <- 100 - sqef[sqef$frameCond == "Change", prob.cols]

# number of forecasts made
sqef$nFcasts <- rowSums(!is.na(sqef[prob.cols]))

# mean probability assigned to 4 teams
sqef$pMean <- rowMeans(sqef[prob.cols], na.rm=T)

# log of number of games watched
sqef$logfanNgames <- sqef$fanNgames
sqef$logfanNgames <- log(sqef$logfanNgames)
sqef$logfanNgames[sqef$logfanNgames==-Inf] <- 0

####### ---------------------------------
#######  Exclusion Criteria
####### ---------------------------------

total.n <- nrow(sqef)
total.n.src <- table(sqef$src)

# quit before the page with instructions
length(sqef$id[is.na(sqef$intro)])

# did not make all 4 forecasts
length(sqef$id[sqef$nFcasts < 4])
excl.incomplete <- length(sqef$id[sqef$nFcasts < 4])

# time to complete study < 30 seconds
qplot(sqef$total.time, geom="histogram") + xlim(30,500)
excl.time <- length(sqef$id[sqef$total.time < 30])

# duplicate ip address
sqef <- sqef[order(sqef$start.time),]
sqef$ip.dup <- duplicated(sqef$ip)
length(sqef$id[sqef$ip.dup])
excl.dup <- length(sqef$id[sqef$ip.dup])

# subset based on exclusion rules
sqef <- subset(sqef,!is.na(sqef$intro))
sqef <- subset(sqef,sqef$nFcasts == 4)
sqef <- subset(sqef,sqef$total.time > 30)
sqef <- subset(sqef,!ip.dup)

post.excl.n <- nrow(sqef)

####### ---------------------------------
#######  Lottery Winner
####### ---------------------------------

# entrants <- subset(sqef, src=="reddit" & !is.na(contact))
# winner <- sample(rownames(entrants),1)
# entrants[winner,]

####### ---------------------------------
#######  Sample Characteristics
####### ---------------------------------

sqef$yob    <- sqef$yob+1919    #values start at 1920=1
sqef$age    <- 2012 - sqef$yob
describe(sqef$age)              # mean and sd of age

prop.table(table(sqef$gender))  # ratio of gender
prop.table(table(sqef$edu))     # table of educational attainment
length(unique(sqef$geo))        # number of US states represented
table(sqef$src)                 # recruitment source

# football fandom
ggplot(data=sqef, aes(x=fanLikert, fill=src)) + geom_density(alpha=.5, adjust=1.4)
ggplot(data=sqef, aes(x=fanNgames, fill=src)) + geom_density(alpha=.5)

####### ---------------------------------
#######  Composite measures
####### ---------------------------------
fan.measures <- sqef[c("logfanNgames","fanLikert", "src")]
fan.measures$src <- as.numeric(fan.measures$src)
alpha(fan.measures)
sqef$fandom <- rowMeans(fan.measures, na.rm=TRUE)

ggplot(sqef, aes(logfanNgames, fanLikert)) +
  geom_jitter(aes(color=src)) +
  geom_smooth(method="lm")

####### ---------------------------------
#######  Betting odds benchmark
####### ---------------------------------
book.probs       <- book.odds

# convert decimal odds to implied probabilities
book.probs[4:27] <- (1/book.odds[4:27]) * 100  

# average across all 24 sports books
book.probs$pMean <- rowMeans(book.probs[4:27])

# keep and refshape the probabilities for the status quo teams
book.probs.sq    <- subset(book.probs, champ.2011==1, 
                           select=c(division, pMean))
book.probs.sq    <- arrange(book.probs.sq, division)
rownames(book.probs.sq) <- book.probs.sq$division
book.probs.sq$division <- NULL
book.probs.sq <- t(book.probs.sq)
book.probs.sq.mat <- matrix(book.probs.sq, nrow=nrow(sqef),ncol=ncol(book.probs.sq), byrow=T)

# calculate individual deviation from book probabilities
book.dev <- sqef[prob.cols] - book.probs.sq.mat
colnames(book.dev) <- paste("dev",prob.cols,sep="")
book.dev$devMean <- rowMeans(book.dev, na.rm=T)
sqef <- cbind(sqef,book.dev)

# check relationship between book odds and participant probs
sqef.probs.sq <- colMeans(sqef[prob.cols],na.rm=T)
plot(book.probs.sq, sqef.probs.sq ,xlim=c(0,100), ylim=c(0,100))
cor(as.vector(book.probs.sq), as.vector(sqef.probs.sq))

####### ---------------------------------
#######  Status-quo base rate in NFL since 2002
####### ---------------------------------
# format data.frame of division champs
rownames(nfl.hist) <- nfl.hist$Season
nfl.hist$Season <- NULL

# create empty logical matrix of repeat winners
nfl.sq <- matrix(nrow=nrow(nfl.hist),
                 ncol=ncol(nfl.hist),
                 dimnames=list(rownames(nfl.hist), colnames(nfl.hist)))

# for each cell, test if it is equal to the same division a year earlier
for(div in colnames(nfl.hist)){
  for(year in rownames(nfl.hist)){
    nfl.sq[year,div] <- nfl.hist[year,div] == nfl.hist[as.character(as.integer(year)-1),div]
  } 
}

# tally the results
prop.sq <- prop.table(table(nfl.sq))["TRUE"]

t.sq.hist <- t.test(sqef$pMean,mu=prop.sq)

####### ---------------------------------
#######  Hypothesis Tests
####### ---------------------------------

# do source and frame manipulations predict status quo forecasts?
summary(aov(pMean ~ sourceCond*frameCond, data=sqef))
bargraph.CI(data=sqef, x.factor=frameCond, response=pMean, group=sourceCond, 
            legend=T, ylim=c(0,100), 
            ylab="Probability of Status Quo", xlab="Framing Condition")

# do source and frame manipulations predict devation from book odds of status quo?
summary(aov.dev <- aov(devMean ~ sourceCond*frameCond, data=sqef))
bargraph.CI(data=sqef, x.factor=frameCond, response=devMean, group=sourceCond, 
            legend=T, 
            ylab="Deviation from Book Status Quo", xlab="Framing Condition")
t.devmean.sq <- t.test(sqef$devMean[sqef$frameCond=="Status Quo"],mu=0)
t.devmean.ch <- t.test(sqef$devMean[sqef$frameCond=="Change"],mu=0)


# mean fcast per division
colMeans(sqef[prob.cols], na.rm=T) - 
head(book.probs.sq,n=1)

#expertise and status quo endoresment
summary(lm(pMean ~ fandom + frameCond * sourceCond, data=sqef))

## melted for mixed-model analysis
sqef.melt <- melt(sqef, id.vars = c("id","sourceCond","frameCond","src"), 
                  measure.vars  = prob.cols, 
                  variable.name = "division",
                  value.name    = "probability",
                  na.rm         = TRUE)




