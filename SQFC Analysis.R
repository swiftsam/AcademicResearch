library(plyr)
library(reshape2)
library(psych)
library(ggplot2)
library(sciplot)
library(lme4)

####### ---------------------------------
#######  Data Prep
####### ---------------------------------

# load data
sqfc <- read.csv("http://samswift.org/data/SQFC-data-2012-12-04.csv", stringsAsFactors=F, header=TRUE)
companies <- read.csv("http://samswift.org/data/SQFC-companies-2012-12-04.csv", stringsAsFactors=F, header=TRUE)

# set empty strings to NA
sqfc[sqfc==""] <- NA

# remove uncessary columns
sqfc <- sqfc[,!names(sqfc) %in% c("X.V1","V2","V3","V4","V5","V7","V10","SC0_0","SC0_1","SC0_2","X")]

# rename unnamed columns
sqfc <- rename(sqfc, c("V6"="ip",
                       "V8"="start.time",
                       "V9"="end.time"))

# rename probability judgment columns somethign shorter
sqfc <- rename(sqfc, c("fcast_1.1."  = "fc1",
                       "fcast_1.2."  = "fc2",
                       "fcast_1.3."  = "fc3",
                       "fcast_1.4."  = "fc4",
                       "fcast_1.5."  = "fc5",
                       "fcast_1.6."  = "fc6",
                       "fcast_1.7."  = "fc7",
                       "fcast_1.8."  = "fc8",
                       "fcast_1.9."  = "fc9",
                       "fcast_1.10." = "fc10"))

# add simple IDs for each record
sqfc$id <- factor(c(1:nrow(sqfc)))

# make factors
sqfc$FrameCond  <- factor(sqfc$FrameCond)
sqfc$SourceCond <- factor(sqfc$SourceCond)
table(sqfc$FrameCond, sqfc$SourceCond)

sqfc$gender     <- factor(sqfc$gender,
                          levels=c(1,2),
                          labels=c("Male","Female"))

sqfc$major     <- factor(sqfc$major,
                         levels=c(1,2),
                         labels=c("Business/Econ","Other"))

sqfc$reading   <- factor(sqfc$reading,
                         levels=c(1,2,3,4,5),
                         labels=c("< 1 hr","1-2 hr","3-5 hr","5-10 hr","> 10 hr"),
                         ordered = TRUE)

####### ---------------------------------
#######  Calculated Variables
####### ---------------------------------

# time to complete study
sqfc$start.time <- as.POSIXct(sqfc$start.time)
sqfc$end.time   <- as.POSIXct(sqfc$end.time)
sqfc$total.time <- -as.double(sqfc$start.time - sqfc$end.time) #number of seconds between start- and end- time

####### ---------------------------------
#######  Exclusion Criteria
####### ---------------------------------

# remove ids used for testing runs
testing.ids <- c("gjp_pdr","99999")
sqfc <- sqfc[!(sqfc$LabID %in% testing.ids),]

total.n <- nrow(sqfc)

# time to complete study
qplot(sqfc$total.time, geom="histogram")

####### ---------------------------------
#######  Sample Characteristics
####### ---------------------------------

describe(sqfc$age)              # mean and sd of age

prop.table(table(sqfc$gender))  # ratio of gender
prop.table(table(sqfc$reading)) # table of amount of reading the news
prop.table(table(sqfc$major))   # table of amount of reading the news

####### ---------------------------------
#######  Composite measures
####### ---------------------------------

# perspective taking scale, 7 items
# reverse items 2 and 5
sqfc$PersTaking_2 <- 8-sqfc$PersTaking_2
sqfc$PersTaking_5 <- 8-sqfc$PersTaking_5

persp.taking.cols <- paste("PersTaking_",1:7,sep="")

alpha(sqfc[persp.taking.cols])
sqfc$perstaking <- rowMeans(sqfc[persp.taking.cols], na.rm=TRUE)

####### ---------------------------------
#######  Analysis of forecasts
####### ---------------------------------

fcast.cols <- paste("fc",1:10,sep="")
id.cols    <- c("id","FrameCond","SourceCond")
fcasts     <- melt(sqfc[c(id.cols,fcast.cols)],
                   id.vars = id.cols,
                   measure.vars = fcast.cols,
                   variable.name = "company",
                   value.name = "fcast")

# reverse code forecasts of change to be equivalent to forecasts of the status quo
fcasts$fcast[fcasts$FrameCond == "Change"] <- 100 - fcasts$fcast[fcasts$FrameCond == "Change"]


fcast.company <- ddply(fcasts, c("company"), function(df){
                         c("mean.fcast" = mean(df$fcast),
                           "n.fcast" = nrow(df))
                       })
pt <- sqfc[c("id","perstaking")]

fcasts <- merge(fcasts,pt, by="id")

summary(lmer(fcast ~ FrameCond + SourceCond + perstaking + (1|id), fcasts))
summary(aov(fcast ~ FrameCond + SourceCond + perstaking, data=fcasts))


bargraph.CI(FrameCond, fcast, group=SourceCond, data=fcasts, ylim=c(0,100), legend=TRUE)




