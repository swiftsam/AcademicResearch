library(plyr)
library(psych)
library(ggplot2)

####### ---------------------------------
#######  Data Prep
####### ---------------------------------

#load data
sqef.a <- read.csv("http://samswift.org/data/SQEF-data-a-2012-09-07.csv", stringsAsFactors=F)
sqef.b <- read.csv("http://samswift.org/data/SQEF-data-b-2012-09-07.csv", stringsAsFactors=F)

#combine datasets
sqef.a <- rename(sqef.a, c("Q34"="Q38"))
sqef   <- rbind(sqef.a,sqef.b)
rm(sqef.a,sqef.b)

#set empty strings to NA
sqef[sqef==""] <- NA

#remove uncessary columns
sqef <- sqef[,!names(sqef) %in% c("V1","V2","V3","V4","V5","V7","V10")]

#rename unnamed columns
sqef <- rename(sqef, c("V6"="ip",
                     "V8"="start.time",
                     "V9"="end.time",
                     "Q17"="comments",
                     "Q39"="contact"))

#add simple IDs for each record
sqef$id <- factor(c(1:nrow(sqef)))
