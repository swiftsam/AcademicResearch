library(plyr)
library(psych)

####### ---------------------------------
#######  Data Prep
####### ---------------------------------

#load data
sva <- read.csv("http://swift.cbdr.cmu.edu/data/SVA-data-2012-03-21.csv", stringsAsFactors=F)

#set empty strings to NA
sva[sva==""] <- NA

#remove uncessary columns
sva <- sva[,!names(sva) %in% c("V1","V2","V3","V4","V5","V7","V10")]

#rename unnamed columns
sva <- rename(sva, c("V6"="IP","V8"="StartTime","V9"="EndTime","X186k_miles"="186k_miles","X190_2.3"="model_190_2.3","X1991"="year_1991"))

#add simple IDs for each record
sva$ID <- factor(c(1:nrow(sva)))

ratings <- as.data.frame(t(sapply(sva[4:27],describe)))
