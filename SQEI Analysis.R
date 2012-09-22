library(foreign)
library(sciplot)
library(plyr)
library(reshape2)


# load data
sqei <- read.spss("http://samswift.org/data/SQEI-data-2012-09-22.sav",use.value.labels=T,to.data.frame=T,use.missings=T)
str(sqei)

# rename experimental condition factors
sqei <- rename(sqei, c("sqch"="frameCond", "posneg"="negCond"))

# add participant ID
sqei$id <- seq(1:nrow(sqei))

# plot and test effects of two experimental manipulations
bargraph.CI(negCond,probchan,group=frameCond,data=sqei,legend=T)
summary(aov(probchan ~ negCond * frameCond + avgconf, data=sqei))

# subset and test only in the 'positive' condition
sqei.pos <- subset(sqei,negCond=="positive")
bargraph.CI(frameCond,probchan,data=sqei.pos,legend=T)
summary(aov(probchan ~ frameCond, data=sqei))

# subset and test only in the subset of participants who reported an average confidence > 2
sqei.exp <- subset(sqei, avgconf > 2)
bargraph.CI(negCond,probchan,group=frameCond,data=sqei.exp,legend=T)
summary(aov(probchan ~ negCond * frameCond, data=sqei.exp))

#### reshape data into a long format for repeated measures analysis

# identify data columns
responses <- c(paste("ans",seq(1:8),sep=""), paste("conf",seq(1:8),sep=""))

# melt data frame
sqei.melt <- melt(data=sqei, id.vars=c("id","frameCond","negCond"), measure.vars=responses)

# break out item (1:8) and response type (answer, confidence) factors from combined identifier
sqei.melt$variable <- as.character(sqei.melt$variable)
sqei.melt$item <- substr(sqei.melt$variable,start=nchar(sqei.melt$variable),stop=nchar(sqei.melt$variable))
sqei.melt$resp.type <- substr(sqei.melt$variable,start=1,stop=nchar(sqei.melt$variable)-1)
sqei.melt$variable <- NULL

# cast into long format
sqei.long <- dcast(sqei.melt, id + item + frameCond + negCond ~ resp.type, value.var="value")

# test repeated measures mixed model
summary(aov(ans ~ item + conf + frameCond * negCond + Error(id), data=sqei.long))
