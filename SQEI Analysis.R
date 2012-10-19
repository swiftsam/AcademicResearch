library(foreign)
library(sciplot)
library(plyr)
library(reshape2)
library(ggplot2)
library(psych)

# load data
sqei <- read.spss("http://samswift.org/data/SQEI-data-2012-09-22.sav",use.value.labels=T,to.data.frame=T,use.missings=T)
str(sqei)

# re-map Ilana's labelling of the frame conditions to what makes sense to me
sqei$frameCond <- NA
sqei$frameCond[sqei$sqch == "Change" & sqei$posneg == "positive"] <- "change"
sqei$frameCond[sqei$sqch == "Change" & sqei$posneg == "negative"] <- "statusquo"
sqei$frameCond[sqei$sqch == "SQ"     & sqei$posneg == "positive"] <- "statusquo"
sqei$frameCond[sqei$sqch == "SQ"     & sqei$posneg == "negative"] <- "change"
sqei$frameCond <- factor(sqei$frameCond)

# copy positive/negative condition onto new variable with consistent name
sqei$negCond <-  sqei$posneg

# add participant ID
sqei$id <- seq(1:nrow(sqei))

# convert all probabilities to prob of status quo
# Change + positive  : reverse
# Change +  negative : keep
# SQ + positive : keep
# SQ + negative : reverse

prob.cols <- paste("ans",seq(1:8),sep="")

sq <- sqei[prob.cols]
names(sq) <- paste("pSQ",seq(1:8),sep="")

change.pos.rows <- which(sqei$frameCond=="change"    & sqei$negCond=="positive")
sq.neg.rows     <- which(sqei$frameCond=="statusquo" & sqei$negCond=="negative")

sq[c(change.pos.rows, sq.neg.rows),] <- 100-sq[c(change.pos.rows, sq.neg.rows),]
sq$meanpSQ <- rowMeans(sq)
sqei <- cbind(sqei,sq)

# plot and test effects of two experimental manipulations
bargraph.CI(frameCond,meanpSQ,group=negCond,data=sqei,
            legend=T, 
            ylim=c(0,100), 
            ylab="Mean probability of status quo", xlab="Frame Condition")
summary(aov(meanpSQ ~ negCond * frameCond + avgconf, data=sqei))

# subset and test only in the 'positive' condition
sqei.pos <- subset(sqei,negCond=="positive")
bargraph.CI(frameCond,meanpSQ,data=sqei.pos,legend=T)
summary(aov(meanpSQ ~ frameCond, data=sqei))

# subset and test only in the subset of participants who reported an average confidence > 2
sqei.exp <- subset(sqei, avgconf > 1)
bargraph.CI(negCond,meanpSQ,group=frameCond,data=sqei.exp,legend=T)
summary(aov(meanpSQ ~ negCond * frameCond, data=sqei.exp))

#### reshape data into a long format for repeated measures analysis

# identify data columns
responses <- c(paste("pSQ",seq(1:8),sep=""), paste("conf",seq(1:8),sep=""))

# melt data frame
sqei.melt <- melt(data=sqei, id.vars=c("id","frameCond","negCond"), measure.vars=responses)

# break out item (1:8) and response type (answer, confidence) factors from combined identifier
sqei.melt$variable  <- as.character(sqei.melt$variable)
sqei.melt$item      <- substr(sqei.melt$variable,
                              start=nchar(sqei.melt$variable),
                              stop=nchar(sqei.melt$variable))
sqei.melt$item      <- factor(sqei.melt$item)
sqei.melt$resp.type <- substr(sqei.melt$variable,
                              start=1,
                              stop=nchar(sqei.melt$variable)-1)
sqei.melt$variable  <- NULL

# cast into long format
sqei.long <- dcast(sqei.melt, id + item + frameCond + negCond ~ resp.type, value.var="value")

# test repeated measures mixed model
summary(aov(pSQ ~ frameCond * negCond + conf + Error(id + item), data=sqei.long))
summary(lm(pSQ ~ frameCond * negCond + conf, data=sqei.long))

describeBy(x=sqei.long$pSQ, list(sqei.long$frameCond, sqei.long$negCond))
ggplot(sqei.long, aes(item,pSQ)) + geom_boxplot(aes(color=negCond))

# test repeated measures mixed model on subset where expertise was > 1
prop.table(table(sqei.long$conf))
sqei.long.exp <- subset(sqei.long, conf > 1)
summary(aov(pSQ ~ conf + frameCond * negCond + Error(id + item), data=sqei.long.exp))


