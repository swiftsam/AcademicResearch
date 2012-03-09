library(plyr)
library(tm)

#ABS Text analysis

####### ---------------------------------
#######  Data Prep
####### ---------------------------------

#load data
ABS2 <- read.csv("http://swift.cbdr.cmu.edu/data/ABS2-data-2012-02-24.csv", stringsAsFactors=F)

#data prep
source("ABS2 Data Prep.R")

table(ABS2$RoleCond)

####### ---------------------------------
#######  Create Corpus
####### ---------------------------------

#Free response of salient attributes
corpSal.B <- Corpus(DataframeSource(data.frame(ABS2[ABS2$RoleCond=="Buyer", "AttrSalFre"])))
corpSal.B <- tm_map(corpSal.B, removePunctuation)
corpSal.B <- tm_map(corpSal.B, tolower)
corpSal.B <- tm_map(corpSal.B, function(x) removeWords(x, stopwords("english")))
corpSal.B.tdm <- TermDocumentMatrix(corpSal.B)

m.B <- as.matrix(corpSal.B.tdm)
v.B <- sort(rowSums(m.B),decreasing=TRUE)
d.B <- data.frame(word = names(v.B),freq=v.B)

corpSal.S <- Corpus(DataframeSource(data.frame(ABS2[ABS2$RoleCond=="Seller", "AttrSalFre"])))
corpSal.S <- tm_map(corpSal.S, removePunctuation)
corpSal.S <- tm_map(corpSal.S, tolower)
corpSal.S <- tm_map(corpSal.S, function(x) removeWords(x, stopwords("english")))
corpSal.S.tdm <- TermDocumentMatrix(corpSal.S)

m.S <- as.matrix(corpSal.S.tdm)
v.S <- sort(rowSums(m.S),decreasing=TRUE)
d.S <- data.frame(word = names(v.S),freq=v.S)

d <- merge(d.B,d.S,all.x=T, all.y=T,by="word")
names(d)<- c("word","freqB","freqS")
d[is.na(d)] <- 0
d$freqDiff <- d$freqB-d$freqS
d$freqSum <- d$freqB + d$freqS
d <- arrange(d,freqDiff)
d15 <- subset(d,freqSum > 15)




