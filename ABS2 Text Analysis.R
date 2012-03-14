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

#Free response of salient attributes (Buyer)
corpSal.B <- Corpus(DataframeSource(data.frame(ABS2[ABS2$RoleCond=="Buyer", "AttrSalFre"])))
corpSal.B <- tm_map(corpSal.B, removePunctuation)
corpSal.B <- tm_map(corpSal.B, tolower)
corpSal.B <- tm_map(corpSal.B, function(x) removeWords(x, stopwords("english")))
corpSal.B.tdm <- TermDocumentMatrix(corpSal.B)

m.B <- as.matrix(corpSal.B.tdm)
v.B <- sort(rowSums(m.B),decreasing=TRUE)
d.B <- data.frame(word = names(v.B),freq=v.B)

#Free response of salient attributes (Seller)
corpSal.S <- Corpus(DataframeSource(data.frame(ABS2[ABS2$RoleCond=="Seller", "AttrSalFre"])))
corpSal.S <- tm_map(corpSal.S, removePunctuation)
corpSal.S <- tm_map(corpSal.S, tolower)
corpSal.S <- tm_map(corpSal.S, function(x) removeWords(x, stopwords("english")))
corpSal.S.tdm <- TermDocumentMatrix(corpSal.S)

m.S <- as.matrix(corpSal.S.tdm)
v.S <- sort(rowSums(m.S),decreasing=TRUE)
d.S <- data.frame(word = names(v.S),freq=v.S)

#Buyer-Seller differences in salient attributes
dsa <- merge(d.B,d.S,all.x=T, all.y=T,by="word")
names(dsa)<- c("word","freqB","freqS")
dsa[is.na(dsa)] <- 0
dsa$freqDiff <- dsa$freqB-dsa$freqS
dsa$freqSum <- dsa$freqB + dsa$freqS
dsa$relFreqDiff <- dsa$freqDiff/dsa$freqSum
dsa <- arrange(dsa,relFreqDiff)
dsa15 <- subset(dsa,freqSum > 15)


#Arguments (Buyer)
corpArg.B <- Corpus(DataframeSource(data.frame(ABS2[ABS2$ArgCond=="Arg" & ABS2$RoleCond=="Buyer", "Arg"])))
corpArg.B <- tm_map(corpArg.B, removePunctuation)
corpArg.B <- tm_map(corpArg.B, tolower)
corpArg.B <- tm_map(corpArg.B, function(x) removeWords(x, stopwords("english")))
corpArg.B.tdm <- TermDocumentMatrix(corpArg.B)

m.cAB <- as.matrix(corpArg.B.tdm)
v.cAB <- sort(rowSums(m.cAB),decreasing=TRUE)
d.cAB <- data.frame(word = names(v.cAB),freq=v.cAB)

#Arguments (Seller)
corpArg.S <- Corpus(DataframeSource(data.frame(ABS2[ABS2$ArgCond=="Arg" & ABS2$RoleCond=="Seller", "Arg"])))
corpArg.S <- tm_map(corpArg.S, removePunctuation)
corpArg.S <- tm_map(corpArg.S, tolower)
corpArg.S <- tm_map(corpArg.S, function(x) removeWords(x, stopwords("english")))
corpArg.S.tdm <- TermDocumentMatrix(corpArg.S)

m.cAS <- as.matrix(corpArg.S.tdm)
v.cAS <- sort(rowSums(m.cAS),decreasing=TRUE)
d.cAS <- data.frame(word = names(v.cAS),freq=v.cAS)

#Buyer-Seller differences in arguments
darg <- merge(d.cAB,d.cAS,all.x=T, all.y=T,by="word")
names(darg)<- c("word","freqB","freqS")
darg[is.na(darg)] <- 0
darg$freqDiff <- darg$freqB-darg$freqS
darg$freqSum <- darg$freqB + darg$freqS
darg$relFreqDiff <- darg$freqDiff/darg$freqSum
darg <- arrange(darg,relFreqDiff)
darg15 <- subset(darg,freqSum > 15)


library(ggplot2)

ggplot(darg15, aes(reorder(factor(word),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=freqSum)) + 
  coord_flip() +
  xlab("Words") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  opts(title="Relative word use in Argument by Role")

ggplot(dsa15, aes(reorder(factor(word),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=freqSum)) + 
  coord_flip() +
  xlab("Words") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  opts(title="Relative word use in 'Most Memorable Details' by Role")

