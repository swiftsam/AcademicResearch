library(plyr)
library(tm)
library(ggplot2)

#ABS Text analysis

####### ---------------------------------
#######  Data Prep
####### ---------------------------------

#load data
ABS2 <- read.csv("http://swift.cbdr.cmu.edu/data/ABS2-data-2012-02-24.csv", stringsAsFactors=F)

#data prep
source("ABS2 Data Prep.R")

table(ABS2$RoleCond)

#function to calculate word frequency in a set
wordFreqList <- function(df){
  corp <- Corpus(VectorSource(df))
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, tolower)
  corp <- tm_map(corp, function(x) removeWords(x, stopwords("english")))
  corp.tdm <- TermDocumentMatrix(corp) 
  
  m <- as.matrix(corp.tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
}

#function to compare word frequency between two sets
wordFreqDiff <- function(x,y, minFreq = 0){
  df             <- merge(x,y,all.x=T, all.y=T,by="word")
  names(df)      <- c("word","freqX","freqY")
  df[is.na(df)]  <- 0
  df$freqDiff    <- df$freqX-df$freqY
  df$freqSum     <- df$freqX + df$freqY
  df$relFreqDiff <- df$freqDiff/df$freqSum
  df             <- arrange(df,relFreqDiff)
  dfMin          <- subset(df,freqSum > minFreq)
}


####### ---------------------------------
#######  Generate Word Frequency Lists
####### ---------------------------------

#All free response of salient attributes
wf.Sal <- wordFreqList(ABS2[,"AttrSalFre"])

#Free response of salient attributes (Buyer)
wf.Sal.B <- wordFreqList(ABS2[ABS2$RoleCond=="Buyer", "AttrSalFre"])

#Free response of salient attributes (Seller)
wf.Sal.S <- wordFreqList(ABS2[ABS2$RoleCond=="Seller", "AttrSalFre"])

#All Arguments
wf.Arg <- wordFreqList(ABS2[ABS2$ArgCond=="Arg","Arg"])

#Buyer Arguments
wf.Arg.B <- wordFreqList(ABS2[ABS2$ArgCond=="Arg" & ABS2$RoleCond=="Buyer","Arg"])

#Seller Arguments
wf.Arg.S <-wordFreqList(ABS2[ABS2$ArgCond=="Arg" & ABS2$RoleCond=="Seller","Arg"])

####### ---------------------------------
#######  Compare Word Frequency by Role
####### ---------------------------------

#Buyer-Seller differences in salient attributes
wfDiff.BS.Sal <- wordFreqDiff(wf.Sal.B, wf.Sal.S, 15)

#Buyer-Seller differences in arguments
wfDiff.BS.Arg <- wordFreqDiff(wf.Arg.B, wf.Arg.S, 15)

####### ---------------------------------
#######  Plots
####### ---------------------------------

ggplot(wfDiff.BS.Sal, aes(reorder(factor(word),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=log(freqSum))) + 
  coord_flip() +
  xlab("Words") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  opts(title="Relative word use in 'Most Memorable Details' by Role")

ggplot(wfDiff.BS.Arg, aes(reorder(factor(word),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=log(freqSum))) + 
  coord_flip() +
  xlab("Words") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  opts(title="Relative word use in Argument by Role")

