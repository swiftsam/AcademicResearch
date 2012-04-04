library(plyr)
library(tm)
library(ggplot2)

#ABS Text analysis

####### ---------------------------------
#######  Data Prep
####### ---------------------------------

#load and prep ABS2 data
ABS2 <- read.csv("http://swift.cbdr.cmu.edu/data/ABS2-data-2012-02-24.csv", stringsAsFactors=F)
source("ABS2 Data Prep.R")
table(ABS2$RoleCond)

#load SVA analyses for rated word value
source("SVA Analysis.R")

####### ---------------------------------
#######  Functions
####### ---------------------------------

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
freqDiff <- function(x,y, by="word", minFreq = 0){
  df             <- merge(x,y,all.x=T, all.y=T,by=by)
  names(df)      <- c(by,"freqX","freqY")
  df[is.na(df)]  <- 0
  df$freqDiff    <- df$freqX-df$freqY
  df$freqSum     <- df$freqX + df$freqY
  df$relFreqDiff <- df$freqDiff/df$freqSum
  df             <- arrange(df,relFreqDiff)
  dfMin          <- subset(df,freqSum > minFreq)
}

#function to compute topic frequency given word frequency and wordValueMap
topicFreq <- function(wf, wvm){
  tf <- ddply(wvm, "code", function(df){
    tf <- 0
    for(word in df$word){
      freq <- wf[word,"freq"]
      if(is.na(freq)){ freq <- 0}
      tf <- tf + freq
    }
    "TopicFreq" <- tf
  })
  names(tf) <- c("topic","freq")
  return(tf)
}

#function to compute sum topic value of a set
topicValue <- function(df,wvm){
  
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
#######  Convert Word Frequency to Topic Frequency
####### ---------------------------------

tf.Arg.B <- topicFreq(wf.Arg.B, wordValueMap)
tf.Arg.S <- topicFreq(wf.Arg.S, wordValueMap)

tf.Sal.B <- topicFreq(wf.Sal.B, wordValueMap)
tf.Sal.S <- topicFreq(wf.Sal.S, wordValueMap)

####### ---------------------------------
#######  Compare Word and Topic Frequency by Role
####### ---------------------------------

#Buyer-Seller differences in salient attributes
wfDiff.BS.Sal <- freqDiff(wf.Sal.B, wf.Sal.S, minFreq=15)

#Buyer-Seller differences in Argument words
wfDiff.BS.Arg <- freqDiff(wf.Arg.B, wf.Arg.S, minFreq=15)

#Buyer-Seller differences in Salient Topics
tfDiff.BS.Sal <- freqDiff(tf.Sal.B, tf.Sal.S, by="topic", minFreq=5)

#Buyer-Seller differences in Argument Topics
tfDiff.BS.Arg <- freqDiff(tf.Arg.B, tf.Arg.S, by="topic", minFreq=5)

####### ---------------------------------
#######  Match Word and Topic Frequency to coded values
####### ---------------------------------

wfDiff.BS.Arg <- merge(wfDiff.BS.Arg,wordValueMap,all.x=T)
wfDiff.BS.Sal <- merge(wfDiff.BS.Sal,wordValueMap,all.x=T)

tfDiff.BS.Arg <- merge(tfDiff.BS.Arg,ratings[c("mean","code")],by.x="topic",by.y="code",all.x=T)
tfDiff.BS.Sal <- merge(tfDiff.BS.Sal,ratings[c("mean","code")],by.x="topic",by.y="code",all.x=T)

####### ---------------------------------
#######  Plots
####### ---------------------------------

#Relative Word Freq in Argument
ggplot(wfDiff.BS.Arg, aes(reorder(factor(word),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=as.numeric(rating))) + 
  scale_fill_continuous(limits=c(-1.75, 1.75), low="red",high="green",name="Rated Value") +
  coord_flip() +
  scale_y_continuous(limits=c(-1,1)) +
  xlab("Words") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  opts(title="Relative word use in Argument by Role")

#Relative Topic Freq in Argument
ggplot(tfDiff.BS.Arg, aes(reorder(factor(topic),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=as.numeric(mean))) +
  scale_fill_continuous(limits=c(-1.75, 1.75),low="red",high="green",name="Rated Value") +
  coord_flip() +
  scale_y_continuous(limits=c(-1,1)) +
  xlab("Topics") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  opts(title="Relative Topic use in Argument by Role")

#Relative Word Freq in Salience
ggplot(wfDiff.BS.Sal, aes(reorder(factor(word),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=as.numeric(rating))) +
  scale_fill_continuous(limits=c(-1.75, 1.75), low="red",high="green",name="Rated Value") +
  coord_flip() +
  scale_y_continuous(limits=c(-1,1)) +
  xlab("Words") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  opts(title="Relative word use in 'Most Memorable Details' by Role")

#Relative Topic Freq in Salience
ggplot(tfDiff.BS.Sal, aes(reorder(factor(topic),relFreqDiff),relFreqDiff)) + 
  geom_bar(aes(fill=as.numeric(mean))) +
  scale_fill_continuous(limits=c(-1.75, 1.75),low="red",high="green",name="Rated Value") +
  coord_flip() +
  scale_y_continuous(limits=c(-1,1)) +
  xlab("Topics") +
  ylab("Relative use by Sellers(-) & Buyers(+)") +
  opts(title="Relative Topic use in 'Most Memorable Details' by Role")
