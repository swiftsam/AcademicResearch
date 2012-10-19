library(plyr)
library(tm)
library(ggplot2)

#CSC Text analysis

####### ---------------------------------
#######  Data Prep
####### ---------------------------------

#csc <- read.csv("http://samswift.org/data/CSC-data-2011-12-01.csv", stringsAsFactors=FALSE)

#set empty strings to NA
#csc[csc==""] <- NA

#rename variables and delete uneeded columns
#source("CSC Data Prep.R")

####### ---------------------------------
#######  Functions
####### ---------------------------------

#function to calculate word frequency in a set
wordFreq <- function(vec,likelihood=TRUE){
  corp <- Corpus(VectorSource(vec))
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, tolower)
  corp <- tm_map(corp, function(x) removeWords(x, stopwords("english")))
  corp.tdm <- TermDocumentMatrix(corp) 
  
  m <- as.matrix(corp.tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  if(likelihood){
    l <- v/length(vec)
    d <- data.frame(word = names(v),freq=v,likelihood=l)       
  } else {
    d <- data.frame(word = names(v),freq=v)
  }
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


#function to compute the typicality of a argument for its role
argTypicality <- function(arg,wf.Arg.role){
  wf.Arg <- wordFreq(arg,likelihood=FALSE)
  argTyp <- merge(wf.Arg, wf.Arg.role[c("word","likelihood")], by="word",all.x=T,all.y=T)
  argTyp[is.na(argTyp)]<-0
  names(argTyp) <- c("word","observed","expected")
  typ <- cor(argTyp$observed, argTyp$expected)
}

####### ---------------------------------
#######  Generate Word Frequency Lists
####### ---------------------------------

#Arguments by Free
wf.Arg.Free <- wordFreq(csc[csc$ArgCond=="Free","Arg"])

#Arguments by OwnLow
wf.Arg.OwnLow <- wordFreq(csc[csc$ArgCond=="OwnLow","Arg"])

#Arguments by OwnBatna
wf.Arg.OwnBatna <- wordFreq(csc[csc$ArgCond=="OwnBatna","Arg"])

#Arguments by OtherBatna
wf.Arg.OtherBatna <- wordFreq(csc[csc$ArgCond=="OtherBatna","Arg"])

#Arguments by OtherHigh
wf.Arg.OtherHigh <- wordFreq(csc[csc$ArgCond=="OtherHigh","Arg"])


####### ---------------------------------
#######  Compare Word Frequency by Condition
####### ---------------------------------

#Free-OwnLow differences in salient attributes
wfDiff.FOL.Arg <- freqDiff(wf.Arg.Free, wf.Arg.OwnLow)


